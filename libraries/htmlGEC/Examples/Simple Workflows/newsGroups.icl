module newsGroups

//	In this example newsgroups are created and maintained
//	User 0 is the manager of the newsgroup who can create ne newgroups
//	All other users can subscribe to such a newsgroup, commit a message or read news

import StdEnv, StdHtml

derive gForm 	[]
derive gUpd 	[]

npersons 		= 5							// number of participants
storageKind 	= Session					// storage format
nmessage		= 5							// maximum number of messages to read from group

:: NewsGroups	:== [GroupName]				// list of newsgroup names
:: GroupName	:== String					// Name of the newsgroup
:: NewsGroup	:== [News]					// News stored in a news group
:: News			:== (Subscriber,Message)	// a message and the id of its publisher
:: Subscriber	:== Int						// the id of the publisher
:: Message		:== String					// the message
:: Subscriptions:== [Subscription]			// newsgroup subscriptions of user
:: Subscription	:== (GroupName,Index)		// last message read in corresponding group
:: Index		:== Int						// 0 <= index < length newsgroup 

Start world = doHtmlServer (multiUserTask npersons allTasks) world

allTasks = PmuTasks "newsGroups" [(0,repeatTask newsManager):[(i,repeatTask newsReader) \\ i <- [1 .. npersons - 1]]]

newsManager
=	CTask 	[("newGroup",  PCTask2 (addNewsGroup, STask "Cancel" Void))
			,("showGroup", showGroup)
			]
where
	addNewsGroup
	= 						[Txt "Define name of new news group:",Br,Br] ?>> STask "Define" ""
		=>> \newName	->	readNewsGroups
		=>> \oldNames	->	writeNewsGroups (removeDup (sort [newName:oldNames]))
		#>> returnV Void
	showGroup
	= readNewsGroups =>> \groups -> PDMenu groups #>> returnV Void

PDMenu list =	[] ?>> STask "OK" (PullDown (1,100) (0,[e \\ e <- list]))
				=>> \value	->	returnV (idx value,toString value)
where
	idx (PullDown _ (index,_)) = index

newsReader 
=	taskId
	*>> \me 		->	CTask 	[("subscribe",  PCTask2 (subscribeNewsGroup me, STask "Cancel" Void))
								,("readNews",   readNews me)]
where
	OK :: Task Void
	OK = STask "OK" Void

	subscribeNewsGroup :: Subscriber -> Task Void
	subscribeNewsGroup me
	= 						readNewsGroups
		=>> \groups		->	PDMenu groups
		=>> \(_,group)	->	addSubscription me (group,0)
		#>>					[Txt "You have subscribed to news group ", B [] group,Br,Br] ?>> OK

	readNews :: Subscriber -> Task Void
	readNews me
	= 						readSubscriptions me
		=>> \mygroups	->	PDMenu ([group \\ (group,_) <- mygroups] ++ ["noGroup"])
		=>> \(_,group)->	[Txt "You are looking at news group ", B [] group, Br, Br] ?>>
							PCTask2	( repeatTask (				readIndex me group
												  =>> \index ->	readNewsGroup group
												  =>> \news -> 	showNews index (news%(index,index+nmessage-1)) (length news) ?>>
														CTask 	[("<<",	readNextNewsItems me (group,index) (~nmessage) (length news))
																,("update",		returnV Void)
																,(">>",	readNextNewsItems me (group,index) nmessage (length news))
																,("commitNews",		commitItem group me)
																])
									, STask "leaveGroup" Void
									)

	readNextNewsItems :: Subscriber Subscription Int Int -> Task Void
	readNextNewsItems  me (group,index) offset length
	# nix = index + offset
	# nix = if (nix < 0) 0 (if (length <= nix) index nix)
	= 						addSubscription me (group,nix)
		#>> returnV  Void				 

	commitItem :: GroupName Subscriber -> Task Void
	commitItem group me 
	=								[Txt "Type your message ..."]
									?>>	STask "Commit" (TextArea 4 80 "") <<@ Submit
		=>>	\(TextArea _ _ val) -> 	readNewsGroup group
		=>> \news				->	writeNewsGroup group (news ++ [(me,val)])
		#>>							[Txt "Message commited to news group ",B [] group, Br,Br] ?>> OK


// displaying news groups

showNews ix news nrItems = [STable [] 	[[B [] "Issue:", B [] "By:", B [] "Contents:"]
								:[[Txt (showIndex nr),Txt (toString who),Txt (toString info)] 
									\\ nr <- [ix..] & (who,info) <- news]
								]]
where
	showIndex i	= toString (i+1) <+++ " of " <+++ nrItems
	
// reading and writing of storages

readNewsGroups :: Task NewsGroups
readNewsGroups = readDB newsGroupsId

writeNewsGroups :: NewsGroups -> Task NewsGroups
writeNewsGroups newgroups = writeDB newsGroupsId newgroups

readSubscriptions :: Subscriber -> Task Subscriptions
readSubscriptions me = readDB (readerId me)

writeSubscriptions :: Subscriber Subscriptions -> Task Subscriptions
writeSubscriptions me subscriptions = writeDB (readerId me) subscriptions

addSubscription :: Subscriber Subscription -> Task Subscriptions
addSubscription me (groupname,index)
# index	= if (index < 0) 0 index
=							readSubscriptions me
	=>> \subscriptions	->	writeSubscriptions me [(groupname,index):[(group,index) \\ (group,index) <- subscriptions | group <> groupname]]

readIndex :: Subscriber GroupName -> Task Index
readIndex me groupname
=							readSubscriptions me
	=>> \subscriptions	->	returnV (hd [index \\ (group,index) <- subscriptions | group == groupname])

readNewsGroup :: GroupName -> Task NewsGroup
readNewsGroup groupname = readDB (groupNameId groupname)

writeNewsGroup :: GroupName NewsGroup -> Task NewsGroup
writeNewsGroup groupname news = writeDB (groupNameId groupname) news

// iData database storage access utility functions

newsGroupsId		:==	"newsGroups"
readerId i			= 	"reader" <+++ i
groupNameId name	=	"NewsGroup-" +++ name

readDB 	name 		= appHSt (DB name id)
writeDB name value 	= appHSt (DB name (\_ -> value)) 

DB :: String (a -> a) *HSt -> (a,*HSt) | iData a
DB name fun hst 
# (form,hst)	= mkStoreForm (Init,nFormId name createDefault <@ storageKind) fun hst
= (form.value,hst)

