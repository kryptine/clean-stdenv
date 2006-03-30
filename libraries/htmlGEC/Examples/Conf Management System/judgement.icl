implementation module judgement

import StdEnv, StdHtml, StdMaybe

OK :: Judgement
OK = (True,"")

instance + Judgement
where
	(+) j1 j2 = addJudgement j1 j2
	where
		addJudgement _  (False,j2) 	= (False,j2)
		addJudgement (False,j1) _ 	= (False,j1)
		addJudgement _ _ 			= OK


