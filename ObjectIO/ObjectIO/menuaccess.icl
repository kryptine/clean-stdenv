implementation module menuaccess


//	Clean Object I/O library, version 1.2


import	menuhandle


menuStateHandleGetHandle :: !(MenuStateHandle .pst) -> (!OSMenu,!MenuStateHandle .pst)
menuStateHandleGetHandle msH=:(MenuLSHandle {mlsHandle={mHandle}}) = (mHandle,msH)

menuStateHandleGetMenuId :: !(MenuStateHandle .pst) -> (!Id,!MenuStateHandle .pst)
menuStateHandleGetMenuId msH=:(MenuLSHandle {mlsHandle={mMenuId}}) = (mMenuId,msH)

menuStateHandleGetOSMenuNr :: !(MenuStateHandle .pst) -> (!OSMenuNr,!MenuStateHandle .pst)
menuStateHandleGetOSMenuNr msH=:(MenuLSHandle {mlsHandle={mOSMenuNr}}) = (mOSMenuNr,msH)

menuStateHandleGetTitle :: !(MenuStateHandle .pst) -> (!Title,!MenuStateHandle .pst)
menuStateHandleGetTitle msH=:(MenuLSHandle {mlsHandle={mTitle}}) = (mTitle,msH)

menuStateHandleGetSelect :: !(MenuStateHandle .pst) -> (!Bool,!MenuStateHandle .pst)
menuStateHandleGetSelect msH=:(MenuLSHandle {mlsHandle={mSelect}}) = (mSelect,msH)


menuStateHandleSetHandle :: !OSMenu !(MenuStateHandle .pst) -> MenuStateHandle .pst
menuStateHandleSetHandle menu (MenuLSHandle mlsH=:{mlsHandle=mH}) = MenuLSHandle {mlsH & mlsHandle={mH & mHandle=menu}}

menuStateHandleSetTitle :: !Title !(MenuStateHandle .pst) -> MenuStateHandle .pst
menuStateHandleSetTitle title (MenuLSHandle mlsH=:{mlsHandle=mH}) = MenuLSHandle {mlsH & mlsHandle={mH & mTitle=title}}

menuStateHandleSetSelect :: !Bool !(MenuStateHandle .pst) -> MenuStateHandle .pst
menuStateHandleSetSelect select (MenuLSHandle mlsH=:{mlsHandle=mH}) = MenuLSHandle {mlsH & mlsHandle={mH & mSelect=select}}
