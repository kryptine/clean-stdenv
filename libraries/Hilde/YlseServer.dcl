definition module YlseServer

import StdMaybe
from FamkeRpc import :: Famke(..), :: RpcId(..), :: FamkePort(..), :: FamkeId(..)

:: Path` :== [String]

:: YlseId :== RpcId YlseIn YlseOut

:: YlseIn

:: YlseOut

existFileAt :: !YlseId !Path` !*Famke -> (!Bool, !*Famke)
readFileAt :: !YlseId !Path` !*Famke -> (!Maybe Dynamic, !*Famke)
writeFileAt :: !YlseId !Path` Dynamic !*Famke -> (!Bool, !*Famke)
removeFileAt :: !YlseId !Path` !*Famke -> (!Bool, !*Famke)
makePathAt :: !YlseId !Path` !*Famke -> (!Bool, !*Famke)
listFolderAt :: !YlseId !Path` !*Famke -> (!Maybe [String], !*Famke)
removePathAt :: !YlseId !Path` !*Famke -> (!Bool, !*Famke)

class YlseServer env
where
	ylseExistFile :: !Path` !*env !*Famke -> (!Bool, !*env, !*Famke)
	ylseReadFile :: !Path` !*env !*Famke -> (!Maybe Dynamic, !*env, !*Famke)
	ylseWriteFile :: !Path` Dynamic !*env !*Famke -> *(!Bool, !*env, !*Famke)
	ylseRemoveFile :: !Path` !*env !*Famke -> (!Bool, !*env, !*Famke)
	ylseMakeFolder :: !Path` !*env !*Famke -> (!Bool, !*env, !*Famke)
	ylseListFolder :: !Path` !*env !*Famke -> (!Maybe [String], !*env, !*Famke)
	ylseRemoveFolder :: !Path` !*env !*Famke -> (!Bool, !*env, !*Famke)
	ylseMount :: !Path` !YlseId !*env !*Famke -> (!Bool, !*env, !*Famke)
	ylseUnmount :: !Path` !*env !*Famke -> (!Bool, !*env, !*Famke)

StartYlseServer :: !YlseId !*env !*Famke -> *Famke | YlseServer env
