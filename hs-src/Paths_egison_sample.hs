module Paths_egison where
import Data.Version

version :: Version
version = Version [x,x,x] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName filename = return ("~/egison3/" ++ filename)
