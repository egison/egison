module Paths_egison where
import Data.Version

getDataFileName :: FilePath -> IO FilePath
getDataFileName filename = return ("~/egison3/" ++ filename)
