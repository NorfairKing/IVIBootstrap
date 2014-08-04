module IVIConfig where

import Data.Configurator as C (Worth (..), load, lookup)
import Constants (sourceFileNameConfig, nameConfig, executeFunctionNameConfig, regexesConfig)
{-
    Data structure for a '.ivi' file
-}
data IVIConfig = Config { sourceFileName :: String
                        , name :: String
                        , executeFunctionName :: String
                        , regexes :: [String]
                        }
    deriving (Show)


-- | Parse the configurations in an ivi file
getIVIConfig :: FilePath -> IO IVIConfig
getIVIConfig file = do
    cfg <- C.load [Required file]
    Just sfn <- C.lookup cfg sourceFileNameConfig
    Just n <- C.lookup cfg nameConfig
    Just fn <- C.lookup cfg executeFunctionNameConfig
    Just rs <- C.lookup cfg regexesConfig
    return Config { sourceFileName = sfn, name = n, executeFunctionName = fn, regexes = rs }

