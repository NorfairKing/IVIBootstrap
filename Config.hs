module Config where

import           System.FilePath   (takeDirectory)

import           Data.Configurator as C (Worth (..), load, lookup)

import           Constants         (executeFunctionNameConfig, nameConfig,
                                    regexesConfig, sourceFileNameConfig)
{-
    Data structure for a '.ivi' file
-}
data Config = Config {
                          scriptDir           :: FilePath
                        , sourceFileName      :: String
                        , name                :: String
                        , executeFunctionName :: String
                        , regexes             :: [String]
                     }
    deriving (Show)


-- | Parse the configurations in an ivi file
getConfig :: FilePath -> IO Config
getConfig file = do
    cfg <- C.load [Required file]
    Just sfn <- C.lookup cfg sourceFileNameConfig
    Just n <- C.lookup cfg nameConfig
    Just fn <- C.lookup cfg executeFunctionNameConfig
    Just rs <- C.lookup cfg regexesConfig
    return Config {
                      scriptDir = takeDirectory file
                    , sourceFileName = sfn
                    , name = n
                    , executeFunctionName = fn
                    , regexes = rs
                  }

