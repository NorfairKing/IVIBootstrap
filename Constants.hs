module Constants where

import Data.Configurator.Types (Name)

scriptListFile :: FilePath
scriptListFile = "ScriptsList.hs"

sourceFileNameConfig, nameConfig, executeFunctionNameConfig, regexesConfig :: Name
sourceFileNameConfig = "sourceFileName"
nameConfig = "name"
executeFunctionNameConfig = "executeFunctionName"
regexesConfig = "regexes"
