{-|
    An IVI script entry.
-}
module Entry where

import Config

data Entry = Entry Import Script

type Import = String
type Script = String

generateEntry :: Config -> Entry
generateEntry cfg = Entry i s
    where
        i =  "import "
          ++ "Scripts"
          ++ "."
          ++ scriptDir cfg
          ++ "."
          ++ sourceFileName cfg
          ++ " ("
          ++ executeFunctionName cfg
          ++ ")"

        s = "Script "
          ++ show (name cfg)
          ++ " "
          ++ "Scripts."
          ++ scriptDir cfg
          ++ "."
          ++ sourceFileName cfg
          ++ "."
          ++ executeFunctionName cfg
          ++ " "
          ++ show (regexes cfg)
