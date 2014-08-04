{-|
    An IVI script entry.
-}
module Entry where

data Entry = Entry Import Script

type Import = String
type Script = String
