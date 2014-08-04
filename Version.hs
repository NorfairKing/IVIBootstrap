module Version where

-- | Check if the current IVI version matches the IVI version of the script
checkVersion :: String -> String -> Bool
checkVersion sv iv = 
    sa == ia && sb == ib && ic >= sc
    where 
        (sa,sb,sc,_) = stripVersion sv
        (ia,ib,ic,_) = stripVersion iv
        
-- | Parse a version string into a quadruple for processing (warning: unsafe)
stripVersion :: String -> (String, String, String, String)
stripVersion str = (\[a,b,c,d] -> (a,b,c,d)) $ go str [] []
    where
        go [] acc accs = accs ++ [acc]
        go ('.':vs) acc accs = go vs [] (accs ++ [acc])
        go ( c :vs) acc accs = go vs (acc ++ [c]) accs
