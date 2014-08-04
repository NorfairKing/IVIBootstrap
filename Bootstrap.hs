import Control.Monad (filterM, liftM)
import System.Directory (getCurrentDirectory, getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>), takeDirectory)
import System.FilePath.Posix (takeExtension)

import Config (getConfig)
import Constants (scriptListFile, iviExtension, versionFileName)
import Entry (Entry (..), generateEntry)
import Version (checkVersion)

-- | Execute IVI's bootstrap procedure
main :: IO ()
main = do
    candidates <- filterM isScriptDir =<< getDirectoryContents =<< getCurrentDirectory
    entriess <- mapM parseScriptDir candidates
    let fileContents = joinEntries $ concat entriess
    writeFile scriptListFile fileContents


-- | Determine whether the given path leads to a script directory
isScriptDir :: FilePath -> IO Bool
isScriptDir "."  = return False
isScriptDir ".." = return False
isScriptDir dirName = do
    isdir <- doesDirectoryExist dirName
    if not isdir
    then return False
    else do
        iviFiles <- getConfigFiles dirName
        return $ (not . null) iviFiles

-- | Get all paths to ivi files in a given directory
getConfigFiles :: FilePath -> IO [FilePath]
getConfigFiles dir = do
        dirContents <- getDirectoryContents dir
        let iviFiles = filter (\x -> takeExtension x == iviExtension) dirContents
        return iviFiles
        
-- | Parse all scripts in a script dir
parseScriptDir :: FilePath -> IO [Entry]
parseScriptDir dir = do
    scriptVersion <- readFile $ dir </> versionFileName
    
    scriptVersionFile <- ((</> versionFileName) . takeDirectory . takeDirectory) `liftM` getCurrentDirectory
    iviVersion <- readFile scriptVersionFile
    
    if checkVersion scriptVersion iviVersion
    then do
        configFileNames <- getConfigFiles dir
        putStrLn dir    
        let configFiles = map (dir </>) configFileNames
        configs <- mapM getConfig configFiles
        let entries = map generateEntry configs
        mapM_ (putStrLn . ("|- " ++)) configFileNames
        putStrLn ""
        return entries
    else do
        putStrLn $ "Not adding scripts in " ++ dir ++ "because the ivi versions aren't compatible"
        return []


-- | Join the imports and entries into the final source file.
joinEntries :: [Entry] -> String
joinEntries entries = contents
    where
        (is,ss) = unzip $ map (\(Entry i s) -> (i,s)) entries

        contents = moduleDoc
                ++ moduleDeclaration
                ++ scriptImport
                ++ imports
                ++ "\n"
                ++ scriptsListDoc
                ++ scriptsListHeader
                ++ scriptsList

        moduleDoc = "{-|\n"
                ++ "Module      : ScriptsList\n"
                ++ "Description : The list of scripts that can be used\n"
                ++ "This file is generated, there is no use in modifying it directly\n"
                ++ "Please just make sure the .ivi files are in order.\n"
                ++ "-}\n"

        moduleDeclaration = "module Scripts.ScriptsList where\n\n"

        scriptImport = "import Script\n\n"
    
        imports = unlines is

        scriptsListDoc = "-- | The list of scripts\n"        

        scriptsListHeader = "scripts :: [IVIScript] \n"

        scriptsList = "scripts = [\n"
                    ++ fix ss
                    ++ "          ]\n"

        fix [] = ""
        fix [e] = "              " ++ e ++ "\n"
        fix (e:es) = fix es ++ "            , " ++ e ++ "\n"
