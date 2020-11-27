{-# LANGUAGE ViewPatterns #-}

module Directory (
    (</>),
    listContents,
    listFiles,
    listFileNames,
    listCsvs,
    listCsvNames,
    withExt,
    delExt,
    getName,
    getDir,
    cd,
    pwd,
    module System.Directory
) where

import Control.Monad ( filterM )
import System.Directory (
        getDirectoryContents,
        doesFileExist,
        getCurrentDirectory,
        withCurrentDirectory
    )

import Utils ( split )

(</>) :: FilePath -> FilePath -> FilePath
_ </> path@('/':_) = path
base@(last -> '/') </> path = base ++ path
base </> path = base ++ "/" ++ path

listContents :: FilePath -> IO [FilePath]
listContents path = do
    contents <- getDirectoryContents path
    return $ map (path </>) contents

listFiles :: FilePath -> IO [FilePath]
listFiles path = filterM doesFileExist =<< listContents path

listFileNames :: FilePath -> IO [String]
listFileNames path = map getName <$> listFiles path

listCsvs :: FilePath -> IO [FilePath]
listCsvs path = filter (withExt "csv") <$> listFiles path

listCsvNames :: FilePath -> IO [String]
listCsvNames path = map getName <$> listCsvs path

withExt :: String -> FilePath -> Bool
withExt ext path
    | length parts >= 2 = last parts == ext
    | otherwise = ext == ""
    where parts = split '.' (getName path)

delExt :: FilePath -> FilePath
delExt path
    | length parts >= 2 = dir ++ (delLast parts)
    | otherwise = path
    where dir = getDir path
          name = getName path
          parts = split '.' name
          delLast [_] = ""
          delLast [p, _] = p
          delLast (p:ps) = p ++ "." ++ delLast ps

getName :: FilePath -> FilePath
getName path = last $ split '/' path

getDir :: FilePath -> FilePath
getDir path = delLast $ split '/' path
    where delLast [_] = ""
          delLast [p, _] = p ++ "/"
          delLast (p:ps) = p ++ "/" ++ delLast ps

cd :: FilePath -> IO a -> IO a
cd = withCurrentDirectory

pwd :: IO FilePath
pwd = getCurrentDirectory
