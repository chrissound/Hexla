module Main where

import System.Directory
import System.Posix.Files
import System.Posix.User (getUserEntryForID, getGroupEntryForID, userName, groupName)
import Data.Bool
import System.Posix.Types(FileMode, UserID, GroupID)
import System.Posix.Files.ByteString(intersectFileModes)
import Data.Map.Strict (Map, fromList, (!))
import Text.PrettyPrint.Boxes as Boxes
import Data.List

hasMode :: FileMode -> FileMode -> Bool
hasMode fa fb = intersectFileModes fa fb == fa


data PathEntry = PathEntry FilePath FileStatus

type UserIDMap = Map UserID String
type GroupIDMap = Map GroupID String

getUserMap :: [UserID] -> IO UserIDMap
getUserMap x = do
  us <- mapM getUserEntryForID x
  return $ fromList $ zip x (userName <$> us)

getGroupMap :: [GroupID] -> IO GroupIDMap
getGroupMap x = do
  us <- mapM getGroupEntryForID x
  return $ fromList $ zip x (groupName <$> us)

main :: IO ()
main = do
  c <- getDirectoryContents "."
  fs <- mapM getFileStatus c
  let p = zipWith (PathEntry) c fs
  um <- getUserMap (fileOwner <$> fs)
  gm <- getGroupMap (fileGroup <$> fs)
  let tableV = (\z -> Main.render z um gm) <$> p
  putStrLn $ table tableV

table :: [[String]] -> String
table r = Boxes.render $ hsep 1 left (map (vcat left . map text) (transpose r))

rwxString :: FileMode -> String
rwxString fm = ""
  ++ (bool "-" "r" $ hasMode ownerReadMode fm)
  ++ (bool "-" "w" $ hasMode ownerWriteMode fm)
  ++ (bool "-" "x" $ hasMode ownerExecuteMode fm)
  ++ (bool "-" "r" $ hasMode groupReadMode fm)
  ++ (bool "-" "w" $ hasMode groupWriteMode fm)
  ++ (bool "-" "x" $ hasMode groupExecuteMode fm)
  ++ (bool "-" "r" $ hasMode otherReadMode fm)
  ++ (bool "-" "w" $ hasMode otherWriteMode fm)
  ++ (bool "-" "x" $ hasMode otherExecuteMode fm)

render :: PathEntry -> UserIDMap -> GroupIDMap -> [String]
render (PathEntry fp fs) um gm = [
   rwxString $ fileMode fs
 , um ! fileOwner fs
 , gm ! fileGroup fs
 , show $ fileSize fs
 , fp ++ (bool "" "/" (isDirectory fs))
 ]
