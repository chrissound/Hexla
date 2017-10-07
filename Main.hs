module Main where

import System.Directory
import System.Posix.Files
import System.Posix.User (getUserEntryForID, getGroupEntryForID, userName, groupName)
import Data.Bool
import System.Posix.Types(FileMode, UserID, GroupID)
import System.Posix.Files.ByteString(intersectFileModes)
import System.FilePath.Posix (takeExtension)
import Data.Map.Strict (Map, fromList, (!))
import Text.PrettyPrint.Boxes as Boxes
import Data.List
import Data.ByteUnits
import Data.Function
import Data.Ord (comparing)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX

hasMode :: FileMode -> FileMode -> Bool
hasMode fa fb = intersectFileModes fa fb == fa

data ListingEntry = ListingPathEntry PathEntry | ListingString String
data PathEntry = PathEntry {
  filePath :: FilePath,
  fileExtension :: FileExtension,
  fileStatus :: FileStatus,
  user :: String,
  group :: String
  }

type UserIDMap = Map UserID String
type GroupIDMap = Map GroupID String
type FileExtension = String

getUserMap :: [UserID] -> IO UserIDMap
getUserMap x = do
  us <- mapM getUserEntryForID x
  return $ fromList $ zip x (userName <$> us)

getGroupMap :: [GroupID] -> IO GroupIDMap
getGroupMap x = do
  us <- mapM getGroupEntryForID x
  return $ fromList $ zip x (groupName <$> us)

getMetaListing :: [[PathEntry]] -> [ListingEntry]
getMetaListing (x:xs) = ListingString (fileExtension $ head x) : (++) (fmap ListingPathEntry x) (getMetaListing xs)
getMetaListing ([]) =  []

main :: IO ()
main = do
  dc <- getDirectoryContents "."
  --- So tired --- sorry -- gotta sleep
  let c = fmap fst $ concat $ groupBy ((==) `on` snd) $ sortBy (comparing snd) $ fmap (\x -> (x,takeExtension x)) dc
  fs <- mapM getFileStatus c
  um <- getUserMap (fileOwner <$> fs) :: IO UserIDMap
  gm <- getGroupMap (fileGroup <$> fs) :: IO GroupIDMap
  let p = zipWith5 PathEntry c fe fs userIds groupIds where
        fe = takeExtension <$> c
        userIds = ((um !) <$> (fileOwner <$> fs))
        groupIds = ((gm !)) <$> (fileGroup <$> fs)
  let tableV = renderEntry <$>
        (getMetaListing $ groupBy (\(PathEntry _ fe _ _ _) (PathEntry _ fe' _ _ _) -> fe == fe') p)
  putStrLn $ table tableV
  putStrLn $ "Total count: " ++ (show $ length p)

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

renderPathEntry :: PathEntry -> [String]
renderPathEntry (PathEntry fp _ fs u g) = [
   ""
 , (bool "." "d" (isDirectory fs))
 , rwxString $ fileMode fs
 , u
 , g
 , getShortHand . getAppropriateUnits $ ByteValue (fromIntegral . toInteger $ fileSize fs) Bytes
 , formatTime defaultTimeLocale "%x %r" ( posixSecondsToUTCTime (realToFrac $ modificationTime fs :: NominalDiffTime) :: UTCTime)
 , fp ++ (bool "" "/" (isDirectory fs))
 ]

renderEntry :: ListingEntry -> [String]
renderEntry (ListingPathEntry pe) = renderPathEntry pe
renderEntry (ListingString s) = [s, "", "", "", "", "", "", ""]
