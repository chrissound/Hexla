{-# Language OverloadedStrings #-}
{-# Options -Wno-unused-imports #-}
module Main where

import System.Directory
import System.Posix.Files
import System.Posix.User (getUserEntryForID, getGroupEntryForID, userName, groupName)
import Data.Bool
import System.Posix.Types(FileMode, UserID, GroupID)
import System.Posix.Files.ByteString(intersectFileModes)
import System.FilePath.Posix (takeExtension)
import Data.Map.Strict (Map, fromList, (!))
import Data.List
import Data.ByteUnits
import Data.Function
import Data.Ord (comparing)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
--import Debug.Trace
import Rainbox
-- import Rainbox.Core
import qualified Rainbow
import Data.Text (Text, pack, unpack)
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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
  -- let tableVV = table tableV
  -- mapM_ RainbowputChunk . toList $ render $ tableVV
  mapM_ Rainbow.putChunk . toList $ render $ horizontalStationTable tableV

  putStrLn $ "Total count: " ++ (show $ length p)
  -- putStrLn $ show $ content tableVV
  -- let cnt = content tableVV
  -- case (cnt) of
  --   Row x -> do
  --     mapM_ (\z -> do
  --              putStrLn ""
  --              putStrLn "New box:"
  --              print z) x
  --     print $ length x
  --   _ -> print "Nope"

table :: [[String]] -> Box Vertical
table x = -- boxV where
  mconcat $
  fmap
    (\x' -> rowF x')
    x

rowF :: [String] -> Box Vertical
rowF x = mconcat $ (\x' -> wrap left Rainbow.magenta ((textBox Rainbow.white $ pack $ x') :: Box Vertical)) <$> x

textBox :: Rainbow.Radiant -> Text -> Rainbox.Box a
textBox r = Rainbox.fromChunk Rainbox.center r . Rainbow.chunk

-- stationRow :: Rainbow.Radiant -> Station -> [Rainbox.Cell]
-- stationRow bk st =
--   [ nameCell bk . name $ st
--   , linesCell bk . metroLines $ st
--   , addressCell bk . address $ st
--   , undergroundCell bk . underground $ st
--   ]stationColumn :: Station -> [Rainbox.Cell]


myCell :: Rainbow.Radiant -> Rainbow.Radiant -> Text -> Rainbox.Cell
myCell b f vv = Rainbox.Cell v Rainbox.top Rainbox.left b
  where
    v = Seq.singleton . Seq.singleton $ (Rainbow.chunk vv & Rainbow.fore f)

stationColumn :: [(String, Rainbow.Radiant)] -> Seq Cell
stationColumn = fcol . xyz . Seq.fromList . fmap (\(v,c) -> myCell Rainbow.black c (pack v))

fcol :: Seq Cell -> Seq Cell
fcol =
    Seq.adjust (\x -> x { _background = Rainbow.grey}) 0
  -- . Seq.adjust (\x -> x { _rows = fmap (fmap (id)) _rows x}) 6

xyz :: Seq Cell -> Seq Cell
xyz = (Rainbox.intersperse (separator Rainbow.black 1))

horizontalStationTable :: [[String]] -> Rainbox.Box Rainbox.Vertical
horizontalStationTable vvv
  = Rainbox.tableByRows
  . Seq.fromList
  $ (stationColumn <$> (fmap (\x -> zip x (colssss)) vvv ))

colssss :: [Rainbow.Radiant]
colssss = [
    Rainbow.white
  , Rainbow.white
  , Rainbow.white
  , Rainbow.yellow
  , Rainbow.yellow
  , Rainbow.green
  , Rainbow.blue
  , Rainbow.white
  ]

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
