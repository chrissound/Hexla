module PathEntry where

import System.Directory
import System.Posix.Types

type FileSize = FileOffset
type FileOwner = UserID
type FileGroup = GroupID
type FileModified = EpochTime
type FileCreated = EpochTime
type FileName = String
type FileDirectoy = Bool

data FilePermission = FilePermission FileMode FileMode

data PathEntry = PathEntry {
  fileDirectoy :: FileDirectoy,
  filePermission :: FileMode,
  fileSize :: FileSize,
  fileOwner :: FileOwner,
  fileGroup :: FileGroup,
  fileCreated :: FileCreated,
  fileModified :: FileModified,
  fileName :: FileName,
  fileCommandHook :: String
  }
