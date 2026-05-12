module Gradebook.Version
  ( version
  , versionString
  ) where

-- | Current version of gb
-- Increment this when adding new features:
--   - Major: breaking changes to commands or config format
--   - Minor: new commands or features
--   - Patch: bug fixes
version :: (Int, Int, Int)
version = (0, 10, 1)

-- | Version as a display string
versionString :: String
versionString =
  let (major, minor, patch) = version
  in "gb version " ++ show major ++ "." ++ show minor ++ "." ++ show patch
