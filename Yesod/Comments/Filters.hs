-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.Filters
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
-- Maintainer  :  pbrisbin@gmail.com 
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Yesod.Comments.Filters
    ( applyFilters
    -- * Example filters
    , blacklistFile
    ) where

import Yesod
import Yesod.Comments.Core (Comment(..))
import Control.Monad (liftM)

-- | Apply each filter a given list, return True if the Comment matches 
--   any one filter
applyFilters :: (Yesod m) => [(Comment -> GHandler s m Bool)] -> Comment -> GHandler s m Bool
applyFilters [] _     = return False
applyFilters (p:ps) c = p c >>= \b -> if b then return True else applyFilters ps c

-- | Read IPs from a file, one per line, return True if the comment's IP 
--   matches one in the file
blacklistFile :: (Yesod m) => FilePath -> Comment -> GHandler s m Bool
blacklistFile f c = do
    blacklist <- liftM lines $ liftIO $ readFile f
    return $ (ipAddress c) `elem` blacklist
