{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.Core
-- Copyright   :  (c) Patrick Brisbin 2010
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Yesod.Comments.Core
    ( Comment(..)
    , CommentId
    , ThreadId
    , YesodComments (..)
    , getNextCommentId
    , isCommentingUser
    ) where

import Yesod
import Yesod.Auth
import Yesod.Markdown

import Data.Text (Text)
import Data.Time (UTCTime)

type ThreadId  = Text
type CommentId = Int

class YesodAuth m => YesodComments m where
    getComment    :: ThreadId -> CommentId -> GHandler s m (Maybe Comment)
    storeComment  :: Comment -> GHandler s m ()
    updateComment :: Comment -> Comment -> GHandler s m ()
    deleteComment :: Comment -> GHandler s m ()
    loadComments  :: Maybe ThreadId -> GHandler s m [Comment]

    -- | If using Auth, provide the function to get from a user id to
    --   the string to use as the commenter's username. This should
    --   return something friendly probably pulled from the user's
    --   profile on your site.
    displayUser :: AuthId m -> GHandler s m Text
    displayUser _ = return ""

    -- | If using Auth, provide the function to get from a user id to
    --   the string to use as the commenter's email.
    displayEmail :: AuthId m -> GHandler s m Text
    displayEmail _ = return ""

data Comment = Comment
    { threadId  :: ThreadId
    , commentId :: CommentId
    , timeStamp :: UTCTime
    , ipAddress :: Text
    , userName  :: Text
    , userEmail :: Text
    , content   :: Markdown
    , isAuth    :: Bool -- ^ compatability field
    }

instance Eq Comment where
    a == b = (threadId a == threadId b) && (commentId a == commentId b)

-- | As the final step before insert, this is called to get the next
--   comment id for the thread. super-high concurrency is probably not
--   well-supported here.
getNextCommentId :: YesodComments m => ThreadId -> GHandler s m CommentId
getNextCommentId tid = go =<< loadComments (Just tid)

    where
        go :: YesodComments m => [Comment] -> GHandler s m CommentId
        go [] = return 1
        go cs = return $ maximum (map commentId cs) + 1

-- | Returns False when not logged in.
isCommentingUser :: YesodAuth m => Comment -> GHandler s m Bool
isCommentingUser comment = do
    muid <- maybeAuthId
    return $ case muid of
        Just uid -> isAuth comment && toPathPiece uid == userName comment
        _        -> False
