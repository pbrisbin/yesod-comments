{-# LANGUAGE RankNTypes #-}
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
-- Core comment data types.
--
-------------------------------------------------------------------------------
module Yesod.Comments.Core 
    ( Comment(..)
    , ThreadId
    , CommentId
    , CommentForm(..)
    , CommentStorage(..)
    , CommentsTemplate
    , CommentConf(..)
    ) where

import Yesod
import Yesod.Markdown
import Data.Time.Clock (UTCTime)

-- | A unique thread identifier, probably the post slug or similar
type ThreadId = String

-- | A unique identifier for a comment within a thread, usually an
--   incrementing number that the user need not deal with
type CommentId = Int

-- | A convenience synonym
type CommentsTemplate = (Yesod m) => [Comment] -> GWidget s m () -> Enctype -> GWidget s m ()

-- | The actual comment data type.
data Comment = Comment
    { threadId  :: ThreadId
    , commentId :: CommentId
    , timeStamp :: UTCTime
    , ipAddress :: String
    , userName  :: String
    , content   :: Markdown
    } deriving Show

-- | The form data type, this is used to gather the comment from the
--   user and is handed off to commentFromForm just before storeComment
data CommentForm = CommentForm
    { formUser    :: String
    , formComment :: Markdown
    }

-- | A data type to represent your backend. Provides total flexibility
--   by abstracting the actual storage away behind the three required
--   functions. See 'Comments.Storage' for example backends
data CommentStorage s m = CommentStorage
    { storeComment  :: Comment  -> GHandler s m ()
    , loadComments  :: ThreadId -> GHandler s m [Comment]
    , deleteComment :: ThreadId -> CommentId -> GHandler s m ()
    }

-- | The main configuration
data CommentConf s m = CommentConf
    { template :: CommentsTemplate
    , storage  :: CommentStorage s m
    , filters  :: [(Comment -> GHandler s m Bool)]
    }
