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
    ( YesodComments(..)
    , CommentId
    , ThreadId
    , Comment(..)
    , UserDetails(..)
    , CommentStorage(..)
    ) where

import Yesod
import Yesod.Auth
import Yesod.Markdown

import Data.Text  (Text)
import Data.Time  (UTCTime)

type ThreadId  = Text
type CommentId = Int

-- | The core data type a Comment
data Comment = Comment
    { commentId  :: CommentId
    , cThreadId  :: ThreadId
    , cTimeStamp :: UTCTime
    , cIpAddress :: Text
    , cUserName  :: Text
    , cUserEmail :: Text
    , cContent   :: Markdown
    , cIsAuth    :: Bool -- ^ compatability field, always true
    }

instance Eq Comment where
    a == b = (cThreadId a == cThreadId b) && (commentId a == commentId b)

-- | Information about the User that's needed to store comments.
data UserDetails = UserDetails
    { textUserName :: Text -- ^ Text version of user id, @toPathPiece
                           --   userId@ is recommended. comments are stored
                           --   using this value so users can freely change
                           --   names without losing comments.
    , friendlyName :: Text -- ^ The name that's actually displayed
    , emailAddress :: Text -- ^ Not shown but stored
    } deriving Eq

-- | How to save and restore comments from persisten storage. All necesary
--   actions are accomplished through these 5 functions. Currently, only
--   @persistStorage@ is available.
data CommentStorage s m = CommentStorage
    { csGet    :: ThreadId -> CommentId -> GHandler s m (Maybe Comment)
    , csStore  :: Comment -> GHandler s m ()
    , csUpdate :: Comment -> Comment -> GHandler s m ()
    , csDelete :: Comment -> GHandler s m ()
    , csLoad   :: Maybe ThreadId -> GHandler s m [Comment]
    }

class YesodAuth m => YesodComments m where
    -- | How to store and load comments from persistent storage.
    commentStorage :: CommentStorage s m

    -- | If @Nothing@ is returned, the user cannot add a comment. This can
    --   be used to blacklist users. Note that comments left by them will
    --   still appear until manually deleted.
    userDetails :: AuthId m -> GHandler s m (Maybe UserDetails)

    -- | A thread's route. Currently, only used for linking back from the
    --   admin subsite.
    threadRoute :: ThreadId -> Route m

    -- | A route to the admin subsite's EditCommentR action. If @Nothing@,
    --   the Edit link will not be shown.
    editRoute :: Maybe (ThreadId -> CommentId -> Route m)
    editRoute = Nothing

    -- | A route to the admin subsite's DeleteCommentR action. If
    --   @Nothing@, the Delete link will not be shown.
    deleteRoute :: Maybe (ThreadId -> CommentId -> Route m)
    deleteRoute = Nothing
