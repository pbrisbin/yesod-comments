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
    ) where

import Yesod
import Yesod.Auth
import Yesod.Markdown

import Data.Text  (Text)
import Data.Time  (UTCTime)

type ThreadId  = Text
type CommentId = Int

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

data UserDetails = UserDetails
    { textUserName :: Text
    , friendlyName :: Text
    , emailAddress :: Text
    }

class YesodAuth m => YesodComments m where
    getComment    :: ThreadId -> CommentId -> GHandler s m (Maybe Comment)
    storeComment  :: Comment -> GHandler s m ()
    updateComment :: Comment -> Comment -> GHandler s m ()
    deleteComment :: Comment -> GHandler s m ()
    loadComments  :: Maybe ThreadId -> GHandler s m [Comment]

    -- | If @Nothing@ is returned, the user cannot comment. this can be
    --   used to blacklist users.
    userDetails :: AuthId m -> GHandler s m (Maybe UserDetails)

