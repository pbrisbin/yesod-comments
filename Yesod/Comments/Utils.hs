{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.Utils
-- Copyright   :  (c) Patrick Brisbin 2010
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Yesod.Comments.Utils
    ( commentUserDetails
    , currentUserDetails
    , requireUserDetails
    , defaultUserDetails
    , isCommentingUser
    , gravatar
    ) where

import Yesod
import Yesod.Auth
import Yesod.Comments.Core

import Data.Text  (Text)
import Data.Maybe (fromMaybe)

import Network.Gravatar hiding (gravatar)
import qualified Network.Gravatar as G

-- | Map the commenter's id to user details or return defaults.
commentUserDetails :: YesodComments m => Comment -> HandlerT m IO UserDetails
commentUserDetails c =
    return . fromMaybe (defaultUserDetails c) =<<
        case (cIsAuth c, fromPathPiece (cUserName c)) of
            (True, Just uid) -> userDetails uid
            _                -> return Nothing

-- | Returns @Nothing@ if user is not authenticated
currentUserDetails :: YesodComments m => HandlerT m IO (Maybe UserDetails)
currentUserDetails = do
    muid <- maybeAuthId
    case muid of
        Just uid -> userDetails uid
        _        -> return Nothing

-- | Halts with @permissionDenied@ if user is not authenticated
requireUserDetails :: YesodComments m => HandlerT m IO (UserDetails)
requireUserDetails = do
    mudetails <- currentUserDetails
    case mudetails of
        Just udetails -> return udetails
        _             -> permissionDenied "you must be logged in"

-- | For a comment that was not authenticated or cannot be mapped, the
--   default details are the id and email stored directly on the comment.
defaultUserDetails :: Comment -> UserDetails
defaultUserDetails c = UserDetails (cUserName c) (cUserName c) (cUserEmail c)

-- | Given pixel size and email, return the gravatar url
gravatar :: Int -> Text -> String
gravatar s = G.gravatar defaultConfig { gDefault = Just MM, gSize = Just $ Size s }

isCommentingUser :: YesodComments m => Comment -> HandlerT m IO Bool
isCommentingUser comment = do
    mudetails <- currentUserDetails
    cudetails <- commentUserDetails comment

    return $ maybe False (== cudetails) mudetails
