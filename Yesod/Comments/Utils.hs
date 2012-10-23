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
commentUserDetails :: YesodComments m => Comment -> GHandler s m UserDetails
commentUserDetails c =
    return . fromMaybe (defaultUserDetails c) =<<
        case (isAuth c, fromPathPiece (userName c)) of
            (True, Just uid) -> userDetails uid
            _                -> return Nothing

-- | Returns Nothing if user is not authenticated
currentUserDetails :: YesodComments m => GHandler s m (Maybe UserDetails)
currentUserDetails = do
    muid <- maybeAuthId
    case muid of
        Just uid -> userDetails uid
        _        -> return Nothing

-- | Returns permissionDenied if user is not authorized
requireUserDetails :: YesodComments m => GHandler s m (UserDetails)
requireUserDetails = do
    mudetails <- currentUserDetails
    case mudetails of
        Just udetails -> return udetails
        _             -> permissionDenied "you must be logged in"

-- | For a comment that was not authenticated or cannot be mapped, the
--   default details are the id and email stored directly on the comment.
defaultUserDetails :: Comment -> UserDetails
defaultUserDetails c = UserDetails (userName c) (userName c) (userEmail c)

gravatar :: Int  -- ^ size
         -> Text -- ^ email
         -> String
gravatar s = G.gravatar defaultConfig { gDefault = Just MM, gSize = Just $ Size s }

isCommentingUser :: YesodComments m => Comment -> GHandler s m Bool
isCommentingUser comment = do
    mudetails <- currentUserDetails
    cudetails <- commentUserDetails comment

    return $ maybe False (== cudetails) mudetails
