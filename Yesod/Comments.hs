{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments
-- Copyright   :  (c) Patrick Brisbin 2010
-- License     :  as-is
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A generic Comments interface for a Yesod application. This module is
-- in the early stages of development. Beware bugs, patches welcome.
--
-------------------------------------------------------------------------------
module Yesod.Comments
    ( addComments
    , module Yesod.Comments.Core
    ) where

import Yesod
import Yesod.Auth

import Yesod.Comments.Core
import Yesod.Comments.Form
import Yesod.Comments.View

import Network.Gravatar
import Data.Text (Text)

-- | Comments that require authentication
addComments :: (RenderMessage m FormMessage, YesodAuth m, YesodComments m)
                => ThreadId -- ^ the thread you're adding comments to
                -> GWidget s m ()
addComments tid = do
    (isAuthenticated, uid, username, email) <- lift $ do
        muid <- maybeAuthId
        case muid of
            Nothing  -> return (False, "", "", "")
            Just uid -> do
                uname <- displayUser uid
                email <- displayEmail uid
                return (True, toPathPiece uid, uname, email)

    comments               <- lift $ loadComments (Just tid)
    ((res, form), enctype) <- lift $ runFormPost (commentForm uid email)

    handleForm res tid

    [whamlet|
        <div .yesod_comments>
            ^{showComments comments}

            $if isAuthenticated
                <div .avatar>
                    <a target="_blank" title="change your profile picture at gravatar" href="http://gravatar.com/emails/">
                        <img src="#{img email}">

                <div .input>
                    <form enctype="#{enctype}" method="post" .form-stacked>
                        <div .clearfix .optional>
                            <label for="username">Username
                            <div .input>
                                <p #username>#{username}

                        ^{form}

                        <div .actions>
                            <button .btn .primary type="submit">Add comment

            $else
                <h4>Please ^{login} to post a comment.
    |]

    where
        img :: Text -> String
        img = gravatar defaultConfig { gDefault = Just MM, gSize = Just $ Size 48 }

        login :: Yesod m => GWidget s m ()
        login = do
            mroute <- lift $ do
                setUltDestCurrent
                fmap authRoute getYesod

            case mroute of
                Just r  -> [whamlet|<a href="@{r}">log in|]
                Nothing -> [whamlet|log in|]
