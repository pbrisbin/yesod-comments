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
    , addCommentsAuth
    , module Yesod.Comments.Core
    ) where

import Yesod
import Yesod.Auth
import Yesod.Comments.Core

-- | Comments that anyone can enter anonymously
addComments :: (RenderMessage m FormMessage, YesodComments m)
            => ThreadId -- ^ the thread you're adding comments to
            -> GWidget s m ()
addComments tid = do
    comments               <- lift $ loadComments (Just tid)
    ((res, form), enctype) <- lift $ runFormPost commentForm

    handleForm res tid
    addStyling
    [whamlet|
        <div .yesod_comments>
            <h4>Add a comment:
            <div .yesod_comment_input>
                <form enctype="#{enctype}" method="post">^{form}
                <p .helptext>Comments are parsed as pandoc-style markdown

            $if not $ null comments
                <h4>Showing #{toHtml $ helper $ length comments}:

                $forall comment <- comments
                    <div .yesod_comment>^{showComment comment}

    |]

-- | Comments that require authentication
addCommentsAuth :: (RenderMessage m FormMessage, YesodAuth m, YesodComments m)
                => ThreadId -- ^ the thread you're adding comments to
                -> GWidget s m ()
addCommentsAuth tid = do
    (isAuthenticated, uid, username, email) <- lift $ do
        muid <- maybeAuthId
        case muid of
            Nothing  -> return (False, "", "", "")
            Just uid -> do
                uname <- displayUser uid
                email <- displayEmail uid
                return (True, toSinglePiece uid, uname, email)

    comments               <- lift $ loadComments (Just tid)
    ((res, form), enctype) <- lift $ runFormPost (commentFormAuth uid username email)

    handleForm res tid
    addStyling
    [whamlet|
        <div .yesod_comments>
            $if isAuthenticated
                <h4>Add a comment:
                <div .yesod_comment_input>
                    <form enctype="#{enctype}" method="post">^{form}
                    <p .helptext>Comments are parsed as pandoc-style markdown
            $else
                <h4>Please ^{login} to post a comment.

            $if not $ null comments
                <h4>Showing #{toHtml $ helper $ length comments}:

                $forall comment <- comments
                    <div .yesod_comment>^{showCommentAuth comment}
    |]

helper :: Int -> String
helper 0 = "no comments"
helper 1 = "1 comment"
helper n = show n ++ " comments"

-- | Show the authroute as a link if set up
login :: Yesod m => GWidget s m ()
login = do
    lift $ setUltDest' -- so we come back here after login
    mroute <- lift $ fmap authRoute getYesod
    case mroute of
        Just r  -> [whamlet|<a href="@{r}">log in|]
        Nothing -> [whamlet|log in|]
