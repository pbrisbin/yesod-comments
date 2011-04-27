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
import Yesod.Comments.Core
import Yesod.Comments.Filters (applyFilters)
import Yesod.Helpers.Auth

import qualified Data.Text as T

-- | Comments that anyone can enter anonymously
addComments :: YesodComments m 
            => ThreadId -- ^ the thread you're adding comments to
            -> GWidget s m ()
addComments tid = do
    comments               <- lift $ loadComments (Just tid)
    cid                    <- lift $ getNextCommentId comments
    ((res, form), enctype) <- lift $ runFormMonadPost commentForm

    handleForm res tid cid
    addStyling
    [hamlet|
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
addCommentsAuth :: (YesodAuth m, YesodComments m)
                => ThreadId -- ^ the thread you're adding comments to
                -> GWidget s m ()
addCommentsAuth tid = do
    (isAuthenticated, uid, username) <- lift $ do
        muid <- maybeAuthId
        case muid of
            Nothing  -> return (False, "", "")
            Just uid -> do
                uname <- displayUser uid
                return (True, toSinglePiece uid, uname)

    comments               <- lift $ loadComments (Just tid)
    cid                    <- lift $ getNextCommentId comments
    ((res, form), enctype) <- lift $ runFormMonadPost $ commentFormAuth uid username

    handleForm res tid cid
    addStyling
    [hamlet|
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


-- | Add styling common to the auth and non-auth forms
addStyling :: Yesod m => GWidget s m ()
addStyling = addCassius [cassius|
    .yesod_comment_input th
        text-align: left
        vertical-align: top
    .yesod_comment_input textarea
        height: 10ex
        width: 50ex
    |]

-- | Handle the posted form and actually insert the comment
handleForm :: YesodComments m 
           => FormResult CommentForm 
           -> ThreadId 
           -> CommentId 
           -> GWidget s m ()
handleForm res tid cid = case res of
    FormMissing    -> return ()
    FormFailure _  -> return ()
    FormSuccess cf -> lift $ do
        comment <- commentFromForm tid cid cf
        matches <- applyFilters commentFilters comment
        if matches
            then setMessage "comment dropped. matched filters."
            else do
                storeComment comment
                setMessage "comment added."
        redirectCurrentRoute

    where
        -- | Redirect back to the current route after a POST request
        redirectCurrentRoute :: Yesod m => GHandler s m ()
        redirectCurrentRoute = do
            tm <- getRouteToMaster
            mr <- getCurrentRoute
            case mr of
                Just r  -> redirect RedirectTemporary $ tm r
                Nothing -> notFound

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
        Just r  -> [hamlet|<a href="@{r}">log in|]
        Nothing -> [hamlet|log in|]
