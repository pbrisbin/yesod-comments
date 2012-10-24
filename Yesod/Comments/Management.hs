{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
-------------------------------------------------------------------------------
-- |
-- Module        : Yesod.Comments.Management
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : unstable
-- Portability   : unportable
--
-- > /comments CommentsAdminR CommentsAdmin getCommentsAdmin
--
-------------------------------------------------------------------------------
module Yesod.Comments.Management
    ( CommentsAdmin
    , getCommentsAdmin
    , Route(..)
    ) where

import Yesod
import Yesod.Auth

import Yesod.Comments.Core
import Yesod.Comments.Utils
import Yesod.Comments.Form
import Yesod.Comments.View

import Control.Monad (forM, unless)
import Data.List (sortBy, nub)
import Data.Text (Text)
import Data.Time (UTCTime)
import Language.Haskell.TH.Syntax hiding (lift)

data CommentsAdmin = CommentsAdmin

getCommentsAdmin :: a -> CommentsAdmin
getCommentsAdmin = const CommentsAdmin

mkYesodSub "CommentsAdmin"
    [ ClassP ''YesodComments [ VarT $ mkName "master" ] ]
    [parseRoutes|
        /                            CommentsR      GET
        /edit/#ThreadId/#CommentId   EditCommentR   GET POST
        /delete/#ThreadId/#CommentId DeleteCommentR GET POST
        |]

getCommentsR :: YesodComments m => GHandler CommentsAdmin m RepHtml
getCommentsR = do
    comments <- getThreadedComments

    layout "Your comments" [whamlet|
        $forall (t, cs) <- comments
            <div .thread>
                <h3>
                    <a href="@{threadRoute t}">#{t}

                <div .comments>
                    ^{showComments cs}
        |]

getEditCommentR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getEditCommentR thread cid = withUserComment thread cid $ \c -> do
    ud <- requireUserDetails

    layout "Edit comment" [whamlet|
        ^{runFormEdit c thread (Just ud)}
        |]

postEditCommentR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
postEditCommentR = getEditCommentR

getDeleteCommentR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getDeleteCommentR _ _ = layout "Delete comment" [whamlet|
    <p>Are you sure?
    <form method="post" .form-stacked>
        <div .actions>
            <button .btn .btn-danger type="submit">Delete comment
    |]

postDeleteCommentR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
postDeleteCommentR thread cid = withUserComment thread cid $ \c -> do
    tm <- getRouteToMaster
    csDelete commentStorage c
    setMessage "comment deleted."
    redirect $ tm CommentsR

-- | Return tuples of thread id and associated comments sorted by most
--   recently commented on thread.
getThreadedComments :: YesodComments m => GHandler s m [(ThreadId, [Comment])]
getThreadedComments = do
    allComments <- csLoad commentStorage Nothing
    allThreads  <- forM allComments $ \comment -> do
        mine <- isCommentingUser comment
        return $ if mine then [cThreadId comment] else []

    unsorted <- forM (nub $ concat allThreads) $ \tid ->
        return (tid, filter ((== tid) . cThreadId) allComments)

    return . sortBy latest $ unsorted

    where
        latest :: (ThreadId, [Comment]) -> (ThreadId, [Comment]) -> Ordering
        latest (t1, cs1) (t2, cs2) =
            -- reversed comparason so more recent threads sort first
            case compare (latest' cs1) (latest' cs2) of
                EQ -> compare t1 t2
                GT -> LT
                LT -> GT

        latest' :: [Comment] -> UTCTime
        latest' = maximum . map cTimeStamp

-- | Halts with @permissionDenied@ or runs the action if the comment
--   belongs to the currently logged in user
withUserComment :: YesodComments m => ThreadId -> CommentId -> (Comment -> GHandler s m RepHtml) -> GHandler s m RepHtml
withUserComment thread cid f = do
    mcomment <- csGet commentStorage thread cid
    case mcomment of
        Just comment -> do
            _    <- requireAuthId
            mine <- isCommentingUser comment
            unless mine $ permissionDenied "you can only manage your own comments"
            f comment

        Nothing -> notFound

-- | Runs the form and updates the comment on success
runFormEdit :: YesodComments m => Comment -> ThreadId -> Maybe UserDetails -> GWidget CommentsAdmin m ()
runFormEdit comment = runFormWith (Just comment) $ \cf -> do
    tm <- getRouteToMaster
    csUpdate commentStorage comment $ comment { cContent = formComment cf }
    setMessage "comment updated."
    redirect $ tm CommentsR

layout :: Yesod m => Text -> GWidget s m () -> GHandler s m RepHtml
layout title inner = defaultLayout $ do
    setTitle $ toHtml title

    [whamlet|
        <div .page_header>
            <h1>#{title}

        <div .yesod_comments>
            ^{inner}
    |]
