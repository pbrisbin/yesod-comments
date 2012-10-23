{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
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
-- This module allows for self-management of comments by any
-- authenticating commenter on your site.
--
-- The use, add a route like so:
--
-- > /comments CommentsAdminR CommentsAdmin getCommentsAdmin
--
-- Then place a link somewhere to @CommentsAdminR OverviewR@.
--
-- The overview page will show all of the comments (grouped by thread)
-- that the currently logged in user has left on the site along with
-- links to view more details, edit the comment content, or delete the
-- comment entirely.
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

    defaultLayout $ do
        setTitle "All comments"

        [whamlet|
            <div .yesod_comments>
                $forall (t, cs) <- comments
                    <div .thread>
                        <h3>
                            ^{linkTo t}
                        <div .comments>
                            ^{showComments cs}
        |]

    where
        linkTo :: YesodComments m => ThreadId -> GWidget s m ()
        linkTo thread = [whamlet|
            $maybe threadR <- threadRoute
                <a href="@{threadR thread}">#{thread}
            $nothing
                #{thread}
        |]

getEditCommentR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getEditCommentR thread cid = withUserComment thread cid $ \c -> do
    ud <- requireUserDetails

    defaultLayout $ do
        setTitle "Edit comment"
        [whamlet|
            <div .yesod_comments>
                ^{runFormEdit c thread (Just ud)}
        |]

postEditCommentR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
postEditCommentR = getEditCommentR

getDeleteCommentR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getDeleteCommentR _ _ = defaultLayout $ do
    setTitle "Delete comment"
    [whamlet|
        <div .yesod_comments>
            <p>Are you sure?
            <form method="post" .form-stacked>
                <div .actions>
                    <button .btn .btn-danger type="submit">Delete comment
    |]

postDeleteCommentR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
postDeleteCommentR thread cid = withUserComment thread cid $ \c -> do
    tm <- getRouteToMaster
    deleteComment c
    setMessage "comment deleted."
    redirect $ tm CommentsR

getThreadedComments :: YesodComments m => GHandler s m [(ThreadId, [Comment])]
getThreadedComments = do
    allComments <- loadComments Nothing
    allThreads  <- forM allComments $ \comment -> do
        mine <- isCommentingUser comment
        return $ if mine then [threadId comment] else []

    unsorted <- forM (nub $ concat allThreads) $ \tid ->
        return (tid, filter ((== tid) . threadId) allComments)

    return . sortBy latest $ unsorted

latest :: (ThreadId, [Comment]) -> (ThreadId, [Comment]) -> Ordering
latest (t1, cs1) (t2,cs2) =
    -- note the comparason is reversed so that the more recent threads
    -- will sort first
    case compare (latest' cs1) (latest' cs2) of
        EQ -> compare t1 t2
        GT -> LT
        LT -> GT

    where
        latest' :: [Comment] -> UTCTime
        latest' = maximum . map timeStamp

withUserComment :: YesodComments m => ThreadId -> CommentId -> (Comment -> GHandler s m RepHtml) -> GHandler s m RepHtml
withUserComment thread cid f = do
    mcomment <- getComment thread cid
    case mcomment of
        Just comment -> do
            _    <- requireAuthId
            mine <- isCommentingUser comment
            unless mine $ permissionDenied "you can only manage your own comments"
            f comment

        Nothing -> notFound

runFormEdit :: YesodComments m => Comment -> ThreadId -> Maybe UserDetails -> GWidget CommentsAdmin m ()
runFormEdit comment = runFormWith (Just comment) $ \cf -> do
    tm <- getRouteToMaster

    updateComment comment $ comment { content = formComment cf }
    setMessage "comment updated."

    redirect $ tm CommentsR
