{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE RankNTypes                  #-}
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
    ( getCommentsAdmin
    , module Yesod.Comments.Management.Routes
    ) where

import Yesod
import Yesod.Auth

import Yesod.Comments.Core
import Yesod.Comments.Utils
import Yesod.Comments.Form
import Yesod.Comments.View
import Yesod.Comments.Management.Routes

import Control.Monad (forM, unless)
import Data.List (sortBy, nub)
import Data.Text (Text)
import Data.Time (UTCTime)

getCommentsAdmin :: a -> CommentsAdmin
getCommentsAdmin = const CommentsAdmin

instance YesodComments master => YesodSubDispatch CommentsAdmin (HandlerT master IO)
    where yesodSubDispatch = $(mkYesodSubDispatch resourcesCommentsAdmin)

type Handler a = forall master. YesodComments master
               => HandlerT CommentsAdmin (HandlerT master IO) a

getCommentsR :: Handler RepHtml
getCommentsR = lift $ do
    comments <- getThreadedComments

    layout "Your comments" [whamlet|
        $forall (t, cs) <- comments
            <div .thread>
                <h3>
                    <a href="@{threadRoute t}">#{t}

                <div .comments>
                    ^{showComments cs}
        |]

getEditCommentR :: ThreadId -> CommentId -> Handler RepHtml
getEditCommentR thread cid = do
    ud@(UserDetails _ name email) <- lift $ requireUserDetails

    -- TODO: Duplication with withUserComment
    comment <- lift $ do
        mcomment <- csGet commentStorage thread cid
        case mcomment of
            Just comment -> do
                _    <- requireAuthId
                mine <- isCommentingUser comment
                unless mine $ permissionDenied "you can only manage your own comments"
                return comment

            Nothing -> notFound

    ((res, form), enctype) <- lift $ runFormPost (commentForm thread ud (Just comment))

    case res of
        FormSuccess cf -> do
            lift $ csUpdate commentStorage comment $ comment { cContent = formComment cf }
            setMessage "comment updated."
            redirect CommentsR
        _ -> return ()

    -- TODO: Duplication with runFormWith
    lift $ layout "Edit comment" [whamlet|
        <div .avatar>
            <a target="_blank" title="change your profile picture at gravatar" href="http://gravatar.com/emails/">
                <img src="#{gravatar 48 email}">

        <div .input>
            <form enctype="#{enctype}" method="post" .form-stacked>
                <div .clearfix .optional>
                    <label for="username">Username
                    <div .input>
                        <p #username>#{name}

                ^{form}

                <div .actions>
                    <button .btn .primary type="submit">Add comment
    |]

postEditCommentR :: ThreadId -> CommentId -> Handler RepHtml
postEditCommentR = getEditCommentR

getDeleteCommentR :: ThreadId -> CommentId -> Handler RepHtml
getDeleteCommentR _ _ = lift $ layout "Delete comment" [whamlet|
    <p>Are you sure?
    <form method="post" .form-stacked>
        <div .actions>
            <button .btn .btn-danger type="submit">Delete comment
    |]

postDeleteCommentR :: ThreadId -> CommentId -> Handler RepHtml
postDeleteCommentR thread cid = do
    lift $ withUserComment thread cid $ \c -> do
        csDelete commentStorage c
        setMessage "comment deleted."

    redirect CommentsR

-- | Return tuples of thread id and associated comments sorted by most
--   recently commented on thread.
getThreadedComments :: YesodComments m => HandlerT m IO [(ThreadId, [Comment])]
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
            -- reversed comparison so more recent threads sort first
            case compare (latest' cs1) (latest' cs2) of
                EQ -> compare t1 t2
                GT -> LT
                LT -> GT

        latest' :: [Comment] -> UTCTime
        latest' = maximum . map cTimeStamp

-- | If the comment belongs to the currently logged in user, runs the
--   action on it. Otherwise, halts with @permissionDenied@.
withUserComment :: YesodComments m => ThreadId -> CommentId -> (Comment -> HandlerT m IO a) -> HandlerT m IO a
withUserComment thread cid f = do
    mcomment <- csGet commentStorage thread cid
    case mcomment of
        Just comment -> do
            _    <- requireAuthId
            mine <- isCommentingUser comment
            unless mine $ permissionDenied "you can only manage your own comments"
            f comment

        Nothing -> notFound

layout :: Yesod m => Text -> WidgetT m IO () -> HandlerT m IO RepHtml
layout title inner = defaultLayout $ do
    setTitle $ toHtml title

    [whamlet|
        <div .page_header>
            <h1>#{title}

        <div .yesod_comments>
            ^{inner}
    |]
