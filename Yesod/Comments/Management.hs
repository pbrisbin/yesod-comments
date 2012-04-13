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
import Yesod.Markdown
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM, unless)
import Data.List (sortBy, nub)
import Data.Time (UTCTime, formatTime)
import Language.Haskell.TH.Syntax hiding (lift)
import System.Locale (defaultTimeLocale, rfc822DateFormat)

data CommentsAdmin = CommentsAdmin

getCommentsAdmin :: a -> CommentsAdmin
getCommentsAdmin = const CommentsAdmin

mkYesodSub "CommentsAdmin"
    [ ClassP ''YesodAuth     [ VarT $ mkName "master" ]
    , ClassP ''YesodComments [ VarT $ mkName "master" ] ]
    [parseRoutes|
        /                               OverviewR  GET
        /view/#ThreadId/#CommentId      ViewR      GET
        /edit/#ThreadId/#CommentId      EditR      GET POST
        /delete/#ThreadId/#CommentId    DeleteR    GET
        |]

getOverviewR :: (YesodAuth m, YesodComments m) => GHandler CommentsAdmin m RepHtml
getOverviewR = do
    _       <- requireAuthId
    threads <- getThreadedComments
    defaultLayout $ do
        setTitle "Comments administration"

        [whamlet|
            <h1>Comments overview
            <div .yesod_comments .overview>
                $if null threads
                    <p>No comments found.
                $else
                    $forall thread <- threads
                         ^{showThreadedComments thread}
            |]

getViewR :: (YesodAuth m, YesodComments m) => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getViewR tid cid = withUserComment tid cid $ \comment ->
    defaultLayout $ do
        setTitle "View comment"

        [whamlet|
            <h1>View comment
            <div .yesod_comments .view>
                <table>
                    <tr>
                        <th>Thread:
                        <td>#{tid}
                    <tr>
                        <th>Comment Id:
                        <td>#{cid}
                    <tr>
                        <th>Source IP:
                        <td>#{ipAddress comment}
                    <tr>
                        <th>Time stamp:
                        <td>#{formatTimeStamp $ timeStamp comment}

                <p>
                    <strong>Comment:

                <blockquote>
                    #{markdownToHtml $ content comment}

                ^{updateLinks comment}
            |]

    where
        formatTimeStamp :: UTCTime -> String -- todo: make my own format
        formatTimeStamp = formatTime defaultTimeLocale rfc822DateFormat

getEditR :: (YesodAuth m, YesodComments m) => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getEditR tid cid = withUserComment tid cid $ \comment -> do
    tm <- getRouteToMaster
    ((res, form), enctype) <- runFormPost $ commentFormEdit comment
    defaultLayout $ do
        setTitle "Edit comment"
        handleFormEdit (tm OverviewR) res comment
        [whamlet|
            <h1>Edit comment
            <div .yesod_comments .edit>
                <h3>Update comment
                <div .input>
                    <form enctype="#{enctype}" method="post" .form-stacked>
                        ^{form}

                        <div .actions>
                            <button .btn .primary type="submit">Add comment
        |]

postEditR :: (YesodAuth m, YesodComments m) => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
postEditR = getEditR

getDeleteR :: (YesodAuth m, YesodComments m) => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getDeleteR tid cid = withUserComment tid cid $ \comment -> do
    tm <- getRouteToMaster
    deleteComment comment
    setMessage "comment deleted."
    redirect $ tm OverviewR

getThreadedComments :: (YesodAuth m, YesodComments m) => GHandler s m [(ThreadId, [Comment])]
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

showThreadedComments :: (YesodAuth m, YesodComments m) => (ThreadId, [Comment]) -> GWidget CommentsAdmin m ()
showThreadedComments (tid, comments) = [whamlet|
    <div .yesod_comments .thread>
        <h3>#{tid}
        $forall comment <- comments
            ^{showThreadComment comment}
    |]

    where
        showThreadComment :: (YesodAuth m, YesodComments m) => Comment -> GWidget CommentsAdmin m ()
        showThreadComment comment = do
            mine <- lift $ isCommentingUser comment
            [whamlet|
                $if mine
                    <div .yours>
                        ^{showCommentAuth comment}
                        ^{updateLinks comment}
                $else
                    <div>
                        ^{showCommentAuth comment}
                |]

updateLinks :: (YesodAuth m, YesodComments m) => Comment -> GWidget CommentsAdmin m ()
updateLinks (Comment tid cid _ _ _ _ _ _ )= do
    tm <- lift $ getRouteToMaster
    [whamlet|
        <div .update_links>
            <p>
                <a href=@{tm $ ViewR tid cid}>View
                \ | 
                <a href=@{tm $ EditR tid cid}>Edit
                \ | 
                <a href=@{tm $ DeleteR tid cid}>Delete
        |]

-- | Find a comment by thread/id, ensure it's the logged in user's
--   comment and execute an action on it. Gives notFound or
--   permissionDenied in failing cases.
withUserComment :: (YesodAuth m, YesodComments m)
                => ThreadId
                -> CommentId
                -> (Comment-> GHandler s m a)
                -> GHandler s m a
withUserComment tid cid f = do
    mcomment <- getComment tid cid
    case mcomment of
        Just comment -> do
            _    <- requireAuthId
            mine <- isCommentingUser comment
            unless mine $ permissionDenied "you can only manage your own comments"
            f comment

        Nothing -> notFound

commentFormEdit :: RenderMessage m FormMessage => Comment -> Form s m CommentForm
commentFormEdit comment = renderBootstrap $ CommentForm
    <$> pure "" <*> pure ""
    <*> areq markdownField commentLabel (Just $ content comment)
    <*> pure True

handleFormEdit :: YesodComments m => Route m -> FormResult CommentForm -> Comment -> GWidget s m ()
handleFormEdit r (FormSuccess cf) comment = lift $ do
    updateComment comment $ comment { content = formComment cf }
    setMessage "comment updated."
    redirect r

handleFormEdit _ _ _ = return ()
