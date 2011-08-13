{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Yesod.Comments.Management where

import Yesod
import Yesod.Helpers.Auth
import Yesod.Comments.Core
import Yesod.Goodies.Markdown
import Control.Monad (forM, unless)
import Data.List (nub, sort)
import Data.Time (UTCTime, formatTime)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Language.Haskell.TH.Syntax hiding (lift)

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
        addStyling
        [hamlet|
            <h1>Comments overview
            <article .yesod_comments_overview>
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
        addStyling
        [hamlet|
            <h1>View comment
            <article .yesod_comments_view_comment>
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
    ((res, form), enctype) <- runFormMonadPost $ commentFormEdit comment
    defaultLayout $ do
        setTitle "Edit comment"
        handleFormEdit (tm OverviewR) res comment
        addStyling
        [hamlet|
            <h1>Edit comment
            <article .yesod_comments_edit_comment>
                <h3>Update comment
                <div .yesod_comment_input>
                    <form enctype="#{enctype}" method="post">^{form}
                    <p .helptext>Comments are parsed as pandoc-style markdown
        |]

postEditR :: (YesodAuth m, YesodComments m) => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
postEditR = getEditR

getDeleteR :: (YesodAuth m, YesodComments m) => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getDeleteR tid cid = withUserComment tid cid $ \comment -> do
    tm <- getRouteToMaster
    deleteComment comment
    setMessage "comment deleted."
    redirect RedirectTemporary $ tm OverviewR

getThreadedComments :: (YesodAuth m, YesodComments m) => GHandler s m [(ThreadId, [Comment])]
getThreadedComments = do
    allComments <- loadComments Nothing
    comments' <- forM allComments $ \comment -> do
        check <- isCommentingUser comment
        return $ if check then [comment] else []

    let comments = concat comments'

    forM (sort . nub $ map threadId comments) $ \tid ->
        return $ (tid, filter ((== tid) . threadId) comments)

showThreadedComments :: (YesodAuth m, YesodComments m) => (ThreadId, [Comment]) -> GWidget CommentsAdmin m ()
showThreadedComments (tid, comments) = [hamlet|
    <div .yesod_comments_overview_thread>
        <h3>#{tid}
        $forall comment <- comments
            <div .yesod_comments_overview_comment>
                ^{showCommentAuth comment}
                ^{updateLinks comment}
    |]

updateLinks :: (YesodAuth m, YesodComments m) => Comment -> GWidget CommentsAdmin m ()
updateLinks (Comment tid cid _ _ _ _ _ _ )= do
    tm <- lift $ getRouteToMaster
    [hamlet|
        <div .yesod_comments_update_links>
            <p>
                <a href=@{tm $ ViewR tid cid}>View
                \ | 
                <a href=@{tm $ EditR tid cid}>Edit
                \ | 
                <a href=@{tm $ DeleteR tid cid}>Delete
        |]

-- | Find a comment by thread/id, ensure it's the logged in user's
--   comment and execute and action on it. Gives notFound or
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
            _     <- requireAuthId
            check <- isCommentingUser comment
            unless check $ permissionDenied "you can only manage your own comments"
            f comment

        Nothing -> notFound
