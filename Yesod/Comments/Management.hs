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
import Control.Monad (forM)
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
    requireAuthId
    comments <- getUsersComments
    defaultLayout $ do
        setTitle "Comments administration"
        addStyling
        [hamlet|
            <h1>Comments overview
            <article .yesod_comments_overview>
                $forall comment <- comments
                    <div .yesod_comments_overview_comment>
                        ^{showCommentAuth comment}
                        ^{updateLinks comment}
            |]

getViewR :: (YesodAuth m, YesodComments m) => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getViewR tid cid = do
    mcomment <- getComment tid cid
    case mcomment of
        Just comment -> defaultLayout $ do
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

        _ -> notFound

    where
        formatTimeStamp :: UTCTime -> String -- todo: make my own format
        formatTimeStamp = formatTime defaultTimeLocale rfc822DateFormat

getEditR :: (YesodAuth m, YesodComments m) => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getEditR tid cid = do
    mcomment <- getComment tid cid
    case mcomment of
        Just comment -> do
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
getDeleteR tid cid = do
    mcomment <- getComment tid cid
    case mcomment of
        Just comment -> do
            requireUserComment comment
            tm <- getRouteToMaster
            deleteComment comment
            setMessage "comment deleted."
            redirect RedirectTemporary $ tm OverviewR

        _ -> notFound

getUsersComments :: (YesodAuth m, YesodComments m) => GHandler s m [Comment]
getUsersComments = do
    comments  <- loadComments Nothing
    comments' <- forM comments $ \comment -> do
        check <- isCommentingUser comment
        if check then return [comment] else return []

    return $ concat comments'

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

-- | Require auth and require that the comment being edited/deleted was
--   actually entered by the user that's logged in.
requireUserComment :: (YesodAuth m, YesodComments m)
                   => Comment
                   -> GHandler s m ()
requireUserComment comment = do
    _     <- requireAuthId
    check <- isCommentingUser comment
    if check
        then return ()
        else permissionDenied "you can only manage your own comment"
