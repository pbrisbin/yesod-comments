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
import Control.Monad (forM)
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
        [hamlet|
            <article .yesod_comments_overview>
                $forall comment <- comments
                   ^{showCommentAuth comment}
            |]

getUsersComments :: YesodComments m => GHandler s m [Comment]
getUsersComments = do
    comments  <- loadComments Nothing
    comments' <- forM comments $ \comment -> do
        check <- isCommentingUser comment
        if check then return [comment] else return []

    return $ concat comments'

getViewR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getViewR = undefined

getEditR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
getEditR = undefined

postEditR :: YesodComments m => ThreadId -> CommentId -> GHandler CommentsAdmin m RepHtml
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

-- | Require auth and require that the comment being edited/deleted was
--   actually entered by the user that's logged in.
requireUserComment :: (YesodAuth m, YesodComments m)
                   => Comment
                   -> GHandler s m ()
requireUserComment comment = undefined
