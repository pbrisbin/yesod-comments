{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.Form
-- Copyright   :  (c) Patrick Brisbin 2010
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Yesod.Comments.Form
  ( CommentForm(..)
  , commentForm
  , commentFromForm
  , runForm
  ) where

import Yesod
import Yesod.Markdown
import Yesod.Comments.Core
import Yesod.Comments.Utils

import Control.Applicative ((<$>), (<*>), pure)
import Data.Time   (getCurrentTime)
import Network.Wai (remoteHost)

import qualified Data.Text as T

type Form s m x = Html -> MForm s m (FormResult x, GWidget s m ())

data CommentForm = CommentForm
    { formUser    :: UserDetails
    , formComment :: Markdown
    }

commentFromForm :: YesodComments m => ThreadId -> CommentForm -> GHandler s m Comment
commentFromForm thread cf = do
    now <- liftIO getCurrentTime
    ip  <- fmap (show . remoteHost) waiRequest
    cid <- getNextCommentId thread

    return Comment
        { threadId  = thread
        , commentId = cid
        , timeStamp = now
        , ipAddress = T.pack ip
        , userName  = textUserName $ formUser cf
        , userEmail = emailAddress $ formUser cf
        , content   = formComment cf
        , isAuth    = True
        }

    where
        getNextCommentId :: YesodComments m => ThreadId -> GHandler s m CommentId
        getNextCommentId tid = go =<< loadComments (Just tid)

        go :: YesodComments m => [Comment] -> GHandler s m CommentId
        go [] = return 1
        go cs = return $ maximum (map commentId cs) + 1

commentForm :: RenderMessage m FormMessage => UserDetails -> Form s m CommentForm
commentForm udetails = renderBootstrap $ CommentForm
    <$> pure udetails <*> areq markdownField commentLabel Nothing

    where
        commentLabel ::  FieldSettings master
        commentLabel = "Comment" { fsTooltip = Just "Comments are parsed as pandoc-style markdown." }

runForm :: YesodComments m => ThreadId -> Maybe UserDetails -> GWidget s m ()
runForm _ Nothing = [whamlet|<h4>Please ^{login} to post a comment.|]

    where
        login :: Yesod m => GWidget s m ()
        login = do
            mroute <- lift $ do
                setUltDestCurrent
                fmap authRoute getYesod

            case mroute of
                Just r  -> [whamlet|<a href="@{r}">log in|]
                Nothing -> [whamlet|log in|]

runForm thread (Just ud@(UserDetails _ name email)) = do
    ((res, form), enctype) <- lift $ runFormPost (commentForm ud)

    case res of
        FormSuccess cf -> lift $ do
            storeComment =<< commentFromForm thread cf
            setMessage "comment added."
            redirectCurrentRoute

        _ -> return ()

    [whamlet|
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

    where
        redirectCurrentRoute :: Yesod m => GHandler s m ()
        redirectCurrentRoute = do
            tm <- getRouteToMaster
            mr <- getCurrentRoute
            case mr of
                Just r  -> redirect $ tm r
                Nothing -> notFound
