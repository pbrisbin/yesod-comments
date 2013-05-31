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
  , runFormWith
  ) where

import Yesod
import Yesod.Markdown
import Yesod.Comments.Core
import Yesod.Comments.Utils

import Control.Applicative ((<$>), (<*>), pure)
import Data.Time   (getCurrentTime)
import Network.Wai (remoteHost)

import qualified Data.Text as T

type Form m x = Html -> MForm (HandlerT m IO) (FormResult x, WidgetT m IO ())

data CommentForm = CommentForm
    { formUser    :: UserDetails
    , formThread  :: ThreadId
    , formComment :: Markdown
    }

commentFromForm :: YesodComments m => CommentForm -> HandlerT m IO Comment
commentFromForm cf = do
    now <- liftIO getCurrentTime
    ip  <- fmap (show . remoteHost) waiRequest
    cid <- getNextCommentId $ formThread cf

    return Comment
        { commentId = cid
        , cThreadId  = formThread cf
        , cTimeStamp = now
        , cIpAddress = T.pack ip
        , cUserName  = textUserId $ formUser cf
        , cUserEmail = emailAddress $ formUser cf
        , cContent   = formComment cf
        , cIsAuth    = True
        }

    where
        getNextCommentId :: YesodComments m => ThreadId -> HandlerT m IO CommentId
        getNextCommentId tid = go =<< csLoad commentStorage (Just tid)

        go :: YesodComments m => [Comment] -> HandlerT m IO CommentId
        go [] = return 1
        go cs = return $ maximum (map commentId cs) + 1

commentForm :: RenderMessage m FormMessage => ThreadId -> UserDetails -> Maybe Comment -> Form m CommentForm
commentForm thread udetails mcomment = renderBootstrap $ CommentForm
    <$> pure udetails <*> pure thread
    <*> areq markdownField commentLabel (fmap cContent mcomment)

    where
        commentLabel ::  FieldSettings master
        commentLabel = "Comment" { fsTooltip = Just "Comments are parsed as pandoc-style markdown." }

-- | Run the form and stores the comment on successful submission
runForm :: YesodComments m => ThreadId -> Maybe UserDetails -> WidgetT m IO ()
runForm = runFormWith Nothing $ \cf -> do
    csStore commentStorage =<< commentFromForm cf
    setMessage "comment added."

    -- redirect to current route
    maybe notFound redirect =<< getCurrentRoute

-- | Both handle form submission and present form HTML. On @FormSuccess@,
--   run the given function on the submitted value.
runFormWith :: YesodComments m
            => Maybe Comment
            -> (CommentForm -> HandlerT m IO ())
            -> ThreadId
            -> Maybe UserDetails
            -> WidgetT m IO ()
runFormWith _ _ _ Nothing = [whamlet|<h4>Please ^{login} to post a comment.|]
runFormWith mcomment f thread (Just ud@(UserDetails _ name email)) = do
    ((res, form), enctype) <- handlerToWidget $ runFormPost (commentForm thread ud mcomment)

    case res of
        FormSuccess cf -> handlerToWidget $ f cf
        _              -> return ()

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

login :: Yesod m => WidgetT m IO ()
login = do
    mroute <- handlerToWidget $ do
        setUltDestCurrent
        fmap authRoute getYesod

    case mroute of
        Just r  -> [whamlet|<a href="@{r}">log in|]
        Nothing -> [whamlet|log in|]
