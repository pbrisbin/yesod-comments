{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.View
-- Copyright   :  (c) Patrick Brisbin 2010
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Yesod.Comments.View
  ( showComments
  , showComment
  ) where

import Yesod
import Yesod.Comments.Core
import Yesod.Comments.Utils
import Yesod.Markdown

import Data.Time.Format.Human
import Data.Monoid (mempty)

showComments :: YesodComments m => [Comment] -> GWidget s m ()
showComments comments = [whamlet|
    <div .list>
        $if not $ null comments
            <h4>#{helper $ length comments}:

            $forall comment <- comments
                ^{showComment comment}
    |]

    where
        -- pluralize comments
        helper :: Int -> String
        helper 0 = "no comments"
        helper 1 = "1 comment"
        helper n = show n ++ " comments"

showComment :: YesodComments m => Comment -> GWidget s m ()
showComment comment = do
    mine                     <- lift $ isCommentingUser comment
    commentTimestamp         <- lift . liftIO . humanReadableTime $ cTimeStamp comment
    UserDetails _ name email <- lift $ commentUserDetails comment

    let anchor = "comment_" ++ show (commentId comment)

    [whamlet|
        <div .comment :mine:.mine:>
            <div .attribution>
                <p>
                    <span .avatar>
                        <img src="#{gravatar 20 email}">

                    <a href="##{anchor}" id="#{anchor}">#{commentTimestamp}
                    , #{name} wrote:

            <div .content>
                <blockquote>
                    #{markdownToHtml $ cContent comment}

            $if mine
                 <div .controls>
                    ^{commentControls editRoute deleteRoute (cThreadId comment) (commentId comment)}
        |]

-- | Edit and Delete links if configured
commentControls :: Maybe (ThreadId -> CommentId -> Route m) -- ^ Edit route
                -> Maybe (ThreadId -> CommentId -> Route m) -- ^ Delete route
                -> ThreadId -> CommentId -> GWidget s m ()
commentControls e@(Just _) d@(Just _) thread cid = [whamlet|
    ^{commentControls e Nothing thread cid}
    \ | 
    ^{commentControls Nothing d thread cid}
    |]

commentControls (Just editR) Nothing        thread cid = [whamlet|<a href="@{editR thread cid}">Edit|]
commentControls Nothing      (Just deleteR) thread cid = [whamlet|<a href="@{deleteR thread cid}">Delete|]
commentControls _ _ _ _ = mempty
