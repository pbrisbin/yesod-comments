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
    commentTimestamp         <- lift . liftIO . humanReadableTime $ timeStamp comment
    UserDetails _ name email <- lift $ commentUserDetails comment

    let anchor = "comment_" ++ show (commentId comment)

    [whamlet|
        <div .comment>
            <div .attribution>
                <p>
                    <span .avatar>
                        <img src="#{gravatar 20 email}">

                    <a href="##{anchor}" id="#{anchor}">#{commentTimestamp}
                    , #{name} wrote:

            <div .content>
                <blockquote>
                    #{markdownToHtml $ content comment}
        |]
