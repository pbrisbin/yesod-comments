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
import Yesod.Markdown

import Data.Text (Text)
import Data.Time.Format.Human
import Network.Gravatar

showComments :: YesodComments m => [Comment] -> GWidget s m ()
showComments comments = [whamlet|
    <div .list>
        $if not $ null comments
            <h4>#{toHtml $ helper $ length comments}:

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
    commentTimestamp <- lift . liftIO . humanReadableTime $ timeStamp comment

    let cusername = userName comment
    let anchor    = "comment_" ++ show (commentId comment)

    (username, email) <-
        if isAuth comment
            then case fromPathPiece $ cusername of
                Just uid -> do
                    u <- lift $ displayUser  uid
                    e <- lift $ displayEmail uid
                    return (u, e)
                _ -> return (cusername, userEmail comment)
            else return (cusername, userEmail comment)

    [whamlet|
        <div .comment>
            <div .attribution>
                <p>
                    <span .avatar>
                        <img src="#{img email}">

                    <a href="##{anchor}" id="#{anchor}">#{commentTimestamp}
                    , #{username} wrote:

            <div .content>
                <blockquote>
                    #{markdownToHtml $ content comment}
        |]

    where
        img :: Text -> String
        img = gravatar defaultConfig { gDefault = Just MM, gSize = Just $ Size 20 }
