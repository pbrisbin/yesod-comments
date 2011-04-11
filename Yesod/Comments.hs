{-# LANGUAGE QuasiQuotes #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
-- Maintainer  :  pbrisbin@gmail.com 
-- Stability   :  unstable
-- Portability :  unportable
--
-- A generic Comments interface for a Yesod application. This module is
-- in the early stages of development. Beware bugs, patches welcome.
--
-------------------------------------------------------------------------------
module Yesod.Comments 
    ( addComments
    , addCommentsAuth
    , module Yesod.Comments.Core
    ) where

import Yesod
import Yesod.Comments.Core
import Yesod.Comments.Filters (applyFilters)
import Yesod.Helpers.Auth

import Data.Time
import System.Locale

import Data.Char (isSpace)
import Text.Blaze (toHtml)

-- | Add comments that anyone can enter anonymously
addComments :: YesodComments m 
            => ThreadId -- ^ the thread you're adding comments to
            -> GWidget s m ()
addComments tid = do
    comments <- lift $ loadComments tid
    cid      <- lift $ getNextCommentId comments
    
    -- run the form
    ((res, form), enctype) <- lift $ runFormMonadPost (commentForm Nothing)
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> lift $ do
            comment <- commentFromForm tid cid cf
            matches <- applyFilters commentFilters comment
            if matches
                then setMessage $ toHtml "comment dropped. matched filters."
                else do
                    storeComment comment
                    setMessage $ toHtml "comment added."
            redirectCurrentRoute

    -- make the input box a bit bigger
    addCassius [cassius|
        .yesod_comment_input th
            text-align: left
            vertical-align: top
        .yesod_comment_input textarea
            height: 10ex
            width: 50ex
        |]
        
    -- show the input form
    [hamlet|
        <div .yesod_comments>
            <h4>Add a comment:
            <div .yesod_comment_input>
                <form enctype="#{enctype}" method="post">
                    ^{form}
                <p .helptext>
                    Comments are parsed as pandoc-style markdown

            <h4>Showing #{toHtml $ helper $ length comments}:
            $forall comment <- comments
                <div .yesod_comment>
                    ^{showComment comment}
        |]

-- | Add comments that require site-wide authentication
addCommentsAuth :: (YesodComments m, YesodAuth m)
                => ThreadId -- ^ the thread you're adding comments to
                -> GWidget s m ()
addCommentsAuth tid = do
    (isAuthenticated, username) <- lift $ do
        muid <- maybeAuthId
        case muid of
            Nothing  -> return (False, "")
            Just uid -> do
                uname <- displayUser uid
                return (True, uname)

    comments <- lift $ loadComments tid
    cid      <- lift $ getNextCommentId comments
    
    -- run the form
    ((res, form), enctype) <- lift $ runFormMonadPost (commentForm $ Just username)
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> lift $ do
            comment <- commentFromForm tid cid cf
            matches <- applyFilters commentFilters comment
            if matches
                then setMessage $ toHtml "comment dropped. matched filters."
                else do
                    storeComment comment
                    setMessage $ toHtml "comment added."
            redirectCurrentRoute

    -- make the input box a bit bigger
    addCassius [cassius|
        .yesod_comment_input th
            text-align: left
            vertical-align: top
        .yesod_comment_input textarea
            height: 10ex
            width: 50ex
        |]
        
    -- show the input form
    [hamlet|
        <div .yesod_comments>
            $if isAuthenticated
                <h4>Add a comment:
                <div .yesod_comment_input>
                    <form enctype="#{enctype}" method="post">
                        ^{form}
                    <p .helptext>
                        Comments are parsed as pandoc-style markdown
            $else
                <h4>Please 
                    ^{loginLink}
                    \ to post a comment.

            $if not $ null comments
                <h4>Showing #{toHtml $ helper $ length comments}:
                $forall comment <- comments
                    <div .yesod_comment>
                        ^{showComment comment}
        |]

loginLink :: Yesod m => GWidget s m ()
loginLink = do
    lift $ setUltDest' -- so we come back here after login
    mroute <- lift $ fmap authRoute getYesod
    case mroute of
        Just r  -> [hamlet|<a href="@{r}">log in|]
        Nothing -> [hamlet|log in|] -- this shouldn't happen

helper :: Int -> String
helper 0 = "no comments"
helper 1 = "1 comment"
helper n = show n ++ " comments"

-- | Redirect back to the current route after a POST request
redirectCurrentRoute :: Yesod m => GHandler s m ()
redirectCurrentRoute = do
    tm <- getRouteToMaster
    mr <- getCurrentRoute
    case mr of
        Just r  -> redirect RedirectTemporary $ tm r
        Nothing -> notFound
    
-- | Show a single comment, provides numbered anchors
showComment :: Yesod m => Comment -> GWidget s m ()
showComment comment =  do
    commentContent   <- lift . markdownToHtml $ content comment
    commentTimestamp <- return . flip humanReadableTimeDiff (timeStamp comment) =<< liftIO getCurrentTime
    let anchor = "#comment_" ++ show (commentId comment)
    addHamlet [hamlet|
        <p>
            <a href="#{anchor}" id="#{anchor}">#{commentTimestamp}
            , #{userName comment} wrote:

        <blockquote>
            #{commentContent}
        |]

-- <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs>
-- <https://github.com/snoyberg/haskellers/blob/master/LICENSE>
humanReadableTimeDiff :: UTCTime     -- ^ current time
                      -> UTCTime     -- ^ old time
                      -> String
humanReadableTimeDiff curTime oldTime =
    helper diff
  where
    diff    = diffUTCTime curTime oldTime

    minutes :: NominalDiffTime -> Double
    minutes n = realToFrac $ n / 60

    hours :: NominalDiffTime -> Double
    hours   n = minutes n / 60

    days :: NominalDiffTime -> Double
    days    n = hours n / 24

    weeks :: NominalDiffTime -> Double
    weeks   n = days n / 7

    years :: NominalDiffTime -> Double
    years   n = days n / 365

    i2s :: RealFrac a => a -> String
    i2s n = show m where m = truncate n :: Int

    old = utcToLocalTime utc oldTime

    trim = f . f where f = reverse . dropWhile isSpace

    dow           = trim $! formatTime defaultTimeLocale "%l:%M %p on %A" old
    thisYear      = trim $! formatTime defaultTimeLocale "%b %e" old
    previousYears = trim $! formatTime defaultTimeLocale "%b %e, %Y" old

    helper  d | d < 1          = "just now"
              | d < 60         = i2s d ++ " seconds ago"
              | minutes d < 2  = "one minute ago"
              | minutes d < 60 =  i2s (minutes d) ++ " minutes ago"
              | hours d < 2    = "one hour ago"
              | hours d < 24   = "about " ++ i2s (hours d) ++ " hours ago"
              | days d < 5     = "at " ++ dow
              | days d < 10    = i2s (days d)  ++ " days ago"
              | weeks d < 2    = i2s (weeks d) ++ " week ago"
              | weeks d < 5    = i2s (weeks d)  ++ " weeks ago"
              | years d < 1    = "on " ++ thisYear
              | otherwise      = "on " ++ previousYears
