{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.Core
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Yesod.Comments.Core
    ( Comment(..)
    , CommentForm(..)
    , CommentId
    , ThreadId
    , YesodComments (..)
    , commentFromForm
    , commentForm
    , commentFormAuth
    , showComment
    , showCommentAuth
    ) where

import Yesod
import Yesod.Markdown -- my fork, <https://github.com/pbrisbin/yesod-markdown> required
import Yesod.Form.Core
import Yesod.Helpers.Auth

import Data.Time
import System.Locale

import Data.Char           (isSpace)
import Control.Applicative ((<$>), (<*>))
import Data.Time.Clock     (UTCTime, getCurrentTime)
import Network.Wai         (remoteHost)
import Text.Blaze          (toHtml)

type ThreadId  = String
type CommentId = Int

class Yesod m => YesodComments m where
    -- Data base actions
    getComment    :: ThreadId -> CommentId -> GHandler s m (Maybe Comment)
    storeComment  :: Comment -> GHandler s m ()
    deleteComment :: Comment -> GHandler s m ()

    -- Loading onto pages
    loadComments     :: ThreadId -> GHandler s m [Comment]

    getNextCommentId :: [Comment] -> GHandler s m CommentId
    getNextCommentId [] = return 1
    getNextCommentId cs = return $ maximum (map commentId cs) + 1

    --- other
    commentFilters :: [(Comment -> GHandler s m Bool)]
    commentFilters = [const $ return False]

    -- | if using Auth, provide the function to get from a user id to 
    --   the string to use as the commenter's username
    displayUser :: AuthId m -> GHandler s m String
    displayUser _ = return ""

data Comment = Comment
    { threadId  :: ThreadId
    , commentId :: CommentId
    , timeStamp :: UTCTime
    , ipAddress :: String
    , userName  :: String
    , content   :: Markdown
    } deriving (Eq,Show)

data CommentForm = CommentForm
    { formUser    :: String
    , formComment :: Markdown
    } deriving Show

-- | Render from markdown, yesod-style
markdownToHtml :: Markdown -> Html
markdownToHtml = writePandoc yesodDefaultWriterOptions . parseMarkdown yesodDefaultParserState

-- | Cleanse form input and create a 'Comment' to be stored
commentFromForm :: ThreadId -> CommentId -> CommentForm -> GHandler s m Comment
commentFromForm tid cid cf = do
    now <- liftIO getCurrentTime
    ip  <- return . show . remoteHost =<< waiRequest
    return Comment 
        { threadId  = tid 
        , commentId = cid 
        , timeStamp = now
        , ipAddress = ip
        , userName  = formUser cf
        , content   = formComment cf
        }

-- | The comment form itself
commentForm :: GFormMonad s m (FormResult CommentForm, GWidget s m ())
commentForm = do
    (user   , fiUser   ) <- stringField   "name:"    Nothing
    (comment, fiComment) <- markdownField "comment:" Nothing
    return (CommentForm <$> user <*> comment, [hamlet|
        <table>
            ^{fieldRow fiUser}
            ^{fieldRow fiComment}
            <tr>
                <td>&nbsp;
                <td colspan="2">
                    <input type="submit" value="Add comment">
        |])

-- | The comment form if using authentication (uid is hidden and display
--   name is shown 
commentFormAuth :: String -> String -> GFormMonad s m (FormResult CommentForm, GWidget s m ())
commentFormAuth uid username = do
    (user   , fiUser   ) <- hiddenField   "name:"    (Just $ uid)
    (comment, fiComment) <- markdownField "comment:" Nothing
    return (CommentForm <$> user <*> comment, [hamlet|
        <table>
            <tr style="display: none;">
                    <th>
                        <label for="#{fiIdent fiUser}">&nbsp;
                    <td colspan="s">
                        ^{fiInput fiUser}

            <tr>
                <th>name:
                <td colspan="2">#{username}

            ^{fieldRow fiComment}
            <tr>
                <td>&nbsp;
                <td colspan="2">
                    <input type="submit" value="Add comment">
        |])

fieldRow :: FieldInfo s m -> GWidget s m ()
fieldRow fi = [hamlet|
    <tr .#{toHtml (clazz fi)}>
        <th>
            <label for="#{fiIdent fi}">#{fiLabel fi}
            <div .tooltip>#{fiTooltip fi}
        <td>
            ^{fiInput fi}
        <td>
            $maybe error <- fiErrors fi
                #{error}
            $nothing
                &nbsp;
    |]


clazz :: FieldInfo s m -> String
clazz fi = if fiRequired fi then "required" else "optional"

-- | Show a single comment
showComment :: Yesod m => Comment -> GWidget s m ()
showComment comment = showHelper comment $ userName comment

-- | Show a single comment, auth version
showCommentAuth :: (Yesod m, YesodAuth m, YesodComments m) => Comment -> GWidget s m ()
showCommentAuth comment = do
    let cusername = userName comment
    muid <- lift $ fmap (flip readAuthId cusername) getYesod
    case muid of
        Nothing  -> showHelper comment cusername
        Just uid -> showHelper comment =<< lift (displayUser uid)

-- | Factor out common code
showHelper :: Yesod m => Comment -> String -> GWidget s m ()
showHelper comment username = do
    commentTimestamp <- fmap (flip humanReadableTimeDiff (timeStamp comment)) $ liftIO getCurrentTime
    let anchor = "#comment_" ++ show (commentId comment)
    addHamlet [hamlet|
        <p>
            <a href="#{anchor}" id="#{anchor}">#{commentTimestamp}
            , #{username} wrote:

        <blockquote>
            #{markdownToHtml $ content comment}
        |]

-- <https://github.com/snoyberg/haskellers/blob/master/Haskellers.hs>
-- <https://github.com/snoyberg/haskellers/blob/master/LICENSE>
humanReadableTimeDiff :: UTCTime     -- ^ current time
                      -> UTCTime     -- ^ old time
                      -> String
humanReadableTimeDiff curTime oldTime = helper diff

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
