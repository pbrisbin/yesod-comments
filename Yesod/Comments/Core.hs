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
import Yesod.Form.Core
import Yesod.Helpers.Auth
import Yesod.Goodies.Gravatar
import Yesod.Goodies.Markdown
import Yesod.Goodies.Time
import Control.Applicative ((<$>), (<*>))
import Data.Time           (UTCTime, getCurrentTime)
import Network.Wai         (remoteHost)

import qualified Data.Text as T

type ThreadId  = T.Text
type CommentId = Int

class Yesod m => YesodComments m where
    -- Data base actions
    getComment    :: ThreadId -> CommentId -> GHandler s m (Maybe Comment)
    storeComment  :: Comment -> GHandler s m ()
    deleteComment :: Comment -> GHandler s m ()

    -- | Loading all comments, possibly filtered to a single thread.
    loadComments  :: Maybe ThreadId -> GHandler s m [Comment]

    -- | Get the next available Id given the passed list of comments. In 
    --   Handler in case there is a database call involved.
    getNextCommentId :: [Comment] -> GHandler s m CommentId
    getNextCommentId [] = return 1
    getNextCommentId cs = return $ maximum (map commentId cs) + 1

    -- | See "Yesod.Comments.Filters"
    commentFilters :: [(Comment -> GHandler s m Bool)]
    commentFilters = []

    -- | if using Auth, provide the function to get from a user id to 
    --   the string to use as the commenter's username. This should 
    --   return something friendlier than just a conversion to 'String'
    displayUser :: AuthId m -> GHandler s m T.Text
    displayUser _ = return ""

    -- | if using Auth, provide the function to get form a user id to 
    --   the string to use as the commenter's email.
    displayEmail :: AuthId m -> GHandler s m T.Text
    displayEmail _ = return ""

data Comment = Comment
    { threadId  :: ThreadId
    , commentId :: CommentId
    , timeStamp :: UTCTime
    , ipAddress :: T.Text
    , userName  :: T.Text
    , userEmail :: T.Text
    , content   :: Markdown
    }

data CommentForm = CommentForm
    { formUser    :: T.Text
    , formEmail   :: T.Text
    , formComment :: Markdown
    }

-- | Cleanse form input and create a 'Comment' to be stored
commentFromForm :: ThreadId -> CommentId -> CommentForm -> GHandler s m Comment
commentFromForm tid cid cf = do
    now <- liftIO getCurrentTime
    ip  <- return . show . remoteHost =<< waiRequest
    return Comment 
        { threadId  = tid 
        , commentId = cid 
        , timeStamp = now
        , ipAddress = T.pack ip
        , userName  = formUser cf
        , userEmail = formEmail cf
        , content   = formComment cf
        }

-- | The comment form itself
commentForm :: GFormMonad s m (FormResult CommentForm, GWidget s m ())
commentForm = do
    (user   , fiUser   ) <- stringField   "name:"    Nothing
    (email  , fiEmail  ) <- emailField    "email:"   Nothing
    (comment, fiComment) <- markdownField "comment:" Nothing
    return (CommentForm <$> user <*> email <*> comment, [hamlet|
        <table>
            ^{fieldRow fiUser}
            ^{fieldRow fiEmail}
            ^{fieldRow fiComment}
            <tr>
                <td>&nbsp;
                <td colspan="2">
                    <input type="submit" value="Add comment">
        |])

-- | The comment form if using authentication (uid is hidden and display
--   name is shown)
commentFormAuth :: T.Text -> T.Text -> T.Text -> GFormMonad s m (FormResult CommentForm, GWidget s m ())
commentFormAuth uid username email = do
    (user   , fiUser   ) <- hiddenField   "name:"    (Just uid)
    (email' , fiEmail  ) <- hiddenField   "email:"   (Just email)
    (comment, fiComment) <- markdownField "comment:" Nothing

    let img = gravatarImg email defaultOptions

    return (CommentForm <$> user <*> email' <*> comment, [hamlet|
        <div .yesod_comment_avatar_input>
            <a title="change your profile picture at gravatar" href="http://gravatar.com/emails/">
                <img src="#{img}">

        <table>
            <tr style="display: none;">
                    <th>
                        <label for="#{fiIdent fiUser}">&nbsp;
                    <td colspan="2">
                        ^{fiInput fiUser}

            <tr style="display: none;">
                    <th>
                        <label for="#{fiIdent fiEmail}">&nbsp;
                    <td colspan="2">
                        ^{fiInput fiEmail}

            <tr>
                <th>name:
                <td colspan="2">#{username}

            <tr>
                <th>email:
                <td colspan="2">#{email}

            ^{fieldRow fiComment}
            <tr>
                <td>&nbsp;
                <td colspan="2">
                    <input type="submit" value="Add comment">
        |])

fieldRow :: FieldInfo s m -> GWidget s m ()
fieldRow fi = [hamlet|
    <tr .#{clazz fi}>
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
    case fromSinglePiece $ cusername of
        Nothing  -> showHelper comment cusername
        Just uid -> showHelper comment =<< lift (displayUser uid)

-- | Factor out common code
showHelper :: Yesod m => Comment -> T.Text -> GWidget s m ()
showHelper comment username = do
    commentTimestamp <- lift . humanReadableTime $ timeStamp comment
    let anchor = "#comment_" ++ show (commentId comment)
    let img = gravatarImg (userEmail comment) defaultOptions { gSize = Just $ Size 20 }
    addHamlet [hamlet|
        <div .yesod_comment_avatar_list>
            <img src="#{img}">

        <p>
            <a href="#{anchor}" id="#{anchor}">#{commentTimestamp}
            , #{username} wrote:

        <blockquote>
            #{markdownToHtml $ content comment}
        |]
