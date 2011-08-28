{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
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
    , commentFormEdit
    , handleForm
    , handleFormEdit
    , addStyling
    , showComment
    , showCommentAuth
    , getNextCommentId
    , isCommentingUser
    ) where

import Yesod
import Yesod.Auth
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
    -- | Find a specific comment
    getComment :: ThreadId -> CommentId -> GHandler s m (Maybe Comment)

    -- | Store a new comment
    storeComment :: Comment -> GHandler s m ()

    -- | Update a comment
    updateComment :: Comment -> Comment -> GHandler s m ()

    -- | Remove a comment
    deleteComment :: Comment -> GHandler s m ()

    -- | Load all comments, possibly filtered to a single thread.
    loadComments :: Maybe ThreadId -> GHandler s m [Comment]

    -- | If using Auth, provide the function to get from a user id to 
    --   the string to use as the commenter's username. This should 
    --   return something friendly probably pulled from the user's
    --   profile on your site.
    displayUser :: AuthId m -> GHandler s m T.Text
    displayUser _ = return "" -- fixme: use toSinglePiece in new auth pkg

    -- | If using Auth, provide the function to get from a user id to 
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
    , isAuth    :: Bool
    }

instance Eq Comment where
    a == b = (threadId a == threadId b) && (commentId a == commentId b)

data CommentForm = CommentForm
    { formUser    :: T.Text
    , formEmail   :: T.Text
    , formComment :: Markdown
    , formIsAuth  :: Bool
    }

-- | Cleanse form input and create a 'Comment' to be stored
commentFromForm :: YesodComments m => ThreadId -> CommentForm -> GHandler s m Comment
commentFromForm tid cf = do
    now <- liftIO getCurrentTime
    ip  <- return . show . remoteHost =<< waiRequest
    cid <- getNextCommentId tid
    return Comment 
        { threadId  = tid 
        , commentId = cid 
        , timeStamp = now
        , ipAddress = T.pack ip
        , userName  = formUser    cf
        , userEmail = formEmail   cf
        , content   = formComment cf
        , isAuth    = formIsAuth  cf
        }

-- | The comment form itself
commentForm :: RenderMessage m FormMessage => Html -> Form s m (FormResult CommentForm, GWidget s m ())
commentForm fragment = do
    (user   , fiUser   ) <- mreq textField     "name:"    Nothing
    (email  , fiEmail  ) <- mreq emailField    "email:"   Nothing
    (comment, fiComment) <- mreq markdownField "comment:" Nothing
    return (CommentForm <$> user <*> email <*> comment <*> FormSuccess False, [whamlet|
        #{fragment}
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
commentFormAuth :: RenderMessage m FormMessage
                => T.Text -- ^ text version of uid
                -> T.Text -- ^ friendly name
                -> T.Text -- ^ email
                -> Html   -- ^ nonce fragment
                -> Form s m (FormResult CommentForm, GWidget s m ())
commentFormAuth user username email fragment = do
    let img = gravatarImg email defaultOptions { gDefault = Just MM }

    (fComment, fiComment) <- mreq markdownField "comment:" Nothing
    return (CommentForm <$> FormSuccess user <*> FormSuccess email <*> fComment <*> FormSuccess True, [whamlet|
        #{fragment}
        <div .yesod_comment_avatar_input>
            <a title="change your profile picture at gravatar" href="http://gravatar.com/emails/">
                <img src="#{img}">

        <table>
            <tr>
                <th>name:
                <td colspan="2">#{username}

            ^{fieldRow fiComment}
            <tr>
                <td>&nbsp;
                <td colspan="2">
                    <input type="submit" value="Add comment">
        |])

-- | The comment form used in the management edit page.
commentFormEdit :: RenderMessage m FormMessage
                => Comment
                -> Html
                -> Form s m (FormResult CommentForm, GWidget s m ())
commentFormEdit comment fragment = do
    (fComment, fiComment) <- mreq markdownField "comment:" (Just $ content comment)
    return (CommentForm <$> FormSuccess "" <*> FormSuccess "" <*> fComment <*> FormSuccess True, [whamlet|
        #{fragment}
        <table>
            ^{fieldRow fiComment}
            <tr>
                <td>&nbsp;
                <td colspan="2">
                    <input type="submit" value="Update comment">
        |])

fieldRow :: FieldView s m -> GWidget s m ()
fieldRow fv = [whamlet|
    <tr .#{clazz fv}>
        <th>
            <label for="#{fvId fv}">#{fvLabel fv}
            $maybe tt <- fvTooltip fv
                <div .tooltip>#{tt}
        <td>
            ^{fvInput fv}
        <td>
            $maybe error <- fvErrors fv
                #{error}
            $nothing
                &nbsp;
    |]

clazz :: FieldView s m -> String
clazz fv = if fvRequired fv then "required" else "optional"

-- | Add some cassius that is common to all the yesod-comments pages
addStyling :: Yesod m => GWidget s m ()
addStyling = addCassius [cassius|
    .yesod_comment_input th
        text-align: left
        vertical-align: top
    .yesod_comment_input textarea
        height: 10ex
        width: 50ex
    .yesod_comment_avatar_input, .yesod_comment_avatar_list
        float: left
    .yesod_comment_avatar_input
        margin-right: 5px
    .yesod_comment_avatar_list
        margin-right: 3px
    |]

-- | POST the form and insert the new comment
handleForm :: YesodComments m
           => FormResult CommentForm
           -> ThreadId
           -> GWidget s m ()
handleForm res tid = case res of
    FormMissing    -> return ()
    FormFailure _  -> return ()
    FormSuccess cf -> lift $ do
        storeComment =<< commentFromForm tid cf
        setMessage "comment added."
        redirectCurrentRoute

-- | POST the form and update an existing comment
handleFormEdit :: YesodComments m
               => Route m
               -> FormResult CommentForm
               -> Comment
               -> GWidget s m ()
handleFormEdit r res comment = case res of
    FormMissing    -> return ()
    FormFailure _  -> return ()
    FormSuccess cf -> lift $ do
        updateComment comment $ comment { content = formComment cf }
        setMessage "comment updateded."
        redirect RedirectTemporary r

-- | Redirect back to the current route after a POST request
redirectCurrentRoute :: Yesod m => GHandler s m ()
redirectCurrentRoute = do
    tm <- getRouteToMaster
    mr <- getCurrentRoute
    case mr of
        Just r  -> redirect RedirectTemporary $ tm r
        Nothing -> notFound

-- | Show a single comment
showComment :: Yesod m => Comment -> GWidget s m ()
showComment comment = showHelper comment (userName comment, userEmail comment)

-- | Show a single comment, auth version
showCommentAuth :: (Yesod m, YesodAuth m, YesodComments m) => Comment -> GWidget s m ()
showCommentAuth comment = do
    let cusername = userName comment

    (cuname, cemail) <-
        if isAuth comment
            then case fromSinglePiece $ cusername of
                Just uid -> do
                    uname <- lift $ displayUser  uid
                    email <- lift $ displayEmail uid
                    return (uname, email)
                _ -> return (cusername, userEmail comment)
            else return (cusername, userEmail comment)

    showHelper comment (cuname, cemail)

-- | Factor out common code
showHelper :: Yesod m => Comment -> (T.Text,T.Text) -> GWidget s m ()
showHelper comment (username, email) = do
    commentTimestamp <- lift . humanReadableTime $ timeStamp comment
    let anchor = "comment_" ++ show (commentId comment)
    let img    = gravatarImg email defaultOptions { gDefault = Just MM, gSize = Just $ Size 20 }
    addHamlet [hamlet|
        <div .yesod_comment_avatar_list>
            <img src="#{img}">

        <p>
            <a href="##{anchor}" id="#{anchor}">#{commentTimestamp}
            , #{username} wrote:

        <blockquote>
            #{markdownToHtml $ content comment}
        |]

-- | As the final step before insert, this is called to get the next
--   comment id for the thread. super-high concurrency is probably not
--   well-supported here...
getNextCommentId :: YesodComments m => ThreadId -> GHandler s m CommentId
getNextCommentId tid = go =<< loadComments (Just tid)

    where
        go :: YesodComments m => [Comment] -> GHandler s m CommentId
        go [] = return 1
        go cs = return $ maximum (map commentId cs) + 1

-- | Note: this function does not requireAuthId so a non-logged in user
--   just returns false.
isCommentingUser :: (YesodAuth m, YesodComments m)
                 => Comment
                 -> GHandler s m Bool
isCommentingUser comment = do
    muid <- maybeAuthId
    case muid of
        Just uid -> return $ isAuth comment && toSinglePiece uid == userName comment
        _        -> return False
