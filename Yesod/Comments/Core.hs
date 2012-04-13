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
    , Form
    , commentFromForm
    , commentForm
    , commentFormAuth
    , commentLabel
    , handleForm
    , showComments
    , showComment
    , showCommentAuth
    , getNextCommentId
    , isCommentingUser
    ) where

import Yesod
import Yesod.Auth
import Yesod.Markdown
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text           (Text)
import Data.Time           (UTCTime, getCurrentTime)
import Data.Time.Format.Human
import Network.Gravatar
import Network.Wai         (remoteHost)
import qualified Data.Text as T

type ThreadId  = Text
type CommentId = Int

class Yesod m => YesodComments m where
    getComment    :: ThreadId -> CommentId -> GHandler s m (Maybe Comment)
    storeComment  :: Comment -> GHandler s m ()
    updateComment :: Comment -> Comment -> GHandler s m ()
    deleteComment :: Comment -> GHandler s m ()
    loadComments  :: Maybe ThreadId -> GHandler s m [Comment]

    -- | If using Auth, provide the function to get from a user id to
    --   the string to use as the commenter's username. This should
    --   return something friendly probably pulled from the user's
    --   profile on your site.
    displayUser :: AuthId m -> GHandler s m Text
    displayUser _ = return ""

    -- | If using Auth, provide the function to get from a user id to
    --   the string to use as the commenter's email.
    displayEmail :: AuthId m -> GHandler s m Text
    displayEmail _ = return ""

data Comment = Comment
    { threadId  :: ThreadId
    , commentId :: CommentId
    , timeStamp :: UTCTime
    , ipAddress :: Text
    , userName  :: Text
    , userEmail :: Text
    , content   :: Markdown
    , isAuth    :: Bool
    }

instance Eq Comment where
    a == b = (threadId a == threadId b) && (commentId a == commentId b)

data CommentForm = CommentForm
    { formUser    :: Text
    , formEmail   :: Text
    , formComment :: Markdown
    , formIsAuth  :: Bool
    }

type Form s m x = Html -> MForm s m (FormResult x, GWidget s m ())

commentFromForm :: YesodComments m => ThreadId -> CommentForm -> GHandler s m Comment
commentFromForm tid cf = do
    now <- liftIO getCurrentTime
    ip  <- fmap (show . remoteHost) waiRequest
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

commentForm :: RenderMessage m FormMessage => Form s m CommentForm
commentForm = renderBootstrap $ CommentForm
    <$> areq textField  "Name"  Nothing
    <*> areq emailField "Email" Nothing
    <*> areq markdownField commentLabel Nothing
    <*> pure False

commentFormAuth :: RenderMessage m FormMessage
                => Text -- ^ Text version of uid
                -> Text -- ^ user's email
                -> Form s m CommentForm
commentFormAuth user email = renderBootstrap $ CommentForm
    <$> pure user <*> pure email
    <*> areq markdownField commentLabel Nothing
    <*> pure True


handleForm :: YesodComments m => FormResult CommentForm -> ThreadId -> GWidget s m ()
handleForm (FormSuccess cf) tid = lift $ do
    storeComment =<< commentFromForm tid cf
    setMessage "comment added."
    redirectCurrentRoute

handleForm _ _ = return ()


showComment :: Comment -> GWidget s m ()
showComment comment = showHelper comment (userName comment, userEmail comment)

showCommentAuth :: (YesodAuth m, YesodComments m) => Comment -> GWidget s m ()
showCommentAuth comment = do
    let cusername = userName comment

    (cuname, cemail) <-
        if isAuth comment
            then case fromPathPiece $ cusername of
                Just uid -> do
                    uname <- lift $ displayUser  uid
                    email <- lift $ displayEmail uid
                    return (uname, email)
                _ -> return (cusername, userEmail comment)
            else return (cusername, userEmail comment)

    showHelper comment (cuname, cemail)

showComments :: [Comment] -> (Comment -> GWidget s m ()) -> GWidget s m ()
showComments comments f = [whamlet|
    <div .list>
        $if not $ null comments
            <h4>Showing #{toHtml $ helper $ length comments}:

            $forall comment <- comments
                ^{f comment}
    |]

    where
        -- pluralize comments
        helper :: Int -> String
        helper 0 = "no comments"
        helper 1 = "1 comment"
        helper n = show n ++ " comments"

showHelper :: Comment -> (Text,Text) -> GWidget s m ()
showHelper comment (username, email) = do
    commentTimestamp <- lift . liftIO . humanReadableTime $ timeStamp comment

    let anchor = "comment_" ++ show (commentId comment)

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
        img = gravatar def { gDefault = Just MM, gSize = Just $ Size 20 }

-- | As the final step before insert, this is called to get the next
--   comment id for the thread. super-high concurrency is probably not
--   well-supported here.
getNextCommentId :: YesodComments m => ThreadId -> GHandler s m CommentId
getNextCommentId tid = go =<< loadComments (Just tid)

    where
        go :: YesodComments m => [Comment] -> GHandler s m CommentId
        go [] = return 1
        go cs = return $ maximum (map commentId cs) + 1

-- | Returns False when not logged in.
isCommentingUser :: YesodAuth m => Comment -> GHandler s m Bool
isCommentingUser comment = do
    muid <- maybeAuthId
    return $ case muid of
        Just uid -> isAuth comment && toPathPiece uid == userName comment
        _        -> False

-- | Redirect back to the current route after a POST request. Calls not
--   found if the current route is unknown.
redirectCurrentRoute :: Yesod m => GHandler s m ()
redirectCurrentRoute = do
    tm <- getRouteToMaster
    mr <- getCurrentRoute
    case mr of
        Just r  -> redirect $ tm r
        Nothing -> notFound

commentLabel ::  FieldSettings master
commentLabel = "Comment" { fsTooltip = Just "Comments are parsed as pandoc-style markdown." }
