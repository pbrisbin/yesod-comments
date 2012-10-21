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
  , commentFromForm
  , commentForm
  , handleForm
  ) where

import Yesod
import Yesod.Markdown
import Yesod.Comments.Core
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Network.Wai         (remoteHost)
import qualified Data.Text as T

type Form s m x = Html -> MForm s m (FormResult x, GWidget s m ())

data CommentForm = CommentForm
    { formUser    :: Text
    , formEmail   :: Text
    , formComment :: Markdown
    , formIsAuth  :: Bool
    }

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

commentForm:: RenderMessage m FormMessage
           => Text -- ^ Text version of uid
           -> Text -- ^ user's email
           -> Form s m CommentForm
commentForm user email = renderBootstrap $ CommentForm
    <$> pure user <*> pure email
    <*> areq markdownField commentLabel Nothing
    <*> pure True

handleForm :: YesodComments m => FormResult CommentForm -> ThreadId -> GWidget s m ()
handleForm (FormSuccess cf) tid = lift $ do
    storeComment =<< commentFromForm tid cf
    setMessage "comment added."
    redirectCurrentRoute

handleForm _ _ = return ()

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
