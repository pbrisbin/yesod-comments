{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
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
-- See 
-- @http:\/\/github.com\/pbrisbin\/yesod-comments\/blob\/master\/Test.hs@ 
-- for example usage.
--
-------------------------------------------------------------------------------
module Yesod.Comments ( 
    -- * Usage
    -- $usage
      addComments
    -- * Templates
    , module Yesod.Comments.Templates
    -- * Storage
    , module Yesod.Comments.Storage
    , module Yesod.Comments.Core
    ) where

import Yesod.Comments.Templates
import Yesod.Comments.Storage
import Yesod.Comments.Core

import Yesod
import Yesod.Markdown
import Yesod.Form.Core
import Control.Applicative   ((<$>), (<*>))
import Control.Monad         (when)
import Data.Time.Clock       (getCurrentTime)
import Data.Maybe            (isNothing, fromJust)
import Network.Wai           (remoteHost)
import Text.Hamlet           (toHtml)
import Text.HTML.SanitizeXSS (sanitizeXSS)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- $usage
--
-- Define a 'CommentConf', then use the widget 'addComments'. If you 
-- want it as embeddable 'Hamlet', use the helper 'hamletFromWidget'.
--

-- | Cleanse form input and create a 'Comment' to be stored
commentFromForm :: ThreadId -> CommentId -> CommentForm -> GHandler s m Comment
commentFromForm tId cId cf = do
    timeNow <- liftIO getCurrentTime
    ip      <- return . B.unpack . remoteHost =<< waiRequest
    return $ Comment tId cId timeNow ip (formUser cf) (formComment cf)
    where 
        unMarkdown (Markdown s) = s

-- | Sub-template for the input form itself
commentForm :: GFormMonad s m (FormResult CommentForm, GWidget s m ())
commentForm = do
    (user   , fiUser   ) <- stringField   "name:"    Nothing
    (comment, fiComment) <- markdownField "comment:" Nothing
    return (CommentForm <$> user <*> comment, [$hamlet|
    %table
        ^fieldRow.fiUser^        
        ^fieldRow.fiComment^
        %tr
            %td &nbsp;
            %td!colspan="2"
                %input!type="submit"!value="Add comment"
    |])
    where
        fieldRow fi = [$hamlet|
            %tr
                %th
                    %label!for=$fiIdent.fi$ $fiLabel.fi$
                    .tooltip $fiTooltip.fi$
                %td
                    ^fiInput.fi^
                %td
                    $maybe fiErrors.fi error
                        $error$
                    $nothing
                        &nbsp;
            |]

        clazz fi = string $ if fiRequired fi then "required" else "optional"

-- | Unexported code from Yesod.Markdown {{{
markdownField :: (IsForm f, FormType f ~ Markdown)
              => FormFieldSettings -> Maybe Markdown -> f
markdownField = requiredFieldHelper markdownFieldProfile

markdownFieldProfile :: FieldProfile sub y Markdown
markdownFieldProfile = FieldProfile
    { fpParse  = Right . Markdown . unlines . lines'
    , fpRender = \(Markdown m) -> m
    , fpWidget = \theId name val _isReq -> addHamlet [$hamlet|
        %textarea.markdown#$theId$!name=$name$ $val$
        |]
    }

lines' :: String -> [String]
lines' = map go . lines
    where
        go []        = []
        go ('\r':[]) = []
        go (x:xs)    = x : go xs
-- }}}

-- | Add the comment section as a widget
addComments :: Yesod m
            => CommentConf s m -- ^ your configuation
            -> ThreadId        -- ^ the id for the thread you're requesting
            -> GWidget s m ()
addComments conf thread = do
    tm <- liftHandler getRouteToMaster
    mr <- liftHandler getCurrentRoute
    when (isNothing mr) (liftHandler notFound)
    let r = tm $ fromJust mr

    comments <- liftHandler $ loadComments (storage conf) thread
    cId      <- liftHandler $ getNextId comments
    
    -- run the form
    ((res, form), enctype) <- liftHandler $ runFormMonadPost commentForm
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> liftHandler $ do
            -- store the entered comment and redirect
            commentFromForm thread cId cf >>= storeComment (storage conf)
            setMessage $ [$hamlet| %em comment added |]
            redirect RedirectTemporary r

    -- add the form/exisint comments hamlet
    (template conf) comments form enctype

-- | Get the next available comment Id, assumes the passed list of
--   commments is already filtered to a specific thread, it's in the
--   Handler Monad incase data base actions are required in the future
getNextId :: [Comment] -> GHandler s m CommentId
getNextId []       = return 1
getNextId comments = return $ maximum (map commentId comments) + 1
