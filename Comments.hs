{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Comments
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A generic Comments interface for a Yesod application.
--
-------------------------------------------------------------------------------
module Comments 
    ( runCommentsForm
    , module Comments.Templates
    , module Comments.Storage
    ) where

import Comments.Core
import Comments.Fields
import Comments.Templates
import Comments.Storage

import Yesod
import Yesod.Form.Core
import Control.Applicative   ((<$>), (<*>))
import Data.Time.Clock       (getCurrentTime)
import Network.Wai           (remoteHost)
import Text.Hamlet           (toHtml)
import Text.HTML.SanitizeXSS (sanitizeXSS)

import qualified Data.ByteString.Char8 as B

-- | Cleans form input and create a comment type to be stored
commentFromForm :: ThreadId 
                -> CommentId 
                -> CommentForm 
                -> GHandler s m Comment
commentFromForm tId cId cf = do
    timeNow <- liftIO getCurrentTime
    ip      <- return . B.unpack . remoteHost =<< waiRequest

    if formIsHtml cf
        then return $ Comment tId cId timeNow ip (formUser cf) (htmlToHtml $ formComment cf)
        else return $ Comment tId cId timeNow ip (formUser cf) (textToHtml $ formComment cf)
    where
        -- the user entered html source directly
        htmlToHtml :: Textarea -> Html
        htmlToHtml = preEscapedString . sanitizeXSS . stripCRLF . unTextarea
        
        -- the user intends plaintext
        textToHtml :: Textarea -> Html
        textToHtml = toHtml . liftT stripCR

        -- with html, \r\n and \n should always become space
        stripCRLF []               = []
        stripCRLF ('\r':'\n':rest) = ' ' : stripCRLF rest
        stripCRLF ('\n':rest)      = ' ' : stripCRLF rest
        stripCRLF (x:rest)         = x   : stripCRLF rest

        -- with plaintext, \n will become <br>, \r should just be discarded
        stripCR []          = []
        stripCR ('\r':rest) =     stripCR rest
        stripCR (x:rest)    = x : stripCR rest

-- | lift a String function into Textara
liftT :: (String -> String) -> Textarea -> Textarea
liftT f = Textarea . f . unTextarea

-- | The input form itself; todo: custom fields
commentForm :: GFormMonad s m (FormResult CommentForm, GWidget s m ())
commentForm = do
    (user   , fiUser   ) <- stringField   "name:"    Nothing
    (comment, fiComment) <- textareaField "comment:" Nothing
    (isHtml , fiIsHtml ) <- boolField     "html?"    Nothing
    return (CommentForm <$> user <*> comment <*> isHtml, [$hamlet|
    %tr.$clazz.fiUser$
        %td
            %label!for=$fiIdent.fiUser$ $fiLabel.fiUser$
            .tootip $fiTooltip.fiUser$
        %td
            ^fiInput.fiUser^
        %td.errors
            $maybe fiErrors.fiUser error
                $error$
            $nothing
                &nbsp;

    %tr.$clazz.fiComment$
        %td 
            %label!for=$fiIdent.fiComment$ $fiLabel.fiComment$
            .tooltip $fiTooltip.fiComment$
        %td!colspan="2" 
            ^fiInput.fiComment^

    %tr.errors
        %td &nbsp;
        %td.errors!colspans="2"
            $maybe fiErrors.fiComment error
                $error$
            $nothing
                &nbsp;

    %tr.$clazz.fiIsHtml$
        %td 
            %label!for=$fiIdent.fiIsHtml$ $fiLabel.fiIsHtml$
            .tooltip $fiTooltip.fiIsHtml$
        %td 
            ^fiInput.fiIsHtml^
        %td 
            &nbsp;
    |])
    where
        clazz fi = string $ if fiRequired fi then "required" else "optional"

-- | Provides a single call to retrieve the html for the comments
--   section of a page
runCommentsForm :: (Yesod m)
                => CommentsTemplate    -- ^ the overall template
                -> CommentStorage s m  -- ^ how you store your comments
                -> ThreadId            -- ^ the id for the thread you're requesting
                -> Route m             -- ^ a route to redirect to after a POST
                -> GHandler s m (Hamlet (Route m))
runCommentsForm template db thread r = do
    -- load existing comments
    comments <- loadComments db thread
    let cId = getNextId comments

    -- run the form
    ((res, form), enctype) <- runFormMonadPost commentForm
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> do
            comment <- commentFromForm thread cId cf
            storeComment db comment
            -- redirect to prevent accidental reposts and to clear the
            -- form data
            setMessage $ [$hamlet| %em comment added |]
            redirect RedirectTemporary r

    -- return it as a widget
    --return $ template comments form enctype
    
    -- return it as hamlet; todo: this is a _hack_
    pc <- widgetToPageContent $ template comments form enctype
    return $ pageBody pc

-- | Get the next available comment Id, assumes the passed list of
--   commments is already filtered to a specific thread
getNextId :: [Comment] -> CommentId
getNextId []       = 0
getNextId comments = maximum (map commentId comments) + 1
