{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
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
    runCommentsForm,
    -- * Templates
    -- $templates
    module Yesod.Comments.Templates,
    -- * Storage
    -- $storage
    module Yesod.Comments.Storage
    ) where

import Yesod.Comments.Core
import Yesod.Comments.Templates
import Yesod.Comments.Storage

import Yesod
import Yesod.Form.Core
import Control.Applicative   ((<$>), (<*>))
import Data.Time.Clock       (getCurrentTime)
import Network.Wai           (remoteHost)
import Text.Hamlet           (toHtml)
import Text.HTML.SanitizeXSS (sanitizeXSS)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- $usage
--
-- Use the function 'runCommentsForm' to retrieve 'Hamlet' corresponding
-- to a comments section for the currently displaying page.
--

-- $templates
--
-- In "Yesod.Comments.Templates" you'll find prebuilt templates defining 
-- the layout of the comments section.
-- 
-- You could also define your own; see "Yesod.Comments.Core" for the
-- 'CommentsTemplate' type synonym, it's essentially 
--
-- > :: [Comment] -> Widget ()
--

-- $storage
--
-- In "Yesod.Comments.Storage" you'll find some prebuilt backends for 
-- use in persisting the comments. Again, you could roll you're own by 
-- defining a function of type 'CommentStorage' by specifying how to
-- read comments by 'ThreadId', how to store a comment, and how to
-- delete a comment.
--

-- | Sub-template for the input form itself
commentForm :: GFormMonad s m (FormResult CommentForm, GWidget s m ())
commentForm = do
    (user   , fiUser   ) <- stringField   "name:"    Nothing
    (comment, fiComment) <- textareaField "comment:" Nothing
    (isHtml , fiIsHtml ) <- boolField     "html?"    Nothing
    return (CommentForm <$> user <*> comment <*> isHtml, [$hamlet|
    %table
        %tr.$clazz.fiUser$
            %td
                %label!for=$fiIdent.fiUser$ $fiLabel.fiUser$
                .tooltip $fiTooltip.fiUser$
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
            %td
                ^fiInput.fiComment^
            %td.errors
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
        %tr
            %td
                &nbsp;
            %td!colspan="2"
                %input!type="submit"!value="Add comment"
    |])
    where
        clazz fi = string $ if fiRequired fi then "required" else "optional"

-- | Cleanse form input and create a 'Comment' to be stored
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
        textToHtml = liftH wrapP . toHtml . liftT stripCR

        -- with html, \r\n and \n should always become space
        stripCRLF []               = []
        stripCRLF ('\r':'\n':rest) = ' ' : stripCRLF rest
        stripCRLF ('\n':rest)      = ' ' : stripCRLF rest
        stripCRLF (x:rest)         = x   : stripCRLF rest

        -- with plaintext, \n will become <br>, \r should just be discarded
        stripCR []          = []
        stripCR ('\r':rest) =     stripCR rest
        stripCR (x:rest)    = x : stripCR rest

        wrapP = ("<p>"++) . (++"</p>")

-- | lift a String function into Textarea
liftT :: (String -> String) -> Textarea -> Textarea
liftT f = Textarea . f . unTextarea

-- | lift a String function into Html
liftH :: (String -> String) -> Html -> Html
liftH f = preEscapedString . f . L.unpack . renderHtml

-- | The single call to retrieve the hamlet for the comments
runCommentsForm :: (Yesod m)
                => CommentsTemplate    -- ^ the overall template
                -> CommentStorage s m  -- ^ how you store your comments
                -> ThreadId            -- ^ the id for the thread you're requesting
                -> Route m             -- ^ a route to redirect to after a POST
                -> GHandler s m (Hamlet (Route m))
runCommentsForm template db thread r = do
    -- load existing comments
    comments <- loadComments db thread
    cId      <- getNextId comments

    -- run the form
    ((res, form), enctype) <- runFormMonadPost commentForm
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> do
            -- store the entered comment
            commentFromForm thread cId cf >>= storeComment db
            setMessage $ [$hamlet| %em comment added |]

            -- redirect to prevent accidental reposts and to clear the
            -- form data
            redirect RedirectTemporary r

    -- like extractBody but in the GHandler Monad; todo: there's a
    -- better way to do this right?
    return . pageBody =<< widgetToPageContent (template comments form enctype)

-- | Get the next available comment Id, assumes the passed list of
--   commments is already filtered to a specific thread, it's in the
--   Handler Monad incase data base actions are required in the future
getNextId :: [Comment] -> GHandler s m CommentId
getNextId []       = return 1
getNextId comments = return $ maximum (map commentId comments) + 1
