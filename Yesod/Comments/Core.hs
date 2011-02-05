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
module Yesod.Comments.Core where

import Yesod
import Yesod.Markdown -- my fork, <https://github.com/pbrisbin/yesod-markdown> required
import Yesod.Form.Core
import Control.Applicative ((<$>), (<*>))
import Data.Time.Clock     (UTCTime, getCurrentTime)
import Network.Wai         (remoteHost)
import qualified Data.ByteString.Char8 as B

type ThreadId  = String
type CommentId = Int

class Yesod m => YesodComments m where
    -- Data base actions
    getComment     :: ThreadId -> CommentId -> GHandler s m (Maybe Comment)
    storeComment   :: Comment -> GHandler s m ()
    deleteComment  :: Comment -> GHandler s m ()

    -- Loading onto pages
    loadComments     :: ThreadId -> GHandler s m [Comment]
    getNextCommentId :: [Comment] -> GHandler s m CommentId
    getNextCommentId [] = return 1
    getNextCommentId cs = return $ maximum (map commentId cs) + 1

    --- other
    commentFilters   :: [(Comment -> GHandler s m Bool)]
    commentFilters   = [const $ return False]
    
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
markdownToHtml :: Yesod m => Markdown -> GHandler s m Html
markdownToHtml = (writePandoc yesodDefaultWriterOptions <$>) 
               . localLinks 
               . parseMarkdown yesodDefaultParserState

-- | The comment form itself
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
            %tr.$clazz.fi$
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

-- | Cleanse form input and create a 'Comment' to be stored
commentFromForm :: ThreadId -> CommentId -> CommentForm -> GHandler s m Comment
commentFromForm tid cid cf = do
    now <- liftIO getCurrentTime
    ip  <- return . B.unpack . remoteHost =<< waiRequest
    return Comment 
        { threadId  = tid 
        , commentId = cid 
        , timeStamp = now
        , ipAddress = ip
        , userName  = formUser cf
        , content   = formComment cf
        }
