{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.Templates
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Some pre-built templates for use with 'Comments.runCommentsForm'.
--
-------------------------------------------------------------------------------
module Yesod.Comments.Templates
    ( defaultTemplate
    -- * helpers
    , hamletFromWidget
    , markdownToHtml 
    ) where

import Yesod
import Yesod.Markdown
import Control.Applicative ((<$>))
import Data.Time.Format    (formatTime)
import System.Locale       (defaultTimeLocale)

import Yesod.Comments.Core

-- | A default template, entry box on top, comments shown below
defaultTemplate :: CommentsTemplate
defaultTemplate comments form enctype = do
    formHamlet <- liftHandler $ hamletFromWidget form
    -- show the input form
    addHamlet [$hamlet|
        %h4 Add a comment:
        %form!enctype=$enctype$!method="post"
            ^formHamlet^
        %p 
            %em comments are parsed as pandoc-style markdown

        %h4 Showing $string.show.length.comments$ comments:
        |]

    -- show the existing comments
    mapM_ showComment comments

-- | Show a single comment, provides numbered anchors
showComment :: Yesod m => Comment -> GWidget s m ()
showComment comment =  do
    commentContent <- liftHandler . markdownToHtml $ content comment
    addHamlet [$hamlet|
        .comment
            %p
                comment 
                %a!href="#comment_$show.commentId.comment$"!id="#comment_$show.commentId.comment$" $show.commentId.comment$
                : on 
                %strong $format.timeStamp.comment$
                , 
                %strong $userName.comment$
                \ wrote:

            %blockquote
                $commentContent$
        |]
    where
        format = formatTime defaultTimeLocale "%a, %b %d at %X"

hamletFromWidget :: Yesod m => GWidget s m () -> GHandler s m (Hamlet (Route m))
hamletFromWidget widget = return . pageBody =<< widgetToPageContent widget

-- | Render from markdown, yesod-style
markdownToHtml :: Yesod m => Markdown -> GHandler s m Html
markdownToHtml = (writePandoc yesodDefaultWriterOptions <$>) 
               . localLinks 
               . parseMarkdown yesodDefaultParserStateTrusted

-- todo: humanReadableDiffTime
