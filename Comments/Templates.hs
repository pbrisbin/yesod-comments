{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Comments.Templates
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
module Comments.Templates 
    ( defaultTemplate
    , entryAfterTemplate
    , commentTemplate
    ) where

import Yesod
import Comments.Core

import Text.Hamlet      (HamletValue)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)

-- | A default template, entry box on top, comments shown below;
--   comments should be passed presorted
defaultTemplate :: CommentsTemplate
defaultTemplate comments form enctype = [$hamlet|
#comments
    %h2
        %a!href="#comments"!id="#comments" Comments

    %h4 Add a comment:
    %form!enctype=$enctype$!method="post"
        %table
            ^form^
            %tr
                %td
                    &nbsp;
                %td!colspan="2"
                    %input!type="submit"!value="Add comment"
    %p 
        %em when using html, assume your text will be wrapped in &lt;p&gt &lt;/p&gt;

    %h4 Showing $string.show.length.comments$ comments:

    $forall comments comment
        ^commentTemplate.comment^
|]

-- | Same as default but with the entry box at the end
entryAfterTemplate :: CommentsTemplate
entryAfterTemplate comments form enctype = [$hamlet|
#comments
    %h2
        %a!href="#comments"!id="#comments" Comments

    %h4 Showing $string.show.length.comments$ comments:

    $forall comments comment
        ^commentTemplate.comment^

    %h4 Add a comment:
    %form!enctype=$enctype$!method="post"
        %table
            ^form^
            %tr
                %td
                    &nbsp;
                %td!colspan="2"
                    %input!type="submit"!value="Add comment"
    %p 
        %em when using html, assume your text will be wrapped in &lt;p&gt &lt;/p&gt;
|]

-- | Example sub-template for a single comment, provides numbered
--   anchors to each comment.
commentTemplate :: (HamletValue a) => Comment -> a
commentTemplate comment = 
    let date = formatTime defaultTimeLocale "%a, %b %d at %H:%S" $ timeStamp comment
    in [$hamlet|
    %p
        Comment 
        %a!href="#comment_$show.commentId.comment$"!id="#comment_$show.commentId.comment$" $show.commentId.comment$
        : On 
        %strong $date$
        , 
        %strong $userName.comment$
        \ wrote:

    %blockquote
        %p
            $content.comment$
    |]
