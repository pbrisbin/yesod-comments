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
    , entryAfterTemplate
    , commentTemplate
    ) where

import Yesod.Comments.Core

import Yesod
import Text.Hamlet      (HamletValue)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)

-- | A default template, entry box on top, comments shown below;
--   comments are expected to be sorted already
defaultTemplate :: CommentsTemplate
defaultTemplate comments form enctype = [$hamlet|
#comments
    %h4 Add a comment:

    %form!enctype=$enctype$!method="post"
        ^form^

    %p 
        %em if using html, please keep it well-formed and valid (ex: &lt;p&gt;Hello&lt;/p&gt;).

    %h4 Showing $string.show.length.comments$ comments:

    $forall comments comment
        ^commentTemplate.comment^
|]

-- | Same as default but with the entry box at the end
entryAfterTemplate :: CommentsTemplate
entryAfterTemplate comments form enctype = [$hamlet|
#comments
    %h4 Showing $string.show.length.comments$ comments:

    $forall comments comment
        ^commentTemplate.comment^

    %h4 Add a comment:

    %form!enctype=$enctype$!method="post"
        ^form^

    %p 
        %em if using html, please keep it well-formed and valid (ex: &lt;p&gt;Hello&lt;/p&gt;).
|]

-- | Sub-template for a single comment, provides numbered anchors to
--   each comment.
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
        $content.comment$
    |]

