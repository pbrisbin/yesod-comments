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
-- Drop in templates, for now only one.
--
-------------------------------------------------------------------------------
module Comments.Templates 
    ( defaultTemplate
    , entryAfterTemplate
    ) where

import Yesod
import Comments.Core

import Text.Hamlet      (HamletValue)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)

-- | A default template, entry box on top, comments shown below;
--   comments should be pass presorted as they should be displayed
defaultTemplate :: CommentsTemplate
defaultTemplate comments form enctype = [$hamlet|
#comments
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

-- | Same as default but with the entry box at the ened
entryAfterTemplate :: CommentsTemplate
entryAfterTemplate comments form enctype = [$hamlet|
#comments
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

-- | Sub template for a single comment
commentTemplate :: (HamletValue a) => Comment -> a
commentTemplate comment = 
    let date = formatTime defaultTimeLocale "%a, %b %d at %H:%S" $ timeStamp comment
    in [$hamlet|
%p
    On 
    %strong $date$
    , 
    %strong $userName.comment$
    \ wrote:

%blockquote
    %p
        $content.comment$
|]
