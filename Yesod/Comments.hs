{-# LANGUAGE QuasiQuotes #-}
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
-------------------------------------------------------------------------------
module Yesod.Comments 
    ( addComments
    , module Yesod.Comments.Core
    ) where

import Yesod
import Yesod.Comments.Core
import Yesod.Comments.Filters (applyFilters)

import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)

-- | Add an overall comments section as a widget
addComments :: YesodComments m 
            => ThreadId -- ^ the thread you're adding comments to
            -> GWidget s m ()
addComments tid = do
    comments <- liftHandler $ loadComments tid
    cid      <- liftHandler $ getNextCommentId comments
    
    -- run the form
    ((res, form), enctype) <- liftHandler $ runFormMonadPost commentForm
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> liftHandler $ do
            comment <- commentFromForm tid cid cf
            matches <- applyFilters commentFilters comment
            if matches
                then setMessage $ string "comment dropped. matched filters."
                else do
                    storeComment comment
                    setMessage $ string "comment added."
            redirectCurrentRoute

    -- make the input box a bit bigger
    addCassius [$cassius|
        .yesod_comment_input th
            text-align:     left
            vertical-align: top

        .yesod_comment_input textarea
            height: 10ex
            width:  50ex
        |]
        
    -- show the input form
    [$hamlet|
        .yesod_comments
            %h4 Add a comment:
            .yesod_comment_input
                %form!enctype=$enctype$!method="post"
                    ^form^
                %p 
                    %em comments are parsed as pandoc-style markdown

            %h4 Showing $string.show.length.comments$ comments:
            
            $forall comments comment
                .yesod_comment
                    ^showComment.comment^
        |]
    where
        -- | Redirect back to the current route after a POST request
        redirectCurrentRoute :: Yesod m => GHandler s m ()
        redirectCurrentRoute = do
            tm <- getRouteToMaster
            mr <- getCurrentRoute
            case mr of
                Just r  -> redirect RedirectTemporary $ tm r
                Nothing -> notFound
            
        -- | Show a single comment, provides numbered anchors
        showComment :: Yesod m => Comment -> GWidget s m ()
        showComment comment =  do
            commentContent <- liftHandler . markdownToHtml $ content comment
            let num = show $ commentId comment
            addHamlet [$hamlet|
                %p
                    comment 
                    %span.yesod_comment_num
                        %a!href="#comment_$num$"!id="#comment_$num$" $num$
                    : on 
                    %span.yesod_comment_time_stamp $format.timeStamp.comment$
                    , 
                    %span.yesod_comment_username $userName.comment$
                    \ wrote:

                %blockquote
                    $commentContent$
                |]
            where
                -- todo: humanReadableTimeDiff
                format = formatTime defaultTimeLocale "%a, %b %d at %X"
