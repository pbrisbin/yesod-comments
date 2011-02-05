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

import Control.Monad    (when)
import Data.Maybe       (isNothing, fromJust)
import Data.Time.Clock  (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale    (defaultTimeLocale)

-- | Add an overall comments section as a widget
addComments :: YesodComments m 
            => ThreadId -- ^ the thread you're adding comments to
            -> GWidget s m ()
addComments tid = do
    tm <- liftHandler getRouteToMaster
    mr <- liftHandler getCurrentRoute
    when (isNothing mr) (liftHandler notFound)
    let r = tm $ fromJust mr

    comments <- liftHandler $ loadComments tid
    cId      <- liftHandler $ getNextCommentId comments
    
    -- run the form
    ((res, form), enctype) <- liftHandler $ runFormMonadPost commentForm
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess cf -> liftHandler $ do
            comment <- commentFromForm tid cId cf
            matches <- applyFilters commentFilters comment
            if matches
                then setMessage $ string "comment dropped. matched filters."
                else do
                    storeComment comment
                    setMessage $ string "comment added."
            redirect RedirectTemporary r

    -- make the input box a bit bigger
    addCassius [$cassius|
        .comment_input th
            text-align:     left
            vertical-align: top

        .comment_input textarea
            height: 10ex
            width:  50ex
        |]
        
    -- show the input form
    [$hamlet|
        %h4 Add a comment:
        .comment_input
            %form!enctype=$enctype$!method="post"
                ^form^
            %p 
                %em comments are parsed as pandoc-style markdown

        %h4 Showing $string.show.length.comments$ comments:
        
        $forall comments comment
            .comment
                ^showComment.comment^
        |]
    where
        -- | Show a single comment, provides numbered anchors
        showComment :: Yesod m => Comment -> GWidget s m ()
        showComment comment =  do
            commentContent <- liftHandler . markdownToHtml $ content comment
            addHamlet [$hamlet|
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
                -- todo: humanReadableTimeDiff
                format = formatTime defaultTimeLocale "%a, %b %d at %X"
