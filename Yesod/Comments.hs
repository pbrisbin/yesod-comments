{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments
-- Copyright   :  (c) Patrick Brisbin 2010
-- License     :  as-is
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A generic Comments interface for a Yesod application.
--
-------------------------------------------------------------------------------
module Yesod.Comments
    ( addComments
    , module Yesod.Comments.Core
    ) where

import Yesod
import Yesod.Comments.Core
import Yesod.Comments.Utils
import Yesod.Comments.Form
import Yesod.Comments.View

addComments :: (RenderMessage m FormMessage, YesodComments m) => ThreadId -> GWidget s m ()
addComments thread = do
    comments  <- lift $ csLoad commentStorage (Just thread)
    mudetails <- lift $ currentUserDetails

    [whamlet|
        <div .yesod_comments>
            ^{showComments comments}

            ^{runForm thread mudetails}
    |]
