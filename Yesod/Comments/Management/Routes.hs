{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE QuasiQuotes                 #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
-------------------------------------------------------------------------------
-- |
-- Module        : Yesod.Comments.Management.Routes
-- Copyright     : Patrick Brisbin
-- License       : as-is
--
-- Maintainer    : Patrick Brisbin <me@pbrisbin.com>
-- Stability     : unstable
-- Portability   : unportable
--
-------------------------------------------------------------------------------
module Yesod.Comments.Management.Routes where

import Yesod
import Yesod.Comments.Core

data CommentsAdmin = CommentsAdmin

mkYesodSubData "CommentsAdmin" [parseRoutes|
    /                            CommentsR      GET
    /edit/#ThreadId/#CommentId   EditCommentR   GET POST
    /delete/#ThreadId/#CommentId DeleteCommentR GET POST
    |]
