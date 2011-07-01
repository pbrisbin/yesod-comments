{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.Storage
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Some pre-built function definitions for storing and loading comments.
--
-------------------------------------------------------------------------------
module Yesod.Comments.Storage
    ( 
    -- * Persist
    -- $persist
      getCommentPersist
    , storeCommentPersist
    , deleteCommentPersist
    , loadCommentsPersist
    , migrateComments
    -- * TODO
    -- Add more
    ) where

import Yesod
import Yesod.Comments.Core    (Comment(..), ThreadId, CommentId)
import Yesod.Goodies.Markdown (Markdown(..))
import Data.Time.Clock        (UTCTime)
import qualified Data.Text as T

-- $persist
--
-- Use these functions to store your comments in an instance of 
-- YesodPersist
--

-- | Create the required types and migration function for use in a
--   general yesod app
share2 mkPersist (mkMigrate "migrateComments") [persist|
SqlComment
    threadId  ThreadId Eq noreference
    commentId CommentId Eq Asc noreference
    timeStamp UTCTime
    ipAddress T.Text
    userName  T.Text
    userEmail T.Text
    content   Markdown
    UniqueSqlComment threadId commentId
|]

-- | Make a 'SqlComment' out of a 'Comment' for passing off to insert
toSqlComment :: Comment -> SqlComment
toSqlComment comment = SqlComment
    { sqlCommentThreadId  = threadId  comment
    , sqlCommentCommentId = commentId comment
    , sqlCommentTimeStamp = timeStamp comment
    , sqlCommentIpAddress = ipAddress comment
    , sqlCommentUserName  = userName  comment
    , sqlCommentUserEmail = userEmail comment
    , sqlCommentContent   = content   comment
    }

-- | Read a 'Comment' back from a selected 'SqlComment'
fromSqlComment :: SqlComment -> Comment
fromSqlComment sqlComment = Comment
    { threadId  = sqlCommentThreadId  sqlComment
    , commentId = sqlCommentCommentId sqlComment
    , timeStamp = sqlCommentTimeStamp sqlComment
    , ipAddress = sqlCommentIpAddress sqlComment
    , userName  = sqlCommentUserName  sqlComment
    , userEmail = sqlCommentUserEmail sqlComment
    , content   = sqlCommentContent   sqlComment
    }

getCommentPersist :: (YesodPersist m, PersistBackend (YesodDB m (GGHandler s m IO))) => ThreadId -> CommentId -> GHandler s m (Maybe Comment)
getCommentPersist tid cid = return . fmap (fromSqlComment . snd) =<< runDB (getBy $ UniqueSqlComment tid cid)

storeCommentPersist :: (YesodPersist m, PersistBackend (YesodDB m (GGHandler s m IO))) => Comment -> GHandler s m ()
storeCommentPersist c = return . const () =<< runDB (insert $ toSqlComment c)

deleteCommentPersist :: (YesodPersist m, PersistBackend (YesodDB m (GGHandler s m IO))) => Comment -> GHandler s m ()
deleteCommentPersist c = return . const () =<< runDB (deleteBy $ UniqueSqlComment (threadId c) (commentId c))

-- | Use @'Nothing'@ to retrieve all comments site-wide
loadCommentsPersist :: (YesodPersist m, PersistBackend (YesodDB m (GGHandler s m IO))) => Maybe ThreadId -> GHandler s m [Comment]
loadCommentsPersist (Just tid) = return . fmap (fromSqlComment . snd) =<< runDB (selectList [SqlCommentThreadIdEq tid] [SqlCommentCommentIdAsc] 0 0)
loadCommentsPersist Nothing    = return . fmap (fromSqlComment . snd) =<< runDB (selectList []                         [SqlCommentCommentIdAsc] 0 0)
