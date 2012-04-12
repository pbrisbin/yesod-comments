{-# LANGUAGE GADTs                      #-}
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
    , updateCommentPersist
    , deleteCommentPersist
    , loadCommentsPersist
    , migrateComments
    -- * TODO
    -- $todo
    ) where

import Yesod
import Database.Persist.GenericSql (SqlPersist)
import Yesod.Comments.Core    (Comment(..))
import Yesod.Markdown         (Markdown(..))
import Data.Time.Clock        (UTCTime)
import qualified Data.Text as T

-- $persist
--
-- Use these functions to store your comments in an instance of
-- YesodPersist
--

-- | Create the required types and migration function for use in a
--   general yesod app
--
--   todo: find out how to do the ThreadId/CommentId types with the new
--   persistent module
--
share [mkPersist sqlSettings, mkMigrate "migrateComments"] [persist|
SqlComment
    threadId  T.Text Eq noreference
    commentId Int    Eq Asc noreference
    timeStamp UTCTime
    ipAddress T.Text
    userName  T.Text
    userEmail T.Text
    content   Markdown Update
    isAuth    Bool
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
    , sqlCommentIsAuth    = isAuth    comment
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
    , isAuth    = sqlCommentIsAuth    sqlComment
    }

getCommentPersist :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => T.Text -> Int -> GHandler s m (Maybe Comment)
getCommentPersist tid cid = return . fmap (fromSqlComment . entityVal) =<< runDB (getBy $ UniqueSqlComment tid cid)

storeCommentPersist :: (YesodPersist m, PersistStore (YesodPersistBackend m) (GHandler s m)) => Comment -> GHandler s m ()
storeCommentPersist c = return . const () =<< runDB (insert $ toSqlComment c)

-- | Note, only updates the content record
updateCommentPersist :: (YesodPersist m, PersistUnique (YesodPersistBackend m) (GHandler s m), PersistQuery (YesodPersistBackend m) (GHandler s m)) => Comment -> Comment -> GHandler s m ()
updateCommentPersist (Comment tid cid _ _ _ _ _ _) (Comment _ _ _ _ _ _ newContent _) = do
    mres <- runDB (getBy $ UniqueSqlComment tid cid)
    case mres of
        Just (Entity k _) -> runDB $ update k [SqlCommentContent =. newContent]
        _          -> return ()

deleteCommentPersist :: (YesodPersist m, PersistUnique (YesodPersistBackend m) (GHandler s m)) => Comment -> GHandler s m ()
deleteCommentPersist c = return . const () =<< runDB (deleteBy $ UniqueSqlComment (threadId c) (commentId c))

-- | Use @'Nothing'@ to retrieve all comments site-wide
loadCommentsPersist :: (YesodPersist m, YesodPersistBackend m ~ SqlPersist) => Maybe T.Text -> GHandler s m [Comment]
loadCommentsPersist (Just tid) = return . fmap (fromSqlComment . entityVal) =<< runDB (selectList [SqlCommentThreadId ==. tid] [Asc SqlCommentCommentId])
loadCommentsPersist Nothing    = return . fmap (fromSqlComment . entityVal) =<< runDB (selectList []                           [Asc SqlCommentCommentId])

-- $todo
--
-- Add more storage options...
--
