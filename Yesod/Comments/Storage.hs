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
-------------------------------------------------------------------------------
module Yesod.Comments.Storage
    (
    -- * Persist
    -- $persist
      persistStorage
    , migrateComments

    -- * TODO
    -- $todo
    ) where

import Yesod
import Yesod.Comments.Core
import Yesod.Markdown (Markdown(..))

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.GenericSql (SqlPersist)

-- $persist
--
-- Use these functions to store your comments in an instance of
-- YesodPersist
--

share [mkPersist sqlSettings, mkMigrate "migrateComments"] [persist|
SqlComment
    threadId  Text Eq noreference
    commentId Int    Eq Asc noreference
    timeStamp UTCTime
    ipAddress Text
    userName  Text
    userEmail Text
    content   Markdown Update
    isAuth    Bool
    UniqueSqlComment threadId commentId
|]

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

persistStorage :: ( YesodPersist m
                  , YesodPersistBackend m ~ SqlPersist
                  ) => CommentStorage s m
persistStorage = CommentStorage
    { csGet = \tid cid -> do
        mentity <- runDB (getBy $ UniqueSqlComment tid cid)
        return $ fmap (fromSqlComment . entityVal) mentity

    , csStore = \c -> do
        _ <- runDB (insert $ toSqlComment c)
        return ()

    , csUpdate = \(Comment tid cid _ _ _ _ _ _) (Comment _ _ _ _ _ _ newContent _) -> do
        mres <- runDB (getBy $ UniqueSqlComment tid cid)
        case mres of
            Just (Entity k _) -> runDB $ update k [SqlCommentContent =. newContent]
            _                 -> return ()

    , csDelete = \c -> do
        _ <- runDB (deleteBy $ UniqueSqlComment (threadId c) (commentId c))
        return ()

    , csLoad = \mthread -> do
        entities <- case mthread of
            (Just tid) -> runDB (selectList [SqlCommentThreadId ==. tid] [Asc SqlCommentCommentId])
            Nothing    -> runDB (selectList []                           [Asc SqlCommentCommentId])

        return $ map (fromSqlComment . entityVal) entities
    }

-- $todo
--
-- Add more storage options...
--
