{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
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
    ( persistStorage
    , migrateComments
    ) where

import Yesod
import Yesod.Comments.Core
import Yesod.Markdown (Markdown(..))

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Sql (SqlPersist)

share [mkPersist sqlSettings, mkMigrate "migrateComments"] [persistUpperCase|
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
    { sqlCommentCommentId = commentId comment
    , sqlCommentThreadId  = cThreadId  comment
    , sqlCommentTimeStamp = cTimeStamp comment
    , sqlCommentIpAddress = cIpAddress comment
    , sqlCommentUserName  = cUserName  comment
    , sqlCommentUserEmail = cUserEmail comment
    , sqlCommentContent   = cContent   comment
    , sqlCommentIsAuth    = cIsAuth    comment
    }

fromSqlComment :: SqlComment -> Comment
fromSqlComment sqlComment = Comment
    { commentId = sqlCommentCommentId sqlComment
    , cThreadId  = sqlCommentThreadId  sqlComment
    , cTimeStamp = sqlCommentTimeStamp sqlComment
    , cIpAddress = sqlCommentIpAddress sqlComment
    , cUserName  = sqlCommentUserName  sqlComment
    , cUserEmail = sqlCommentUserEmail sqlComment
    , cContent   = sqlCommentContent   sqlComment
    , cIsAuth    = sqlCommentIsAuth    sqlComment
    }

-- | Store comments in an instance of YesodPersit with a SQL backend
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

    , csUpdate = \(Comment cid tid _ _ _ _ _ _) (Comment _ _ _ _ _ _ newContent _) -> do
        mres <- runDB (getBy $ UniqueSqlComment tid cid)
        case mres of
            Just (Entity k _) -> runDB $ update k [SqlCommentContent =. newContent]
            _                 -> return ()

    , csDelete = \c -> do
        _ <- runDB (deleteBy $ UniqueSqlComment (cThreadId c) (commentId c))
        return ()

    , csLoad = \mthread -> do
        entities <- case mthread of
            (Just tid) -> runDB (selectList [SqlCommentThreadId ==. tid] [Asc SqlCommentCommentId])
            Nothing    -> runDB (selectList []                           [Asc SqlCommentCommentId])

        return $ map (fromSqlComment . entityVal) entities
    }
