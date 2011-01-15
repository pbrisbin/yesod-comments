{-# LANGUAGE QuasiQuotes                #-}
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
-- Some pre-built backend definitions for use with runCommentsForm.
--
-------------------------------------------------------------------------------
module Yesod.Comments.Storage
    ( testDB
    , persistentDB
    , migrateComments
    ) where

import Yesod
import Yesod.Comments.Core

import Data.Time.Clock  (UTCTime)
import System.IO        (hPutStrLn, stderr)

import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

import qualified Data.ByteString.Lazy.Char8 as L

-- | some utilities
htmlToString :: Html -> String
htmlToString = L.unpack . renderHtml

-- | For use during testing, always loads no comments and prints the
--   comment to stderr as the /store/ action
testDB :: CommentStorage s m
testDB = CommentStorage
    { storeComment  = liftIO . hPutStrLn stderr . show
    , loadComments  = \_   -> return []
    , deleteComment = \_ _ -> return ()
    }

-- | Create the required types and migration function for use in a
--   general yesod app
share2 mkPersist (mkMigrate "migrateComments") [$persist|
SqlComment
    threadId  String Eq
    commentId Int Eq Asc
    timeStamp UTCTime
    ipAddress String
    userName  String
    content   String
    UniqueSqlComment threadId commentId
|]

-- | Make a SqlComment out of a 'Comment' for passing off to insert
toSqlComment :: Comment -> SqlComment
toSqlComment comment = SqlComment
    { sqlCommentThreadId  = threadId  comment
    , sqlCommentCommentId = commentId comment
    , sqlCommentTimeStamp = timeStamp comment
    , sqlCommentIpAddress = ipAddress comment
    , sqlCommentUserName  = userName  comment
    , sqlCommentContent   = htmlToString $ content comment
    }

-- | Maybe read a 'Comment' back from a selected SqlComment
fromSqlComment :: SqlComment -> Comment
fromSqlComment sqlComment = Comment
    { threadId  = sqlCommentThreadId  sqlComment
    , commentId = sqlCommentCommentId sqlComment
    , timeStamp = sqlCommentTimeStamp sqlComment
    , ipAddress = sqlCommentIpAddress sqlComment
    , userName  = sqlCommentUserName  sqlComment
    , content   = preEscapedString $ sqlCommentContent sqlComment
    }

-- | If your app is an instance of YesodPersist, you can use this
--   backend to store comments in your database without any further
--   changes to your app.
persistentDB :: (YesodPersist m, 
                 PersistBackend (YesodDB m (GHandler s m))) 
             => CommentStorage s m
persistentDB = CommentStorage
    { storeComment = \comment -> do
        runDB $ insert $ toSqlComment comment
        return ()

    , loadComments = \tid -> do
        results <- runDB $ selectList [SqlCommentThreadIdEq tid] [SqlCommentCommentIdAsc] 0 0
        return $ map (fromSqlComment . snd) results

    , deleteComment = \tid cid -> runDB $ deleteBy $ UniqueSqlComment tid cid
    }
