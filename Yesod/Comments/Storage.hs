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
-- Some pre-built backend definitions for use with 'Comment.runCommentsForm'
--
-------------------------------------------------------------------------------
module Yesod.Comments.Storage
    ( testDB
    , fileDB
    , persistentDB
    , migrateComments
    ) where

import Yesod
import Yesod.Comments.Core

import Data.List.Split  (wordsBy)
import Data.Time.Clock  (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import Data.Maybe       (mapMaybe, maybeToList)
import System.IO        (hPutStrLn, stderr)
import System.Locale    (defaultTimeLocale)

import Database.Persist.TH         (share2)
import Database.Persist.GenericSql (mkMigrate)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- | strict reads are required since reading/writing occur so close to
--   each other and ghc leaves the handles open
import qualified System.IO.Strict

-- | some utilities
htmlToString :: Html -> String
htmlToString = L.unpack . renderHtml

formatTime' :: UTCTime -> String
formatTime' = formatTime defaultTimeLocale "%s"

parseTime' :: String -> Maybe UTCTime
parseTime' = parseTime defaultTimeLocale "%s"

-- | For use during testing, always loads no comments and prints the
--   comment to stderr as the /store/ action
testDB :: CommentStorage s m
testDB = CommentStorage
    { storeComment  = liftIO . hPutStrLn stderr . show
    , loadComments  = \_   -> return []
    , deleteComment = \_ _ -> return ()
    }

-- | Convert a 'Comment' into a pipe-delimited string for storage as a
--   single line in a file
toFileString :: Comment -> String
toFileString comment = concat
    [ threadId comment,                          "|"
    , show $ commentId comment,                  "|"
    , formatTime' $ timeStamp comment,           "|"
    , ipAddress comment,                         "|"
    , userName comment,                          "|"
    , fixPipe . htmlToString $ content comment, "\n"
    ]
    where
        -- since we use | as a delimiter we need to htmlize it before
        -- storing the comment
        fixPipe []         = []
        fixPipe ('|':rest) = "&#124;" ++ fixPipe rest
        fixPipe (x:rest)   = x : fixPipe rest

-- | Maybe read the same delimited string into a 'Comment'
fromFileString :: String -> String -> Maybe Comment
fromFileString thread str =
    case wordsBy (=='|') str of
        [t, c, ts, ip, u, h] -> 
            if t == thread
                then case parseTime' ts of
                    Just utc -> Just
                        Comment
                            { threadId  = t
                            , commentId = read c :: Int
                            , timeStamp = utc
                            , ipAddress = ip
                            , userName  = u
                            , content   = preEscapedString h
                            }
                    _ -> Nothing
                else Nothing
        _ -> Nothing

-- | A simple flat file storage method; this is dead-simple to setup but
--   probably shouldn't be used since it's unreliable and likely to be
--   slow
fileDB :: (Yesod m) => FilePath -> CommentStorage s m
fileDB f = CommentStorage
    { storeComment = liftIO . appendFile f . toFileString

    , loadComments = \id -> do
        contents <- liftIO $ System.IO.Strict.readFile f
        return $ mapMaybe (fromFileString id) (lines contents)

        -- todo: how to do this without rewriting the whole file?
    , deleteComment = \tid cid -> undefined
    }
    where

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
