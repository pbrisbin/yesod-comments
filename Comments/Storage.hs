-------------------------------------------------------------------------------
-- |
-- Module      :  Comments.Storage
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Some pre-built backend definitions. So far just test and file, soon
-- persistent.
--
-------------------------------------------------------------------------------
module Comments.Storage 
    ( testDB
    , fileDB
    ) where

import Yesod
import Comments.Core

import Data.List.Split  (wordsBy)
import Data.Time.Clock  (UTCTime)
import Data.Time.Format (parseTime, formatTime)
import Data.Maybe       (mapMaybe)
import System.IO        (hPutStrLn, stderr)
import System.Locale    (defaultTimeLocale)

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L

-- strict reads are required since reading/writing occur so close to
-- each other and ghc leaves the handles open
import qualified System.IO.Strict

-- | For use during testing, always loads no comments and prints the
--   comment to stderr as "store"
testDB :: CommentStorage
testDB = CommentStorage
    { storeComment  = liftIO . hPutStrLn stderr . show
    , loadComments  = \_   -> return []
    , deleteComment = \_ _ -> return ()
    }

-- | A simple flat file storage method, this is way unreliable, probably
--   wicked slow, but dead simple to setup/use
fileDB :: FilePath -> CommentStorage
fileDB f = CommentStorage
    { storeComment  = storeComment'
    , loadComments  = loadComments'
    , deleteComment = deleteComment'
    }
    where
        storeComment' comment = do
            let str = concat [ threadId comment,                "|"
                             , show $ commentId comment,        "|"
                             , formatTime' $ timeStamp comment, "|"
                             , ipAddress comment,               "|"
                             , userName comment,                "|"
                             , htmlToString $ content comment, "\n"
                             ]
            liftIO $ appendFile f str

        formatTime'  = formatTime defaultTimeLocale "%s"
        htmlToString = fixPipe . L.unpack . renderHtml

        -- since we use | as a delimiter we need to htmlize it before
        -- storing the comment
        fixPipe []         = []
        fixPipe ('|':rest) = "&#124;" ++ fixPipe rest
        fixPipe (x:rest)   = x : fixPipe rest

        loadComments' id = do
            contents <- liftIO $ System.IO.Strict.readFile f
            return $ mapMaybe (readComment id) (lines contents)

        readComment :: String -> String -> Maybe Comment
        readComment id' s = 
            case wordsBy (=='|') s of
                [t, c, ts, ip, u, h] -> 
                    if t == id'
                        then
                            case readTime ts of
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

        readTime :: String -> Maybe UTCTime
        readTime = parseTime defaultTimeLocale "%s"

        deleteComment' = undefined
