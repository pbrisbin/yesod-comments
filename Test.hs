{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
--
-- pbrisbin 2010
--
-- How to use Yesod.Comments
--
module Test where

import Yesod.Comments
import Yesod.Comments.Storage
import Yesod.Comments.Filters (blacklistFile)

import Yesod
import Network.Wai.Handler.SimpleServer (run)
import Database.Persist.Sqlite
import Database.Persist.GenericSql

data CommentTest = CommentTest { connPool :: ConnectionPool }
type Handler = GHandler CommentTest CommentTest
type Widget  = GWidget  CommentTest CommentTest

mkYesod "CommentTest" [$parseRoutes| 
    / RootR GET POST 
    |]

instance Yesod CommentTest where approot _ = ""

instance YesodPersist CommentTest where
    type YesodDB CommentTest = SqlPersist
    runDB db = fmap connPool getYesod >>= runSqlPool db

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "comments.db3" 10

instance YesodComments CommentTest where
    getComment     = getCommentPersist
    storeComment   = storeCommentPersist
    deleteComment  = deleteCommentPersist
    loadComments   = loadCommentsPersist
    commentFilters = [blacklistFile "blacklist.txt"]

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle  $ string "comments test page"

    -- i hope he doesn't mind...
    addHamletHead [$hamlet| 
        %link!href="http://johnmacfarlane.net/css/hk-pyg.css"!rel="stylesheet"!media="screen"!type="text/css"
        |]

    addHamlet [$hamlet|
        %h1 Test Page
        %p  Welcome to my comments test page.
        %h3 Comments
        |]
    addComments "test"

postRootR :: Handler RepHtml
postRootR = getRootR

main :: IO ()
main = putStrLn "Loaded" >> withCommentTest (run 3000)

withCommentTest :: (Application -> IO a) -> IO a
withCommentTest f = withConnectionPool $ \p -> do
    -- | make sure you run the migration to create the necessary tables,
    --   "Yesod.Comments.Storage" exports 'migrateComments' for this 
    --   purpose.
    runSqlPool (runMigration migrateComments) p
    let h = CommentTest p
    toWaiApp h >>= f
