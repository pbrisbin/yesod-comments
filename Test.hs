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

import Yesod
import Network.Wai.Handler.SimpleServer (run)
import Database.Persist.Sqlite
import Database.Persist.GenericSql

data CommentTest = CommentTest { connPool :: ConnectionPool }
type Handler = GHandler CommentTest CommentTest
type Widget  = GWidget  CommentTest CommentTest

-- | Make sure you allow the POST route on any routes where comments
--   will be entered. It can just link to GET (see 'postRootR')
mkYesod "CommentTest" [$parseRoutes| / RootR GET POST |]

instance Yesod CommentTest where approot _ = ""

instance YesodPersist CommentTest where
    type YesodDB CommentTest = SqlPersist
    runDB db = fmap connPool getYesod >>= runSqlPool db

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "comments.db3" 10

-- define my configuration
myConf = CommentConf defaultTemplate persistentDB ""

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    setTitle  $ string "test homepage"
    addHamlet [$hamlet|
        %h1 Test Page
        %p Welcome to my comments test page.
        %h3 Comments
        |]
    -- add the comments
    addComments myConf "test"

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
