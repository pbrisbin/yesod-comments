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
type Widget  = GWidget CommentTest CommentTest

-- | Make sure you allow the POST route on any routes where comments
--   will be entered. It can just link to GET (see 'postRootR')
mkYesod "CommentTest" [$parseRoutes| / RootR GET POST |]

instance Yesod CommentTest where approot _ = ""

instance YesodPersist CommentTest where
    type YesodDB CommentTest = SqlPersist
    runDB db = fmap connPool getYesod >>= runSqlPool db

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "comments.db3" 10

getRootR :: Handler RepHtml
getRootR = do
    -- | Fetch the Hamlet for this page's comments section:
    pageComments <- runCommentsForm 
        defaultTemplate -- ^ The template to use (see "Yesod.Comments.Templates")
        persistentDB    -- ^ The storage backed (see "Yesod.Comments.Storage")
        "testpage"      -- ^ The 'ThreadId' of this page
        RootR           -- ^ The 'Route' to redirect to after a POST

    -- | and just ^embed^ it in your template where desired.
    defaultLayout $ do
        setTitle  $ string "test homepage"
        addHamlet $
            [$hamlet|
                #header
                    %h1 Test Page
                    %hr
                #body
                    %p Welcome to my comments test page.
                    %h3 Comments
                    ^pageComments^
            |]

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
