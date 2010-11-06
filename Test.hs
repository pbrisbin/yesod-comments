{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
--
-- This is a single-file test application which shows how to use the
-- comments module.
--
module Test where

import Comments

import Yesod
import Network.Wai.Handler.SimpleServer
import Database.Persist.Sqlite
import Database.Persist.GenericSql

-- required settings to make your app an instance of YesodPersist
withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "comments.db3" 10

runConnectionPool :: MonadInvertIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

data CommentTest = CommentTest { connPool :: ConnectionPool }
type Handler = GHandler CommentTest CommentTest
type Widget  = GWidget CommentTest CommentTest

-- | make sure you allow the POST route on any routes where comments
--   will be entered. it can just link to GET (see 'postRootR')
mkYesod "CommentTest" [$parseRoutes|
/ RootR GET POST
|]

instance Yesod CommentTest where
    approot _ = ""

instance YesodPersist CommentTest where
    type YesodDB CommentTest = SqlPersist
    runDB db = fmap connPool getYesod >>= runConnectionPool db

getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do

        -- | 'runCommentsForm' returns Hamlet in the GHandler Monad.
        --   Since we're in the GWidget Monad right now, we'll use
        --   liftHandler to make the call.
        --
        --   If you have a per-route layout not using a widget, you
        --   could call runCommentsForm without the lift and just insert
        --   ^pageComments^ in your template where desired.
        pageComments <- liftHandler $ runCommentsForm 
            defaultTemplate -- ^ the template to use (see "Comments.Templates")
            persistentDB    -- ^ the storage backed (see "Comments.Storage")
            "testpage"      -- ^ the 'ThreadId' of this page
            RootR           -- ^ the 'Route' to redirect to after a POST

        setTitle $ string "test homepage"
        addWidget $
            [$hamlet|
                %h1 Test Page
                %p
                  Welcome to my comments test page. It's very ugly 
                  because I'm using no styling whatsoever. However, each 
                  element in the comments section is wrapped in a 
                  %em comments
                  \ div tag to make styling simple.
            |]

        -- | Add the comments section at the end of the page
        addHamlet $ pageComments

postRootR :: Handler RepHtml
postRootR = getRootR

main :: IO ()
main = putStrLn "Loaded" >> withCommentTest (run 3000)
    where
        withCommentTest :: (Application -> IO a) -> IO a
        withCommentTest f = withConnectionPool $ \p -> do
            runConnectionPool (runMigration migrateAll) p
            let h = CommentTest p
            toWaiApp h >>= f
