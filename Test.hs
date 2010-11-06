{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
--
-- pbrisbin 2010
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

data CommentTest = CommentTest { connPool :: ConnectionPool }
type Handler = GHandler CommentTest CommentTest
type Widget  = GWidget CommentTest CommentTest

-- | Make sure you allow the POST route on any routes where comments
--   will be entered. It can just link to GET (see 'postRootR')
mkYesod "CommentTest" [$parseRoutes| / RootR GET POST |]

-- | Main app definition
instance Yesod CommentTest where approot _ = ""

-- | App is an instance of YesodPersist
instance YesodPersist CommentTest where
    type YesodDB CommentTest = SqlPersist
    runDB db = fmap connPool getYesod >>= runSqlPool db

-- | Database setting
withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool "comments.db3" 10

-- | The main route definition
getRootR :: Handler RepHtml
getRootR = do

    -- | Fetch the Hamlet for this page's comments section:
    pageComments <- runCommentsForm 
        defaultTemplate -- ^ The template to use (see "Comments.Templates")
        persistentDB    -- ^ The storage backed (see "Comments.Storage")
        "testpage"      -- ^ The 'ThreadId' of this page
        RootR           -- ^ The 'Route' to redirect to after a POST

    defaultLayout $ do
        setTitle $ string "test homepage"
        addWidget $
            [$hamlet|
                %h1 Test Page
                %p
                  Welcome to my comments test page. It's very ugly 
                  because I'm using no styling whatsoever. However, the 
                  entire comments section is wrapped in a 
                  %em comments
                  \ div tag to make styling simple.
            |]

        -- | Since page comments is 'Hamlet', we can either ^include^ it
        --   in another template where appropriate, or (in this case)
        --   use 'addHamlet' to add it to a widget.
        addHamlet pageComments

-- | POST is just GET in our case
postRootR :: Handler RepHtml
postRootR = getRootR

-- | Run the app
main :: IO ()
main = putStrLn "Loaded" >> withCommentTest (run 3000)

withCommentTest :: (Application -> IO a) -> IO a
withCommentTest f = withConnectionPool $ \p -> do
    runSqlPool (runMigration migrateAll) p
    let h = CommentTest p
    toWaiApp h >>= f
