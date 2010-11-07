{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
--
-- pbrisbin 2010
--
-- This is a single-file test application which shows how to use the
-- comments module to add comments to an arbitrary page.
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

        -- | and just ^embed^ it in your template where desired.
        addHamlet $
            [$hamlet|
                #header
                    %h1 Test Page
                    %hr
                #body
                    %p Welcome to my comments test page.
                    %h3 Comments
                    ^pageComments^
                #footer
                    %hr
                    %p pbrisbin 2010
            |]

        -- | An example of the kind of styling you can do.
        addCassius $
            [$cassius|
                body
                    display: block
                    width: 710px
                    margin-left: auto
                    margin-right: auto
                    color: #bbbbbb
                    background-color: #303030
                    font-family: Verdana, sans-serif
                    font-weight: normal

                h1
                    color: #ffffff
                    text-align: center
                    font-weight: normal

                h3
                    color: #ffffff
                    font-weight: normal
                    margin-left: -5%

                h4
                    color: #ffffff
                    font-weight: normal
                    margin-left: -2%

                #comments
                      border: 1px solid #ffffff
                      border-top: 1px solid #909090
                      border-right: 1px solid #909090
                      padding-left: 5%  
                      padding-right: 5%  
                      padding-bottom: 1%
                      font-size: 90% 

                #comments blockquote
                      color: #9c9c6b
                      font-style: italic
                      border-left: solid 3px #404040
                      padding-left: 2%  
                      padding-bottom: 1%

                #comments td
                      vertical-align: top 

                #footer
                    text-align: center
                    font-size: 85%
                a:link, a:visited
                    color: #91bfc2
                    outline: none
                    text-decoration: none

                a:hover
                    color: #a291c2
                    outline: none
                    text-decoration: none

                strong
                    color: #ffffff
                    font-weight: normal
            |]

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
