> [!NOTE]
> All of my GitHub repositories have been **archived** and will be migrated to
> Codeberg as I next work on them. This repository either now lives, or will
> live, at:
>
> https://codeberg.org/pbrisbin/yesod-comments
>
> If you need to report an Issue or raise a PR, and this migration hasn't
> happened yet, send an email to me@pbrisbin.com.

# Yesod Comments

A drop-in comments module for a Yesod application.

## Installation

~~~ { .bash }
# Stable
$ cabal install yesod-coments

# Development
$ git clone https://github.com/pbrisbin/yesod-comments
$ cd yesod-comments
$ cabal install
~~~

## Usage

Make your foundation type an instance of `YesodComments`:

~~~ { .haskell }
import Yesod.Comments

instance YesodComments MySite where
    --
    -- Store comments using YesodPersist. It's the only backend 
    -- available at this time.
    --
    commentStorage = persistStorage

    --
    -- userDetails :: UserId -> Handler (Maybe UserDetails)
    --
    -- This maps an AuthId m to more useful details like friendly name 
    -- and email address.
    --
    userDetails = undefined
~~~

Next, add comments to some view somewhere. You only have to provide a 
unique thread identifier, and the widget will handle showing existing 
comments and a form for submitting new ones.

**Note**: Users must be authenticated to comment, therefore your site 
must be an instance of `YesodAuth` in order to use `YesodComments`.

~~~ { .haskell }
-- addComments handles displaying the form on GET and processing it on 
-- POST, this follows the common Yesod idiom of handling both methods 
-- with one handler:

getPostR :: String -> Handler RepHtml
getPostR slug = do
    post <- getPostBySlug slug

    defaultLayout $ do
        setTitle "A post"

        [whamlet|
            <div .content>
                #{postContent post}

            <div .comments>
                #{addComments slug}
        |]

postPostR :: String -> Handler RepHtml
postPostR = getPostR
~~~

Finally, add a `migrateComments` call to your Application runner.

~~~ { .haskell }
makeFoundation :: AppConfig DefaultEnv () -> Logger -> IO MySite
makeFoundation conf setLogger = do
    --
    -- ...
    --
    Database.Persist.Store.runPool dbconf (runMigration migrateComments) p
    return $ MySite conf setLogger s p manager dbconf
~~~

## Administration

An administration subsite is provided to allow users to edit and delete 
comments they've left themselves. Any additional moderation must be done 
manually (for now).

Add the subsite to your routes:

~~~
/comments CommentsAdminR CommentsAdmin getCommentsAdmin
~~~

Optionally, define a few more functions in the `YesodComments` instance:

~~~ { .haskell }
instance YesodComments MySite where
    --
    -- ...
    --

    -- How to get from a thread identifier to the page where it resides, 
    -- this is used to provide links back from the "my comments" page
    threadRoute = Just $ \thread -> PostR thread

    -- How to get to the subsite's Edit action for a comment
    editRoute = Just $ \thread cid -> CommentsAdminR $ EditCommentR thread cid

    -- How to get to the subsite's Delete action for a comment
    deleteRoute = Just $ \thread cid -> CommentsAdminR $ DeleteCommentR thread cid
~~~

With these defined, Edit and Delete links will appear whenever a user 
sees a comment they've left.

## Styling

There is no styling provided by this library, but the markup is designed 
for Twitter's Bootstrap. If you use that, it should look OK out of the 
box. Even if you don't, the `div`s that Bootstrap requires should be 
more than adequate for creating your own styles.
