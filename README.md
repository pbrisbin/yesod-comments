# Yesod Comments

### Description

A drop-in comments module for a Yesod application with a focus on 
flexibility and simplicity.

### Usage

See ./Test.hs or read the [haddocks][].

### Try it

Assuming you've got the required dependencies you can run the Test app 
directly:

    git clone git://github.com/pbrisbin/yesod-comments.git
    cd yesod-comments
    runhaskell Test.hs
    $BROWSER http://localhost:3000

Note:

This module expects my forked version of yesod-markdown. Either install 
it, or uncomment the lines regarding `markdownField` as noted in 
Yesod/Comments.hs.

Here's what it looks like on my server:

![Yesod MPC Shot](http://pbrisbin.com/static/fileshare/yesod_comments.png)

[haddocks]: http://pbrisbin.com/haskell/docs/html/yesod-comments "yesod comments haddocks"
