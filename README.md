# Yesod Comments

### Description

A drop-in comments module for a Yesod application with a focus on 
flexibility and simplicity.

Note:

This module expects my fork of yesod-markdown[repo][].

### Usage

See ./Test.hs or read the [haddocks][].

### Try it

Assuming you've got the required dependencies you can run the Test app 
directly:

    git clone git://github.com/pbrisbin/yesod-comments.git
    cd yesod-comments
    runhaskell Test.hs
    $BROWSER http://localhost:3000


Here's what it looks like on my server:

![Yesod MPC Shot](http://pbrisbin.com/static/fileshare/yesod_comments.png)

[repo]:     https://github.com/pbrisbin/yesod-markdown "my fork of yesod markdown"
[haddocks]: http://pbrisbin.com/haskell/docs/html/yesod-comments "yesod comments haddocks"
