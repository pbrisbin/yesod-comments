# Yesod Comments

## Description

A drop-in comments module for a Yesod application with a focus on 
flexibility and simplicity.

Now available on 
[hackage](http://hackage.haskell.org/package/yesod-comments).

### Usage

See the [haddocks][].

Also, check out my [landlord][] project to see it in use.

[haddocks]: http://pbrisbin.com/haskell/docs/html/yesod-comments "yesod comments haddocks"
[landlord]: http://rentersreality.com "review landlords at renters' reality"

### Styling

By default there is no styling, but there are (hopefully) useful classes 
on all the involved divs.

The forms themselves are somewhat catered towards Twitter's bootstrap. 
If you use that on your site, things should look decent out of the box.

The following additional styling works pretty well for me:

~~~ { .css }
.yesod_comments {
  width: 500px;
  margin-left: 25px;
}
.yesod_comments .avatar {
  float: left;
  height: 48px;
  width:  48px;
  margin-right: 12px;
}
.yesod_comments .list .avatar {
  margin: 0px;
  margin-right: 5px;
  height: 20px;
  width:  20px;

  vertical-align: middle;
}
.yesod_comments .list .attribution a,
.yesod_comments .list .attribution p {
  font-size: 95%;
}
.yesod_comments .list .attribution p {
  color: #999;
}
.yesod_comments .list .comment {
  padding: 5px;
  margin-bottom: 20px;
  border: solid 1px #eee;
}
.yesod_comments .list .comment .content {
  padding: 0px;
  margin: 20px 5px;
}
.yesod_comments .input form .clearfix {
  margin-left: 60px;
}
.yesod_comments .input textarea {
  height: 180px;
  width:  400px;
}
.form-stacked .clearfix {
  margin-bottom: 0px;
}
~~~
