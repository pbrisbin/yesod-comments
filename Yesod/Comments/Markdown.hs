{-# LANGUAGE CPP                        #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Comments.Markdown
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Convenience functions build on top of "Text.Pandoc".
--
-- This was pulled almost entirely from 
-- <https://github.com/ajdunlap/yesod-markdown>, but I needed some 
-- things changed (my pull request is still open) and there was a lot I 
-- wasn't using.
--
-------------------------------------------------------------------------------
module Yesod.Comments.Markdown
  ( Markdown(..)
  -- * Conversions
  , parseMarkdown
  , writePandoc
  -- * Wrappers
  , markdownToHtml
  , markdownFromFile
  -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultParserState
  -- * Form helpers
  , markdownField
  , maybeMarkdownField
  )
  where

import Yesod
import Yesod.Form.Core
import Text.Pandoc
import Text.Pandoc.Shared

import Data.Monoid         (Monoid)
import Data.String         (IsString)
import System.Directory    (doesFileExist)

import qualified Data.Text as T

newtype Markdown = Markdown String
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance ToFormField Markdown y where
    toFormField = markdownField

instance ToFormField (Maybe Markdown) y where
    toFormField = maybeMarkdownField

markdownField :: (IsForm f, FormType f ~ Markdown) => FormFieldSettings -> Maybe Markdown -> f
markdownField = requiredFieldHelper markdownFieldProfile

maybeMarkdownField :: FormFieldSettings -> FormletField sub y (Maybe Markdown)
maybeMarkdownField = optionalFieldHelper markdownFieldProfile

markdownFieldProfile :: FieldProfile sub y Markdown
markdownFieldProfile = FieldProfile
    { fpParse = Right . Markdown . unlines . lines' . T.unpack
    , fpRender = \(Markdown m) -> T.pack m
    , fpWidget = \theId name val _isReq -> addHamlet
#if __GLASGOW_HASKELL__ >= 700
        [hamlet|
#else
        [$hamlet|
#endif
            <textarea id="#{theId}" name="#{name}" .markdown>#{val}
            |]
    }

    where
        lines' :: String -> [String]
        lines' = map go . lines

        go []        = []
        go ('\r':xs) = go xs
        go (x:xs)    = x : go xs

-- | Converts markdown directly to html using the yesod default option 
--   sets
markdownToHtml :: Markdown -> Html
markdownToHtml = writePandoc yesodDefaultWriterOptions
               . parseMarkdown yesodDefaultParserState

-- | Reads markdown in from the specified file; returns the empty string 
--   if the file does not exist
markdownFromFile :: FilePath -> IO Markdown
markdownFromFile f = do
    exists <- doesFileExist f
    content <- do
        if exists
            then readFile f
            else return ""

    return $ Markdown content

-- | Write 'Pandoc' to 'Html'.
writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedString . writeHtmlString wo

-- | Read in 'Markdown' to an intermediate 'Pandoc' type.
parseMarkdown :: ParserState -> Markdown -> Pandoc
parseMarkdown ro (Markdown m) = readMarkdown ro m

-- | A set of default Pandoc writer options good for Yesod websites. 
--   Enables Javascript-based email obfuscation, eliminates div tags 
--   around sections, and disables text wrapping.
yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = defaultWriterOptions
  { writerEmailObfuscation = JavascriptObfuscation
  , writerSectionDivs      = False
  , writerWrapText         = False
  }

-- | A set of default Pandoc reader options good for Yesod websites. 
yesodDefaultParserState :: ParserState
yesodDefaultParserState = defaultParserState
    { stateSmart    = True
    , stateParseRaw = True
    }
