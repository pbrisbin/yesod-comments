{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Comments.Fields
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Custom field definitions used for username and comment input
--
-------------------------------------------------------------------------------
module Comments.Fields 
    ( userField
    , commentField
    ) where

import Yesod
import Yesod.Form
import Yesod.Form.Core
import Control.Monad   (mplus)
import Data.Maybe      (fromMaybe)

-- | A custom stringField but with special validation
userField :: String -> FormletField sub y String
userField label initial = GForm $ do
    userId   <- newFormIdent
    userName <- newFormIdent
    env      <- askParams

    let res = case env of
                [] -> FormMissing
                _  ->
                    case lookup userName env of
                        Just userString -> 
                            if isValid userString
                                then FormSuccess userString
                                else FormFailure ["[a-zA-Z-_. ]"]
                        _               -> FormFailure ["Value is required"]

    let userValue = fromMaybe "" $ lookup userName env `mplus` initial
    let fi = FieldInfo { fiLabel   = string label
                       , fiTooltip = string ""
                       , fiIdent = userId
                       , fiInput = [$hamlet|
%input#userId!name=$userName$!type=text!value=$userValue$!size="22"
|]
                       , fiErrors =
                           case res of
                               FormFailure [x] -> Just $ string x
                               _               -> Nothing
                       }

    return (res, [fi], UrlEncoded)
    where
        isValid s  = (s /= []) && all (`elem` validChars) s
        validChars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-_. " 

-- | A copy of textareaField but with a larger entry box
commentField :: FormFieldSettings -> FormletField sub y Textarea
commentField = requiredFieldHelper textareaFieldProfile'

textareaFieldProfile' :: FieldProfile sub y Textarea
textareaFieldProfile' = FieldProfile
    { fpParse  = Right . Textarea
    , fpRender = unTextarea
    , fpWidget = \theId name val _isReq -> addBody [$hamlet|
%textarea#$theId$!name=$name$!cols="80%"!rows="10" $val$
|]
    }
