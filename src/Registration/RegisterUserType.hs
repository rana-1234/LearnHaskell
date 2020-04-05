{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Registration.RegisterUserType where

import Prelude
import Data.Aeson
import Data.Aeson.Lens
import Data.Text hiding (drop)
import GHC.Generics
import Control.Lens

data RegisterUserRequest =
    RegisterUserRequest
    { _userName :: Text
    , _userEmail :: Text
    , _userMobile :: Text
    } deriving (Show, Eq, Generic)

instance ToJSON RegisterUserRequest where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON RegisterUserRequest where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data RegisterUserResponse =
    RegisterUserResponse
    { _userRegistrationToken :: Text
    , _userId :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON RegisterUserResponse where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

instance ToJSON RegisterUserResponse where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

makeLenses ''RegisterUserRequest
makeLenses ''RegisterUserResponse