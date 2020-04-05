{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Table.UserRegistration where

import Prelude as Prelude
import Database.Beam as B
import Database.PostgreSQL.Simple
import Database.Beam.Postgres
import Data.Text
import Data.Aeson
import Control.Lens

data UserRegistrationT f =
    UserRegistration
    { _id :: B.C f Text
    , _userName :: B.C f Text
    , _userMobile :: B.C f Text
    , _userEmail :: B.C f Text
    , _userRegistrationToken :: B.C f Text
    } deriving (Generic, Beamable)

type UserRegistration = UserRegistrationT Identity

type UserRegistrationPrimaryKey = B.PrimaryKey UserRegistrationT Identity

instance B.Table UserRegistrationT where
    data PrimaryKey UserRegistrationT f = UserRegistrationPrimaryKey (B.C f Text) deriving (Generic, B.Beamable)
    primaryKey = UserRegistrationPrimaryKey . _id

userRegistrationEntityModification :: B.EntityModification (B.DatabaseEntity be db) be  (B.TableEntity UserRegistrationT)
userRegistrationEntityModification = B.setEntityName "UserRegistrations" <>
        B.modifyTableFields
            B.tableModification
            {    _id = "id"
            ,   _userName = "userName"
            ,   _userMobile = "userMobile"
            ,   _userEmail  = "userEmail"
            ,   _userRegistrationToken = "userRegistrationToken"
            }


instance FromJSON UserRegistration where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = Prelude.drop 1}

instance ToJSON UserRegistration where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = Prelude.drop 1}

deriving instance Show UserRegistration
deriving instance Eq UserRegistration

UserRegistration
    (B.LensFor id)
    (B.LensFor userName)
    (B.LensFor userMobile)
    (B.LensFor userEmail)
    (B.LensFor userRegistrationToken) = B.tableLenses

insertExpression c = insertExpressionList [c]
insertExpressionList cs = B.insertExpressions (toRowExpression <$> cs)
    where
        toRowExpression UserRegistration {..} =
            UserRegistration
                (B.val_ _id)
                (B.val_ _userName)
                (B.val_ _userMobile)
                (B.val_ _userEmail)
                (B.val_ _userRegistrationToken)

