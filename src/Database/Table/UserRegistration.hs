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

module Database.Table.UserRegistration where

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

