{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Database.DB where

import qualified Database.Table.UserRegistration as UserRegistrationTable
import Database.Beam as B

data RanaPaymentDb f =
    RanaPaymentDb
    { userRegistration :: f (B.TableEntity UserRegistrationTable.UserRegistrationT)
    } deriving (Generic, B.Database be)

ranaPaymentDb :: B.DatabaseSettings be RanaPaymentDb
ranaPaymentDb =
    B.defaultDbSettings `B.withDbModification`
        B.dbModification
        { userRegistration = UserRegistrationTable.userRegistrationEntityModification
        }

