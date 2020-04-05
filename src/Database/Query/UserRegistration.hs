module Database.Query.UserRegistration where

import qualified Database.DB as DB
import Database.Beam as B
import qualified Database.Table.UserRegistration as UserRegistrationTable

dbTable :: B.DatabaseEntity be DB.RanaPaymentDb (B.TableEntity UserRegistrationTable.UserRegistrationT)
dbTable  = DB.userRegistration DB.ranaPaymentDb

