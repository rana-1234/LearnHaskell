-- {-# LANGUAGE DeriveGeneric  #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE UndecidableInstances #-}

-- module DB.UserTable where

-- import Data.Text (Text)
-- import Database.Beam as B
-- import Database.Beam.Postgres
-- import Database.PostgreSQL.Simple
-- import Database.Beam.Backend.SQL
-- import Control.Lens

-- --let conn = connectPostgreSQL "dbName=LocalDb"

-- data UserT f = User
--   { _userEmail     :: Columnar f Text
--   , _userFirstName :: Columnar f Text
--   , _userLastName  :: Columnar f Text
--   , _userPassword  :: Columnar f Text
--   } deriving (Generic)


-- -- The above User Table declaration will map to column
-- -- email , first_name, last_name and password by default
-- -- for deriving Generics we need {-# LANGUAGE DeriveGeneric #-}

-- type User = UserT Identity  --Means User type just looks like a record type-- f stands for Identity

-- -- equivalent to below record
-- -- data User =
-- --     User{
-- --         _userEmail :: Text,
-- --         _userFirstName :: Text,
-- --         _userLastName :: Text,
-- --         _userPassword :: Text
-- --     } deriving (Generic)

-- --This is why UserT has data constructor User

-- type UserId = PrimaryKey UserT Identity  -- user id , that in actual database email

-- --Now let the beam know about our table

-- instance Beamable UserT
-- instance Beamable (PrimaryKey UserT)

-- instance Table UserT where
--   data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
--   primaryKey = UserId . _userEmail
-- {--
-- This is where we  need
--         {-# LANGUAGE FlexibleInstances #-}
-- -- and  {-# LANGUAGE TypeFamilies      #-}
-- --}

-- -- now let the beam know about our database
-- data ShoppingCartDb f = ShoppingCartDb
--   { _shoppingCartUsers :: f (TableEntity UserT)
--   } deriving (Generic)

-- --instance Database ShoppingCartDb

-- shoppingCartDb :: DatabaseSettings be ShoppingCartDb
-- shoppingCartDb = defaultDbSettings

-- --instance Database LocalDb

-- -- localDb :: DatabaseSettings be LocalDb
-- -- localDb = defaultDbSettings
-- -- -- this localDb is our handle, we use it to get at our tables

-- -- -- Operations on the db

-- -- -- insertion into table
-- -- insertIntoUser :: Connection -> IO ()
-- -- insertIntoUser conn =
-- --     withDatabaseDebug putStrLn conn $ B.runInsert $
-- --         B.insert (_localDbUsers localDb) $
-- --         insertValues [
-- --                 User "james@example.com" "James" "Smith" "b4cc344d25a2efe540adbf2678e2304c",
-- --                 User "betty@example.com" "Betty" "Jones" "82b054bd83ffad9b6cf8bdb98ce3cc2f",
-- --                 User "james@pallo.com" "James" "Pallo" "b4cc344d25a2efe540adbf2678e2304c",
-- --                 User "james@oreily.com" "James" "O'Reily" "b4cc344d25a2efe540adbf2678e2304c",
-- --                 User "betty@sims.com" "Betty" "Sims" "82b054bd83ffad9b6cf8bdb98ce3cc2f",
-- --                 User "sam@sophitz.com" "Sam" "Sophitz" "332532dcfaa1cbf61e2a266bd723612c",
-- --                 User "sam@jely.com" "Sam" "Jely" "332532dcfaa1cbf61e2a266bd723612c",
-- --                 User "sam@example.com" "Sam" "Taylor" "332532dcfaa1cbf61e2a266bd723612c"
-- --             ]



-- data AddressT f = Address
--   { _addressId :: C f (Int)
--   , _addressLine1 :: C f Text
--   , _addressLine2 :: C f (Maybe Text)
--   , _addressCity :: C f Text
--   , _addressState :: C f Text
--   , _addressZip :: C f Text
--   , _addressForUser :: PrimaryKey UserT f
--   } deriving (Generic)

-- type Address = AddressT Identity
-- type AddressId = PrimaryKey AddressT Identity

-- deriving instance Show UserId
-- deriving instance Show Address

-- instance Beamable AddressT
-- instance Beamable (PrimaryKey AddressT)

-- instance Table AddressT where
--     data PrimaryKey AddressT f = AddressId (Columnar f (Int)) deriving Generic
--     primaryKey = AddressId . _addressId

-- Address
--     (LensFor addressId)
--     (LensFor addressLine1)
--     (LensFor addressLine2)
--     (LensFor addressCity)
--     (LensFor addressState)
--     (LensFor addressZip)
--     (UserId (LensFor addressForUserId)) = tableLenses

-- User
--     (LensFor userEmail)
--     (LensFor userFirstName)
--     (LensFor userLastName)
--     (LensFor userPassword) = tableLenses

-- ShoppingCartDb
--     (TableLens shoppingCartUsers)
--     (TableLens shoppingCartUserAddresses) = dbLenses