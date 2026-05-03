{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.Schema where

import           Data.Text           (Text)
import           Data.Time           (Day, UTCTime)
import           Database.Persist.TH

import           Type.General        (Email)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    name       Text
    password   Text
    email      Email
    dob        Day Maybe
    created_at UTCTime

    UniqueEmail email
    deriving Show

TempUser sql=temp_users
    name       Text
    password   Text
    email      Email
    dob        Day Maybe
    expired_at UTCTime
    token      Text

    UniqueTempEmail email
    deriving Show

Post sql=posts
    title      Text
    content    Text
    user_id    UserId
    created_at UTCTime
    updated_at UTCTime

    deriving Show
|]
