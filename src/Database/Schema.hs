{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Database.Schema where

import Database.Persist.TH
import Data.Time (Day, UTCTime)
import Data.Text (Text)

import Type.General (Email)

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
