{-# LANGUAGE DeriveGeneric #-}
module Types
    ( ServiceConfig (..)
    , Interface (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data ServiceConfig = ServiceConfig
    { interfaces :: ![Interface]
    } deriving (Generic, Show)

data Interface = Interface
    { role     :: !String
    , address  :: !String
    , port     :: !Int
    , protocol :: !String
    } deriving (Generic, Show)

instance FromJSON ServiceConfig
instance FromJSON Interface
instance ToJSON ServiceConfig
instance ToJSON Interface

