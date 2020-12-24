{-# LANGUAGE TemplateHaskell #-}

module TonaApp.Type where

import Tonalude

import Data.Aeson.TH
import TonaApp.Type.Option



data Status
  = StatusA Text Text
  | StatusB
  deriving (Eq, Show)

deriveToJSON customOptions ''Status


data Response = Response
  { status :: Status
  } deriving (Eq, Show)

deriveToJSON defaultOptions ''Response
