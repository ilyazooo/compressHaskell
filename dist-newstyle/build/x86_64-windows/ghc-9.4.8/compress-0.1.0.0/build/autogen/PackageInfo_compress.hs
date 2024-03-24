{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_compress (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "compress"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Lossless compression"
copyright :: String
copyright = "2023 CY Tech"
homepage :: String
homepage = ""
