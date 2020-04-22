{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GI.Cairo.Render.LibRSvg.Context (Svg(..), Cairo, unCairo, rsvgCtx) where

import Data.Monoid (mempty, (<>))
import qualified Language.C.Inline as C
import           Language.C.Inline.Context
import qualified Language.C.Types as C
import Foreign.ForeignPtr
import GI.Cairo.Render.Types

newtype Svg = Svg (ForeignPtr Svg)
  deriving (Eq, Show)

rsvgCtx :: C.Context
rsvgCtx = C.baseCtx <> ctx'
  where ctx' = mempty {
    ctxTypesTable =
      [ (C.TypeName "cairo_t", [t| Cairo |])
      , (C.TypeName "RsvgHandle", [t| Svg |])
      ]
    }
