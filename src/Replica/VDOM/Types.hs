{-# LANGUAGE OverloadedStrings #-}

module Replica.VDOM.Types where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Text as T

t :: T.Text -> T.Text
t = id

type HTML = [VDOM]

-- | A namespace URI.
--
-- For details, see the documentation on the @namespaceURI@ argument
-- to [createElementNS](https://developer.mozilla.org/en-US/docs/Web/API/Document/createElementNS).
newtype Namespace = Namespace { getNamespace :: T.Text } deriving (Eq, Ord, Show)

-- | Representation of the Document Object Model in Haskell.

-- TODO: add note about difference about Text and RawText
data VDOM
    = VNode !T.Text !Attrs !(Maybe Namespace) ![VDOM]
    | VLeaf !T.Text !Attrs !(Maybe Namespace)
    | VText !T.Text
    | VRawText !T.Text

instance A.ToJSON VDOM where
    toJSON (VText text) =
        A.object
            [ "type" .= t "text"
            , "text" .= text
            ]
    toJSON (VRawText text) =
        A.object
            [ "type" .= t "text"
            , "text" .= text
            ]
    toJSON (VLeaf element attrs mNamespace) =
        A.object
            [ "type" .= t "leaf"
            , "element" .= element
            , "attrs" .= attrs
            , "namespace" .= fmap getNamespace mNamespace
            ]
    toJSON (VNode element attrs mNamespace children) =
        A.object
            [ "type" .= t "node"
            , "element" .= element
            , "attrs" .= attrs
            , "namespace" .= fmap getNamespace mNamespace
            , "children" .= children
            ]

type Attrs = M.Map T.Text Attr

-- TODO: Add a note why we need `AMap !Attrs'
data Attr
    = AText !T.Text
    | ABool !Bool
    | AEvent EventOptions !(DOMEvent -> IO ())
    | AMap !Attrs

instance A.ToJSON Attr where
    toJSON (AText v) = A.String v
    toJSON (ABool v) = A.Bool v
    toJSON (AEvent opts _) = A.toJSON opts
    toJSON (AMap v) = A.toJSON $ fmap A.toJSON v

newtype DOMEvent = DOMEvent {getDOMEvent :: A.Value} deriving Show

data EventOptions = EventOptions
  { evtCapture :: Bool
  , evtPreventDefault :: Bool
  , evtStopPropagation :: Bool
  } deriving (Eq, Ord, Show)

instance A.ToJSON EventOptions where
  toJSON opts = A.object
    [ "capture" .= evtCapture opts
    , "preventDefault" .= evtPreventDefault opts
    , "stopPropagation" .= evtStopPropagation opts
    ]

