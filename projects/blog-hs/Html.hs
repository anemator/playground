-- stable interface to Html
module Html
  ( Html -- export type only NOT constructor
  , Title
  , Structure
  , concatStructure
  , empty_
  , html_
  , h_
  , p_
  , pre_
  , ol_
  , ul_
  , render
  )
  where

import Html.Internal
