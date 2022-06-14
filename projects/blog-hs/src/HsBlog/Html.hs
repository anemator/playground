-- stable interface to Html
module HsBlog.Html
  ( Html -- export type only NOT constructor
  , Title
  , Structure
  , html_
  , h_
  , p_
  , pre_
  , ol_
  , ul_
  , render
  )
  where

import HsBlog.Html.Internal
