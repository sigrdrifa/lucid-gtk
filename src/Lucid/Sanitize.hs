{-# LANGUAGE PackageImports #-}

-- | A program to sanitize an HTML tag to a Haskell function.
module Lucid.Sanitize (
  module Lucid.Sanitize,
) where

import Lucid.Supplemental (svgCamelCaseAttrs)
import "base" Data.Char (toLower, toUpper)
import "base" Data.List (isPrefixOf)

{- | Sanitize a tag. This function returns a name that can be used as
combinator in haskell source code.

Examples:

> sanitize "class" == "class_"
> sanitize "http-equiv" == "httpEquiv"
-}
sanitize :: String -> String
sanitize str
  | "x-transition" == lower = lower
  | "x-transition:enter" == lower = "xTransitionEnter_"
  | "x-transition:enter-start" == lower = "xTransitionEnterStart_"
  | "x-transition:leave-start" == lower = "xTransitionLeaveStart_"
  | "x-transition:leave" == lower = "xTransitionLeave_"
  | "@keydown.widow.escape" == lower = "xKeyDownWindowEscape_"
  | "data-" `isPrefixOf` lower = lower
  -- begin hack for svg
  | str `elem` svgCamelCaseAttrs = str
  | ':' `elem` str =
      appendUnderscore $ removeColon lower
  -- end hack for svg
  -- begin hack for x-transition
  -- end hack for x-transition
  | otherwise =
      appendUnderscore $ removeDash lower
 where
  lower = map toLower str

  -- Remove a dash, replacing it by camelcase notation
  --
  -- Example:
  --
  -- > removeDash "foo-bar" == "fooBar"
  --
  removeDash ('-' : x : xs) = toUpper x : removeDash xs
  removeDash (x : xs) = x : removeDash xs
  removeDash [] = []
  -- hack for svg
  removeColon (':' : x : xs) = toUpper x : removeColon xs
  removeColon (x : xs) = x : removeColon xs
  removeColon [] = []

  appendUnderscore = (++ "_")

{- | Carefully turn tag to a lower case. This function returns
a name that can be compared with the known tag-names.

Examples:

> lowerize "Meta" == "meta"
> lowerize "vewBox" == "vewBox"
-}
lowerize :: String -> String
lowerize str
  -- begin hack for svg
  | str `elem` svgCamelCaseAttrs = str
  -- end hack for svg
  | "data-" `isPrefixOf` lower = lower
  | otherwise =
      lower
 where
  lower = map toLower str
