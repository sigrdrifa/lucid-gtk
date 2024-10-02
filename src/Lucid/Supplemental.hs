{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

{- | Supplemental to Html5 terms.
Some of them are obsolete or deprecated but still used in real pages.
-}
module Lucid.Supplemental where

import Data.Text (Text, intercalate)
import Lucid.Base

----- List of tags and attributes, don't forget to add here if
----- corresponding function is added

-- | parent tags
parentElements :: [String]
parentElements =
  ["tt", "svg"]

-- | leaf tags
leafElements :: [String]
leafElements =
  [ "relative-time"
  , "time-until"
  , "time-ago"
  , "local-time"
  , "g:plusone"
  ]

-- | attributes
attributeElements :: [String]
attributeElements =
  [ "aria-hidden"
  , "color"
  , "aria-label"
  , "autocapitalize"
  , "aria-expanded"
  , "aria-haspopup"
  , "aria-labelledby"
  , "aria-describedby"
  , "as"
  , "border"
  , "cellpadding"
  , "cellspacing"
  , "frameborder"
  , "hspace"
  , "vspace"
  , "marginheight"
  , "marginwidth"
  , "allowtransparency"
  , "allowfullscreen"
  , "clear"
  , "fb-xfbml-state"
  , "fb-iframe-plugin-query"
  , "gapi_processed"
  , "ng-non-bindable"
  , "property"
  , "language"
  , "scrolling"
  , "align"
  , "itemscope"
  , "itemtype"
  ]
    -- hack for svg
    ++ svgAttrs
    ++ svgCamelCaseAttrs

------ Parent elements ---------------------
---------------------------------------------

-- | @\<tt\>@ tag, deprecated
tt_ :: (Term arg result) => arg -> result
tt_ = term "tt"

-- here is hack for <svg> tag

-- | @\<svg\>@ tag.
svg_ :: (TermRaw arg result) => arg -> result
svg_ = termRaw "svg"

defs_ :: (TermRaw arg result) => arg -> result 
defs_ = termRaw "defs"

pattern_ :: (TermRaw arg result) => arg -> result
pattern_ = termRaw "pattern"

rect_ :: (TermRaw arg result) => arg -> result
rect_ = termRaw "rect"

------ Leaf elements -----------------------
-------------------------------------------

-- | @\<relative-time\>@ tag, from GitHub
relativeTime_ :: (Monad m) => [Attribute] -> HtmlT m ()
relativeTime_ = with (makeElementNoEnd "relative-time")

-- | @time-until@ element
timeUntil_ :: (Monad m) => [Attribute] -> HtmlT m ()
timeUntil_ = with (makeElementNoEnd "time-until")

-- | @time-ago@ element
timeAgo_ :: (Monad m) => [Attribute] -> HtmlT m ()
timeAgo_ = with (makeElementNoEnd "time-ago")

-- | @local-time@ element
localTime_ :: (Monad m) => [Attribute] -> HtmlT m ()
localTime_ = with (makeElementNoEnd "local-time")

-- | @g:plusone@ element
gPlusone_ :: (Monad m) => [Attribute] -> HtmlT m ()
gPlusone_ = with (makeElementNoEnd "g:plusone")

------ Attributes --------------------------
--------------------------------------------

-- | The @aria-expanded@ attribute
ariaExpanded_ :: Text -> Attribute
ariaExpanded_ = makeAttribute "aria-expanded"

-- | The @aria-haspopup@ attribute
ariaHaspopup_ :: Text -> Attribute
ariaHaspopup_ = makeAttribute "aria-haspopup"

-- | The @aria-hidden@ attribute
ariaHidden_ :: Text -> Attribute
ariaHidden_ = makeAttribute "aria-hidden"

-- | The @aria-label@ attribute for svg.
ariaLabel_ :: Text -> Attribute
ariaLabel_ = makeAttribute "aria-label"

ariaCurrent_ :: Text -> Attribute
ariaCurrent_ = makeAttribute "aria-current"

ariaOrientation_ :: Text -> Attribute
ariaOrientation_ = makeAttribute "aria-orientation"

-- | The @aria-labelledby@ attribute for svg.
ariaLabelledby_ :: Text -> Attribute
ariaLabelledby_ = makeAttribute "aria-labelledby"

-- | The @aria-describedby@ attribute for svg.
ariaDescribedby_ :: Text -> Attribute
ariaDescribedby_ = makeAttribute "aria-describedby"

ariaControls_ :: Text -> Attribute
ariaControls_ = makeAttribute "aria-controls"

-- | The @as@ attribute
as_ :: Text -> Attribute
as_ = makeAttribute "as"

-- | The @autocapitalize@ attribute
autocapitalize_ :: Text -> Attribute
autocapitalize_ = makeAttribute "autocapitalize"

{- | The @border@ attribute
This attribute is deprecated!
-}
border_ :: Text -> Attribute
border_ = makeAttribute "border"

{- | The @cellpadding@ attribute
This attribute is deprecated!
-}
cellpadding_ :: Text -> Attribute
cellpadding_ = makeAttribute "cellpadding"

{- | The @cellspacing@ attribute
This attribute is obsolete!
-}
cellspacing_ :: Text -> Attribute
cellspacing_ = makeAttribute "cellspacing"

-- | The @ng-non-bindable@ attribute
ngNonBindable_ :: Attribute
ngNonBindable_ = makeAttribute "ng-non-bindable" mempty

patternUnits_ :: Text -> Attribute
patternUnits_ = makeAttribute "patternUnits"


patternunits_ :: Text -> Attribute
patternunits_ = makeAttribute "patternunits"

{- | The @hspace@ attribute
This attribute is deprecated!
-}
hspace_ :: Text -> Attribute
hspace_ = makeAttribute "hspace"

{- | The @vspace@ attribute
This attribute is deprecated!
-}
vspace_ :: Text -> Attribute
vspace_ = makeAttribute "vspace"

{- | The @marginheight@ attribute
This attribute is deprecated!
-}
marginheight_ :: Text -> Attribute
marginheight_ = makeAttribute "marginheight"

{- | The @marginwidth@ attribute
This attribute is deprecated!
-}
marginwidth_ :: Text -> Attribute
marginwidth_ = makeAttribute "marginwidth"

-- | The @color@ attribute
color_ :: Text -> Attribute
color_ = makeAttribute "color"

{- | The @frameborder@ attribute
This attribute is deprecated!
-}
frameborder_ :: Text -> Attribute
frameborder_ = makeAttribute "frameborder"

-- | The @allowtransparency@ attribute
allowtransparency_ :: Text -> Attribute
allowtransparency_ = makeAttribute "allowtransparency"

-- | The @allowfullscreen@ attribute
allowfullscreen_ :: Text -> Attribute
allowfullscreen_ = makeAttribute "allowfullscreen"

{- | The @clear@ attribute
This attribute is deprecated!
-}
clear_ :: Text -> Attribute
clear_ = makeAttribute "clear"

-- | The @fb-xfbml-state@ attribute
fbXfbmlState_ :: Text -> Attribute
fbXfbmlState_ = makeAttribute "fb-xfbml-state"

-- | The @fb-iframe-plugin-query@ attribute
fbIframePluginQuery_ :: Text -> Attribute
fbIframePluginQuery_ = makeAttribute "fb-iframe-plugin-query"

-- | The @gapi_processed@ attribute
gapi_processed_ :: Text -> Attribute
gapi_processed_ = makeAttribute "gapi_processed"

-- | The @itemscope@ attribute
itemscope_ :: Text -> Attribute
itemscope_ = makeAttribute "itemscope"

-- | The @itemtype@ attribute
itemtype_ :: Text -> Attribute
itemtype_ = makeAttribute "itemtype"

-- | The @property@ attribute
property_ :: Text -> Attribute
property_ = makeAttribute "property"

-- | The @scrolling@ attribute
scrolling_ :: Text -> Attribute
scrolling_ = makeAttribute "scrolling"

-- | The @language@ attribute, deprecated
language_ :: Text -> Attribute
language_ = makeAttribute "language"

{- | The @align@ attribute.
This attribute is obsolete!
-}
align_ :: Text -> Attribute
align_ = makeAttribute "align"

------------ Svg attributes, remove when fix !!!! -----

-- | The @version@ attribute for svg.
version_ :: Text -> Attribute
version_ = makeAttribute "version"

-- | The @x@ attribute.
x_ :: Text -> Attribute
x_ = makeAttribute "x"

-- | The @y@ attribute.
y_ :: Text -> Attribute
y_ = makeAttribute "y"

-- | The @xmlns:xlink@ attribute.
xmlnsXlink_ :: Text -> Attribute
xmlnsXlink_ = makeAttribute "xmlns:xlink"

-- | The @xml:space@ attribute.
xmlSpace_ :: Text -> Attribute
xmlSpace_ = makeAttribute "xml:space"

-- | The @enable-background@ attribute
enableBackground_ :: Text -> Attribute
enableBackground_ = makeAttribute "enable-background"

-- | The @viewBox@ attribute
viewBox_ :: Text -> Attribute
viewBox_ = makeAttribute "viewBox"

{- | x-data
Declare a new Alpine component and its data for a block of HTML
-}
xData_ :: Text -> Attribute
xData_ = makeAttribute "x-data"

fillRule_ :: Text -> Attribute
fillRule_ = makeAttribute "fill-rule"

clipRule_ :: Text -> Attribute
clipRule_ = makeAttribute "clip-rule"

{-
<div x-data="{ open: false }">
    ...
</div>
-}

{- | x-bind
Dynamically set HTML attributes on an element
-}
xBind_ ::
  -- | Attributes name
  Text ->
  Text ->
  Attribute
xBind_ attr = makeAttribute ("x-bind:" <> attr)

{-
<div x-bind:class="! open ? 'hidden' : ''">
  ...
</div>
-}

{- | x-on
Listen for browser events on an element
-}
xOn_ ::
  -- | Event name
  Text ->
  Text ->
  Attribute
xOn_ event = makeAttribute ("x-on:" <> event)

{- | x-id
 - Target a specific id
-}
xId_ ::
  -- | Element id
  Text ->
  Attribute
xId_ = makeAttribute "x-id"

{-
<button x-on:click="open = ! open">
  Toggle
</button>
-}

clickAway_ :: Text -> Attribute
clickAway_ = makeAttribute "@click.away"

clickOutside_ :: Text -> Attribute
clickOutside_ = makeAttribute "@click.outside"

click_ :: Text -> Attribute
click_ = makeAttribute "@click"

{- | x-text
Set the text content of an element
-}
xText_ :: Text -> Attribute
xText_ = makeAttribute "x-text"

xPlaceholder_ :: Text -> Attribute
xPlaceholder_ = makeAttribute "x-placeholder"

{-
<div>
  Copyright ©

  <span x-text="new Date().getFullYear()"></span>
</div>
-}

{- | x-html
Set the inner HTML of an element
-}
xHtml_ :: Text -> Attribute
xHtml_ = makeAttribute "x-html"

{-
<div x-html="(await axios.get('/some/html/partial')).data">
  ...
</div>
-}

{- | x-model
Synchronize a piece of data with an input element
-}
xModel_ ::
  -- | List of x-model modifiers
  [Text] ->
  Text ->
  Attribute
xModel_ mods = case mods of
  [] -> makeAttribute "x-model"
  _ -> makeAttribute ("x-model." <> intercalate "." mods)

{-
<div x-data="{ search: '' }">
  <input type="text" x-model="search">

  Searching for: <span x-text="search"></span>
</div>
-}

{- | x-show
Toggle the visibility of an element
-}
xShow_ :: Text -> Attribute
xShow_ = makeAttribute "x-show"

{-
<div x-show="open">
  ...
</div>
-}

{- | x-transition
Transition an element in and out using CSS transitions
-}
xTransition_ ::
  -- | Transition directive
  Maybe Text ->
  -- | List of x-transition modifiers
  [Text] ->
  Text ->
  Attribute
xTransition_ Nothing [] _ = makeAttribute "x-transition" mempty -- No directive or modifiers
xTransition_ (Just dir) [] attrVal = makeAttribute ("x-transition:" <> dir) attrVal -- Directive with custom transition classes
xTransition_ Nothing mods _ = makeAttribute ("x-transition." <> intercalate "." mods) mempty -- No directive, but with modifiers
xTransition_ (Just dir) mods _ = makeAttribute ("x-transition:" <> dir <> "." <> intercalate "." mods) mempty -- Directive with modifiers

{-
<div x-show="open" x-transition>
  ...
</div>
-}

{- | x-for
Repeat a block of HTML based on a data set
-}
xFor_ :: Text -> Attribute
xFor_ = makeAttribute "x-for"

xForKey_ :: Text -> Attribute
xForKey_ = makeAttribute ":key"

xKeyDownWindowEscape_ :: Text -> Attribute
xKeyDownWindowEscape_ = makeAttribute "@keydown.window.escape"

xKeydownEscapeStop_ :: Text -> Attribute
xKeydownEscapeStop_ = makeAttribute "@keydown.escape.stop"

xKeydownSpacePrevent_ :: Text -> Attribute
xKeydownSpacePrevent_ = makeAttribute "@keydown.space.prevent"

xKeyupSpacePrevent_ :: Text -> Attribute
xKeyupSpacePrevent_ = makeAttribute "@keyup.space.prevent"


xKeydownEnterPrevent_ :: Text -> Attribute
xKeydownEnterPrevent_ = makeAttribute "@keydown.enter.prevent"

xBindAriaExpanded_ :: Text -> Attribute
xBindAriaExpanded_ = makeAttribute "x-bind:aria-expanded"

xKeyDownArrowUpPrevent_ :: Text -> Attribute
xKeyDownArrowUpPrevent_ = makeAttribute "@keydown.arrow-up.prevent"

xKeyDownArrowDownPrevent_ :: Text -> Attribute
xKeyDownArrowDownPrevent_ = makeAttribute "@keydown.arrow-down.prevent"

xBindAriaActiveDescendant_ :: Text -> Attribute
xBindAriaActiveDescendant_ = makeAttribute "x-bind:aria-activedescendant"

xKeyDownTab :: Text -> Attribute
xKeyDownTab = makeAttribute "@keydown.tab"

xClass_ :: Text -> Attribute
xClass_ = makeAttribute ":class"

mouseEnter_ :: Text -> Attribute
mouseEnter_ = makeAttribute "@mouseenter"

mouseLeave_ :: Text -> Attribute
mouseLeave_ = makeAttribute "@mouseleave"

mouseMove_ :: Text -> Attribute
mouseMove_ = makeAttribute "@mousemove"

{-
<template x-for="post in posts">
  <h2 x-text="post.title"></h2>
</template>
-}

{- | x-if
Conditionally add/remove a block of HTML from the page entirely.
-}
xIf_ :: Text -> Attribute
xIf_ = makeAttribute "x-if"

{-
<template x-if="open">
  <div>...</div>
</template>
-}

{- | x-init
Run code when an element is initialized by Alpine
-}
xInit_ :: Text -> Attribute
xInit_ = makeAttribute "x-init"

{-
<div x-init="date = new Date()"></div>
-}

{- | x-effect
Execute a script each time one of its dependancies change
-}
xEffect_ :: Text -> Attribute
xEffect_ = makeAttribute "x-effect"

{-
<div x-effect="console.log('Count is '+count)"></div>
-}

{- | x-ref
Reference elements directly by their specified keys using the $refs magic property
-}
xRef_ :: Text -> Attribute
xRef_ = makeAttribute "x-ref"

{-
<input type="text" x-ref="content">

<button x-on:click="navigator.clipboard.writeText($refs.content.value)">
  Copy
</button>
-}

{- | x-cloak
Hide a block of HTML until after Alpine is finished initializing its contents
-}
xCloak_ :: Attribute
xCloak_ = makeAttribute "x-cloak" mempty

{-
<div x-cloak>
  ...
</div>
-}

xStateOn_ :: Text -> Attribute
xStateOn_ = makeAttribute "x-state:on"

xStateOff_ :: Text -> Attribute
xStateOff_ = makeAttribute "x-state:off"

xStateDescription_ :: Text -> Attribute
xStateDescription_ = makeAttribute "x-state:description"

{- | x-ignore
Prevent a block of HTML from being initialized by Alpine
-}
xIgnore_ :: Attribute
xIgnore_ = makeAttribute "x-ignore" mempty

svgAttrs :: [String]
svgAttrs =
  [ "version"
  , "x"
  , "y"
  , "xmlns:xlink"
  , "xml:space"
  , "enable-background"
  ]

svgCamelCaseAttrs :: [String]
svgCamelCaseAttrs = ["viewBox"]

{-
-- | Generates code for a list of HTML attributes
--
-- >>> genSvgAttribs ["xmlns:dc", "xmlns:sodipodi"]

import Lucid.Sanitize
:{
genAttr :: [String] -> IO ()
genAttr =
  putStr . unlines . map unlines
  . map (\str -> ["-- | The @" ++ str ++ "@ attribute"
                 , sanitize str ++ " :: Text -> Attribute"
                 , sanitize str ++ " = makeAttribute " ++ show str])

genTag1 :: [String] -> IO ()
genTag1 =
  putStr . unlines . map unlines
  . map (\str -> ["-- | @" ++ str ++ "@ element"
                 , sanitize str ++ " :: Monad m => [Attribute] -> HtmlT m ()"
                 , sanitize str ++ " = with (makeElementNoEnd " ++ show str ++ ")"])

genSvgAttribs :: [String] -> IO ()
genSvgAttribs =
  putStr . unlines . map unlines
  . map (\str -> ["-- | The @" ++ str ++ "@ attribute for svg."
                 , sanitize str ++ " :: Text -> Attribute"
                 , sanitize str ++ " = makeAttribute " ++ show str])
:}

-}
