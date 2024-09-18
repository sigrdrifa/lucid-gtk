{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

#define DO_NOT_EDIT (doNotEdit __FILE__ __LINE__)

-- | Generates code for HTML tags.
--
module Lucid.Combinators (
    module Lucid.Combinators
)   where

import "base" Data.List (sortBy, intersperse, intercalate)
import "base" Data.Ord (comparing)
import "directory" System.Directory (createDirectoryIfMissing)
import "filepath" System.FilePath ((</>), (<.>))
import "base" Data.Char (toLower)

import Lucid.Sanitize (sanitize)
import Lucid.Supplemental as S

-- | Datatype for an HTML variant.
--
data HtmlVariant = HtmlVariant
    { version     :: [String]
    , docType     :: [String]
    , parents     :: [String]
    , leafs       :: [String]
    , attributes  :: [String]
    , selfClosing :: Bool
    } deriving (Eq)

instance Show HtmlVariant where
    show = map toLower . intercalate "-" . version

-- | Get the full module name for an HTML variant.
--
getModuleName :: HtmlVariant -> String
getModuleName = ("Lucid." ++) . intercalate "." . version

-- | Write an HTML variant.
--
writeHtmlVariant :: HtmlVariant -> IO ()
writeHtmlVariant htmlVariant = do
    -- Make a directory.
    createDirectoryIfMissing True basePath

    let tags =  zip parents' (repeat makeParent)
             ++ zip leafs' (repeat (makeLeaf $ selfClosing htmlVariant))
        sortedTags = sortBy (comparing fst) tags
        appliedTags = map (\(x, f) -> f x) sortedTags

    -- Write the main module.
    writeFile' (basePath <.> "hs") $ removeTrailingNewlines $ unlines
        [ DO_NOT_EDIT
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "-- | This module exports HTML combinators used to create documents."
        , "--"
        , exportList modulName $ "module Lucid"
                               : "docType"
                               : "docTypeHtml"
                               : map (sanitize . fst) sortedTags
        , DO_NOT_EDIT
        , "import Lucid"
        , ""
        , makeDocType $ docType htmlVariant
        , makeDocTypeHtml $ docType htmlVariant
        , unlines appliedTags
        ]

  where
    basePath  = "src" </> "Lucid" </> foldl1 (</>) version'
    modulName = getModuleName htmlVariant
    parents'    = parents htmlVariant
    leafs'      = leafs htmlVariant
    version'    = version htmlVariant
    removeTrailingNewlines = reverse . drop 2 . reverse
    writeFile' file content = do
        putStrLn ("Generating " ++ file)
        writeFile file content

-- | Create a string, consisting of @x@ spaces, where @x@ is the length of the
-- argument.
--
spaces :: String -> String
spaces = flip replicate ' ' . length

-- | Join blocks of code with a newline in between.
--
unblocks :: [String] -> String
unblocks = unlines . intersperse "\n"

-- | A warning to not edit the generated code.
--
doNotEdit :: FilePath -> Int -> String
doNotEdit fileName lineNumber = init $ unlines
    [ "-- WARNING: The next block of code was automatically generated by"
    , "-- " ++ fileName ++ ":" ++ show lineNumber
    , "--"
    ]

-- | Generate an export list for a Haskell module.
--
exportList :: String   -- ^ Module name.
           -> [String] -- ^ List of functions.
           -> String   -- ^ Resulting string.
exportList _    []            = error "exportList without functions."
exportList name (f:functions) = unlines $
    [ "module " ++ name
    , "    ( " ++ f
    ] ++
    map ("    , " ++) functions ++
    [ "    ) where"]

-- | Generate a function for a doctype.
--
makeDocType :: [String] -> String
makeDocType lines' = unlines
    [ DO_NOT_EDIT
    , "-- | Combinator for the document type. This should be placed at the top"
    , "-- of every HTML page."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > docType"
    , "--"
    , "-- Result:"
    , "--"
    , unlines (map ("-- > " ++) lines') ++ "--"
    , "docType_ :: Html ()  -- ^ The document type HTML."
    , "docType_ = preEscapedText " ++ show (unlines lines')
    , "{-# INLINE docType_ #-}"
    ]

-- | Generate a function for the HTML tag (including the doctype).
--
makeDocTypeHtml :: [String]  -- ^ The doctype.
                -> String    -- ^ Resulting combinator function.
makeDocTypeHtml lines' = unlines
    [ DO_NOT_EDIT
    , "-- | Combinator for the @\\<html>@ element. This combinator will also"
    , "-- insert the correct doctype."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > docTypeHtml_ $ span_ $ toHtml \"foo\""
    , "--"
    , "-- Result:"
    , "--"
    , unlines (map ("-- > " ++) lines') ++ "-- > <html><span>foo</span></html>"
    , "--"
    , "docTypeHtml_ :: Html ()  -- ^ Inner HTML."
    , "             -> Html  -- ^ Resulting HTML."
    , "docTypeHtml_ inner = docType_ >> html_ inner"
    , "{-# INLINE docTypeHtml_ #-}"
    ]

-- | Generate a function for an HTML tag that can be a parent.
--
makeParent :: String -> String
makeParent tag = unlines
    [ DO_NOT_EDIT
    , "-- | Combinator for the @\\<" ++ tag ++ ">@ element."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > " ++ function ++ " $ span_ $ toHtml \"foo\""
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <" ++ tag ++ "><span>foo</span></" ++ tag ++ ">"
    , "--"
    , function        ++ " :: Html  -- ^ Inner HTML."
    , spaces function ++ " -> Html  -- ^ Resulting HTML."
    , function        ++ " = Parent \"" ++ tag ++ "\" \"<" ++ tag
                      ++ "\" \"</" ++ tag ++ ">\"" ++ modifier
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag
    modifier = if tag `elem` ["style", "script"] then " . external" else ""

-- | Generate a function for an HTML tag that must be a leaf.
--
makeLeaf :: Bool    -- ^ Make leaf tags self-closing
         -> String  -- ^ Tag for the combinator
         -> String  -- ^ Combinator code
makeLeaf closing tag = unlines
    [ DO_NOT_EDIT
    , "-- | Combinator for the @\\<" ++ tag ++ " />@ element."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > " ++ function
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <" ++ tag ++ " />"
    , "--"
    , function ++ " :: Html  -- ^ Resulting HTML."
    , function ++ " = Leaf \"" ++ tag ++ "\" \"<" ++ tag ++ "\" " ++ "\""
               ++ (if closing then " /" else "") ++ ">\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize tag

-- | Generate a function for an HTML attribute.
--
makeAttribute :: String -> String
makeAttribute name = unlines
    [ DO_NOT_EDIT
    , "-- | Combinator for the @" ++ name ++ "@ attribute."
    , "--"
    , "-- Example:"
    , "--"
    , "-- > div ! " ++ function ++ " \"bar\" $ \"Hello.\""
    , "--"
    , "-- Result:"
    , "--"
    , "-- > <div " ++ name ++ "=\"bar\">Hello.</div>"
    , "--"
    , function        ++ " :: AttributeValue  -- ^ Attribute value."
    , spaces function ++ " -> Attribute       -- ^ Resulting attribute."
    , function        ++ " = attribute \"" ++ name ++ "\" \" "
                      ++ name ++ "=\\\"\""
    , "{-# INLINE " ++ function ++ " #-}"
    ]
  where
    function = sanitize name


-- | HTML 5.0
-- A good reference can be found here:
-- http://www.w3schools.com/html5/html5_reference.asp
--
html5 :: HtmlVariant
html5 = HtmlVariant
    { version = ["Html5"]
    , docType = ["<!DOCTYPE HTML>"]
    , parents =
        [ "a", "abbr", "address", "article", "aside", "audio", "b"
        , "bdo", "blockquote", "body", "button", "canvas", "caption", "cite"
        , "code", "colgroup", "command", "datalist", "dd", "del", "details"
        , "dfn", "div", "dl", "dt", "em", "fieldset", "figcaption", "figure"
        , "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header"
        , "hgroup", "html", "i", "iframe", "ins", "kbd", "label"
        , "legend", "li", "main", "map", "mark", "menu", "meter", "nav"
        , "noscript", "object", "ol", "optgroup", "option", "output", "p"
        , "pre", "path", "progress", "q", "rp", "rt", "ruby", "samp", "script"
        , "section", "select", "small", "span", "strong", "style", "sub"
        , "summary", "sup", "table", "tbody", "td", "textarea", "tfoot", "th"
        , "thead", "time", "title", "tr", "ul", "var", "video"
        ]
    , leafs =
        -- http://www.whatwg.org/specs/web-apps/current-work/multipage/syntax.html#void-elements
        [ "area", "base", "br", "col", "embed", "hr", "img", "input", "keygen"
        , "link", "menuitem", "meta", "param", "source", "track", "wbr"
        ]
    , attributes =
        [ "accept", "accept-charset", "accesskey", "aria-modal", "action", "alt", "async"
        , "autocomplete", "autofocus", "autoplay", "challenge", "charset"
        , "checked", "cite", "class", "cols", "colspan", "content"
        , "contenteditable", "contextmenu", "controls", "coords", "crossorigin", "d", "data"
        , "datetime", "defer", "dir", "disabled", "draggable", "enctype", "for", "fill"
        , "form", "formaction", "formenctype", "formmethod", "formnovalidate"
        , "formtarget", "headers", "height", "hidden", "high", "href"
        , "hreflang", "http-equiv", "icon", "id", "ismap", "item", "itemprop"
        , "keytype", "label", "lang", "list", "loop", "low", "manifest", "max"
        , "maxlength", "media", "method", "min", "multiple", "name"
        , "novalidate", "onbeforeonload", "onbeforeprint", "onblur", "oncanplay"
        , "oncanplaythrough", "onchange", "oncontextmenu", "onclick"
        , "ondblclick", "ondrag", "ondragend", "ondragenter", "ondragleave"
        , "ondragover", "ondragstart", "ondrop", "ondurationchange", "onemptied"
        , "onended", "onerror", "onfocus", "onformchange", "onforminput"
        , "onhaschange", "oninput", "oninvalid", "onkeydown", "onkeyup"
        , "onload", "onloadeddata", "onloadedmetadata", "onloadstart"
        , "onmessage", "onmousedown", "onmousemove", "onmouseout", "onmouseover"
        , "onmouseup", "onmousewheel", "ononline", "onpagehide", "onpageshow"
        , "onpause", "onplay", "onplaying", "onprogress", "onpropstate"
        , "onratechange", "onreadystatechange", "onredo", "onresize", "onscroll"
        , "onseeked", "onseeking", "onselect", "onstalled", "onstorage"
        , "onsubmit", "onsuspend", "ontimeupdate", "onundo", "onunload"
        , "onvolumechange", "onwaiting", "open", "optimum", "pattern", "ping"
        , "placeholder", "preload", "path", "pubdate", "radiogroup", "readonly"
        , "rel", "required", "reversed", "rows", "rowspan", "sandbox", "scope"
        , "role"
        , "scoped", "seamless", "selected", "shape", "stroke", "stroke-width",
        "stroke-linecap", "stroke-linejoin",
        "size", "sizes", "span"
        , "spellcheck", "src", "srcdoc", "start", "step", "style", "subject"
        , "summary", "tabindex", "target", "title", "type", "usemap", "value"
        , "width", "wrap", "xmlns"
        ]
    , selfClosing = False
    }


html5S :: HtmlVariant
html5S = html5 { version = ["html5","with","supplements"]
               , parents = parents html5 ++ S.parentElements
               , leafs = leafs html5 ++ S.leafElements
               , attributes = attributes html5 ++ S.attributeElements
               }

{- experiment for replacing tags by Lucid functions

html5SSan :: HtmlVariant  
html5SSan = html5 { parents = map sanitize (parents html5S)
                  , leafs = map sanitize (leafs html5S)
                  , attributes = map sanitize (attributes html5S)
                  }
-}
