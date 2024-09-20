{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | A module for conversion from HTML to Lucid Haskell code.
--

module Lucid.Generate (
    module Lucid.Generate
)   where

import "base" Data.List (stripPrefix, intercalate)
import "base" Data.Maybe (listToMaybe)
import "base" Data.Char (isSpace, showLitChar)
import "base" Control.Arrow (first)

import "tagsoup" Text.HTML.TagSoup

import Lucid.Sanitize (sanitize, lowerize)
import Lucid.Combinators

-- | Simple type to represent attributes.
--
type Attributes = [(String, String)]

-- | Intermediate tree representation. This representation contains several
-- constructors aimed at pretty-printing.
--
data Html = Parent String Attributes Html
          | Block [Html]
          | Text String
          | Comment String
          | Doctype
          deriving (Show)

-- | Different combinator types.
--
data CombinatorType = ParentCombinator
                    | LeafCombinator
                    | UnknownCombinator
                    deriving (Eq, Show)

-- | Traverse the list of tags to produce an intermediate representation of the
-- HTML tree.
--
makeTree :: HtmlVariant           -- ^ HTML variant used
         -> Bool                  -- ^ Should ignore errors
         -> [String]              -- ^ Stack of open tags
         -> [Tag String]          -- ^ Tags to parse
         -> (Html, [Tag String])  -- ^ (Result, unparsed part)
makeTree _ ignore stack []
    | null stack || ignore = (Block [], [])
    | otherwise = error $ "Error: tags left open at the end: " ++ show stack
makeTree variant ignore stack (TagPosition row _ : x : xs) = case x of
    TagOpen tag attrs -> if lowerize tag == "!doctype"
        then addHtml Doctype xs
        else let tag' = lowerize tag
                 (inner, t) = case combinatorType variant tag' of
                    LeafCombinator -> (Block [], xs)
                    _ -> makeTree variant ignore (tag' : stack) xs
                 p = Parent tag' (map (first lowerize) attrs) inner
             in addHtml p t
    -- The closing tag must match the stack. If it is a closing leaf, we can
    -- ignore it
    TagClose tag ->
        let tag' = lowerize tag
            isLeafCombinator = combinatorType variant tag' == LeafCombinator
            matchesStack = listToMaybe stack == Just tag'
        in case (isLeafCombinator, matchesStack, ignore) of
            -- It's a leaf combinator, don't care about this element
            (True, _, _)          -> makeTree variant ignore stack xs
            -- It's a parent and the stack doesn't match
            (False, False, False) -> error $
                "Line " ++ show row ++ ": " ++ show tag
                        ++ " (" ++ show tag' ++ ")"
                        ++ " closed but "
                        ++ show stack ++ " should be closed instead."
            -- Stack might not match but we ignore it anyway
            (False, _, _)         -> (Block [], xs)
    TagText text -> addHtml (Text text) xs
    TagComment comment -> addHtml (Comment comment) xs
    _ -> makeTree variant ignore stack xs
  where
    addHtml html xs' = let (Block l, r) = makeTree variant ignore stack xs'
                       in (Block (html : l), r)

makeTree _ _ _ _ = error "TagSoup error"

-- | Remove empty text from the HTML.
--
removeEmptyText :: Html -> Html
removeEmptyText (Block b) = Block $ map removeEmptyText $ flip filter b $ \h ->
    case h of Text text -> not (all isSpace text)
              _         -> True
removeEmptyText (Parent tag attrs inner) =
    Parent tag attrs $ removeEmptyText inner
removeEmptyText x = x

-- | Try to eliminiate Block constructors as much as possible.
--
minimizeBlocks :: Html -> Html
minimizeBlocks (Parent t a (Block [x])) = minimizeBlocks $ Parent t a x
minimizeBlocks (Parent t a x) = Parent t a $ minimizeBlocks x
minimizeBlocks (Block x) = Block $ map minimizeBlocks x
minimizeBlocks x = x

-- | Get the type of a combinator
--
combinatorType :: HtmlVariant -> String -> CombinatorType
combinatorType variant combinator
    | combinator == "docTypeHtml" = ParentCombinator
    | combinator `elem` parents variant = ParentCombinator
    | combinator `elem` leafs variant = LeafCombinator
    | otherwise = UnknownCombinator

-- | Produce the Lucid code from the HTML. The result is a list of lines.
--
fromHtml :: HtmlVariant  -- ^ Used HTML variant
         -> Options      -- ^ Building options
         -> Html         -- ^ HTML tree
         -> [String]     -- ^ Resulting lines of code
fromHtml _ _ Doctype = ["doctype_"]
fromHtml _ opts t
  | (Text text) <- t
    = ["\"" ++ foldr escape "" (trim text) ++ "\""]
  -- preserve comment as is
  | (Comment comment) <- t
    = ["toHtmlRaw  \"<!--" ++ foldr escape "" comment ++ "-->\""]
  where
    -- Remove whitespace on both ends of a string
    trim
      | noTrimText_ opts = id
      | otherwise        = reverse . dropWhile isSpace . reverse . dropWhile isSpace
    -- Escape a number of characters
    escape '"'  = showString "\\\""
    escape x = showLitChar x
    {-
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape '\r' = "\\r"
    escape '\\' = "\\\\"
    escape x    = [x]
    --}
-- fromHtml _ (Comment comment) = map ("-- " ++) $ lines comment
fromHtml variant opts (Block block) =
    concatMap (fromHtml variant opts) block
fromHtml variant opts (Parent tag attrs inner) =
    case combinatorType variant tag of
        -- Actual parent tags
        ParentCombinator -> case inner of
            (Block ls) -> if null ls
                then [combinator ++
                        (if null attrs then " " else " $ ") ++ "\"\""]
                else (combinator ++ " $ do") :
                        indent opts (fromHtml variant opts inner)
            -- We join non-block parents for better readability.
            x -> let ls = fromHtml variant opts x
                     apply = if dropApply x then " " else " $ "
                 in case ls of (y : ys) -> (combinator ++ apply ++ y) : ys
                               [] -> [combinator]

        -- Leaf tags
        LeafCombinator -> [combinator]

        -- Unknown tag
        UnknownCombinator -> if ignore_ opts
            then fromHtml variant opts inner
            else error $ "Tag '" ++ tag ++ "' is illegal in "
                         ++ show variant
  where
    combinator :: String
    combinator = sanitize tag ++ attributes' attrs
    attributes' :: Show a => [(String, a)] -> String
    -- hack for <br> that needs attributes in Lucid
    attributes' [] = if sanitize tag `elem` ["br_","hr_"]
                       then " []"
                       else ""
    attributes' xs =  (" [ " ++) . (++ " ]") . intercalate ", " . fmap displayAttr $ xs
    displayAttr :: Show a => (String, a) -> String
    displayAttr (k, v) = if k `elem` attributes variant then (let k' = sanitize k
                                                              in case k' of
                                                                     "autofocus_" -> k'
                                                                     "checked_" -> k'
                                                                     "ngNonBindable_" -> k'
                                                                     _ -> k' ++ " " ++ show v) else (case stripPrefix "data-" k of
        Just prefix -> "data_" ++ " "
                    ++ show prefix
                    ++ " " ++ show v
        Nothing | ignore_ opts -> ""
                | otherwise  -> error $ "Attribute '"
                             ++ k ++ "' is illegal in "
                             ++ show variant)

    -- Check if we can drop the apply operator ($), for readability reasons.
    -- This would change:
    --
    -- > p $ "Some text"
    --
    -- Into
    --
    -- > p "Some text"
    --
    dropApply (Parent {}) = False
    dropApply (Block _) = False
    dropApply _ = null attrs

-- | Produce the code needed for initial imports.
--
getImports :: [String]
getImports =
    [ "{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}"
    , ""
    , "import Lucid"
    , "import Lucid.Supplemental"
    ]


-- | Produce the code for IO
--
getIOImports :: [String]
getIOImports =
    [ "import System.IO (stdout, hSetEncoding, utf8)"
    , "import Data.Text.Lazy.IO as L"
    , ""
    , "main :: IO ()"
    , "main = do"
    , "  hSetEncoding stdout utf8"
    , "  L.hPutStr stdout (renderText template1)"
    , ""
    ]


-- | Convert the HTML to lucid code.
--
lucidFromHtml :: HtmlVariant  -- ^ Variant to use
              -> Options      -- ^ Build options
              -> String       -- ^ Template name
              -> String       -- ^ HTML code
              -> IO String       -- ^ Resulting code
lucidFromHtml variant opts name code = do
    let res = unlines . addSignature . fromHtml variant opts
            . minimizeBlocks
            . removeEmptyText   -- causes glueing of words, see bug #13 
            . fst . makeTree variant (ignore_ opts) []
            -- . canonicalizeTags
            . parseTagsOptions parseOptions { optTagPosition = True}
    pure (res code)
  where
    addSignature body = [ name ++ " :: Html ()"
                        , name ++ " = do"
                        ] ++ indent opts body



lucidFromHtml' :: HtmlVariant  -- ^ Variant to use
              -> Options      -- ^ Build options
              -> String       -- ^ Template name
              -> String       -- ^ HTML code
              -> String       -- ^ Resulting code
lucidFromHtml' variant opts name =
          unlines . addSignature . fromHtml variant opts
            . minimizeBlocks
            . removeEmptyText   -- causes glueing of words, see bug #13 
            . fst . makeTree variant (ignore_ opts) []
            -- . canonicalizeTags
            . parseTagsOptions parseOptions { optTagPosition = True}
  where
    addSignature body = [ name ++ " :: Html ()"
                        , name ++ " = do"
                        ] ++ indent opts body
    -- popts :: ParseOptions String
    -- popts = (parseOptionsEntities (const Nothing)){ optTagPosition = True }
          -- (parseOptions :: ParseOptions String){ optTagPosition = True ,
                          -- optEntityData = \(str,_) -> [TagText $ "&" ++ str ++ [';' | b]],
                          -- optEntityAttrib = \(str,_) -> ("&" ++ str ++ [';' | b], []) }

-- | Indent block of code.
--
indent :: Options -> [String] -> [String]
indent opts = map (replicate (indentWidth_ opts) ' ' ++)

-- | The options record passed to 'lucidFromHtml'
--
data Options = Options
             { ignore_      :: Bool -- ^ ignore errors
             , noTrimText_  :: Bool -- ^ do not trim text
             , indentWidth_ :: Int  -- ^ how many spaces to indent with
             }
  deriving (Show)

