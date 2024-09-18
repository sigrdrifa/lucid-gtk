{-# LANGUAGE PackageImports #-}

module Main (
    main
)   where

import "base" Control.Monad (forM_, when)
import "base" Control.Applicative ((<$>))
import "base" System.Environment (getArgs)
-- import "filepath" System.FilePath (takeFileName)
import "base" System.Console.GetOpt
import "base" System.Exit
import "base" System.IO

import Lucid.Generate
import Lucid.Combinators

-- | Main function
--
main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    args <- getOpt Permute options <$> getArgs
    let (o, n, errs) = args
    case () of
      _ | elem ArgHelp o  -> putStr help
        | not (null errs) -> do hPutStr stderr (concat errs)
                                hPutStrLn stderr "use -h for usage help"
                                exitFailure
        | otherwise       -> let i = ignore' o
                                 t = trim' o
                                 w = indentWidth' o
                                 opts = Options i t w
                             in do imports' o
                                   main' opts n
  where
    -- No files given, work with stdin
    main' opts [] = interact $
        lucidFromHtml html5S opts "template1"

    -- Handle all files
    main' opts files = forM_ (zip files [1 .. (length files)]) $ \(file, num) -> 
            withFile file ReadMode (\handle -> do
              hSetEncoding handle utf8
              body <- hGetContents handle
              putStrLn $ "-- Template for file: " ++ file
              putStrLn $ lucidFromHtml html5S opts
                                      ("template" ++ (show num)) body
            )

    -- Print imports if needed
    imports' opts = when (standalone' opts) $
        putStrLn $ unlines $ getImports 
                              ++ getIOImports

    -- Should we produce standalone code?
    standalone' opts = ArgStandalone `elem` opts

    -- Should we ignore errors?
    ignore' opts = ArgIgnoreErrors `elem` opts

    -- Should we trim whitespace from text?
    trim' opts = ArgNoTrimText `elem` opts

    indentWidth' opts = case opts of
      [] -> 2
      x:xs -> case x of
        ArgIndentWidth w -> w
        _ -> indentWidth' xs


-- | Help information.
--
help :: String
help = unlines $
    [ "This is a tool to convert HTML5 code to LucidHtml code. It is still"
    , "experimental and the results might need to be edited manually."
    , ""
    , "USAGE"
    , ""
    , "  lucid-from-html [OPTIONS...] [FILES ...]"
    , ""
    , "When no files are given, it works as a filter."
    , ""
    , "EXAMPLE"
    , ""
    , "  lucid-from-html -s index.html"
    , ""
    , "This converts the index.html file to Haskell code, writing to stdout."
    , ""
    , "OPTIONS"
    , usageInfo "" options
    , ""
    , "Note: Tag <tt> in input file is allowed and will be converted to <code>."
    ]

-- | Options for the CLI program
--
data Arg = ArgStandalone
         | ArgIgnoreErrors
         | ArgNoTrimText
         | ArgHelp
         | ArgIndentWidth Int
         deriving (Show, Eq, Read)

-- | A description of the options
--
options :: [OptDescr Arg]
options =
    [ Option "s" ["standalone"] (NoArg ArgStandalone) "Produce standalone code"
    , Option "e" ["ignore-errors"] (NoArg ArgIgnoreErrors) "Ignore most errors"
    , Option "t" ["no-trim-text"]  (NoArg ArgNoTrimText) "Do not trim text"
    , Option "w" ["indent-width"]  (ReqArg (ArgIndentWidth . read) "WIDTH") "Number of spaces to use for indentation"
    , Option "h" ["help"] (NoArg ArgHelp) "Show help"
    ]
