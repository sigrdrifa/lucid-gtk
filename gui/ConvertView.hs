{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module ConvertView where

import AppState (AppState)
import Control.Exception (SomeException (..), try)
import Data.GI.Base (AttrOp ((:=)), castTo, new)
import Data.Maybe (fromJust)
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import GI.GtkSource (styleSchemeManagerGetScheme)
import GI.GtkSource qualified as S
import Lucid.Combinators (html5S)
import Lucid.Generate (Options (Options, ignore_, indentWidth_, noTrimText_), lucidFromHtml)

data ConvertView
  = ConvertView
  { title :: T.Text
  , id :: T.Text
  , appState :: AppState
  , convertViewBox :: !Gtk.Box
  , htmlTextView :: !S.View
  , lucidTextView :: !S.View
  , toastOverlay :: !Adw.ToastOverlay
  , convertBtn :: !Gtk.Button
  }

onRunConvertBtnClicked :: AppState -> ConvertView -> IO ()
onRunConvertBtnClicked _ convertView = do
  htmlBuf <- Gtk.textViewGetBuffer (htmlTextView convertView)
  textIterStart <- Gtk.textBufferGetStartIter htmlBuf
  textIterEnd <- Gtk.textBufferGetEndIter htmlBuf
  htmlText <- Gtk.textBufferGetText htmlBuf textIterStart textIterEnd False
  let opts = Options{noTrimText_ = True, indentWidth_ = 2, ignore_ = True}

  result <- try $ lucidFromHtml html5S opts "template1" (T.unpack htmlText)
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "Error: " ++ show e
      toast <-
        Data.GI.Base.new
          Adw.Toast
          [ #timeout := 2
          , #title
              := "Error generating Lucid2 result code."
          ]
      Adw.toastOverlayAddToast (toastOverlay convertView) toast
      pure ()
    Right code -> do
      putStrLn "Success"
      toast <-
        Data.GI.Base.new
          Adw.Toast
          [ #timeout := 2
          , #title
              := "Successfully generated Lucid2 result code."
          ]
      Adw.toastOverlayAddToast (toastOverlay convertView) toast

      -- update output textview with result
      outputBuf <- Gtk.textViewGetBuffer (lucidTextView convertView)
      Gtk.textBufferSetText outputBuf (T.pack code) (-1)
      pure ()

initConvertView :: AppState -> Gtk.Button -> Adw.ToastOverlay -> IO ConvertView
initConvertView appState convertBtn overlay = do
  builder <- Gtk.builderNewFromResource "/gui/ConvertView.ui"
  convertBin <- Gtk.builderGetObject builder "encTopBox"
  bin <- fromJust <$> Data.GI.Base.castTo Gtk.Box (fromJust convertBin)

  -- Sourceview instance boxes
  htmlTextBoxPtr <- fromJust <$> Gtk.builderGetObject builder "htmlTextBox"
  htmlTextBox <- fromJust <$> Data.GI.Base.castTo Gtk.Box htmlTextBoxPtr
  lucidTextBoxPtr <- fromJust <$> Gtk.builderGetObject builder "hsTextBox"
  lucidTextBox <- fromJust <$> Data.GI.Base.castTo Gtk.Box lucidTextBoxPtr

  lm <- new S.LanguageManager []
  sm <- new S.StyleSchemeManager []
  S.styleSchemeManagerAppendSearchPath sm "~/.local/share/gtksourceview-5.0/styles"
  S.styleSchemeManagerForceRescan sm
  scheme <- styleSchemeManagerGetScheme sm "dracula"
  maybeLang <- S.languageManagerGetLanguage lm "haskell"
  srcBuffer <- case maybeLang of
    Just lang -> new S.Buffer [#text := "", #language := lang]
    Nothing -> new S.Buffer [#text := ""]
  maybeLangHtml <- S.languageManagerGetLanguage lm "html"
  htmlBuffer <- case maybeLangHtml of
    Just lang -> new S.Buffer [#text := "", #language := lang]
    Nothing -> new S.Buffer [#text := ""]
  S.bufferSetStyleScheme srcBuffer scheme
  S.bufferSetStyleScheme htmlBuffer scheme
  hsView <-
    new
      S.View
      [ #buffer := srcBuffer
      , #backgroundPattern := S.BackgroundPatternTypeNone
      , #showLineNumbers := True
      , #showLineMarks := True
      , #highlightCurrentLine := True
      , #autoIndent := True
      , #indentOnTab := True
      ]
  htmlView <-
    new
      S.View
      [ #buffer := htmlBuffer
      , #backgroundPattern := S.BackgroundPatternTypeNone
      , #showLineNumbers := True
      , #showLineMarks := True
      , #highlightCurrentLine := True
      ]

  Gtk.boxAppend htmlTextBox htmlView
  Gtk.boxAppend lucidTextBox hsView

  -- Connect signals
  -- runConvertBtnPtr <- fromJust <$> Gtk.builderGetObject builder "runConvertBtn"
  -- runConvertBtn <- fromJust <$> Data.GI.Base.castTo Gtk.Button runConvertBtnPtr

  let cv = ConvertView "Convert" "convert" appState bin htmlView hsView overlay convertBtn
  Adw.after convertBtn #clicked $ do onRunConvertBtnClicked appState cv
  pure cv
