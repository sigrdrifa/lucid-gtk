{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module ConvertView where

import AppState (AppState)
import Data.Text qualified as T
import GI.Adw qualified as Adw
import GI.Gtk qualified as Gtk
import Data.Maybe (fromJust)
import Data.GI.Base (AttrOp((:=)), castTo, new)
import Lucid.Generate (lucidFromHtml, Options (Options, indentWidth_, noTrimText_, ignore_))
import Lucid.Combinators (html5S)

data ConvertView =
  ConvertView
    { title :: T.Text
    , id :: T.Text
    , appState :: AppState
    , convertViewBox :: !Gtk.Box
    , topBanner :: !Adw.Banner
    , htmlTextView :: !Gtk.TextView
    , lucidTextView :: !Gtk.TextView
    , outputTextView :: !Gtk.TextView
    , toastOverlay :: !Adw.ToastOverlay
    }

onRunConvertBtnClicked :: AppState -> ConvertView -> IO ()
onRunConvertBtnClicked appState convertView = do
  putStrLn "Convert button clicked"

  htmlBuf <- Gtk.textViewGetBuffer (htmlTextView convertView)
  textIterStart <- Gtk.textBufferGetStartIter htmlBuf
  textIterEnd <- Gtk.textBufferGetEndIter htmlBuf
  htmlText <- Gtk.textBufferGetText htmlBuf textIterStart textIterEnd False
  print htmlText

  let opts = Options {noTrimText_=True, indentWidth_=0, ignore_=True}

  let result = lucidFromHtml html5S opts "template1" (T.unpack htmlText)
  toast <- Data.GI.Base.new Adw.Toast
        [ #timeout := 2
        , #title :=
          "Successfully generated Lucid2 result code."
        ]
  Adw.toastOverlayAddToast (toastOverlay convertView) toast

  -- update output textview with result
  outputBuf <- Gtk.textViewGetBuffer (lucidTextView convertView)
  Gtk.textBufferSetText outputBuf (T.pack result) (-1)
  pure ()

initConvertView :: AppState -> Adw.ToastOverlay -> IO ConvertView
initConvertView appState overlay = do
  builder <- Gtk.builderNewFromResource "/gui/ConvertView.ui"
  convertBin <- Gtk.builderGetObject builder "encTopBox"
  bin <- fromJust <$> Data.GI.Base.castTo Gtk.Box (fromJust convertBin)
  -- build banner instance
  bannerPtr <- fromJust <$> Gtk.builderGetObject builder "banner"
  banner <- fromJust <$> Data.GI.Base.castTo Adw.Banner bannerPtr

  -- Textview instances
  htmlTextViewPtr <- fromJust <$> Gtk.builderGetObject builder "htmlTextView"
  htmlTextView <- fromJust <$> Data.GI.Base.castTo Gtk.TextView htmlTextViewPtr
  lucidTextViewPtr <- fromJust <$> Gtk.builderGetObject builder "lucidTextView"
  lucidTextView <- fromJust <$> Data.GI.Base.castTo Gtk.TextView lucidTextViewPtr
  outputTextViewPtr <- fromJust <$> Gtk.builderGetObject builder "outputTextView"
  outputTextView <- fromJust <$> Data.GI.Base.castTo Gtk.TextView outputTextViewPtr

  -- Connect signals
  runConvertBtnPtr <- fromJust <$> Gtk.builderGetObject builder "runConvertBtn"
  runConvertBtn <- fromJust <$> Data.GI.Base.castTo Gtk.Button runConvertBtnPtr

  let cv = ConvertView "Convert" "convert" appState bin banner htmlTextView lucidTextView outputTextView overlay
  Adw.after runConvertBtn #clicked $ do onRunConvertBtnClicked appState cv
  pure cv



