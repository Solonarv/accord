{-# LANGUAGE LambdaCase #-}
module Main where

import           Graphics.UI.Gtk

import           Accord.Framework

main :: IO ()
main = do
  args <- initGUI
  env <- initAccordEnv args
  window <- windowNew
  widgetShowAll window
  mainGUI
