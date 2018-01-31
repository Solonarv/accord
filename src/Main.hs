{-# LANGUAGE LambdaCase #-}
module Main where

import Framework

main :: IO ()
main = do
  cfg <- retrieveAccordConfig
  flip runReaderT cfg $ do
    st <- initAccordState
    flip evalStateT st $ mainLoop

mainLoop :: AccordStack ()
mainLoop = liftIO $ putStrLn "entering main loop...\naaand it's done"