{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Day3

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  Day3.main
