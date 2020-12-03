{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Day2

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  Day2.main
