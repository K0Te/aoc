{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Day1

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  Day1.main
