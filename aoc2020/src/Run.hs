{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Day5 ( main )

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  main