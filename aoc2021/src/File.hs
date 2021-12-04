{-# LANGUAGE NoImplicitPrelude #-}
module File where

import           Import
import           RIO.Text  as T
import           System.IO (hGetContents)

readLinesT :: FilePath -> IO [Text]
readLinesT path = withFile path ReadMode (\hdl -> do
    content <- hGetContents hdl
    return $!! T.lines . pack $ content
    )

readLinesS :: FilePath -> IO [String]
readLinesS path = withFile path ReadMode (\hdl -> do
    content <- hGetContents hdl
    return $!! Import.lines content
    )
