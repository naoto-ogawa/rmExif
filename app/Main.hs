module Main where

import System.Environment

import Graphics.Exif.RmExif

main :: IO ()
main = do
  args <- getArgs
  if null args
    then mapM_ putStrLn help
    else
      case (args !! 0) of
        "-f" ->
          case length args of
            2 -> rmExifFile' (args !! 1)
            3 -> rmExifFile  (args !! 1) (args !! 2)
            _ -> putStrLn "invalid path"
        "-d" ->
          case length args of
            2 -> rmExifDir (args !! 1)
            _ -> putStrLn "invalid path"
        _    -> putStrLn $ "invalid optoin"

help :: [String]
help = [
    "Usage: rmExif-exe [option]"
  , "  options:"
  , "  -f input_file_path output_file_path  specify both a input and an output file paths."
  , "  -f input_file_path                   append '~' to an input file name for an output file name."
  , "  -d input_dir_path                    process jpeg files on the specified directory."
  ]
