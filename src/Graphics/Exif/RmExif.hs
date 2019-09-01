
module Graphics.Exif.RmExif (
  rmExifDir   ,
  rmExifFile  ,
  rmExifFile' ,
  rmExifByteString
) where

import           Control.Monad
import           Data.Word
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BSL
import           System.Directory

--
--
rmExifByteString :: BSL.ByteString -> Either String BSL.ByteString
rmExifByteString org = fmap (flip rmExifByteString' org) positions
  where
    positions = runGet getAPP1Length org

rmExifFile :: FilePath -> FilePath -> IO ()
rmExifFile from to = do
  input <- BSL.readFile from
  case rmExifByteString input of
    Left  msg  -> putStrLn $ "processing " ++ from ++ "--> " ++ msg
    Right byte -> BSL.writeFile to byte

rmExifFile' :: FilePath -> IO ()
rmExifFile' from = rmExifFile from (p ++ ('~': '.' : e))
  where (p,e) = splitExt from

rmExifDir :: FilePath -> IO ()
rmExifDir path = do
  dir0 <- getDirectoryContents path
  dir1 <- return $ filter (\y -> y /= "." && y /= "..") dir0
  dir2 <- return $ map ((++) path) dir1
  mapM_ rmExifFile' dir2

check :: [Word8] -> Get Bool
check words = replicateM (length words) getWord8 >>= return . (==) words

checkJPEG :: Get Bool
checkJPEG = check [0xFF, 0xD8]

chekckAPP1 :: Get Bool
chekckAPP1 = check [0xFF, 0xE1]

checkExif :: Get Bool
checkExif = check [0x45, 0x78, 0x69, 0x66, 0x00, 0x00]

getAPP1Length :: Get (Either String (Word, Word16))
getAPP1Length = do
  jpeg <- checkJPEG
  if jpeg
    then do
      app1 <- offSetAPP1 2
      case app1 of
        Just x  -> do
          segLen <- getWord16be
          exif   <- checkExif
          if exif
            then return $ Right (x, segLen)
            else return $ Left "No Exif Idenfifier"
        Nothing -> return $ Left "No Application Segiment 1"
    else return $ Left "Not a jpeg file"

offSetAPP1 :: Word -> Get (Maybe Word)
offSetAPP1 w = do
  empty <- isEmpty
  if empty
    then return Nothing
    else do magic <- chekckAPP1
            if magic
              then return $ Just w
              else offSetAPP1 (w+2)

rmExifByteString' :: (Word, Word16) -> BSL.ByteString -> BSL.ByteString
rmExifByteString' (offset2App1, app1Len) org = BSL.append a d
  where
    (a, b) = BSL.splitAt (fromIntegral offset2App1  ) org
    (c, d) = BSL.splitAt (fromIntegral (app1Len + 2)) b

splitExt :: FilePath -> (String, String)
splitExt path = val
  where
    (val, _) = foldr (\z ((a,b),y) ->
            if y
              then if z == '.'
                      then ((a,b)  , False)
                      else ((a,z:b), True)
              else ((z:a,b),y)
          )
          (("", ""), True)
          path

