{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module ParseSpec where

import SpecHelper

import qualified Data.ByteString.Base16.Lazy as B16L

testCases = [
    (0, DigitalIn 128)
  , (7, DigitalOut 255)
  , (0, AnalogIn 4.48)
  , (3, AnalogIn (-4.48))
  , (0, Illum 1337)
  , (1, Presence 14)
  , (2, Temperature (-15.6))
  , (3, Temperature 55.6)
  , (4, Humidity 55.5)
  , (0, Accelerometer 8.48 1.666 32)
  , (0, Accelerometer (8.484) (-1.666) (-1.132))
  , (4, Barometer 55.5)
  , (4, Voltage 230.7)
  , (4, Current 1337.7)
  , (4, Percentage 97)
  , (4, Percentage (-97))
  , (4, Pressure (-97.7))
  , (4, Pressure 99.9)
  , (4, Power 95.9)
  , (4, Energy (-95.9))
  , (4, Direction 100)
  , (0, GPS 8.48 1.37 100)
  , (0, GPS (-48.48) (-75.1337) 100)
  , (0, GPS 42.3519 (-87.9094) (-10))
  ]

spec :: Spec
spec = do
  it "parses samples" $ do
    mapM_ (\x -> (decode . encode) x `shouldBe` x) testCases
    (decode . fst $ B16L.decode "018806765ff2960a0003e8") `shouldBe` (1, GPS 42.3519 (-87.9094) 10.0)
    (decode . fst $ B16L.decode "067104D2FB2E0000")       `shouldBe` (6, Accelerometer 1.234 (-1.234) 0.0)
    (decode . fst $ B16L.decode "0167FFD7")               `shouldBe` (1, Temperature (-4.1))
    (decode . fst $ B16L.decode "03670110056700FF")       `shouldBe` (3, Temperature 27.2)

    (decodeMany . fst $ B16L.decode "018806765ff2960a0003e8067104D2FB2E00000167FFD703670110056700FF")
      `shouldBe`
      [(1, GPS 42.3519 (-87.9094) 10.0), (6, Accelerometer 1.234 (-1.234) 0.0), (1,Temperature (-4.1)), (3,Temperature 27.2), (5,Temperature 25.5)]

main :: IO ()
main = hspec spec
