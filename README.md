data-ttn
========

Parse JSONs from TTN MQTT API.

See https://www.thethingsnetwork.org/docs/applications/mqtt/api.html

Usage
-----

```haskell
import Data.TTN
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  j <- B.readFile "samples/0"
  case parse j of
    Left err -> putStrLn err
    Right t -> print t
```
