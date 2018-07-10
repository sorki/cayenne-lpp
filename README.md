cayene-lpp
==========

Encode and decode Cayene Low Power Protocol
(https://github.com/myDevicesIoT/cayenne-docs/blob/master/docs/LORA.md#cayenne-low-power-payload)

Usage
-----

```haskell
import qualified Data.Cayene as C

main :: IO ()
main = do
  let x = (0, AnalogIn 4.48)
  print $ C.decode $ C.encode x
```
