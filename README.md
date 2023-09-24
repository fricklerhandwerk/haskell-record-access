I wondered if it's possible to access Haskell record fields with a dot notation.
Turns out it is.

My initial idea was to namespace the accessor functions by separating types out into modules and importing them qualified.
That actually worked elegantly:

```haskell
module Main where

import MyType (MyType(..))

main :: IO ()
main = do
    let myValue = MyType { foo = 42 }
    putStrLn $ "myValue = " ++ show (MyType.foo myValue)
```
