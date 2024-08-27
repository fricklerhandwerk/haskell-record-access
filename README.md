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

However, one can't have another type that also has a field `foo`.
This can be alleviated with the `DuplicateRecordFields` language extension:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import MyType (MyType(..))
import AnotherType (AnotherType(..))

main :: IO ()
main = do
    let myValue = MyType { foo = 42 }
    let anotherValue = AnotherType { foo = "bar" }
    putStrLn $ "myValue = " ++ show (MyType.foo myValue)
    putStrLn $ "anotherValue = " ++ show (AnotherType.foo anotherValue)
```

One still can't use field names as variable names, although they're clearly distinct semantically.
This can be helped with `NoFieldSelectors`:

```haskell
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where
import MyType (MyType(..))
import AnotherType (AnotherType(..))

main :: IO ()
main = do
    let foo = MyType { foo = 42 }
    let bar = AnotherType { foo = "bar" }
    putStrLn $ "foo = " ++ show (MyType.foo foo)
    putStrLn $ "bar = " ++ show (AnotherType.foo bar)
```

Finally, since we're delving into language extensions, why not just allow record access with a dot syntax directly with `OverloadedRecordDot`?

```haskell
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where
import MyType (MyType(..))
import AnotherType (AnotherType(..))

main :: IO ()
main = do
    let foo = MyType { foo = 42 }
    let bar = AnotherType { foo = "bar" }
    putStrLn $ "foo = " ++ show (foo.foo)
    putStrLn $ "bar = " ++ show (bar.foo)
```

This means there's no need to artificially move around type definitions:

```haskell
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

data MyType = MyType
    { foo :: Int
    } deriving (Show)

data AnotherType = AnotherType
    { foo :: String
    } deriving (Show)

main :: IO ()
main = do
    let foo = MyType { foo = 42 }
    let bar = AnotherType { foo = "bar" }
    putStrLn $ "foo = " ++ show (foo.foo)
    putStrLn $ "bar = " ++ show (bar.foo)
```
