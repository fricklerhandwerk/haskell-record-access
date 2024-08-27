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
    putStrLn $ "foo = " ++ show foo.foo
    putStrLn $ "bar = " ++ show bar.foo
