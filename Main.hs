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
