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
