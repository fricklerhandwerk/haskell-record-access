module Main where

import MyType (MyType(..))

main :: IO ()
main = do
    let myValue = MyType { foo = 42 }
    putStrLn $ "myValue = " ++ show (MyType.foo myValue)
