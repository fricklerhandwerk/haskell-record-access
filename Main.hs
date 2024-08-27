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
