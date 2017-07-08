module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar i = map (\x -> chr $ ord x + i)

unCaesar :: Int -> String -> String
unCaesar i = caesar (-i)

main :: IO ()
main =
    print $ "unciphered cipher equals original string? "
        ++ show ("hello" == unCaesar 3 (caesar 3 "hello"))