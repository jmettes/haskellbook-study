-- arith4.hs
module Arith4 where
-- id :: a -> a
-- id x = x

roundTrip :: (Show a, Read a) => a -> a
roundTrip x = read . show x

main = do
    print (roundTrip 4 :: Float)
    print (id 4)

