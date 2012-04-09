import Data.List
import Data.Packed.Matrix
import Data.Packed.Vector
import Data.Function.Pointless
import Control.Applicative

dup f x = f x x

isLatinSquare mx = rows mx == cols mx
                && and ( concatMap (dup (zipWith (null .: (\\)) .^ tail))
                         [toList <$> toRows mx, toList <$> toColumns mx] )


mx1, mx2 :: Matrix Float

mx1 = (3 >< 3) [ 1, 2, 3
               , 2, 3, 1
               , 3, 1, 2 ]

mx2 = (3 >< 3) [ 1, 3, 2
               , 2, 3, 1
               , 3, 1, 2 ]

test = map isLatinSquare [mx1, mx2] -- > [True, False]