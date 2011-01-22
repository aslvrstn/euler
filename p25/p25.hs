import Euler
import Data.List

main = print $ find ((>=(10^999)).snd) (zip [0..] fibs)
