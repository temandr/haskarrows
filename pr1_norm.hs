import Control.Applicative 
import Control.Monad (liftM, ap)
import Control.Category

data ACounter a = C (Int -> (Int, a))
data NewCounter a b = ArrowC (a -> b)
data SecondCounter a b = ArrowD a b

-- increment the counter:
inc = get >>= \s -> (C $ \n -> (s+1, s+1))

--incArrow = getArrow Main.>>> arr (\n -> n+1)

-- returning the current value of the counter
get :: ACounter Int
get = C $ \n -> (n, n)

--getArrow :: NewCounter Int Int
--getArrow = ArrowC $ \n -> n


instance Functor ACounter where
  fmap = liftM

instance Applicative ACounter where 
    pure  = return
    (<*>) = ap

-- return is nop, >>= is sequential exectuion
instance Monad ACounter where
    return r = C $ \n -> (n, r)
    (>>=) (C f) g = C $ \n0 -> let (n1, r1) = f n0
                                   C g' = g r1
                               in g' n1

class Arrow a where
    arr   :: (Num b) => (b -> b) -> a b b
    (>>>) :: (Num b) => a b b -> a b b -> a b b

instance Arrow SecondCounter where
      arr f = ArrowD 0 ((f 0) +1)
      (>>>) (ArrowD a b) (ArrowD c d) = ArrowD a (b+d)

--instance Arrow NewCounter where
--    arr f = ArrowC f    
--    (>>>) (ArrowC f) (ArrowC g) = ArrowC (f Prelude.. g)

--class Category c => CounterCat c where
--    id :: (Num a) => c a a
--    (.) :: c d e -> c a b -> c a e
--
--instance CounterCat NewCounter where
--    id = Main.arr Prelude.id
--    (ArrowC b c) . (ArrowC a d) = (ArrowC a c)


run :: ACounter a -> (Int, a)
run (C f) = (f 0)

--runArrow (ArrowC f) = f 0
runSecond (ArrowD a b) = (a,b)

--temp :: Num a => NewCounter a a
--temp = arr (\n -> n+1)

tickC  = do
  inc
  inc
  c <- get
  return c

