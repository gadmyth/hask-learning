import Data.Functor.Contravariant
import Control.Applicative ((<$>))
import Data.Profunctor

--type Pred a = a -> Bool
newtype Pred a = Pred { getPred :: a -> Bool }

instance Contravariant Pred where
         contramap g (Pred p) = Pred (p . g)


veryOdd :: Pred Integer
veryOdd = contramap (`div` 2) (Pred odd)

main :: IO()
main = print $ getPred veryOdd <$> [0 .. 11]

--data Limits a = Limits { step :: a -> (a, a), check :: a -> a -> Bool }
type Limits a = Limits' a a
data Limits' a b = Limits { step :: a -> (a, a), check :: a -> a -> Bool }
instance Profunctor Limits' where
         dimap g h Limits {..} = Limits {
               step = (h *** h) . step . g,
               check = check `on` g }

maybeLimit :: a -> Limits a -> Limits (Maybe a)
maybeLimit d = dimap (maybe d id) Just

millionsLimit :: Limits Double -> Limits Double
millionsLimit = dimap (1.0e6 *) (/ 1.0e6)