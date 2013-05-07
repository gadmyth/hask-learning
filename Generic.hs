{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
class Function f a b | f a -> b where
      funcall :: f -> a -> b

instance Function (a -> b) a b where
         funcall = id

pairToList :: (Function f a c, Function f b c) => f -> (a, b) -> [c]
pairToList f (a, b) = [funcall f a, funcall f b]

{-
funcall' show = show
funcall' fromEnum = fromEnum
pairToList' :: (t t1, t t2) => (t -> e) -> (t1, t2) -> [e]
pairToList' f (a, b) = [funcall' f a, funcall' f b]
-}

data ShowF = ShowF
instance (Show a) => Function ShowF a [Char] where
         funcall _ = show

data FromEnumF = FromEnumF
instance (Enum a) => Function FromEnumF a Int where
         funcall _ = fromEnum