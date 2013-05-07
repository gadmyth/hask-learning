data MyType = Con1 Int | Con2 String deriving (Typeable, Data)
data MyTyCon a = MyTyCon a deriving (Typeable, Data)

cast :: (Typeable a, Typeable b) => a -> Maybe b
cast a b = if typeOf a == typeOf b then Just $ unsafeCoerce a
                                   else Nothing