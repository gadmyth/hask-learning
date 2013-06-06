-- | http://www.haskell.org/haskellwiki/Default_values_in_records

data Foo = Foo { bar :: Int, baz :: Int, quux :: Int } deriving (Show)
 
fooDefault = Foo { bar = 1, baz = 2, quux = 3 }
 
newRecord = fooDefault { quux = 42 }
