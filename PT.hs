data FormData a = FormData String deriving (Show)
changeType :: FormData a -> FormData b
changeType (FormData str) = FormData str

data Validated
data Unvalidated

formData :: String -> FormData Unvalidated
formData str = FormData str

validate :: FormData Unvalidated -> Maybe (FormData Validated)
validate (FormData str) = Just (FormData str)

useData :: FormData Validated -> IO ()
useData (FormData str) = print str



data Expr a = Expr PrimExpr deriving (Show)
constant :: Show a => a -> Expr a
constant a = Expr (ConstExpr $ show a)

(.+.) :: Expr Int -> Expr Int -> Expr Int
(.+.) (Expr (ConstExpr a)) (Expr (ConstExpr b)) = Expr (ConstExpr $ show (read a + read b))
(.+.) (Expr a) (Expr b) = Expr $ BinExpr OpPlus a b

(.==.) :: Expr a -> Expr a -> Expr Bool
(.==.) (Expr (ConstExpr a)) (Expr (ConstExpr b)) = Expr (ConstExpr $ show $ a == b)
(.==.) (Expr a) (Expr b) = Expr $ BinExpr OpEq a b 

(.&&.) :: Expr Bool -> Expr Bool -> Expr Bool
(.&&.) (Expr (ConstExpr a)) (Expr (ConstExpr b)) = Expr (ConstExpr $ show $ read a && read b)

data PrimExpr = BinExpr BinOp PrimExpr PrimExpr
     | UnExpr UnOp PrimExpr
     | ConstExpr String deriving (Show)


data BinOp = OpEq | OpAnd | OpPlus deriving (Show)
data UnOp = OpPositive | OpNegtive deriving (Show)