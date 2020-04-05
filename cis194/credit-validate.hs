toDigits a
  | a < 10 = [a]
  | otherwise = (toDigits (a `div` 10)) ++ [(mod a 10)]

reverseList [] = []
reverseList (a:zs) = (reverseList zs) ++ [a]

toDigitsRev a = reverseList (toDigits a)

doubleCreditList [] = []
doubleCreditList [a] = [a]
doubleCreditList (a:b:c) = a:(2*b):(doubleCreditList c)

sumCreditList [] = 0
sumCreditList (a:zs)
  | a > 10 = (a `div` 10) + (mod a 10) + sumCreditList zs
  | otherwise = a + sumCreditList zs

validateCreditList list = (mod (sumCreditList (doubleCreditList list)) 10) == 0

validateCreditNum :: Int -> Bool
validateCreditNum a = validateCreditList (toDigitsRev a)

