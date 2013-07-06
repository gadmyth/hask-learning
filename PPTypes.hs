module PPTypes where

type Measure = String

data Ingredient = Ingredient {
     ingredientName :: String,
     quantity :: Int,
     measure :: Maybe Measure
} deriving Show

data Step = Step {
     stepName :: String,
     order :: Int,
     stepDuration :: Maybe Duration
} deriving (Eq, Show)

data Duration = Duration {
     duration :: Int,
     durationMeasure :: Measure
} deriving (Eq, Show)

data Recipe = Recipe {
     recipeName :: String,
     ingredients :: [Ingredient],
     steps :: [Step]
} deriving Show