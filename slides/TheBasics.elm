-- A function

add a b = a + b

-- Point-free

sub = (-)

-- Type Annotation

glue : String -> String -> String
glue = (++)

-- Recursion and case-expression with pattern matching

fac : Int -> Int
fac n =
  case n of
    0 -> 1
    n -> n * fac (n-1)
