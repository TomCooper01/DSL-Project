data Expr where
    Int :: Integer -> Expr
    Bool :: Bool -> Expr
    Add :: Expr -> Expr -> Expr
    Sub :: Expr -> Expr -> Expr
    Mul :: Expr -> Expr -> Expr
    Div :: Expr -> Expr -> Expr
    Var :: String -> Expr
    Assign :: String -> Expr -> Expr
    Seq :: Expr -> Expr -> Expr
    While :: Expr -> Expr -> Expr
    If :: Expr -> Expr -> Expr -> Expr
    Equality :: Expr -> Expr -> Expr
    NotEqual :: Expr -> Expr -> Expr
    LessThan :: Expr -> Expr -> Expr
    LessThanEq :: Expr -> Expr -> Expr
    GreaterThan :: Expr -> Expr -> Expr
    GreaterThanEq :: Expr -> Expr -> Expr
    deriving (Show)

type Env = [(String, Expr)]

instance Num Expr where
    fromInteger :: Integer -> Expr
    fromInteger = Int
    (+) :: Expr -> Expr -> Expr
    e1 + e2 = Add e1 e2
    (-) :: Expr -> Expr -> Expr
    e1 - e2 = Sub e1 e2
    (*) :: Expr -> Expr -> Expr
    e1 * e2 = Mul e1 e2
    abs :: Expr -> Expr
    abs = undefined --  Add
    signum :: Expr -> Expr
    signum = undefined  -- Add

instance Eq Expr where
    (==) :: Expr -> Expr -> Expr
    e1 == e2 = Equality e1 e2
    (/=) :: Expr -> Expr -> Expr
    e1 /= e2 = NotEqual e1 e2


instance Ord Expr where
    (<=) :: Expr -> Expr -> Expr
    e1 <= e2 = LessThanEq e1 e2
    (>=) :: Expr -> Expr -> Expr
    e1 >= e2 = GreaterThanEq e1 e2
    (<) :: Expr -> Expr -> Expr
    e1 < e2 = LessThan e1 e2
    (>) :: Expr -> Expr -> Expr
    e1 > e2 = GreaterThan e1 e2
    

instance Fractional Expr where
    fromRational :: Rational -> Expr
    fromRational = Int . round
    (/) :: Expr -> Expr -> Expr
    e1 / e2 = Div e1 e2
    
run :: Env -> Expr -> (Env, Integer)
run env (Int i) = (env, i)
run env (Add e1 e2) = 
    let (env1, v1) = run env e1
        (env2, v2) = run env1 e2
    in (env2, v1 + v2)
run env (Sub e1 e2) =
    let (env1, v1) = run env e1
        (env2, v2) = run env1 e2
    in (env2, v1 - v2)
run env (Mul e1 e2) =
    let (env1, v1) = run env e1
        (env2, v2) = run env1 e2
    in (env2, v1 * v2)
run env (Div e1 e2) =
    let (env1, v1) = run env e1
        (env2, v2) = run env1 e2
    in (env2, v1 `div` v2)
run env (Var s) = 
    case lookup s env of
        Just e -> run env e
        Nothing -> error $ "Variable Not Found" ++ s
run env (Assign s e) =
    let (env1, v) = run env e
    in ((s, Int v):env1, v)
run env (Seq e1 e2) =
    let (env1, v1) = run env e1
        (env2, v2) = run env1 e2
    in (env2, v2)

seq_ :: Expr -> Expr -> Expr
seq_ = Seq

if_ :: Expr -> Expr -> Expr -> Expr
if_ = If

while_ :: Expr -> Expr -> Expr
while_ = While

main :: IO ()
main = do
    let env = []
        e = Assign "x" (Int 0) `seq_`
            Assign "y" (Int 0) `seq_`
            --while_ (Var "x" < 10)
                (Assign "x" (Var "x" + 1) `seq_`
                 Assign "y" (Var "y" + 1))
    print $ run env e