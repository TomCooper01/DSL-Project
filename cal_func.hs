data Expr where
    Lit :: Integer -> Expr
    Add :: Expr -> Expr -> Expr
    Sub :: Expr -> Expr -> Expr
    Mul :: Expr -> Expr -> Expr
    Div :: Expr -> Expr -> Expr
    Var :: String -> Expr
    Assign :: String -> Expr -> Expr
    Seq :: Expr -> Expr -> Expr
    deriving (Show, Eq)

type Env = [(String, Expr)]

instance Num Expr where
    fromInteger :: Integer -> Expr
    fromInteger = Lit
    (+) :: Expr -> Expr -> Expr
    e1 + e2 = Add e1 e2
    (-) :: Expr -> Expr -> Expr
    e1 - e2 = Sub e1 e2
    (*) :: Expr -> Expr -> Expr
    e1 * e2 = Mul e1 e2

instance Fractional Expr where
    fromRational :: Rational -> Expr
    fromRational = Lit . round
    (/) :: Expr -> Expr -> Expr
    e1 / e2 = Div e1 e2
    

run :: Env -> Expr -> (Env, Integer)
run env (Lit i) = (env, i)
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
    in ((s, Lit v):env1, v)
run env (Seq e1 e2) =
    let (env1, v1) = run env e1
        (env2, v2) = run env1 e2
    in (env2, v2)

data ASTNode where
    LitNode :: Integer -> ASTNode
    AddNode :: ASTNode -> ASTNode -> ASTNode
    SubNode :: ASTNode -> ASTNode -> ASTNode
    MulNode :: ASTNode -> ASTNode -> ASTNode
    DivNode :: ASTNode -> ASTNode -> ASTNode
    deriving (Show, Eq)

toAst :: Expr -> ASTNode
toAst (Lit i) = LitNode i
toAst (Add e1 e2) = AddNode (toAst e1) (toAst e2)
toAst (Sub e1 e2) = SubNode (toAst e1) (toAst e2)
toAst (Mul e1 e2) = MulNode (toAst e1) (toAst e2)
toAst (Div e1 e2) = DivNode (toAst e1) (toAst e2)
toAst (Var s) = error $ "Variable Not Allowed" ++ s         -- Can't convert variable to AST
toAst (Assign s e) = error $ "Assignment Not Allowed" ++ s  -- Can't convert assignment to AST
toAst (Seq e1 e2) = error "Sequence Not Allowed"            -- Can't convert sequence to AST

eval :: ASTNode -> Integer
eval (LitNode i) = i
eval (AddNode n1 n2) = eval n1 + eval n2
eval (SubNode n1 n2) = eval n1 - eval n2
eval (MulNode n1 n2) = eval n1 * eval n2
eval (DivNode n1 n2) = eval n1 `div` eval n2

main :: IO ()
main = do
    let ast = eval $ toAst $ (Lit 1 + Lit 2) * Lit 3
    print ast