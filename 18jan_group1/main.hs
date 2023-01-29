data Expr 
    = Const Int
    | Var
    | Expr :+ Expr
    | Expr :- Expr
    | Expr :* Expr
    | Expr :^ Int

infixl 6 :+
infixl 6 :-
infixl 7 :*
infixr 8 :^

eval :: Expr -> Int -> Int
eval (Const c) _ = c
eval (Var) x = x
eval (e1 :+ e2) x = eval e1 x + eval e2 x
eval (e1 :- e2) x = eval e1 x - eval e2 x
eval (e1 :* e2) x = eval e1 x * eval e2 x
eval (e :^ p) x = eval e x ^ p

e::Expr
e = (Var :+ (Const 1)) :^ 3

derive :: Expr -> Expr
derive (Const c) = (Const 0)
derive Var = (Const 1)
derive (e1 :+ e2) = (derive e1) :+ (derive e2)
derive (e1 :- e2) = (derive e1) :- (derive e2)
derive (e1 :* e2) = (derive e1) :* e2 :+ (derive e2) :* e1
derive (e :^ p) = (Const p) :* (e :^ (p-1)) :* (derive e)

simplify :: Expr -> Expr
simplify (e :* (Const 1)) = e
simplify ((Const 1) :* e) = e
simplify (e :^ (Const 1)) = e
-- ...
simplify e = e