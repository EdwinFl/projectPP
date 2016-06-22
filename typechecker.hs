import Data.Maybe

data Expr = Const Int 
          | Var  String
          | Tup (Expr, Expr)
          | Tup3 (Expr, Expr, Expr)
          | BinOp String Expr  Expr 
          | App Expr Expr
          | IfThen Expr Expr Expr
          | Lambda Type Expr
          deriving Show

data Type = IntType
          | BoolType
          | FunType Type Type
          | Tuple (Type, Type)
          | Tuple3 (Type, Type, Type)
          deriving (Show, Eq)

type Env = [(String, Type)]

testEnv :: Env
testEnv = [("+", FunType IntType (FunType IntType IntType))
          ,("*", FunType IntType (FunType IntType IntType))
          ,("-", FunType IntType (FunType IntType IntType))
          ,("&&", FunType BoolType (FunType BoolType BoolType))
          ,("||", FunType BoolType (FunType BoolType BoolType))
          ,("var1", BoolType)
          ,("var2", IntType)]

typeOf :: Env -> Expr -> Type
typeOf env (Var x) | isNothing t = error (x ++ " not in env")
                   | otherwise = fromJust t
                   where t = lookup x env
                   
typeOf env (Const x) = IntType
typeOf env (Tup (x,y)) = case (Just tx, Just ty) of
                         (Nothing,_) -> error "error"
                         (_, Nothing) -> error "error"
                         _ -> Tuple (tx, ty)
                       where tx = typeOf env x
                             ty = typeOf env y
typeOf env (Tup3 (x,y,z)) = case (Just tx, Just ty, Just tz) of
                         (Nothing,_,_) -> error "error"
                         (_, Nothing,_) -> error "error"
                         (_,_,Nothing) -> error "error"
                         _ -> Tuple3 (tx, ty, tz)
                       where tx = typeOf env x
                             ty = typeOf env y
                             tz = typeOf env z

typeOf env (App e e2) = case Just(top) of
                        Nothing -> error "error"
                        Just(FunType t0 t1)
                            | t0 == top2 -> t1
                            | otherwise -> error "err"
                        Just _ -> error "error"
                        where top = typeOf env e
                              top2 = typeOf env e2
                        
typeOf env (BinOp op e e2) = case top of
                            Nothing -> error "err"
                            Just(FunType t0 (FunType t1 t2))
                                | t0 == te && t1 == te2 -> t2
                                | otherwise -> error "error"
                            Just _ -> error "error"
                            where top = lookup op env
                                  te  = typeOf env e
                                  te2 = typeOf env e2

typeOf env (IfThen binop e e2) =  case (Just top) of
                            Nothing -> error "err"
                            Just(BoolType)
                                | te == te2 -> te
                                | otherwise -> error "error"
                            Just _ -> error "error"
                            where top = typeOf env binop
                                  te  = typeOf env e
                                  te2 = typeOf env e2                                   

typeOf env (Lambda t e) = case Just(top) of
                        Nothing -> error "error"
                        Just _ -> FunType t top
                        where top = typeOf env e
                              
                        
test = typeOf testEnv (IfThen (BinOp "&&" (Var "var1") (Var "var1")) (Var "var2") (Var "var2"))
test2 = typeOf testEnv (Tup3 ((Var "var1"),(Const 5),(Var "var2")))
test3 = typeOf testEnv (App (BinOp "&&" (Var "var1") (Var "var1"))(BinOp "&&" (Var "var1") (Var "var1")))
