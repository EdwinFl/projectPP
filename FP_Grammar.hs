{-# LANGUAGE FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}
        -- Necessary for function toRoseTree

module FP_Grammar where

{- ===========================================================================
Contains example grammar + examples of test definitions
NOTE: Compiler directives above
=========================================================================== -}

import FPPrac.Trees       -- Contains now also the function toRoseTree. Re-install it!
import GHC.Generics       -- Necessary for correct function of FPPrac
import Data.Char

import FP_TypesEtc           -- Extend the file TypesEtc with your own alphabet
import FP_ParserGen (parse)  -- Touching this file leaves you at your own devices
-- import Tokenizer       -- You'll have to write a file for tokenizing yourself

-- ==========================================================================================================
-- Example grammar, to illustrate the structure of the definition of the grammar as a function
--      (where the Alphabet is in the file TypesEtc.hs)

grammar :: Grammar
grammar nt = case nt of

        Nmbr    -> [[ nmbr                               ]]
        Var     -> [[ var                                ]]
        Assign  -> [[ assign]]
        Rep     -> [[ rep]]
        Op      -> [[ ArithOp                            ]
                   ,[ CompOp                             ]]
        
        Iff     -> [[ iff]]
        Thenn   -> [[ thenn]]
        Elsee   -> [[ elsee]]
        
        ArithOp -> [[ mul], [ add], [ minus]]
        
        CompOp  -> [[ equal], [ less], [ greater], [ lesseq], [ greatereq]]
        
        ArithExpr    -> [[ lBracket, ArithExpr, ArithOp, ArithExpr, rBracket            ]
                   ,[ Nmbr                                              ]]
        
        Expr    -> [[ArithExpr], [CompExpr]]
        
        
        IfThen  -> [[ Iff, CompExpr, Thenn, Expr, Elsee, Expr, semi]]
        
        CompExpr -> [[ lBracket, Expr, CompOp, Expr, rBracket]]
        
        Program -> [[Expr], [Stmnt], [IfThen]]
        
        Stmnt   -> [[Assign, Var, ArithExpr                           ]
                   ,[Rep ,ArithExpr, lsBracket, Stmnt2                        ]]
        
        Stmnt2  -> [[ Stmnt, Stmnt2                       ]
                   ,[ rsBracket                           ]]
-- shorthand names can be handy, such as:
lBracket  = Terminal "("           -- Terminals WILL be shown in the parse tree
rBracket  = Terminal ")"
lsBracket = Terminal "["
rsBracket = Terminal "]"
mul       = Terminal "*"
add       = Terminal "+"
minus     = Terminal "-"
equal     = Terminal "=="
less      = Terminal "<"
greater   = Terminal ">"
lesseq    = Terminal "<="
greatereq = Terminal ">="
semi      = Terminal ";"
--rep       = Terminal "REPEAT"
--assign    = Terminal "ASSIGN"

-- alternative:
-- lBracket  = Symbol "("          -- Symbols will NOT be shown in the parse tree.
-- rBracket  = Symbol ")"
rep       = SyntCat Rep
assign    = SyntCat Assign
nmbr      = SyntCat Nmbr
var       = SyntCat Var
iff        = SyntCat Iff
thenn      = SyntCat Thenn
elsee      = SyntCat Elsee
--op        = SyntCat Op



-- ==========================================================================================================
-- TESTING: example expression: "((10+20)*30)"

-- Result of tokenizer (to write yourself) should be something like:
tokenList0 = [ (Rep, "REPEAT", 0) 
             , (Bracket,"(",1)
             , (Nmbr,"10",2)
             , (ArithOp,"+",3)
             , (Nmbr,"20",4)
             , (Bracket,")",5)
             , (lsBracket, "[", 6)
             , (Assign, "ASSIGN", 7)
             , (Var,"bab",8)
             , (Nmbr,"5",9)
             , (Assign, "ASSIGN", 10)
             , (Var,"ccc",11)
             , (Nmbr,"9",12)
             , (rsBracket,"]",13)
             ]

-- Parse this tokenlist with a call to the function parse, with
--      - grammar: the name of the grammar above
--      - Expr: the start-nonterminal of the grammar above
--      - tokenList0: the tokenlist above
parseTree0 = parse grammar Program tokenList0

-- prpr: for pretty-printing the parsetree, including error messages
testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
testGr     = showTree $ toRoseTree parseTree0

test1      = toExpress(parse grammar Program (tokenizer "REPEAT(10+20)[ASSIGNbab5ASSIGNccc9]"))
test2      = toExpress(parse grammar Program (tokenizer "If(5==5)Then5Else8;"))

tokenizer :: String -> [(Alphabet, String, Int)]
tokenizer [] = []
tokenizer (x:xs) |  x== '(' = [(Bracket, "(", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== ')' = [(Bracket, ")", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== '[' = [(lsBracket, "[", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== ']' = [(rsBracket, "]", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== '+' = [(ArithOp, "+", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== '-' = [(ArithOp, "-", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== '*' = [(ArithOp, "*", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== '=' && xs /= [] && (head xs) == '='         = [(CompOp, "==", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (tail xs)))
                 |  x== '>' && xs /= [] && (head xs) == '='         = [(CompOp, ">=", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (tail xs)))
                 |  x== '>'                                         = [(CompOp, ">", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== '<' && xs /= [] && (head xs) == '='         = [(CompOp, "<=", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (tail xs)))
                 |  x== '<'                                         = [(CompOp, "<", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  x== ';'                                         = [(semi, ";", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer xs))
                 |  (length xs)>=1 && (take 2 (x:xs))== "If"        = [(Iff, "If",0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop 1 xs)))
                 |  (length xs)>=3 && (take 4 (x:xs))== "Then"      = [(Thenn, "Then",0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop 3 xs)))
                 |  (length xs)>=3 && (take 4 (x:xs))== "Else"      = [(Elsee, "Else",0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop 3 xs)))
                 |  (length xs)>=5 && (take 6 (x:xs))== "REPEAT"    = [(Rep, "REPEAT", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop 5 xs)))
                 |  (length xs)>=5 && (take 6 (x:xs))== "ASSIGN"    = [(Assign, "ASSIGN", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop 5 xs)))
                 |  isDigit x                                       = [(Nmbr,digit, 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop ((length digit)-1) xs)))
                 |  isLetter x                                       = [(Var,var, 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop ((length var)-1) xs)))
                    where var = takeWhile isAlpha (x:xs)
                          digit = takeWhile isDigit (x:xs)

pizza = "REPEAT(10+20)[ASSIGNbab5ASSIGNccc9]"

toSyntaxTree :: ParseTree -> RoseTree
toSyntaxTree (PLeaf a) = toRoseTree a
toSyntaxTree (PNode a xs) | a == Program                          = toSyntaxTree (head xs)
                          | a == Expr                             = toSyntaxTree (head xs)
                          | a == ArithExpr && (length xs == 1)    = toSyntaxTree (xs!!0)
                          | a == ArithExpr                        = RoseNode q [toSyntaxTree (xs!!1), toSyntaxTree (xs!!3)]
                          | a == CompExpr                         = RoseNode q [toSyntaxTree (xs!!1), toSyntaxTree (xs!!3)]
                          | a == Stmnt     && (length xs == 3)    = RoseNode t [toSyntaxTree (xs!!1), toSyntaxTree (xs!!2)]
                          | a == Stmnt                            = RoseNode t ([toSyntaxTree (xs!!1)] ++ n)
                          | a == Var                              = RoseNode g []
                          | a == Nmbr                             = RoseNode g []
                          | a == Stmnt2    && (length xs == 1)    = RoseNode "niet" []
                          | a == Stmnt2                           = if d == "niet" then RoseNode "" [toSyntaxTree (xs!!0)] else RoseNode "" ([toSyntaxTree (xs!!0)] ++ e)
                          | otherwise = RoseNode (show a) []
                          where (PNode b ys) = xs!!2
                                (PLeaf (p, q, r)) = ys!!0
                                (PNode c zs) = xs!!0
                                (PLeaf (s, t, u)) = zs!!0
                                (PLeaf (f, g, h)) = xs!!0
                                (RoseNode m n) = toSyntaxTree (xs!!3)
                                (RoseNode d e) = toSyntaxTree (xs!!1)


data ArithExpres = Const Int                   -- for constants
                 | Vari String
                 | BinExpr ArithOper ArithExpres ArithExpres        -- for ``binary expressions''
                 deriving Show

data Expres = AE ArithExpres | CE CompExpres
            deriving Show
            
data CompExpres = CExpr CompOper Expres Expres
                deriving Show
                 
data CompOper = Eq | Gt | St | GEt | SEt
              deriving Show
     
data ArithOper = Add | Mul | Sub
               deriving Show
     
data Stmnts = Assigning String ArithExpres
            | Repeating ArithExpres [Stmnts]
            deriving Show

data IfThenElse = IFF CompExpres Expres Expres  
                deriving Show
            
data Programs = Ex Expres | Stm Stmnts | IT IfThenElse
              deriving Show
                            
toExpress :: ParseTree -> Programs
toExpress (PNode b xs)      | b == Program   = toExpress (head xs)
                            | b == Expr      = Ex (toExpressExpr (PNode b xs))
                            | b == Stmnt     = Stm (toExpressStm (PNode b xs))
                            | b == IfThen    = IT (toExpressIT (PNode b xs))
                            where (PLeaf (q, r, s)) = xs!!2

toExpressIT :: ParseTree -> IfThenElse
toExpressIT (PNode b xs)      = IFF (toExpressCE (xs!!1)) (toExpressExpr (xs!!3)) (toExpressExpr (xs!!5))                           
                            
toExpressExpr :: ParseTree -> Expres
toExpressExpr (PNode b xs)    | b == ArithExpr = AE (toExpressAE (PNode b xs))
                              | b == CompExpr  = CE (toExpressCE (PNode b xs))
                              | b == Expr      = toExpressExpr (head xs)

toExpressAE :: ParseTree -> ArithExpres
toExpressAE (PLeaf (a, b, c)) | a == Nmbr = Const (read b)
                              | a == Var  = Vari b 
toExpressAE (PNode b xs)      | length xs == 1 = toExpressAE (head xs)
                              | otherwise = BinExpr (toExpressAO (xs!!2)) (toExpressAE (xs!!1)) (toExpressAE (xs!!3))

toExpressCE :: ParseTree -> CompExpres                              
toExpressCE (PNode b xs)      = CExpr (toExpressCO (xs!!2)) (toExpressExpr (xs!!1)) (toExpressExpr (xs!!3))

toExpressAO :: ParseTree -> ArithOper
toExpressAO (PLeaf (a, b, c)) | b == "*" = Mul
                              | b == "+" = Add
                              | b == "-" = Sub
toExpressAO (PNode b xs)      = toExpressAO (head xs)

toExpressCO :: ParseTree -> CompOper                              
toExpressCO (PLeaf (a, b, c)) | b == "==" = Eq 
                              | b == ">" = Gt
                              | b == "<" = St 
                              | b == ">=" = GEt 
                              | b == "<=" = SEt
toExpressCO (PNode b xs)      = toExpressCO (head xs)

toExpressStm :: ParseTree -> Stmnts                             
toExpressStm (PNode b xs)     | length xs == 3 = Assigning p (toExpressAE (xs!!2))
                              | length xs == 4 = Repeating (toExpressAE (xs!!1)) (toExpressStm2 (xs!!3))
                              where (PLeaf (q, p, r)) = hs!!0
                                    (PNode t hs) = xs!!1

toExpressStm2 :: ParseTree -> [Stmnts]                                   
toExpressStm2 (PNode b xs)    | length xs == 1 = []
                              | length xs == 2 = (toExpressStm (xs!!0)):(toExpressStm2 (xs!!1))
                              