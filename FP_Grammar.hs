module FP_Grammar where

{- ===========================================================================
Contains example grammar + examples of test definitions
NOTE: Compiler directives above
=========================================================================== -}

--import FPPrac.Trees       -- Contains now also the function toRoseTree. Re-install it!
import GHC.Generics       -- Necessary for correct function of FPPrac
import Data.Char

import ParseBasis           -- Extend the file TypesEtc with your own alphabet
import ParserGen (parse)  -- Touching this file leaves you at your own devices
-- import Tokenizer       -- You'll have to write a file for tokenizing yourself

-- ==========================================================================================================
-- Example grammar, to illustrate the structure of the definition of the grammar as a function
--      (where the Alphabet is in the file TypesEtc.hs)
grammar nt = case nt of

        Nmbr    -> [[ nmbr                               ]]
        Var     -> [[ var                                ]]
        Op      -> [[ ArithOp                            ]
                   ,[ CompOp                             ]]
        
        Bool    -> [[ true], [ false]]
        
        Iff     -> [[ iff]]
        Elsee   -> [[ elsee]]
        Whilee  -> [[whilee]] 
        
        ArithOp -> [[ mul], [ add], [ minus]]
        
        CompOp  -> [[ equal], [ less], [ greater], [ lesseq], [ greatereq], [ binAnd], [binOr]]
        
        ArithExpr    -> [[ lBracket, ArithExpr, ArithOp, ArithExpr, rBracket            ]
                   ,[ Nmbr                                              ]
                   ,[ Var]]
        
        Expr    -> [[ArithExpr], [CompExpr]]
        
        While   -> [[Whilee, CompExpr, lcBracket, Rep1 [Content] , rcBracket]]
        
        
        IfThen  -> [[ Iff, lBracket, CompExpr, rBracket,lcBracket, Rep1 [Content], rcBracket, Elsee, lcBracket, Rep1 [Content], rcBracket], [ Iff, lBracket, CompExpr, rBracket, lcBracket, Rep1 [Content], rcBracket]]
        
        CompExpr -> [[ lBracket, Expr, CompOp, Expr, rBracket],
                     --[ Expr, CompOp, Expr],
                     [ Bool],
                     [ Var]]
        
        Program -> [[Rep1 [Content]]]
        
        Content -> [[Stmnt], [IfThen], [While]]
        
        Type    -> [[int],[bool]]
        
        Stmnt   -> [[Var, assign, Expr, semi                           ],
                    [Type, Var, assign, Expr, semi                           ]]

-- shorthand names can be handy, such as:
lBracket  = TermSymb "("           -- Terminals WILL be shown in the parse tree
rBracket  = TermSymb ")"
lsBracket = TermSymb "["
rsBracket = TermSymb "]"
lcBracket = TermSymb "{"
rcBracket = TermSymb "}"
mul       = TermSymb "*"
add       = TermSymb "+"
minus     = TermSymb "-"
equal     = TermSymb "=="
less      = TermSymb "<"
greater   = TermSymb ">"
lesseq    = TermSymb "<="
greatereq = TermSymb ">="
semi      = TermSymb ";"
assign    = TermSymb "="
int       = TermSymb "Int"
bool      = TermSymb "Bool"
binAnd    = TermSymb "&&"
binOr     = TermSymb "||"
true      = TermSymb "True"
false     = TermSymb "False"

-- alternative:
-- lBracket  = Symbol "("          -- Symbols will NOT be shown in the parse tree.
-- rBracket  = Symbol ")"
nmbr      = SyntCat Nmbr
var       = SyntCat Var
iff        = SyntCat Iff
-- thenn      = SyntCat Thenn
elsee      = SyntCat Elsee
whilee      = SyntCat Whilee
--op        = SyntCat Op



-- ==========================================================================================================
-- TESTING: example expression: "((10+20)*30)"

-- Result of tokenizer (to write yourself) should be something like:
{-tokenList0 = [ (Rep, "REPEAT", 0) 
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
             ]-}

-- Parse this tokenlist with a call to the function parse, with
--      - grammar: the name of the grammar above
--      - Expr: the start-nonterminal of the grammar above
--      - tokenList0: the tokenlist above
--parseTree0 = parse grammar Program tokenList0

-- prpr: for pretty-printing the parsetree, including error messages
--testTxt    = prpr parseTree0

-- showTree + toRoseTree: for graphical representation in browser
--testGr     = showTree $ toRoseTree parseTree0

test1      = toExpress(parse grammar Program (tokenizer "REPEAT(10+20)[ASSIGNbab5ASSIGNccc9]"))
test2      = toExpress(parse grammar Program (tokenizer "If(5==5)Then5Else8;"))

tokenizer :: String -> [(Alphabet, String)]
tokenizer [] = []
tokenizer (x:xs) |  x== '(' = [(Bracket, "(")] ++ (tokenizer xs)
                 |  x== ')' = [(Bracket, ")")] ++ (tokenizer xs)
                 |  x== '[' = [(lsBracket, "[")] ++ (tokenizer xs)
                 |  x== ']' = [(rsBracket, "]")] ++ (tokenizer xs)
                 |  x== '}' = [(rcBracket, "}")] ++ (tokenizer xs)
                 |  x== '{' = [(lcBracket, "{")] ++ (tokenizer xs)
                 |  x== '+' = [(ArithOp, "+")] ++ (tokenizer xs)
                 |  x== '-' = [(ArithOp, "-")] ++ (tokenizer xs)
                 |  x== '*' = [(ArithOp, "*")] ++ (tokenizer xs)
                 |  x== '=' && xs /= [] && (head xs) == '='         = [(CompOp, "==")] ++ (tokenizer (tail xs))
                 |  x== '>' && xs /= [] && (head xs) == '='         = [(CompOp, ">=")] ++ (tokenizer (tail xs))
                 |  x== '>'                                         = [(CompOp, ">")] ++ (tokenizer xs)
                 |  x== '<' && xs /= [] && (head xs) == '='         = [(CompOp, "<=")] ++ (tokenizer (tail xs))
                 |  x== '<'                                         = [(CompOp, "<")] ++ (tokenizer xs)
                 |  x== '='                                         = [(assign, "=")] ++ (tokenizer xs)
                 |  x== ';'                                         = [(semi, ";")] ++ (tokenizer xs)
                 |  x== '&' && (head xs) == '&'                     = [(binAnd, "&&")] ++ (tokenizer (drop 1 xs))
                 |  x== '|' && (head xs) == '|'                     = [(binOr, "||")] ++ (tokenizer (drop 1 xs))
                 |  (length xs)>=2 && (take 3 (x:xs))== "Int"       = [(int, "Int")] ++ (tokenizer (drop 2 xs))
                 |  (length xs)>=3 && (take 4 (x:xs))== "Bool"      = [(bool, "Bool")] ++ (tokenizer (drop 3 xs))
                 |  (length xs)>=3 && (take 4 (x:xs))== "True"      = [(true, "True")] ++ (tokenizer (drop 3 xs))
                 |  (length xs)>=4 && (take 5 (x:xs))== "False"     = [(false, "False")] ++ (tokenizer (drop 4 xs))
                 |  (length xs)>=1 && (take 2 (x:xs))== "If"        = [(Iff, "If")] ++ (tokenizer (drop 1 xs))
                -- |  (length xs)>=5 && (take 6 (x:xs))== "ElseIf"    = [(ElseIf, "ElseIf",0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop 5 xs)))
                 |  (length xs)>=3 && (take 4 (x:xs))== "Else"      = [(Elsee, "Else")] ++ (tokenizer (drop 3 xs))
                -- |  (length xs)>=2 && (take 3 (x:xs))== "For"       = [(Rep, "For", 0)] ++ (map (\(a,y,z)->(a,y,z+1)) (tokenizer (drop 2 xs)))
                 |  (length xs)>=4 && (take 5 (x:xs))== "While"     = [(Assign, "While")] ++ (tokenizer (drop 4 xs))
                 |  isDigit x                                       = [(Nmbr,digit)] ++ (tokenizer (drop ((length digit)-1) xs))
                 |  isLetter x                                      = [(Var,var)] ++ (tokenizer (drop ((length var)-1) xs))
                    where var = takeWhile isAlpha (x:xs)
                          digit = takeWhile isDigit (x:xs)
                          
pizza = "BooleenBoolean=True;If((eenBoolean==eenBoolean)){BoolandereBoolean=True;}Else{BooleenBoolean=False;}If((eenBoolean==andereBoolean)){Intx=100;}"

{-toSyntaxTree :: ParseTree -> RoseTree
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
                                (RoseNode d e) = toSyntaxTree (xs!!1)-}


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
                            where (PLeaf (q, r)) = xs!!2

toExpressIT :: ParseTree -> IfThenElse
toExpressIT (PNode b xs)      = IFF (toExpressCE (xs!!1)) (toExpressExpr (xs!!3)) (toExpressExpr (xs!!5))                           
                            
toExpressExpr :: ParseTree -> Expres
toExpressExpr (PNode b xs)    | b == ArithExpr = AE (toExpressAE (PNode b xs))
                              | b == CompExpr  = CE (toExpressCE (PNode b xs))
                              | b == Expr      = toExpressExpr (head xs)

toExpressAE :: ParseTree -> ArithExpres
toExpressAE (PLeaf (a, b))    | a == Nmbr = Const (read b)
                              | a == Var  = Vari b 
toExpressAE (PNode b xs)      | length xs == 1 = toExpressAE (head xs)
                              | otherwise = BinExpr (toExpressAO (xs!!2)) (toExpressAE (xs!!1)) (toExpressAE (xs!!3))

toExpressCE :: ParseTree -> CompExpres                              
toExpressCE (PNode b xs)      = CExpr (toExpressCO (xs!!2)) (toExpressExpr (xs!!1)) (toExpressExpr (xs!!3))

toExpressAO :: ParseTree -> ArithOper
toExpressAO (PLeaf (a, b))    | b == "*" = Mul
                              | b == "+" = Add
                              | b == "-" = Sub
toExpressAO (PNode b xs)      = toExpressAO (head xs)

toExpressCO :: ParseTree -> CompOper                              
toExpressCO (PLeaf (a, b))    | b == "==" = Eq 
                              | b == ">" = Gt
                              | b == "<" = St 
                              | b == ">=" = GEt 
                              | b == "<=" = SEt
toExpressCO (PNode b xs)      = toExpressCO (head xs)

toExpressStm :: ParseTree -> Stmnts                             
toExpressStm (PNode b xs)     | length xs == 3 = Assigning p (toExpressAE (xs!!2))
                              | length xs == 4 = Repeating (toExpressAE (xs!!1)) (toExpressStm2 (xs!!3))
                              where (PLeaf (q, p)) = hs!!0
                                    (PNode t hs) = xs!!1

toExpressStm2 :: ParseTree -> [Stmnts]                                   
toExpressStm2 (PNode b xs)    | length xs == 1 = []
                              | length xs == 2 = (toExpressStm (xs!!0)):(toExpressStm2 (xs!!1))
