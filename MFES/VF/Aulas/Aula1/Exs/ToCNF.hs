module ToCNF where

data Op = And | Or | Imp deriving (Eq, Ord)

type FCore = (Op,(Formula,Formula))

data Formula = Var String | Neg Formula | F FCore deriving (Eq, Ord)

-- R <=> Regular, N <=> negated
--data Value = R String | N String deriving (Show,Eq,Ord)

i1 = Right
i2 = Left

f :: Formula
f = F (Imp,(F l,F r)) where
    l = (Or,(Var "A", F (Imp, (Var "A", Var "B"))))
    r = (Or,(Var "A", Neg (Var "B")))

instance Show Op where
    show And = "/\\"
    show Or = "\\/"
    show Imp = "->"

instance Show Formula where
    show (Var s) = s
    show (Neg f) = "Not " ++ show f
    show (F (o,(f1,f2))) = "(" ++ show f1 ++ " " ++ show o ++ " " ++ show f2 ++ ")"

convertImps (F (Imp,(f1,f2))) = F (Or,(Neg $ convertImps f1, convertImps f2))
convertImps (Var s) = Var s
convertImps (Neg s) = Neg $ convertImps s
convertImps (F (o,(f1,f2))) = F (o, (convertImps f1, convertImps f2))

inNegation (Neg (F (And, (f1,f2)))) = F (Or, (Neg f1, Neg f2))
inNegation (Neg (F (Or, (f1,f2)))) = F (And, (Neg f1, Neg f2))
inNegation (F (o, (f1,f2))) = F (o, (inNegation f1, inNegation f2))
inNegation s = s

flattenNots (Neg (Neg f)) = flattenNots f
flattenNots (F (o,(f1,f2))) = F (o, (flattenNots f1, flattenNots f2))
flattenNots (Neg f) = Neg $ flattenNots f
flattenNots f = f
