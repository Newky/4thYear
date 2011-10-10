-- Richard Delaney 08479950
module Ex01 where

type Ident = String

data Expression = Val Float
                | Add Expression Expression
                | Multiply Expression Expression
                | Subtract Expression Expression
                | Divide Expression Expression
                | Var Ident
                | Def Ident Expression Expression
     deriving Show

type Dictionary k d = [(k,d)]

find :: Eq k => Dictionary k d -> k -> Maybe d
find [] _ = Nothing
find ( (k,v) : ds ) name | name == k = Just v
                         | otherwise = find ds name

define :: Dictionary k d -> k -> d -> Dictionary k d
define d k v = (k,v):d

eval :: Dictionary Ident Float -> Expression -> Maybe Float
eval _ (Val x) = Just x
eval d (Add x y) = checkAndOperate d x y (+)
eval d (Multiply x y) = checkAndOperate d x y (*)
eval d (Subtract x y) = checkAndOperate d x y (-)
eval d (Divide x y)  
		    | (eval d y == Just 0.0) = Nothing
		    | otherwise = checkAndOperate d x y (/)
eval d (Var i) = (find d i)
eval d (Def i e1 e2) = eval d' e2
                       where d' = define d i (fromJust (eval d e1))

fromJust (Just a) = a

checkAndOperate d x y op
			| (not (res1 == Nothing)) && (not (res2 == Nothing)) = Just ( op (fromJust res1) (fromJust res2) )
			| otherwise = Nothing
			where res1 = eval d x
			      res2 = eval d y

