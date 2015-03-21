import Parser

type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)



--Question1
espacesP :: Parser ()
espacesP = (unOuPlus (car ' ') >>= \_ -> 
                        return ()) ||| return ()

--Question2
alphab :: Char -> Bool
alphab = flip elem ['a'..'z']

nomP :: Parser Nom
nomP = unOuPlus (carCond alphab) >>= \s ->
       espacesP >>= \_ -> 
       return s

--Question3
varP :: Parser Expression
varP = unOuPlus (carCond alphab) >>= \s ->
       espacesP >>= \_ ->
       return (Var s)

--Question4
applique :: [Expression] -> Expression
applique (e:es) = foldl (\acc x -> (App acc x)) e es

--Question5
exprP :: Parser Expression
exprP = varP ||| lambdaP ||| exprParentheseeP ||| nombreP ||| booleenP

exprsP :: Parser Expression
exprsP = unOuPlus exprP >>= \s -> 
         return (applique s)

--Question6
lambdaP :: Parser Expression
lambdaP =           ( car '\\' >>= \_ ->
                     espacesP >>= \_ ->
                     nomP   >>= \lm ->
                     chaine "->" >>= \_ ->      
                     espacesP >>= \_ ->
                     exprsP >>= \ex ->
                     return (Lam lm ex) ) ||| echoue 

--Question7 (done)

--Question8
exprParentheseeP :: Parser Expression
exprParentheseeP = car '(' >>= \_ ->
                   espacesP >>= \_->
                   unOuPlus exprP >>= \ex ->
                   car ')' >>= \_ ->
                   espacesP >>= \_->
                   return (applique ex)

--Question9
isDigit = flip elem ['0'..'9']

nombreP :: Parser Expression
nombreP = unOuPlus (carCond isDigit) >>= \s ->
          espacesP >>= \_ ->
          return $ Lit $ Entier (read s)


--Question10
booleenP :: Parser Expression
booleenP = (chaine "True" >>= \_ ->
             espacesP >>= \_ ->
             return (Lit (Bool True))) |||
             (chaine "False" >>= \_ ->
             espacesP >>= \_ ->
             return (Lit (Bool False)))

--Question11
expressionP :: Parser Expression
expressionP = (zeroOuPlus (car ' ')) >>= \_ ->
               exprsP >>= \s ->
               return s

--Q12
resultExp Nothing = error "parse error"
resultExp (Just (e, "")) = e
resultExp _ = error "parse error"

ras :: String -> Expression
ras s = expr
       where expr = resultExp (parse expressionP s)

--Partie2
data ValeurA = VLitteralA Litteral
             | VFonctionA (ValeurA -> ValeurA)

instance Show ValeurA where
    show (VFonctionA _) = "λ"
                       -- ^ ou "VFonctionA _", ou "<fun>" ou toute
                       --   autre représentation des fonctions
    show (VLitteralA l) = show l

type Environnement a = [(Nom, a)]

--Question15
--interpreteA :: Environnement ValeurA -> Expression -> ValeurA
