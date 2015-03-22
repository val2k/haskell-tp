import Parser
import Data.Maybe

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
    show (VLitteralA (Entier e)) = show e
    show (VLitteralA (Bool b)) = show b

type Environnement a = [(Nom, a)]

--Question15
interpreteA :: Environnement ValeurA -> Expression -> ValeurA
interpreteA _ (Lit l) = VLitteralA l
interpreteA xs (Var k) = fromJust (lookup k xs)

interpreteA xs (Lam nom expr) = VFonctionA (\v -> interpreteA   ((nom,v):xs) expr)
interpreteA xs (App e1 e2) = f v2
                         where VFonctionA f = interpreteA xs e1
                               v2 = interpreteA xs e2

--Question16
negA :: ValeurA
negA = VFonctionA (\(VLitteralA (Entier e)) -> VLitteralA (Entier (- e)))

--Question17
addA :: ValeurA
addA = VFonctionA (\(VLitteralA (Entier e1)) -> VFonctionA (\(VLitteralA (Entier e2)) ->
                              VLitteralA (Entier (e1 + e2))))

--Question18
envA :: Environnement ValeurA
envA = [ ("neg",   negA)
       , ("add",   releveBinOpEntierA (+))
       , ("soust", releveBinOpEntierA (-))
       , ("mult",  releveBinOpEntierA (*))
       , ("quot",  releveBinOpEntierA quot)
       , ("if",    ifthenelseA) ]

releveBinOpEntierA :: (Integer -> Integer -> Integer) -> ValeurA
releveBinOpEntierA op = VFonctionA (\(VLitteralA (Entier e1)) -> VFonctionA (\(VLitteralA (Entier e2)) -> VLitteralA (Entier (op e1 e2))))

--Question19
checkIf :: Bool -> ValeurA -> ValeurA -> ValeurA
checkIf True (VLitteralA (Entier e1)) _ = VLitteralA (Entier e1) :: ValeurA
checkIf False _ (VLitteralA (Entier e2)) = VLitteralA (Entier e2) ::ValeurA
 
ifthenelseA :: ValeurA
ifthenelseA = VFonctionA (\(VLitteralA (Bool b)) -> VFonctionA (\(VLitteralA (Entier e1)) -> VFonctionA (\(VLitteralA (Entier e2)) -> (checkIf b e1 e2))))
