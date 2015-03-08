module Parser ( Parser
              , Resultat
              , caractere
              , echoue
              , parse
              , (|||)
              , carCond
              , car
              , chaine
              , unOuPlus
              , zeroOuPlus
              , reussi
              , complet
              , resultat )
where

import Data.Maybe (isJust)

-- | Définition du type des analyseurs syntaxiques
type Resultat a  = Maybe (a, String)
newtype Parser a = MkParser (String -> Resultat a)

-- | Briques de base

-- | Définition de la monade associée aux analyseurs syntaxiques
-- Cette définition repose sur un analyseur de base, return
-- et sur le combinateur qui séquentialise deux analyseurs
instance Monad Parser where
    return v = MkParser (\s -> Just (v, s))

    p >>= pf = MkParser (\s -> case parse p s of
                                 Nothing      -> Nothing
                                 Just (a, s') -> parse (pf a) s')

    fail _   = echoue

-- | Analyseur syntaxique qui retrouve le premier caractère de la
-- chaîne
caractere :: Parser Char
caractere = MkParser (\s -> case s of
                              ""     -> Nothing
                              (c:cs) -> Just (c, cs))

-- | Analyseur qui échoue toujours
echoue :: Parser a
echoue = MkParser (const Nothing)

-- | Combinateur qui retourne le résultat du premier analyseur si
-- celui-ci réussit, sinon celui du second
(|||) :: Parser a -> Parser a -> Parser a
p ||| p' = MkParser (\s -> case parse p s of
                             Nothing -> parse p' s
                             r       -> r)

-- | Déclenche un analyseur syntaxique sur une chaîne donnée
parse :: Parser a -> String -> Resultat a
parse (MkParser p) = p


-- | Petits analyseurs utiles

-- | Parse un caractère qui vérifie une condition donnée
carCond :: (Char -> Bool) -> Parser Char
carCond cond = caractere >>= filtre
   where filtre c | cond c    = return c
                  | otherwise = echoue

-- | Parse un unique caractère
car :: Char -> Parser Char
car c = carCond (c ==)

-- | Parse une chaîne constante
chaine :: String -> Parser String
chaine ""     = return ""
chaine (c:cs) = do car c
                   chaine cs
                   return (c:cs)

-- | Combine des parseurs pour des séquences d’au moins un élément
unOuPlus :: Parser a -> Parser [a]
unOuPlus p = do r <- p
                rs <- zeroOuPlus p
                return (r:rs)

-- | Combine des parseurs pour des séquences de zéro ou plusieurs
-- éléments
zeroOuPlus :: Parser a -> Parser [a]
zeroOuPlus p = unOuPlus p ||| return []

-- | Détecte si l’analyse a réussi
reussi :: Resultat a -> Bool
reussi = isJust

-- | Détecte si l’analyse a complètement réussi (c’est-à-dire si toute
-- la chaîne a été analysée)
complet :: Resultat a -> Bool
complet (Just (_, "")) = True
complet _              = False

-- | Extrait le résultat, en cas de réussite du parseur
resultat :: Resultat a -> a
resultat (Just (r, _)) = r
