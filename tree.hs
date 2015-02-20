--KINYOCK Valerian
--PERNET Thibaut
--Groupe3

--TP4 - Des arbres et des couleurs

--import Test.QuickCheck

--Question1

data Arbre coul val = Feuille | Noeud coul val (Arbre coul val) (Arbre coul val) deriving Show    

monArbreComplet = Noeud 'a' 1 (Noeud 'b' 2 Feuille Feuille) (Noeud 'c' 3 Feuille Feuille)
monArbre = Noeud 'R' 1 (Noeud 'B' 2 (Noeud 'B' 4 Feuille Feuille) Feuille) (Noeud 'B' 3 (Noeud 'B' 5 Feuille Feuille) Feuille)                   
                      
--Question2

mapArbre :: (a -> b) -> Arbre coul a -> Arbre coul b
mapArbre _ Feuille = Feuille
mapArbre f (Noeud coul val gauche droite) = Noeud coul (f val) (mapArbre f gauche) (mapArbre f droite)

foldArbre :: (coul -> val -> b -> b -> b) -> b -> Arbre coul val -> b
foldArbre _ r Feuille = r
foldArbre f r (Noeud coul val gauche droite) =  f coul val (foldArbre f r gauche) (foldArbre f r droite)

--Question3  
  
--Hauteur récursive
hauteur1 :: Arbre coul val -> Int
hauteur1 Feuille = 0
hauteur1 (Noeud c v g d) = max (1 + hauteur1 g) (1 + hauteur1 d)

--Hauteur fold
hauteur2 :: Arbre coul val -> Int
hauteur2 a = foldArbre (\x y z t -> (max z t) + 1) 0 a

--Taille recursive
taille1 :: Arbre coul val -> Int
taille1 Feuille = 0
taille1 (Noeud c v g d) = 1 + hauteur1 g + hauteur1 d

--Taille fold
taille2 :: Arbre coul val -> Int
taille2 a = foldArbre (\x y z t -> z + t + 1) 0 a


--Question4
peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Feuille
peigneGauche ((c,a):xs) = Noeud c a (peigneGauche xs) Feuille


--Question5
-- Elle vérifie si la longueur de la liste passé en paramètre est égale à la hauteur du peigneGauche de cette liste.
prop_hauteurPeigne xs = length xs == hauteur1 (peigneGauche xs)

--Question6
--OK

--Question7
--recursive
estComplet :: Arbre c a -> Bool
estComplet Feuille = True
estComplet (Noeud coul val g d) | taille1 g == taille1 d	= estComplet g && estComplet d
				| otherwise 			= False

--fold -- Pas OK
estCompletF :: Arbre c a -> Bool
estCompletF = foldArbre (\x y g d -> g && d) True

--Question8
-- Les peignes gauches complets sont les peignes gauches de hauteur 1


--Question9  STAND BY
complet :: Int -> [(c, a)] -> Arbre c a
complet _ [] = error "error"
complet 0 ((c,a):xs) = Noeud c a Feuille Feuille
complet h ((c,a):xs) = complet (h-1) xs

--Question10
--repeat :: a -> [a]

--repeat x is an infinite list, with x the value of every element.

repeat' :: a -> [a]
repeat' = iterate (\x -> x)   

--Question11
createUnicodeList = [((), x) | x <- ['a'..]] 

--Question12
aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille = []
aplatit (Noeud c v g d)  =  aplatit g ++ [(c,v)] ++ aplatit d

--Question13
element :: Eq a => a -> Arbre c a -> Bool
element el Feuille 					= False
element el (Noeud _ v g d) 
						| el == v   = True
						| otherwise = (element el g ) || (element el d)     
						
--Question14     
printColor :: (Show a) => a -> String
printColor c = "color: " ++ show c

printVal :: (Show a) => a -> String
printVal v = "value: " ++ show v

noeud :: (c -> String) -> (a -> String) -> (c,a) -> String 
noeud pC pV (c, v) = (pC c) ++ (pV v) 

--Question15
getVal (Noeud _ v _ _) = v

arcs :: Arbre c a -> [(a, a)]
arcs Feuille         = []
arcs (Noeud _ v Feuille Feuille) = []
arcs (Noeud _ v Feuille d)       = [(v, getVal d)] ++ arcs d
arcs (Noeud _ v g Feuille)       = [(v, getVal g)] ++ arcs g
arcs (Noeud _ v g d)             = [(v, getVal g)] ++  [(v, getVal d)] ++ arcs g ++ arcs d

--Question16
genString :: Show a => a -> String
genString = show

arc :: (a -> String) -> (a, a) -> String
arc genString (v1, v2) = genString v1 ++ " -> " ++ genString v2

--Question17
genColor :: String -> String
genColor c = "[color=" ++ id c ++ ", fontcolor=" ++ id c ++"]"  















