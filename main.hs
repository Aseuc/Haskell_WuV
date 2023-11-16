module Main where

-- BOL.hs
-- Eine einfache BOL-Implementierung in Haskell

-- Ein BOL-Typ ist entweder ein Konzept oder eine Relation
data BOLType = Concept String | Relation String

-- Ein BOL-Ausdruck ist entweder eine Variable, eine Konstante, eine Negation, eine Konjunktion, eine Disjunktion, eine Implikation, eine Äquivalenz, eine Existenzquantifizierung oder eine Allquantifizierung
data BOLExpr = Var String | Const BOLType | Not BOLExpr | And BOLExpr BOLExpr | Or BOLExpr BOLExpr | Imp BOLExpr BOLExpr | Equ BOLExpr BOLExpr | Exi String BOLExpr | All String BOLExpr

-- Eine BOL-Axiom ist entweder eine Subsumption, eine Gleichheit oder eine Ungleichheit
data BOLAxiom = Sub BOLExpr BOLExpr | Eq BOLExpr BOLExpr | Neq BOLExpr BOLExpr


-- Eine BOL-Ontologie ist eine Liste von BOL-Axiomen
type BOLOntology = [BOLAxiom]



-- Eine Funktion, die einen BOL-Typen in einen String umwandelt
showBOLType :: BOLType -> String
showBOLType (Concept c) = c
showBOLType (Relation r) = r

-- Eine Funktion, die einen BOL-Ausdruck in einen String umwandelt
showBOLExpr :: BOLExpr -> String
showBOLExpr (Var v) = v
showBOLExpr (Const c) = showBOLType c
showBOLExpr (Not e) = "¬" ++ showBOLExpr e
showBOLExpr (And e1 e2) = "(" ++ showBOLExpr e1 ++ " ∧ " ++ showBOLExpr e2 ++ ")"
showBOLExpr (Or e1 e2) = "(" ++ showBOLExpr e1 ++ " ∨ " ++ showBOLExpr e2 ++ ")"
showBOLExpr (Imp e1 e2) = "(" ++ showBOLExpr e1 ++ " → " ++ showBOLExpr e2 ++ ")"
showBOLExpr (Equ e1 e2) = "(" ++ showBOLExpr e1 ++ " ↔ " ++ showBOLExpr e2 ++ ")"
showBOLExpr (Exi v e) = "∃" ++ v ++ "." ++ showBOLExpr e
showBOLExpr (All v e) = "∀" ++ v ++ "." ++ showBOLExpr e

-- Eine Funktion, die ein BOL-Axiom in einen String umwandelt
showBOLAxiom :: BOLAxiom -> String
showBOLAxiom (Sub e1 e2) = showBOLExpr e1 ++ " ⊑ " ++ showBOLExpr e2
showBOLAxiom (Eq e1 e2) = showBOLExpr e1 ++ " = " ++ showBOLExpr e2
showBOLAxiom (Neq e1 e2) = showBOLExpr e1 ++ " ≠ " ++ showBOLExpr e2

-- Eine Funktion, die eine BOL-Ontologie in einen String umwandelt
showBOLOntology :: BOLOntology -> String
showBOLOntology [] = ""
showBOLOntology (a:as) = showBOLAxiom a ++ "\n" ++ showBOLOntology as

-- Eine Funktion, die eine BOL-Ontologie auf der Konsole ausgibt
printBOLOntology :: BOLOntology -> IO ()
printBOLOntology o = putStrLn (showBOLOntology o)

-- Ein Test
main :: IO ()
main = do 
    putStrLn "Die Beispiel-Ontologie ist:"
    printBOLOntology [Sub (Var "Schüler") (Const (Concept "Klasse"))]
    printBOLOntology [Neq (Var "Schüler") (Const (Concept "Klasse"))]
    printBOLOntology 
