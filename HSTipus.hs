module HSTipus where

data Expr = Val Int                 -- Un número (42)
          | Var String              -- Una variable (x, y, suma)
          | Op BinOp Expr Expr      -- Una operació (1 + 2)
          | Not Expr                -- Not
          | App Expr Expr           -- Aplicació de funció (f x)
          | Lam String Expr       -- Funció lambda (\x -> x + 1)
          deriving (Show)

data BinOp = Add | Sub | Mul | Div          -- Aritmètiques
            | And | Or                      -- Lògiques
            | Lt | Leq | Gt | Geq | Eq      -- Comparacions
            deriving (Show)

            -- Modificar abstraccio     (Fet)
            -- Afegir operacions        (Fet)
            -- Main
                -- Dir si passa o no
                    -- Si no:
                        -- Missatges d'on està el fallo
                    -- Si sí:
                        -- Imprimir l'arbre 
            -- Reunió
            -- Inferencia tipus
            -- Reunió 2
            -- Eval lazy
            -- Super combinadors
            -- Guardes
            -- Composició
            -- Constructors
            -- Patrons

            -- Dilluns matí fins 13 i dijous 10-16

