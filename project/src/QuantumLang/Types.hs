module QuantumLang.Types where

-- | A parsed quantum program is an ordered list of statements.
newtype Program = Program {programStatements :: [Statement]}
  deriving (Eq, Show)

-- | Surface-syntax statements for the quantum circuit DSL.
data Statement
  = Init Int
  | Apply Gate [Int]
  | Measure Int String
  | If String [Statement]
  | Repeat Int [Statement]
  | Print String
  deriving (Eq, Show)

-- | Built-in gates. Phase takes an angle in radians.
data Gate
  = H
  | X
  | Y
  | Z
  | CNOT
  | Phase Double
  deriving (Eq, Show)
