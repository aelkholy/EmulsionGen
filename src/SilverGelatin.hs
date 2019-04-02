module SilverGelatin where
-- http://www.tcs.hut.fi/Studies/T-79.186/2004/lecture3.pdf
-- Let AP be a non-empty set of atomic propositions A Kripke structure is a tuple M = (S,s^0,R,L), Where
--  S is a finite set of states
--  s^0∈S is an initial state
--  R:S×S: is a transition relation, for which it holds that ∀s∈S:∃s′∈S:(s,s′)∈R and
--  L:S→2^(AP): is labeling, a function which labels each state with the atomic propositions which hold in that state.

-- A path π in a Kripke structure M = (S,s^0,R,L) is an infinite sequence of states,
-- π=s0 s1 s2..., such that s0∈I and T(si, si+1)for all i≥0.

-- States are emulsion making steps
-- Initial states are unmixed states
-- transitions are mixing things
-- labelling function is the properties (propositions) at each state. Temperature, reactant quantities, etc.

-- Emulsion
data Salt = KI | KBr | NaCl deriving (Eq, Show, Read, Ord) -- order from solubility and reaction preference.

molecularWeight :: Salt -> Float
molecularWeight KI = 166.0028
molecularWeight KBr = 119.002
molecularWeight NaCl = 58.44

-- Kripke
data State = Gelatin | Precipitation | Wash | PrecipitationWash | AfterRipening | Done deriving (Eq, Show, Read)

type Initial = State

transitions :: a -> State -> State

temperature :: a -> State -> Float
time :: a -> State -> Int
silverAdded :: a -> State -> Float
saltAdded :: a -> State -> Salt -> Float
waterAdded :: a -> State -> Float
chemicalsAdded :: a -> State -> [String]