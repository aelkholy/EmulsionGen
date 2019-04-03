module SilverGelatin where
-- http://www.tcs.hut.fi/Studies/T-79.186/2004/lecture3.pdf
-- Let AP be a non-empty set of atomic propositions A Kripke structure is a tuple M = (S,s^0,R,L), Where
--  S is a finite set of states
--  s^0 in S is an initial state
--  R:S X S: is a transition relation, for which it holds that for all s in S: there exists s` in S:(s,s`) in R and
--  L:S->2^(AP): is labeling, a function which labels each state with the atomic propositions which hold in that state.

-- A path pi in a Kripke structure M = (S,s^0,R,L) is an infinite sequence of states,
-- pi=s0 s1 s2..., such that s0 in I and T(si, si+1)for all i>=0.

-- States are emulsion making steps
-- Initial states are unmixed states
-- transitions are mixing things
-- labelling function is the properties (propositions) at each state. Temperature, reactant quantities, etc.

-- Emulsion

type Temperature = Float

data Quantity = Grams {amount :: Float} | Ml {amount :: Float} deriving (Eq, Show, Read)  -- quantity, quantity

type Water = Quantity
type KI = Quantity
type KBr = Quantity
type NaCl = Quantity

type Silver = Quantity
data ChemicalModifier = ChemicalModifier {name :: String, molWeight :: Float, quantity :: Quantity} deriving (Eq, Show, Read) -- Amt, Name, Molecular weight
type Acid = ChemicalModifier
type Modifier = ChemicalModifier

data Salt = KI | KBr | NaCl deriving (Eq, Show, Read, Ord) -- order from solubility and reaction preference.

data Chemical = Salt Salt | Acid Acid | Modifier Modifier | Silver Silver | Water Water

modifierMW :: Modifier -> Float
modifierMW = molWeight

molecularWeight :: Chemical -> Float
molecularWeight (Salt x) = (case x of KI -> 166.0028
                                      KBr -> 119.002
                                      NaCl -> 58.44)
molecularWeight (Acid x) = modifierMW x
molecularWeight (Modifier x) = modifierMW x 
molecularWeight (Silver x) = 169.87
molecularWeight (Water x) = 18.01528

-- solubility :: Salt -> Float

-- Kripke
data State = Gelatin  | Precipitation | Wash | PrecipitationWash | AfterRipening | Done deriving (Eq, Show, Read)

type Initial = State

data Transistion = Transition {time :: Float, transition :: State -> State}

data Emulsion = Emulsion [State] State [Transistion] (State -> [Chemical]) (State -> Temperature)
-- States, Initial State, Transitions, Labeling, Labeling