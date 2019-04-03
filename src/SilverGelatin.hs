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

------------------

-- Emulsion Definitions --

data Unit = GRAM | MILLILITER
data Quantity = QUANTITY {amount :: Double, unit :: Unit}
type Temperature = Double

data Salt = KI { sQuantity :: Quantity}
  | KBr { sQuantity :: Quantity}
  | NaCl { sQuantity :: Quantity}
data Silver = SILVER { vQuantity :: Quantity}
data Acid = ACID { aQuantity :: Quantity}
data ChemicalModifier = CHEMICALMODIFIER { mQuantity :: Quantity, mWeight :: Double}

data Solution = SOLUTION {
  salts :: [Salt],
  ag :: [Silver],
  acids :: [Acid],
  other :: [ChemicalModifier],
  temp :: Temperature
}

class Chemical a where
  molecularWeight :: a -> Double

instance Chemical Salt where
  molecularWeight (KI _) = 166.0028
  molecularWeight (KBr _) = 119.002
  molecularWeight (NaCl _) = 58.44

instance Chemical Silver where
  molecularWeight (SILVER _)  = 169.87

instance Chemical ChemicalModifier where
  molecularWeight (CHEMICALMODIFIER _ m) = m

-- Kripke Definitions --

data State = NoReaction | Precipitation | Wash | PrecipitationWash | AfterRipening | Done deriving (Eq, Show, Read)

type Initial = State

data Transistion = TRANSITION {time :: Float, transition :: State -> State}

data Emulsion = EMULSION [State] State [Transistion] (State -> [Solution])
-- States, Initial State, Transitions, Labeling