module Stage where

import Physics
import qualified Solution       as S
import qualified Emulsion       as E

data Stage = SINGLEJET {receivingSolution :: S.Solution, givingSolution :: S.Pour, duration :: Minute, temperature :: Temperature}
  | DOUBLEJET {receivingSolution :: S.Solution, givingSolutions :: [S.Pour], duration :: Minute, temperature :: Temperature}
  | DIGESTION {duration :: Minute, temperature :: Temperature}
  | WASH

-- Given an emulsion, give me the precipitations
-- precipitations :: Emulsion S.Solution -> [Stage]

-- Given an emulsion, give me digestions
-- digestions :: Emulsion S.Solution -> [Stage]