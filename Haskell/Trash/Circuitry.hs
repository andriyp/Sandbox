import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow

type Bit = Int

type B  = Bit
type B2 = (Bit, Bit)

newtype StateM s i o
      = StateM { runStateM :: i -> (o, StateM s i o) }

instance Category (StateM s) where
  id                      = StateM $ \i -> (i, id)
  (StateM f) . (StateM g) = StateM $ \i ->
    let (gO, gSM) = g i
        (fO, fSM) = f gO
     in (fO, fSM . gSM)
          
rsT :: StateM B2 B2 B2
rsT = StateM (mkRS (0,0))
  where
    tran s   = (s, StateM (mkRS s))
    mkRS s i = case i of
      (0,0) -> tran s
      (0,1) -> tran (1,0)
      (1,0) -> tran (0,1)
      (1,1) -> error "RS-Trigger: Forbidden Input (1,1)!"

test = fst $ runStateM (snd $ runStateM rsT (1,0)) (0,1)





