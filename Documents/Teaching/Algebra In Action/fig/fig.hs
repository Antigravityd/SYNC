{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

unitC = circle 1 # fc red
reds = circle 3 <> vsep 0.5 [unitC, hsep 0.5 [unitC, unitC]] # moveOriginBy (1.25 ^& (-1.25))

unitS = square 2 # fc green
greens = circle 5 # scaleX 0.5
  <> (vsep 0.5 $ take 3 $ repeat unitS) # moveOriginBy (0 ^& (-2.5))

diag :: Diagram B
diag = hsep 3 [reds, greens]

main = mainWith diag
