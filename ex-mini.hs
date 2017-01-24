import Aux
import Gemia
import Graphviz

p :: Gemia Int
p = makeGemia (Leaf 0) 
  [ req 0 Input "a" [1,2]
  , req 1 Input "a" [3]
  ]

q :: Gemia Int
q = makeGemia (Leaf 0) 
  [opt 0 Input "a" 1
  ]

r = rprod q p
s = pprod p q
