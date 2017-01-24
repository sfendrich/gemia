{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- |
Module      :  $Header$ 
Description :  Implements Generalized Modal Interface Automata
Copyright   :  (c) 2013-2016 Sascha Fendrich
License     :  GPL-3

Maintainer  :  sascha.fendrich@uni-bamberg.de
Stability   :  unstable
Portability :  non-portable (TypeSynonymInstances, FlexibleInstances)

The Gemia module implements a generalization of Modal Interface Automata
where IO-assignment is done on transition level instead of action level.
At the current state of implementation, this software does not consider
internal transitions!
-}

module Gemia where

import Data.List
import Data.Ord
import Data.Function
--import LTS
import Graphviz
import Aux


-- Status of a node (errors, inconsistencies)
data Status = Status { err::Bool, incons::Bool } deriving Show

normalStatus = Status False False

-- Tail of a transition (action and targets)
data Tail n a = Tail { action::a, targets::[n] } deriving Show

-- A node with its status all its tails
data TransitionSet n a = 
  TransitionSet { source::n, status::Status, actions::[Tail n a] } deriving Show

data TransitionSystem n a = 
  TransitionSystem { start::n, trans::[TransitionSet n a]} deriving Show

-- All nodes of a transition system
nodes :: Eq n => TransitionSystem n a -> [n]
nodes tsys = nub (start tsys : concatMap f (trans tsys))
  where f tset = (source tset : concatMap targets (actions tset))

-- Remove nodes from a tail
rmNodesTail :: Eq n => [n] -> Tail n a -> Tail n a
rmNodesTail ns t = Tail (action t) ((targets t)\\ns)

-- Remove nodes from a transition set
rmNodesTransitionSet :: Eq n => [n] -> TransitionSet n a -> TransitionSet n a
rmNodesTransitionSet ns t = TransitionSet (source t) (status t) actions'
  where actions' = map (rmNodesTail ns) (actions t)

-- Remove nodes from a transition system
rmNodesTransitionSystem :: Eq n => [n] -> TransitionSystem n a -> TransitionSystem n a
rmNodesTransitionSystem ns t = TransitionSystem (start t) trans'
  where trans' = map (rmNodesTransitionSet ns) trans''
        trans'' = filter (flip notElem ns . source) (trans t)

-- Remove empty tails
rmEmptyTails :: TransitionSet n a -> TransitionSet n a
rmEmptyTails ts = TransitionSet (source ts) (status ts) actions'
  where actions' = filter (not . null . targets) (actions ts)

-- An action has a modality, an IO-type and a label
-- TODO: Action is a too general name for this
data Action m k l = Action { modality::m, ioType::k, label::l } deriving (Show)

-- Concrete instance for modalities 
data Modality = Required | Optional deriving (Eq, Ord, Show)

-- Concrete instance for IO-types
data IOType = Input | Output deriving (Eq, Ord, Show)

-- Concrete instance for nodes
type Node n = BTree n

-- Product of nodes
nprod :: Node n -> Node n -> Node n
nprod = Branch

-- All pairs of nodes
npairs = makePairsBy nprod

setErrorStatus :: TransitionSet n a -> TransitionSet n a
setErrorStatus t = TransitionSet (source t) newStatus (actions t)
  where newStatus = Status True (incons $ status t)

setErrorStates :: (Eq n) => TransitionSystem n a -> [n] -> TransitionSystem n a
setErrorStates t ns = TransitionSystem (start t) newTransitions
  where 
    newTransitions  = map changeStatus (trans t)
    changeStatus tr = if (source tr) `elem` ns then setErrorStatus tr else tr


-- General type for modal input-output transition systems
type GemiaLabel    = String
type GemiaAction   = Action Modality IOType GemiaLabel
type GemiaTail n   = Tail (Node n) GemiaAction
type GemiaTrans n  = TransitionSet (Node n) GemiaAction
type Gemia n       = TransitionSystem (Node n) GemiaAction

-- insert b at position a in [c] which is given by cmp with insert-function ins
insertByWith :: (a -> a -> Bool) -> (a -> a -> a) -> [a] -> a -> [a]
insertByWith _ ins [] a = [a]
insertByWith cmp ins (b:bs) a = if cmp a b then b':bs else b:bs'
  where b'  = ins a b 
        bs' = insertByWith cmp ins bs a

-- Add a transition
addTrans :: Eq n => Gemia n -> GemiaTrans n -> Gemia n
addTrans g t = g { trans = trans' }
  where 
    trans' = foldl addNode (insertByWith cmp ins (trans g) t) newnodes
    newnodes = concatMap targets $ actions t
    addNode ts n = insertByWith cmp ins ts (TransitionSet n normalStatus [])
    cmp      = (==) `on` source 
    ins x y  = TransitionSet (source x) (status x) (((++) `on` actions) x y)

-- Make a gemia from initial state and list of transitions
makeGemia :: Eq n => Node n -> [GemiaTrans n] -> Gemia n
makeGemia i = foldl addTrans (TransitionSystem i [])

-- Make a gemia from initial state, list of transitions and list of error states
makeGemiaErr :: Eq n => Node n -> [GemiaTrans n] -> [Node n] -> Gemia n
makeGemiaErr i ts = setErrorStates (foldl addTrans (TransitionSystem i []) ts) 

-- Make gemia tail
makeTail :: Modality -> IOType -> GemiaLabel -> [Node n] -> GemiaTail n
makeTail m k l ns = Tail (Action m k l) ns

-- Make gemia transition
makeTrans :: Modality -> Node n -> IOType -> GemiaLabel -> [Node n] -> GemiaTrans n 
makeTrans m n k l ns = TransitionSet n normalStatus [makeTail m k l ns]


-- Make gemia required transition
req :: n -> IOType -> GemiaLabel -> [n] -> GemiaTrans n
req n k l ns = makeTrans Required (Leaf n) k l (map Leaf ns)

-- Make gemia optional transition
opt :: n -> IOType -> GemiaLabel -> n -> GemiaTrans n
opt  n k l t = makeTrans Optional (Leaf n) k l [Leaf t]


-- Transform Gemias to Dot-language
instance Dotable GemiaLabel where
  toDot = id

instance Dotable IOType where
  toDot Input  = "?"
  toDot Output = "!"
  --toDot Hidden = ":"

instance Dotable n => Dotable (GemiaTrans n) where
  toDot ts = src ++ tls 
    where s    = toDot (source ts)
          src  = if null srcOptions then "" else makeNode s srcOptions
          srcOptions = incOpt ++ errOpt
          incOpt = if (incons $ status ts) 
                 then ["color=\"#888888\"","fontcolor=\"#888888\""]
                 else []
          errOpt = if (err $ status ts)
                   then ["shape=hexagon"]
                   else []
          as   = actions ts
          tls  = concatMap (arrow s) as
          arrow s a = if length trgs == 1 
            then simpleArrow s l (head trgs) tlOptions 
            else multiArrow s l trgs tlOptions
              where 
                trgs = map toDot (targets a)
                l   = (toDot $ label $ action a) ++ (toDot $ ioType $ action a)
                modOpt = if (modality $ action a) == Optional 
                         then ["style=dashed"] 
                         else []
                tlOptions = modOpt ++ incOpt

instance Dotable n => Dotable (Gemia n) where
  toDot g = "digraph {\n" ++ startNode ++ transitions ++ "}\n"
    where startNode = "\"" ++ toDot (start g) ++ "\"[shape=box,style=rounded]\n" 
          transitions = concatMap toDot (trans g)

-- All action labels of a Gemia
labels :: Gemia n -> [GemiaLabel]
labels = nub . (map (label . action)) . (concatMap actions) . trans

-- All actions of a Gemia by label
actionsByLabel :: Gemia n -> [GemiaAction]
actionsByLabel = nubBy ((==) `on` label) . (map action) . (concatMap actions) . trans

-- All targets of a node reachable
reachableLocally :: Eq n => [GemiaTrans n] -> Node n -> [Node n]
reachableLocally ts n = nub ns
  where as = concatMap actions [t|t<-ts, source t == n]
        ns = concatMap targets as

-- All targets of a node reachable by actions with label l
-- TODO: untested
reachableLocallyLabel :: Eq n => [GemiaTrans n] -> Node n -> GemiaLabel -> [Node n]
reachableLocallyLabel ts n l = nub ns
  where as = concatMap actions [t|t<-ts, source t == n]
        ns = concatMap targets (filter ((==l) . label . action) as)

reachableAux :: Eq n => [GemiaTrans n] -> ([Node n],[Node n]) -> ([Node n],[Node n])
reachableAux _ (done, []) = (done, [])
reachableAux ts (done, t:todo) = reachableAux ts (done', todo')
  where done' = t:done
        todo' = todo ++ (((reachableLocally ts t)\\done')\\todo)
        
reachable :: Eq n => [GemiaTrans n] -> Node n -> [Node n]
reachable ts n = fst (reachableAux ts ([],[n]))

-- prune empty tails
pruneEmptyTails :: Gemia n -> Gemia n
pruneEmptyTails g = TransitionSystem (start g) trans'
  where trans' = map rmEmptyTails (trans g) 

-- prune unreachable states
pruneUnreachable :: Eq n => Gemia n -> Gemia n
pruneUnreachable g = rmNodesTransitionSystem unreachable g
  where unreachable = (nodes g)\\(reachable (trans g) (start g))

-- prune inconsistent states (initial state is not pruned!)
pruneIncons :: Eq n => Gemia n -> Gemia n
pruneIncons g = if null currentIncons then pruneEmptyTails g else pruneIncons g'
  where currentIncons = map source $ filter f (trans g) 
        f = not . null . (filter incons) . actions
        incons a = ((==Required) . modality $ action a) && (null $ targets a)
        g' = rmNodesTransitionSystem currentIncons g
       

-- Alphabet extension
addLoops :: Eq n => Gemia n -> [GemiaAction] -> Gemia n
addLoops g as = foldl addTrans g (concatMap makeloops (nodes g))
  where makeloops n = map (\a->TransitionSet n normalStatus [Tail a [n]]) as
  
-- Extend gemias by their mutually foreign actions
extendMutually :: Eq n => 
  (Modality -> Modality) -> (Modality -> Modality) -> 
  (IOType -> IOType) -> (IOType -> IOType) -> 
  Gemia n -> Gemia n -> (Gemia n, Gemia n)
extendMutually m1 m2 iot1 iot2 g1 g2 = 
  (addLoops g1 actions1, addLoops g2 actions2)
  where native1  = actionsByLabel g1
        native2  = actionsByLabel g2
        foreign1 = deleteFirstsBy ((==) `on` label) native2 native1
        foreign2 = deleteFirstsBy ((==) `on` label) native1 native2
        actions1 = map (updateAction m1 iot1) foreign1
        actions2 = map (updateAction m2 iot2) foreign2
        updateAction m iot a = Action (m $ modality a) (iot $ ioType a) (label a)

-- Auxiliary function for operations where systems should not be extended
noExtend x y = (x,y)

-- Meta product
--metaprod :: Eq n => (GemiaTrans n -> GemiaTrans n -> GemiaTrans n) -> (Modality -> Modality) -> (Modality -> Modality) -> (IOType -> IOType) -> (IOType -> IOType) -> Gemia n -> Gemia n -> Gemia n
metaprod :: Eq n => (GemiaTrans n -> GemiaTrans n -> GemiaTrans n) -> (Gemia n -> Gemia n -> (Gemia n, Gemia n)) -> Gemia n -> Gemia n -> Gemia n
metaprod prodTS extend g1 g2 = TransitionSystem start12 trans12
  where 
    start12   = nprod (start g1') (start g2')
    trans12   = [prodTS n1 n2|n1<-trans g1', n2<-trans g2']
    (g1',g2') = extend g1 g2

-- parallel product with alphabet extension
pprod :: Eq n => Gemia n -> Gemia n -> Gemia n
pprod = metaprod pprodTS extend
  where 
    extend = extendMutually cr cr id id
    cr = const Required

-- local parallel product
pprodTS :: Eq n => GemiaTrans n -> GemiaTrans n -> GemiaTrans n
pprodTS ts1 ts2 = TransitionSet source12 status12 acts12
  where
    src1     = source ts1
    src2     = source ts2
    source12 = nprod src1 src2
    status12 = Status error12 incons12
    error12  = (err $ status ts1) || (err $ status ts2) 
    incons12 = (incons $ status ts1) || (incons $ status ts2) 
    acts12   = [comb tl1 tl2|tl1<-actions ts1, tl2<-actions ts2,
      (label $ action tl1)==(label $ action tl2)]
    comb tl1 tl2 = Tail (Action m k l) trgs
      where m = max (modality $ action tl1) (modality $ action tl2)
            k = max (ioType $ action tl1) (ioType $ action tl2)
            l = label $ action tl1
            trgs = makePairsBy nprod (targets tl1) (targets tl2)

-- pseudo quotient without compatibility
iquot :: Eq n => Gemia n -> Gemia n -> Gemia n 
iquot = metaprod iquotTS extend
  where
    extend = extendMutually cr cr id (const Input)
    cr     = const Required
  -- TODO: extending quotient is not correct

-- local pseudo quotient
iquotTS :: Eq n => GemiaTrans n -> GemiaTrans n -> GemiaTrans n
iquotTS ts1 ts2 = TransitionSet source12 status12 acts12
  where
    source12 = nprod (source ts1) (source ts2)
    status12 = Status False False -- TODO: setup status correctly
    acts12   = [comb tl1 tl2 | tl1<-actions ts1, tl2<-actions ts2,
      (label $ action tl1) == (label $ action tl2)]
    comb tl1 tl2 = Tail act trgs
      where
        act = Action (m m1 m2) (k k1 k2) (label $ action tl1)
        m1  = modality $ action tl1
        m2  = modality $ action tl2
        m mA mB = if k2 == Output then Required else max mA mB
        k1  = ioType $ action tl1
        k2  = ioType $ action tl2
        k kA kB = if kA == kB then Input else Output
        trgs = makePairsBy nprod (targets tl1) (targets tl2)

-- Required and optional actions
reqOnly, optOnly :: [GemiaTail n] -> [GemiaTail n]
reqOnly = filter ((==Required) . modality . action)
optOnly = filter ((==Optional) . modality . action)

-- conjunctive product with alphabet extension
cprod :: Eq n => Gemia n -> Gemia n -> Gemia n
cprod = metaprod cprodTS extend
  where 
    extend = extendMutually co co id id
    co     = const Optional

-- local conjunctive product
cprodTS :: Eq n => GemiaTrans n -> GemiaTrans n -> GemiaTrans n
cprodTS tsA tsB = TransitionSet sourceAB statusAB actsAB
  where 
    sourceAB = nprod (source tsA) (source tsB) 
    statusAB = Status errorAB inconsAB
    errorAB  = (err $ status tsA) && (err $ status tsB) 
    inconsAB = (incons $ status tsA) || (incons $ status tsB) || localIncons
    localIncons = not $ null (filter (null . targets) reqs)
    actsAB   = reqs ++ opts 
    reqs     = (map (f nprod tsB) reqsA) ++ (map (f (flip nprod) tsA) reqsB)
    reqsA    = reqOnly (actions tsA)
    reqsB    = reqOnly (actions tsB)
    f comb ts2 tail1 = Tail (action tail1) 
      [comb trg1 trg2 | trg1<-targets tail1, 
        trg2<-reachableLocallyLabel [ts2] (source ts2) (label $ action tail1)] 
    optsA    = optOnly (actions tsA)
    optsB    = optOnly (actions tsB)
    opts     = [Tail (action tailA) 
                 (makePairsBy nprod (targets tailA) (targets tailB)) |
                 tailA<-optsA, tailB<-optsB,
                 (label $ action tailA) == (label $ action tailB)]

-- refinement product
rprod :: Eq n => Gemia n -> Gemia n -> Gemia n
rprod = metaprod rprodTS extend
  where
    extend = extendMutually co co id id
    co     = const Optional

-- local refinement product
-- TODO: The refinement product is incorrect for disjunctive transitions.
-- TODO: When we construct P<Q and p has disjunctive a-transitions to
-- TODO: P_i, i=1,...,n, then each transition q-a->Q' will result
-- TODO: in a DNF condition consisting of Q^P_i clauses à P_i conjuncts.
-- TODO: Unfolding such a condition via distributivity into a CNF (which
-- TODO: is what dMTS represents) results in a formula consisting of
-- TODO: product_i P_i^(Q^P-i) clauses, which is a hyperexponential blowup.
rprodTS :: Eq n => GemiaTrans n -> GemiaTrans n -> GemiaTrans n
rprodTS tsA tsB = TransitionSet sourceAB statusAB actsAB
  where
    sourceAB = nprod (source tsA) (source tsB)
    statusAB = Status errorAB inconsAB
    errorAB  = (err $ status tsA) && (err $ status tsB) 
    inconsAB = (incons $ status tsB) || localIncons
    localIncons = not $ null (filter (null . targets) actsAB)
    actsAB   = (map (g nprod tsB) (actions tsA))     -- required by A  
                 ++ (map (f (flip nprod) tsA) reqsB) -- required by B
    reqsA    = reqOnly (actions tsA)
    reqsB    = reqOnly (actions tsB)
    g comb ts2 tail1 = Tail (action tail1){modality = Required}
      [comb trg1 trg2 | trg1<-targets tail1,
        trg2<-reachableLocallyLabel [ts2] (source ts2) (label $ action tail1)]
    f comb ts2 tail1 = Tail (action tail1)
      [comb trg1 trg2 | trg1<-targets tail1,
        trg2<-reachableLocallyLabel [ts2'] (source ts2) (label $ action tail1)]
      where ts2' = TransitionSet (source ts2) (status ts2) (reqOnly (actions ts2))

