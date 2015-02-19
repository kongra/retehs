{-# LANGUAGE    Trustworthy           #-}
{-# LANGUAGE    TypeSynonymInstances  #-}
{-# LANGUAGE    FlexibleInstances     #-}
{-# LANGUAGE    MultiParamTypeClasses #-}
{-# LANGUAGE    RankNTypes            #-}
{-# OPTIONS_GHC -W -Wall              #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Print
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-11-14
-- Reworked    : 2015-02-09
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : requires stm
--
-- Textual visualization of Rete network and data.
------------------------------------------------------------------------
module AI.Rete.Print
    (
      -- * Print methods
      toShowS
    , toString

      -- * The 'Depth' constraints of the tree traversal process
    , Depth
    , depth
    , boundless

      -- * 'Switch'es
    , Switch
    , with, no, clear

      -- * Predefined compound 'Switch'es
    , netTopDown
    , netBottomUp
    , nonVerboseData

      -- * Predefined 'Switch'es
    , withNet
    , noNet
    , withData
    , noData
    , up
    , down
    , withIds
    , noIds

      -- * 'Flag's (detailed)
    , Flag (..)
    )
    where

import           AI.Rete.Data
import           AI.Rete.Flow
import           Control.Concurrent.STM
import           Control.Monad (liftM)
import           Data.Foldable (Foldable)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)
import           Data.List (intersperse)
import           Data.Maybe (catMaybes)
import           Data.Tree.Print
import           Kask.Control.Monad (toListM, mapMM)
import           Kask.Data.Function (compose, rcompose)

-- CONFIGURATION

-- | A Boolean (semanticaly) configuration option for the printing
-- process.
data Flag =
  -- Emph flag
  NetEmph | DataEmph

  -- Wme flags
  | WmeIds | WmeSymbolic | WmeAmems | WmeToks | WmeNegJoinResults

  -- Token flags
  | TokIds      | TokWmes           | TokWmesSymbolic | TokNodes | TokParents
  | TokChildren | TokNegJoinResults

  -- Amem flags
  | AmemFields | AmemRefCounts | AmemWmes | AmemSuccessors

  -- Node flags
  | NodeIds | NodeParents | NodeChildren

  -- Unlinking flags
  | Uls

  -- Bmem flags
  | BmemToks

  -- JoinNode flags
  | JoinTests  | JoinAmems | JoinNearestAncestors

  -- NegNode flags
  | NegTests   | NegAmems  | NegNearestAncestors | NegToks

  -- Prod flags
  | ProdBindings | ProdToks deriving (Show, Eq)

flagCode :: Flag -> Int
flagCode NetEmph              = 0
flagCode DataEmph             = 1
flagCode WmeIds               = 2
flagCode WmeSymbolic          = 3
flagCode WmeAmems             = 4
flagCode WmeToks              = 5
flagCode WmeNegJoinResults    = 6
flagCode TokIds               = 7
flagCode TokWmes              = 8
flagCode TokWmesSymbolic      = 9
flagCode TokNodes             = 10
flagCode TokParents           = 11
flagCode TokChildren          = 12
flagCode TokNegJoinResults    = 13
flagCode AmemFields           = 14
flagCode AmemRefCounts        = 15
flagCode AmemWmes             = 16
flagCode AmemSuccessors       = 17
flagCode NodeIds              = 18
flagCode NodeParents          = 19
flagCode NodeChildren         = 20
flagCode Uls                  = 21
flagCode BmemToks             = 22
flagCode JoinTests            = 23
flagCode JoinAmems            = 24
flagCode JoinNearestAncestors = 25
flagCode NegTests             = 26
flagCode NegAmems             = 27
flagCode NegNearestAncestors  = 28
flagCode NegToks              = 29
flagCode ProdBindings         = 30
flagCode ProdToks             = 31
{-# INLINE flagCode #-}

instance Hashable Flag where
  hashWithSalt salt flag = salt `hashWithSalt` flagCode flag

-- | A set of 'Flags's.
type Flags = Set.HashSet Flag

-- | A switch to turn the 'Flag's on/off.
type Switch = Flags -> Flags

-- | Creates a 'Switch' that turns the 'Flag' on.
with :: Flag -> Switch
with = Set.insert
{-# INLINE with #-}

-- | Creates a 'Switch' that turns the 'Flag' off.
no :: Flag -> Switch
no = Set.delete
{-# INLINE no #-}

-- | Creates a 'Switch' that turns all flags off.
clear :: Switch
clear _ = noFlags

-- | Asks whether the 'Flag' is on in 'Flags'.
is :: Flag -> Flags -> Bool
is = Set.member
{-# INLINE is #-}

-- | A set of 'Flag's with all 'Flag's turned off.
noFlags :: Flags
noFlags = Set.empty

-- PREDEFINED Switch CONFIGURATIONS

dataFlags :: [Flag]
dataFlags =  [ WmeToks
             , WmeNegJoinResults
             , TokWmes
             , TokParents
             , TokChildren
             , TokNegJoinResults
             , AmemWmes
             , BmemToks
             , NegToks
             , ProdToks ]

netFlags :: [Flag]
netFlags =  [ WmeAmems
            , TokNodes
            , AmemRefCounts
            , AmemSuccessors
            , NodeParents
            , NodeChildren
            , JoinTests
            , JoinAmems
            , JoinNearestAncestors
            , NegTests
            , NegAmems
            , NegNearestAncestors
            , ProdBindings ]

idFlags :: [Flag]
idFlags =  [WmeIds, TokIds, NodeIds]

-- | A 'Switch' that turns data presentation off.
noData :: Switch
noData = compose (map no dataFlags)

-- | A 'Switch' that turns data presentation on.
withData :: Switch
withData = compose (map with dataFlags)

-- | A 'Switch' that turns network presentation off.
noNet :: Switch
noNet = compose (map no netFlags)

-- | A 'Switch' that turns network presentation on.
withNet :: Switch
withNet = compose (map with netFlags)

-- | A 'Switch' that imposes the presentation traversal from lower
-- nodes to higher.
up :: Switch
up = with NodeParents . no NodeChildren . no AmemSuccessors

-- | A 'Switch' that imposes the presentation traversal from higher
-- nodes to lower.
down :: Switch
down = with NodeChildren . no AmemSuccessors . no NodeParents

-- | A 'Switch' that turns IDs presentation off.
noIds :: Switch
noIds = compose (map no idFlags)

-- | A 'Switch' that turns Ids presentation on.
withIds :: Switch
withIds = compose (map with idFlags)

-- DEFENDING AGAINST CYCLES

data Visited = Visited { visitedWmes  :: !(Set.HashSet Wme )

                       , visitedBtoks :: !(Set.HashSet Btok)
                       , visitedNtoks :: !(Set.HashSet Ntok)
                       , visitedPtoks :: !(Set.HashSet Ptok)

                       , visitedAmems :: !(Set.HashSet Amem)

                       , visitedBmems :: !(Set.HashSet Bmem)
                       , visitedJoins :: !(Set.HashSet Join)
                       , visitedNegs  :: !(Set.HashSet Neg )
                       , visitedProds :: !(Set.HashSet Prod)

                       , visitedDtn   :: !Bool }

cleanVisited :: Visited
cleanVisited =  Visited { visitedWmes  = Set.empty
                        , visitedBtoks = Set.empty
                        , visitedNtoks = Set.empty
                        , visitedPtoks = Set.empty
                        , visitedAmems = Set.empty
                        , visitedBmems = Set.empty
                        , visitedJoins = Set.empty
                        , visitedNegs  = Set.empty
                        , visitedProds = Set.empty
                        , visitedDtn   = False }

class Visitable a where
  visiting :: a -> Visited -> Visited
  visited  :: a -> Visited -> Bool

instance Visitable Wme where
  visiting wme vs = vs { visitedWmes = Set.insert wme (visitedWmes vs) }
  visited  wme vs = Set.member wme (visitedWmes vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Btok where
  visiting btok vs = vs { visitedBtoks = Set.insert btok (visitedBtoks vs) }
  visited  btok vs = Set.member btok (visitedBtoks vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Ntok where
  visiting ntok vs = vs { visitedNtoks = Set.insert ntok (visitedNtoks vs) }
  visited  ntok vs = Set.member ntok (visitedNtoks vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Ptok where
  visiting ptok vs = vs { visitedPtoks = Set.insert ptok (visitedPtoks vs) }
  visited  ptok vs = Set.member ptok (visitedPtoks vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Amem where
  visiting amem vs = vs { visitedAmems = Set.insert amem (visitedAmems vs) }
  visited  amem vs = Set.member amem (visitedAmems vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Bmem where
  visiting bmem vs = vs { visitedBmems = Set.insert bmem (visitedBmems vs) }
  visited  bmem vs = Set.member bmem (visitedBmems vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Join where
  visiting join vs = vs { visitedJoins = Set.insert join (visitedJoins vs) }
  visited  join vs = Set.member join (visitedJoins vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Neg where
  visiting neg vs = vs { visitedNegs = Set.insert neg (visitedNegs vs) }
  visited  neg vs = Set.member neg (visitedNegs vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Prod where
  visiting prod vs = vs { visitedProds = Set.insert prod (visitedProds vs) }
  visited  prod vs = Set.member prod (visitedProds vs)
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Dtn where
  visiting _ vs = vs { visitedDtn = True }
  visited  _    = visitedDtn
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable Location where
  visiting _ vs = vs  -- no need to ever mark Locations as visited
  visited  _ _  = False
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

instance Visitable JoinTest where
  visiting _ vs = vs  -- no need to ever mark JoinTest as visited
  visited  _ _  = False
  {-# INLINE visiting #-}
  {-# INLINE visited  #-}

withEllipsis :: Bool -> ShowS -> STM ShowS
withEllipsis False s = return s
withEllipsis True  s = return (compose [s, showString " ..."])
{-# INLINE withEllipsis #-}

withEllipsisT :: Bool -> STM ShowS -> STM ShowS
withEllipsisT v s = s >>= withEllipsis v
{-# INLINE withEllipsisT #-}

whenNot :: Bool -> STM [Vn] -> STM [Vn]
whenNot True  _   = return []
whenNot False vns = vns
{-# INLINE whenNot #-}

-- Vns (VISUALIZATION NODEs)

type VnShow = Flags -> Visited -> STM ShowS
type VnAdjs = Flags -> Visited -> STM [Vn]

-- | Represents types whose values are convertible to Vn.
class Vnable a where
  toVnShow :: a -> VnShow
  toVnAdjs :: a -> VnAdjs

-- | Visualization node.
data Vn = Vn { vnShowM   :: !VnShow
             , vnAdjs    :: !VnAdjs
             , vnVisited :: !Visited }

-- | Converts the passed object to a Vn.
toVn :: Vnable a => Visited -> a -> Vn
toVn vs x = Vn { vnShowM   = toVnShow x
               , vnAdjs    = toVnAdjs x
               , vnVisited = vs }
{-# INLINE toVn #-}

instance ShowM STM Flags Vn where
  showM flags Vn { vnShowM = f, vnVisited = vs } = f flags vs
  {-# INLINE showM #-}

-- SPECIFIC Vns

-- | Creates a Vn that has no adjs - thus is a leaf.
leafVn :: Visited -> VnShow -> Vn
leafVn vs show' = Vn { vnShowM   = show'
                     , vnAdjs    = \_ _ -> return []
                     , vnVisited = vs }
{-# INLINE leafVn #-}

-- | Creates a label Vn with passed adjs.
labelVn :: Visited -> ShowS -> [Vn] -> Vn
labelVn vs label adjs' = Vn { vnShowM   = \_ _ -> return label
                            , vnAdjs    = \_ _ -> return adjs'
                            , vnVisited = vs }
{-# INLINE labelVn #-}

-- | Creates a Vn that represents a label with a sequence of leaf
-- subnodes (Vns).
labeledLeavesVn :: Visited -> ShowS -> [VnShow] -> Vn
labeledLeavesVn vs label shows' = labelVn vs label (map (leafVn vs) shows')
{-# INLINE labeledLeavesVn #-}

-- | Generates a ShowS representation of an Id.
idS :: Id -> ShowS
idS id' = compose [showString " ", showString $ show id']
{-# INLINE idS #-}

-- | Composes the passed ShowS with a representation of the Id.
withIdS :: ShowS -> Id -> ShowS
withIdS s id' = compose [s, idS id']
{-# INLINE withIdS #-}

-- | Works like withIdS, but uses the Id representation optionally,
-- depending on the passed flag.
withOptIdS :: Bool -> ShowS -> Id -> ShowS
withOptIdS False s _   = s
withOptIdS True  s id' = s `withIdS` id'
{-# INLINE withOptIdS #-}

-- | Converts the monadic foldable into a sequence of Vns. All in the
-- m monad.
toVnsM :: (Monad m, Foldable f, Vnable a) => Visited -> m (f a) -> m [Vn]
toVnsM vs = liftM (map (toVn vs)) . toListM
{-# INLINE toVnsM #-}

-- | Works like toVnsM, but returns a list of VnShows instead of Vns.
toVnShowsM :: (Monad m, Foldable f, Vnable a) => m (f a) -> m [VnShow]
toVnShowsM = liftM (map toVnShow) . toListM
{-# INLINE toVnShowsM #-}

type OptLabelVn = (Monad m, Foldable f, Vnable a)
               => Bool -> String -> Visited -> m (f a) -> m (Maybe Vn)

-- | Returns an optional Vn that represents a label with a
-- sub-sequence of adjs (Vns).
optLabeledVn :: OptLabelVn
optLabeledVn False _     _  _  = return Nothing
optLabeledVn True  label vs adjs' = do
  vns <- toVnsM vs adjs'
  if null vns
     then return Nothing
     else return (Just (labelVn vs (showString label) vns))
{-# INLINE optLabeledVn #-}

-- | Returns an optional Vn that represents a label with a
-- sub-sequence of leaf adjs (Vns).
optLabeledLeavesVn :: OptLabelVn
optLabeledLeavesVn False _     _  _  = return Nothing
optLabeledLeavesVn True  label vs xs = do
  shows' <- toVnShowsM xs
  if null shows'
     then return Nothing
     else return (Just (labeledLeavesVn vs (showString label) shows'))
{-# INLINE optLabeledLeavesVn #-}

-- | Strips off Nothings out of the input collection of Maybe Vns.
optVns :: Monad m => [Maybe Vn] -> m [Vn]
optVns = return . catMaybes
{-# INLINE optVns #-}

-- | Creates a node with an emphasis on the network structure.
netVn :: Flags -> OptLabelVn
netVn flags = if is NetEmph flags then optLabeledVn else optLabeledLeavesVn
{-# INLINE netVn #-}

-- | Creates a node with an emphasis on the data.
datVn :: Flags -> OptLabelVn
datVn flags = if is DataEmph flags then optLabeledVn else optLabeledLeavesVn
{-# INLINE datVn #-}

-- CONFIGURATION

type VConf = Conf STM ShowS Flags Vn

conf :: VConf
conf = Conf { impl     = stmImpl
            , adjs     = \flags Vn { vnAdjs = f, vnVisited = vs } -> f flags vs
            , maxDepth = Nothing
            , opts     = noFlags }

-- | A specifier of depth of the treePrint process.
type Depth = VConf -> VConf

-- | Sets the maxDepth of a configuration to the specified value.
depth :: Int -> Depth
depth d c = c { maxDepth = Just d }
{-# INLINE depth #-}

-- | Unlimits the maxDepth of a configuration.
boundless :: Depth
boundless c = c { maxDepth = Nothing }
{-# INLINE boundless #-}

applySwitch :: Switch -> VConf -> VConf
applySwitch switch c@Conf { opts = opts' } = c { opts = switch opts' }
{-# INLINE applySwitch #-}

-- STM IMPL

stmImpl :: Impl STM ShowS
stmImpl = str

-- WMES VIS.

instance Vnable Wme where
  toVnShow = showWme
  toVnAdjs = wmeAdjs
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

showWme :: Wme -> Flags -> Visited -> STM ShowS
showWme wme flags vs =
  withEllipsis (visited wme vs) $
    if is WmeSymbolic flags
      then showWmeSymbolic                wme
      else showWmeExplicit (is WmeIds flags) wme
{-# INLINE showWme #-}

showWmeSymbolic :: Wme -> ShowS
showWmeSymbolic wme = compose [showString "w", shows $ wmeId wme]
{-# INLINE showWmeSymbolic #-}

showWmeExplicit :: Bool -> Wme -> ShowS
showWmeExplicit oid
  Wme { wmeId = id', wmeObj = obj, wmeAttr = attr, wmeVal = val } =
    withOptIdS oid
      (compose [ showString "("
               , shows obj,  showString ","
               , shows attr, showString ",", shows val
               , showString ")"])
      id'
{-# INLINE showWmeExplicit #-}

showWmeMaybe :: (Wme -> ShowS) -> Maybe Wme -> ShowS
showWmeMaybe _ Nothing    = showString "_"
showWmeMaybe f (Just wme) = f wme
{-# INLINE showWmeMaybe #-}

wmeAdjs :: Wme -> Flags -> Visited -> STM [Vn]
wmeAdjs
  wme@Wme { wmeAmems                = amems
          , wmeToks                 = toks
          , wmeNegJoinResults       = jresults} flags vs =
    whenNot (visited wme vs) $ do
      let vs' = visiting wme vs
      amemsVn <- netVn flags (is WmeAmems flags) "amems" vs' (readTVar amems)
      toksVn  <- datVn flags (is WmeToks  flags) "toks"  vs' (readTVar toks)
      njrsVn  <- datVn flags (is WmeNegJoinResults flags) "njrs (owners)" vs'
                 -- When visualizing the negative join results we only
                 -- show the owner tokens, cause wme in every negative join
                 -- result is this wme.
                 (mapMM (return . njrOwner) (toListT jresults))

      optVns [amemsVn, toksVn, njrsVn]

-- GENERALIZED TOKEN VIS.

instance Vnable WmeTok where
  toVnAdjs (BmemWmeTok btok ) = toVnAdjs btok
  toVnAdjs (NegWmeTok  ntok ) = toVnAdjs ntok
  toVnAdjs (ProdWmeTok ptok ) = toVnAdjs ptok

  toVnShow (BmemWmeTok btok ) = toVnShow btok
  toVnShow (NegWmeTok  ntok ) = toVnShow ntok
  toVnShow (ProdWmeTok ptok ) = toVnShow ptok
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

instance Vnable (Either Dtt Btok) where
  toVnAdjs (Left  dtt ) = toVnAdjs dtt
  toVnAdjs (Right btok) = toVnAdjs btok

  toVnShow (Left  dtt ) = toVnShow dtt
  toVnShow (Right btok) = toVnShow btok
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

instance Vnable (Either JoinTok Ntok) where
  toVnAdjs (Left  jtok) = toVnAdjs jtok
  toVnAdjs (Right ntok) = toVnAdjs ntok

  toVnShow (Left  jtok) = toVnShow jtok
  toVnShow (Right ntok) = toVnShow ntok
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

instance Vnable (Either Ntok Ptok) where
  toVnAdjs (Left  ntok) = toVnAdjs ntok
  toVnAdjs (Right ptok) = toVnAdjs ptok

  toVnShow (Left  ntok) = toVnShow ntok
  toVnShow (Right ptok) = toVnShow ptok
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

showTokWmesSymbolic :: TokWmes a => a -> ShowS
showTokWmesSymbolic = showTokWmes showWmeSymbolic
{-# INLINE showTokWmesSymbolic #-}

showTokWmesExplicit :: TokWmes a => Bool -> a -> ShowS
showTokWmesExplicit owmeids = showTokWmes (showWmeExplicit owmeids)
{-# INLINE showTokWmesExplicit #-}

showTokWmes :: TokWmes a => (Wme -> ShowS) -> a -> ShowS
showTokWmes f = rcompose
              . intersperse (showString ",")
              . map (showWmeMaybe f)
              . tokWmes
{-# INLINE showTokWmes #-}

showTok :: (Visitable a, TokWmes a)
        => (a -> Id) -> a -> Flags -> Visited -> STM ShowS
showTok tokId tok flags vs = do
  let s = if is TokWmes flags
            then compose [ showString "{"
                         , if is TokWmesSymbolic flags
                             then showTokWmesSymbolic tok
                             else showTokWmesExplicit (is WmeIds flags) tok
                         , showString "}"]

            else showString "{..}"
  withEllipsis (visited tok vs) $ withOptIdS (is TokIds flags) s (tokId tok)
{-# INLINE showTok #-}

tokAdjs :: (Vnable n, Vnable p, Vnable c, Foldable f) =>
           Flags -> Visited -> n -> p -> TVar (f c)
           -> STM (Maybe Vn, Maybe Vn, Maybe Vn)
tokAdjs flags vs' node parent children = do
  nVn <- netVn flags (is TokNodes    flags) "node"     vs' (return   [node]  )
  pVn <- datVn flags (is TokParents  flags) "parent"   vs' (return   [parent])
  cVn <- datVn flags (is TokChildren flags) "children" vs' (readTVar children)
  return (nVn, pVn, cVn)

-- DTT VIS.

instance Vnable Dtt where
  toVnAdjs _ _  _ = return []
  toVnShow _ fs _ = return (withOptIdS (is TokIds fs) (showString "{}") (-1))
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

-- BTOK VIS.

instance Vnable Btok where
  toVnAdjs = btokAdjs
  toVnShow = showTok btokId
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

btokAdjs :: Btok -> Flags -> Visited -> STM [Vn]
btokAdjs btok flags vs = whenNot (visited btok vs) $ do
  let vs'      = visiting     btok vs
      node     = btokNode     btok
      parent   = btokParent   btok
      children = btokChildren btok

  (nVn, pVn, cVn) <- tokAdjs flags vs' node parent children
  optVns [nVn, pVn, cVn]

-- NTOK VIS.

instance Vnable Ntok where
  toVnAdjs = ntokAdjs
  toVnShow = showTok ntokId
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

ntokAdjs :: Ntok -> Flags -> Visited -> STM [Vn]
ntokAdjs ntok flags vs = whenNot (visited ntok vs) $ do
  let vs'      = visiting           ntok vs
      node     = ntokNode           ntok
      parent   = ntokParent         ntok
      children = ntokChildren       ntok
      jresults = ntokNegJoinResults ntok

  (nVn, pVn, cVn) <- tokAdjs flags vs' node parent children
  njrsVn          <- datVn flags (is TokNegJoinResults flags)
                     "njrs (wmes)" vs'
                     -- When visualizing the negative join results we only
                     -- show the owner wmes, cause owner in every negative join
                     -- result is this tok.
                     (mapMM (return . njrWme) (toListT jresults))

  optVns [nVn, pVn, cVn, njrsVn]
{-# INLINE ntokAdjs #-}

-- PTOK VIS.

instance Vnable Ptok where
  toVnAdjs = ptokAdjs
  toVnShow = showTok ptokId
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

ptokAdjs :: Ptok -> Flags -> Visited -> STM [Vn]
ptokAdjs ptok flags vs = whenNot (visited ptok vs) $ do
  let vs'      = visiting     ptok vs
      node     = ptokNode     ptok
      parent   = ptokParent   ptok

  nVn <- netVn flags (is TokNodes    flags) "node"   vs' (return [node]  )
  pVn <- datVn flags (is TokParents  flags) "parent" vs' (return [parent])
  optVns [nVn, pVn]

-- AMEMS VISUALIZATION

instance Vnable Amem where
  toVnAdjs = amemAdjs
  toVnShow = showAmem
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

showAmem :: Amem -> Flags -> Visited -> STM ShowS
showAmem amem flags vs = do
  let (Obj  o) = amemObj    amem
      (Attr a) = amemAttr   amem
      (Val  v) = amemVal    amem
      alpha    = showString "A"
      repr     = if is AmemFields flags
                   then compose [ alpha   , showString " ("
                                , shows o , showString ","
                                , shows a , showString ","
                                , shows v , showString ")"]
                   else alpha
  withEllipsisT (visited amem vs) $
    if is AmemRefCounts flags
      then (do rc <- readTVar (amemRefCount amem)
               return $ compose [repr, showString " rc ", shows rc])
      else return repr
{-# INLINE showAmem #-}

amemAdjs :: Amem -> Flags -> Visited -> STM [Vn]
amemAdjs amem flags vs = whenNot (visited amem vs) $ do
  let vs'   = visiting       amem vs
      succs = amemSuccessors amem
      wmes  = amemWmes       amem
  succVn <- netVn flags (is AmemSuccessors flags) "succs" vs' (readTVar succs)
  wmesVn <- datVn flags (is AmemWmes       flags) "wmes"  vs' (readTVar wmes )
  optVns [succVn, wmesVn]

instance Vnable AmemSuccessor where
  toVnAdjs (JoinSuccessor join) = toVnAdjs join
  toVnAdjs (NegSuccessor  neg ) = toVnAdjs neg

  toVnShow (JoinSuccessor join) = toVnShow join
  toVnShow (NegSuccessor  neg ) = toVnShow neg
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

-- BMEM VIS.

instance Vnable Bmem where
  toVnAdjs = bmemAdjs
  toVnShow = showBmem
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

showBmem :: Bmem -> Flags -> Visited -> STM ShowS
showBmem bmem flags vs = withEllipsis (visited bmem vs) $
  withOptIdS (is NodeIds flags) (showString "B") (bmemId bmem)
{-# INLINE showBmem #-}

bmemAdjs :: Bmem -> Flags -> Visited -> STM [Vn]
bmemAdjs bmem flags vs = whenNot (visited bmem vs) $ do
  let vs'    = visiting   bmem vs
      parent = bmemParent bmem
      toks   = bmemToks   bmem

  pVn   <- netVn flags (is NodeParents flags) "parent" vs' (return [parent])
  tVn   <- datVn flags (is BmemToks    flags) "toks"   vs' (readTVar toks)

  allC  <- liftM Map.elems (readTVar (bmemAllChildren bmem))
  c     <- readTVar (bmemChildren bmem)
  let c' = Set.fromList allC `Set.union` c
  cVn   <- netVn flags (is NodeChildren flags) "children (all)" vs' (return c')

  optVns [pVn, cVn, tVn]

-- DTN VIS.

instance Vnable Dtn where
  toVnAdjs       = dtnAdjs
  toVnShow _ _ _ = return (showString "DTN")
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

dtnAdjs :: Dtn -> Flags -> Visited -> STM [Vn]
dtnAdjs dtn flags vs = whenNot (visited dtn vs) $ do
  let vs'      = visiting dtn vs
      children = liftM Map.elems (readTVar (dtnAllChildren dtn))
  cVn   <- netVn flags (is NodeChildren flags) "children (all)" vs' children
  optVns [cVn]

-- JOIN VIS.

instance Vnable Join where
  toVnAdjs = joinAdjs
  toVnShow = showJoin
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

ulSign :: Bool -> Char
ulSign True  = '-'  -- unlinked
ulSign False = '+'  -- linked
{-# INLINE ulSign #-}

ulMark :: TVar Bool -> TVar Bool -> STM String
ulMark lu ru = do
  l <- readTVar lu
  r <- readTVar ru
  return [ulSign l, '/', ulSign r]
{-# INLINE ulMark #-}

ulSingleMark :: TVar Bool -> STM String
ulSingleMark unl = do
  u <- readTVar unl
  return ['/', ulSign u]
{-# INLINE ulSingleMark #-}

showJoin :: Join -> Flags -> Visited -> STM ShowS
showJoin join flags vs = withEllipsisT (visited join vs) $ do
  let lu = joinLeftUnlinked  join
      ru = joinRightUnlinked join

  s <- if is Uls flags
         then (do mark <- ulMark lu ru
                  return (showString ("J " ++ mark)))
         else return (showString "J")

  return (withOptIdS (is NodeIds flags) s (joinId join))
{-# INLINE showJoin #-}

joinAdjs :: Join -> Flags -> Visited -> STM [Vn]
joinAdjs join flags vs = whenNot (visited join vs) $ do
  (bmem, negs, prods) <- joinChildren join
  let vs'       = visiting            join vs
      parent    = joinParent          join
      tests     = joinTests           join
      amem      = joinAmem            join
      ancestor  = joinNearestAncestor join
      ancestor' = return $ case ancestor of { Just a  -> [a]; Nothing -> [] }
      childBmem = return $ case bmem     of { Just b  -> [b]; Nothing -> [] }

  pVn  <- netVn flags (is NodeParents  flags) "parent" vs' (return [parent])

  cbVn <- netVn flags (is NodeChildren flags) "child bmem"  vs' childBmem
  cnVn <- netVn flags (is NodeChildren flags) "child negs"  vs' (return negs    )
  cpVn <- netVn flags (is NodeChildren flags) "child prods" vs' (return prods   )

  tVn  <- netVn flags (is JoinTests            flags) "tests" vs' (return tests )
  aVn  <- netVn flags (is JoinAmems            flags) "amem"  vs' (return [amem])
  anVn <- netVn flags (is JoinNearestAncestors flags) "ancestor" vs' ancestor'

  optVns [pVn, aVn, anVn, tVn, cbVn, cnVn, cpVn]

instance Vnable (Either Dtn Bmem) where
  toVnAdjs (Left  dtn ) = toVnAdjs dtn
  toVnAdjs (Right bmem) = toVnAdjs bmem

  toVnShow (Left  dtn ) = toVnShow dtn
  toVnShow (Right bmem) = toVnShow bmem
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

-- NEG VIS.

instance Vnable Neg where
  toVnAdjs = negAdjs
  toVnShow = showNeg
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

showNeg :: Neg -> Flags -> Visited -> STM ShowS
showNeg neg flags vs = withEllipsisT (visited neg vs) $ do
  let ru = negRightUnlinked neg
  s     <- if is Uls flags
             then (do mark <- ulSingleMark ru
                      return (showString ("N " ++ mark)))
             else return (showString "N")

  return (withOptIdS (is NodeIds flags) s (negId neg))
{-# INLINE showNeg #-}

negAdjs :: Neg -> Flags -> Visited -> STM [Vn]
negAdjs neg flags vs = whenNot (visited neg vs) $ do
  (negs, prods) <- negChildren neg
  let vs'       = visiting           neg vs
      parent    = negParent          neg
      tests     = negTests           neg
      amem      = negAmem            neg
      ancestor  = negNearestAncestor neg
      ancestor' = return $ case ancestor of { Just a  -> [a]; Nothing -> [] }
      toks      = readTVar  (negToks neg)

  pVn  <- netVn flags (is NodeParents  flags) "parent" vs' (return [parent])

  cnVn <- netVn flags (is NodeChildren flags) "child negs"  vs' (return negs    )
  cpVn <- netVn flags (is NodeChildren flags) "child prods" vs' (return prods   )

  tVn  <- netVn flags (is NegTests            flags) "tests" vs' (return tests )
  aVn  <- netVn flags (is NegAmems            flags) "amem"  vs' (return [amem])
  anVn <- netVn flags (is NegNearestAncestors flags) "ancestor" vs' ancestor'
  tkVn <- datVn flags (is NegToks             flags) "toks"     vs' toks

  optVns [pVn, aVn, anVn, tVn, cnVn, cpVn, tkVn]

-- PROD VIS.

instance Vnable Prod where
  toVnAdjs = prodAdjs
  toVnShow = showProd
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

showProd :: Prod -> Flags -> Visited -> STM ShowS
showProd prod flags vs = withEllipsis (visited prod vs) $
  withOptIdS (is NodeIds flags) (showString "P") (prodId prod)
{-# INLINE showProd #-}

prodAdjs :: Prod -> Flags -> Visited -> STM [Vn]
prodAdjs prod flags vs = whenNot (visited prod vs) $ do
  let vs'      = visiting     prod vs
      parent   = prodParent   prod
      toks     = prodToks     prod
      bindings = prodBindings prod

  pVn <- netVn flags (is NodeParents  flags) "parent" vs' (return   [parent])
  vVn <- netVn flags (is ProdBindings flags) "vars"   vs' (varlocs  bindings)
  tVn <- datVn flags (is ProdToks     flags) "toks"   vs' (readTVar toks    )

  optVns [vVn, pVn, tVn]

instance Vnable (Either Join Neg) where
  toVnAdjs (Left  join) = toVnAdjs join
  toVnAdjs (Right neg ) = toVnAdjs neg

  toVnShow (Left  join) = toVnShow join
  toVnShow (Right neg ) = toVnShow neg
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

-- VARIABLE LOCATIONS VIS.

data VLoc = VLoc !Variable !Int !Field

varlocs :: Bindings -> STM [VLoc]
varlocs = return . map vbinding2VLoc . Map.toList
  where vbinding2VLoc (s, Location d f) = VLoc s d f
{-# INLINE varlocs #-}

instance Vnable VLoc where
  toVnAdjs = adjsVLoc
  toVnShow = showVLoc
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

showVLoc :: VLoc -> Flags -> Visited -> STM ShowS
showVLoc (VLoc s f d) _ _ =
  return (compose [ shows s, showString " → "
                  , shows d, showString ",", shows f])
{-# INLINE showVLoc #-}

adjsVLoc :: VLoc -> Flags -> Visited -> STM [Vn]
adjsVLoc _ _ _ = return []
{-# INLINE adjsVLoc #-}

-- JoinTest VISUALIZATION

instance Vnable JoinTest where
  toVnAdjs = adjsJoinTest
  toVnShow = showJoinTest
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

showJoinTest :: JoinTest -> Flags -> Visited -> STM ShowS
showJoinTest
  JoinTest { joinField1   = f1
           , joinField2   = f2
           , joinDistance = d } _ _ =
    return (compose [ showString "⟨"
                    , shows f1, showString ","
                    , shows d,  showString ","
                    , shows f2
                    , showString "⟩"])
{-# INLINE showJoinTest #-}

adjsJoinTest :: JoinTest -> Flags -> Visited -> STM [Vn]
adjsJoinTest _ _ _ = return []
{-# INLINE adjsJoinTest #-}

-- Env VISUALIZATION

instance Vnable Env where
  -- Currently, simply show Dtn. In future: add some report on Env.
  toVnAdjs = toVnAdjs . envDtn
  toVnShow = toVnShow . envDtn
  {-# INLINE toVnShow #-}
  {-# INLINE toVnAdjs #-}

-- PRINT IMPLEMENTATION

-- | Converts the selected object to a tree representation (expressed
-- in ShowS).
toShowS :: Vnable a => Depth -> Switch -> a -> STM ShowS
toShowS d switch obj = printTree (switches conf) (toVn cleanVisited obj)
  where switches = d . applySwitch switch
{-# INLINE toShowS #-}

-- | Works like toShowS, but returns String instead of ShowS
toString :: Vnable a => Depth -> Switch -> a -> STM String
toString d switch = liftM evalShowS . toShowS d switch
  where evalShowS s = s ""
{-# INLINE toString #-}

-- PREDEFINED PRINT CONFIGURATIONS

-- | A 'Switch' for presenting sole Rete net bottom-up.
netBottomUp :: Switch
netBottomUp = up . with NetEmph . withNet . withIds . with AmemFields
            . with Uls

-- | A 'Switch' for presenting sole Rete net top-down.
netTopDown :: Switch
netTopDown = down . with NetEmph . withNet . withIds . with AmemFields
           . with Uls

-- | A default verbosity level for presenting data.
nonVerboseData :: Switch
nonVerboseData = with BmemToks . with NegToks . with ProdToks . with TokWmes
               . with TokNegJoinResults . no   WmeIds . no   TokIds
               . with AmemWmes
