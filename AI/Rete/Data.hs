{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Data
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-12-15
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
------------------------------------------------------------------------
module AI.Rete.Data
    (
      -- * Identity
      Id
    , HavingId
    , getId
    , eqOnId
    , hashWithId

      -- * Symbolic data
    , Constant            (..)
    , Variable            (..)
    , Symbol              (..)
    , S                   (..)

      -- * Environment
    , Env                 (..)

      -- * Working Memory Elements
    , Wme                 (..)
    , WmeKey              (..)
    , WmeSet
    , WmesIndex
    , WmesByObj
    , WmesByAttr
    , WmesByVal

      -- * Fields and their values
    , Obj                 (..)
    , Attr                (..)
    , Val                 (..)
    , Field               (..)

      -- * Tokens
    , Tok                 (..)
    , TokNode             (..)
    , ParentTok           (..)
    , TokSet

      -- Negative join results
    , NegJoinResult       (..)
    , NegJoinResultSet

      -- α memory
    , Amem                (..)
    , AmemSuccessor       (..)

      -- β memory
    , Bmem                (..)
    , BmemParent          (..)
    , BmemChild           (..)

      -- * Joins
    , Join                (..)
    , JoinParent          (..)
    , JoinChild           (..)
    , JoinNearestAncestor (..)
    , JoinTest            (..)

      -- * Negation
    , Neg                 (..)
    , NegParent           (..)
    , NegChild            (..)
    , NegNearestAncestor  (..)

      -- * Negated conjunction
    , Ncc                 (..)
    , NccParent           (..)
    , NccChild            (..)
    , OwnerKey            (..)

    , Partner             (..)
    , PartnerParent       (..)
    , PartnerChild        (..)

      -- * Productions
    , Prod                (..)
    , ProdParent          (..)
    , ProdChild           (..)

      -- * U/L
    , RightUnlinked       (..)
    , LeftUnlinked        (..)

      -- * "Spatial" data
    , Distance
    , Location            (..)

      -- * Actions, variable bindings
    , Action
    , Actx                (..)
    , Bindings

      -- * Conditions
    , Cond                (..)
    )
    where

import           Control.Concurrent.STM (STM, TVar)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Sequence as Seq

-- | Identifier type. We treat negative identifiers as special ones,
-- and the non-negative as auto-generated.
type Id = Int

-- | Represents types whose elements have Id.
class HavingId a where
  -- | Returns an Id of the argument.
  getId :: a -> Id

-- | Equality operator that uses the Ids of its arguments.
eqOnId :: HavingId a => a -> a -> Bool
obj1 `eqOnId` obj2 = getId obj1 == getId obj2
{-# INLINE eqOnId #-}

-- | Hash with salt implemented with Id of the argument.
hashWithId :: HavingId a => Int -> a -> Int
hashWithId salt x = salt `hashWithSalt` getId x
{-# INLINE hashWithId #-}

-- | Constant (non-variable).
data Constant = Constant !Id !String

instance Show     Constant where show  (Constant _   s) = s
instance HavingId Constant where getId (Constant id' _) = id'
instance Eq       Constant where (==)         = eqOnId
instance Hashable Constant where hashWithSalt = hashWithId

-- | Variable.
data Variable = Variable !Id !String

instance Show     Variable where show  (Variable _   s) = s
instance HavingId Variable where getId (Variable id' _) = id'
instance Eq       Variable where (==)         = eqOnId
instance Hashable Variable where hashWithSalt = hashWithId

-- | Constant or Variable is a Symbol.
data Symbol = Const !Constant
            | Var   !Variable

instance Show Symbol where
  show (Const c) = show c
  show (Var   v) = show v

instance HavingId Symbol where
  getId (Const c) = getId c
  getId (Var   v) = getId v

instance Eq Symbol where
  (Const c1) == (Const c2) = c1 == c2
  (Var   v1) == (Var   v2) = v1 == v2
  _          ==          _ = False

instance Hashable Symbol where hashWithSalt = hashWithId

type WmeSet            = Set.HashSet Wme
type WmesIndex a       = Map.HashMap a WmeSet
type WmesByObj         = WmesIndex Obj
type WmesByAttr        = WmesIndex Attr
type WmesByVal         = WmesIndex Val

-- | Environment. Contains a global context for the running algorithm.
data Env =
  Env
  {
    -- | State of the Id generator.
    envIdState :: !(TVar Id)

    -- | Registry of (interned) Constants.
  , envConstants:: !(TVar (Map.HashMap String Constant))

    -- | Registry of (interned) Variables.
  , envVariables:: !(TVar (Map.HashMap String Variable))

    -- | All Wmes indexed by their WmeKey
  , envWmes :: !(TVar (Map.HashMap WmeKey Wme))

    -- 3 Wme indexes by Wme Field value
  , envWmesByObj  :: !(TVar WmesByObj)
  , envWmesByAttr :: !(TVar WmesByAttr)
  , envWmesByVal  :: !(TVar WmesByVal)

    -- | Known alpha memories indexed by their WmeKey
  , envAmems :: !(TVar (Map.HashMap WmeKey Amem))

    -- | Productions the Env knows about
  , envProds :: !(TVar (Set.HashSet Prod))
  }

-- | Object (Constant or Variable).
newtype Obj = Obj Symbol deriving Eq

instance Show Obj where show (Obj s) = show s

instance Hashable Obj where
  hashWithSalt salt (Obj s) = salt `hashWithSalt` s

-- | Attribute (Constant or Variable).
newtype Attr = Attr Symbol deriving Eq

instance Show Attr where show (Attr s) = show s

instance Hashable Attr where
  hashWithSalt salt (Attr s) = salt `hashWithSalt` s

-- | Value (Constant or Variable).
newtype Val = Val Symbol deriving Eq

instance Show Val where show (Val s) = show s

instance Hashable Val where
  hashWithSalt salt (Val s) = salt `hashWithSalt` s

-- | Field is a description of a location in Wmes, Conds etc. Its
-- variants correspond with Obj, Attr and Val.
data Field = O | A | V deriving (Show, Eq)

type TokSet           = Set.HashSet Tok
type NegJoinResultSet = Set.HashSet NegJoinResult

-- | Working Memory Element (fact).
data Wme =
  Wme
  {
    wmeId :: !Id

  , wmeObj  :: !Obj
  , wmeAttr :: !Attr
  , wmeVal  :: !Val

    -- | Amems this Wme belongs to (8 at most).
  , wmeAmems :: !(TVar [Amem])

    -- | Toks with tokenWme = this Wme.
  , wmeToks :: !(TVar TokSet)

    -- | Negative join results in which this Wme participates.
  , wmeNegJoinResults :: !(TVar NegJoinResultSet)
  }

instance Show Wme where
  show Wme { wmeObj = obj, wmeAttr = attr, wmeVal = val } =
    "(" ++ show obj  ++ "," ++ show attr ++ "," ++ show val  ++ ")"

instance HavingId Wme where getId        = wmeId
instance Eq       Wme where (==)         = eqOnId
instance Hashable Wme where hashWithSalt = hashWithId

-- | Key for a Wme.
data WmeKey = WmeKey !Obj !Attr !Val deriving Eq

instance Hashable WmeKey where
  hashWithSalt salt (WmeKey obj attr val) =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val

data TokNode = BmemTokNode    !Bmem
             | NegTokNode     !Neg
             | NccTokNode     !Ncc
             | PartnerTokNode !Partner
             | ProdTokNode    !Prod

-- | Parent token, may be either a Tok or a Dtt (Dummy Top Token).
data ParentTok = ParentTok !Tok
               | Dtt
               deriving Eq

instance Hashable ParentTok where
  hashWithSalt salt Dtt             = salt `hashWithSalt` ((-1) :: Id)
  hashWithSalt salt (ParentTok tok) = salt `hashWithSalt` tok

-- | Negative join result.
data NegJoinResult =
  NegJoinResult { njrOwner :: !Tok
                , njrWme   :: !Wme } deriving Eq

instance Hashable NegJoinResult where
  hashWithSalt salt (NegJoinResult owner wme) =
    salt `hashWithSalt` owner `hashWithSalt` wme

-- | Token.
data Tok =
  Tok
  {
    -- | Identifier of the token.
    tokId :: !Id

    -- | Points to a 'higher' token.
  , tokParent :: !ParentTok

    -- | Wme of this Tok, Nothing for some toks.
  , tokWme :: !(Maybe Wme)

    -- | The node the token is in.
  , tokNode :: !TokNode

    -- | Toks with parent = this.
  , tokChildren :: !(TVar TokSet)

    -- | Used only for Toks in negative nodes.
  , tokNegJoinResults :: !(TVar NegJoinResultSet)

    -- | Similar to tokNegJoinResults but for NCC nodes.
  , tokNccResults :: !(TVar TokSet)

    -- | On Toks in NCC partners: toks in whose local memory this
    -- result resides.
  , tokOwner :: !(TVar (Maybe Tok))
  }

instance HavingId Tok where getId        = tokId
instance Eq       Tok where (==)         = eqOnId
instance Hashable Tok where hashWithSalt = hashWithId

-- | Rete nodes - successors of an Amem.
data AmemSuccessor = JoinAmemSuccessor !Join deriving Eq

-- | Alpha Memory.
data Amem =
  Amem
  {
    -- | Successors must be a list, cause the ordering matters.
    amemSuccessors :: !(TVar (Seq.Seq AmemSuccessor))

    -- | The number of join or negative nodes using this Amem.
  , amemReferenceCount :: !(TVar Int)

    -- | The wmes in this Amem (unindexed).
  , amemWmes :: !(TVar WmeSet)

    -- | Wmes are indexed by their Field value.
  , amemWmesByObj  :: !(TVar WmesByObj)
  , amemWmesByAttr :: !(TVar WmesByAttr)
  , amemWmesByVal  :: !(TVar WmesByVal)

    -- Keys to identify the α memory in the α memories registry.
  , amemObj  :: !Obj
  , amemAttr :: !Attr
  , amemVal  :: !Val
  }

instance Eq Amem where
  Amem   { amemObj = obj1, amemAttr = attr1, amemVal = val1 } ==
    Amem { amemObj = obj2, amemAttr = attr2, amemVal = val2 } =
      obj1 == obj2 && attr1 == attr2 && val1 == val2

instance Hashable Amem where
  hashWithSalt salt Amem { amemObj = obj, amemAttr = attr, amemVal = val } =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val

-- | Parent (node) of a Bmem.
data BmemParent = JoinBmemParent !Join
                | NegBmemParent  !Neg
                | NccBmemParent  !Ncc

-- | Child (node) of a Bmem.
data BmemChild  = BmemChild

-- | Beta Memory.
data Bmem =
  Bmem
  {
    bmemId          :: !Id
  , bmemParent      :: !BmemParent
  , bmemChildren    :: !(TVar (Seq.Seq BmemChild))

  , bmemAllChildren :: !(TVar (Seq.Seq BmemChild))
  , bmemToks        :: !(TVar TokSet)
  }

instance HavingId Bmem where getId = bmemId
instance Eq       Bmem where (==) = eqOnId

-- | Distance within lists of Toks or Conds.
type Distance = Int

-- | Representation of a join test.
data JoinTest =
  JoinTest
  {
    joinField1   :: !Field
  , joinField2   :: !Field
  , joinDistance :: !Distance
  }
  deriving Eq

-- | Parent node of a Join node.
data JoinParent = JoinParent

-- | Child node of a Join node.
data JoinChild = BmemJoinChild !Bmem

-- | Nearest ancestor of a Join node having the same Amem.
data JoinNearestAncestor = JoinNearestAncestor

-- | Right-unlinked flag.
newtype RightUnlinked = RightUnlinked Bool

-- | Left-unlinked flag.
newtype LeftUnlinked = LeftUnlinked Bool

-- | Join Node.
data Join =
  Join
  {
    joinId             :: !Id
  , joinParent         :: !JoinParent
  , joinChildren       :: !(TVar (Seq.Seq JoinChild))

  , joinAmem           :: !Amem
  , joinNearesAncestor :: !JoinNearestAncestor
  , joinTests          :: ![JoinTest]
  , joinLeftUnlinked   :: !(TVar LeftUnlinked)
  , joinRightUnlinked  :: !(TVar RightUnlinked)
  }

instance HavingId Join where getId = joinId
instance Eq       Join where (==)  = eqOnId

-- | Parent node of a Neg node.
data NegParent = NegParent

-- | Child node of a Neg node.
data NegChild = BmemNegChild !Bmem

-- | Nearest ancestor of a Neg node having the same Amem.
data NegNearestAncestor = NegNearestAncestor

-- | Negative Node.
data Neg =
  Neg
  {
    negId              :: !Id
  , negParent          :: !NegParent
  , negChildren        :: !(TVar (Seq.Seq NegChild))

  , negToks            :: !(TVar TokSet)
  , negAmem            :: !Amem
  , negTests           :: ![JoinTest]
  , negNearestAncestor :: !NegNearestAncestor
  , negRightUnlinked   :: !(TVar RightUnlinked)
  }

instance HavingId Neg where getId = negId
instance Eq       Neg where (==)  = eqOnId

-- | Parent node of a Ncc node.
data NccParent = NccParent

-- | Child node of a Ncc node.
data NccChild = BmemNccChild !Bmem

-- | Ncc node.
data Ncc =
  Ncc
  {
    nccId       :: !Id
  , nccParent   :: !NccParent
  , nccChildren :: !(TVar (Seq.Seq NccChild))

  , nccToks      :: !(TVar (Map.HashMap OwnerKey Tok))
  , nccPartner  :: !Partner
  }

instance HavingId Ncc where getId = nccId
instance Eq       Ncc where (==)  = eqOnId

-- | Key in nccToks index.
data OwnerKey = OwnerKey !ParentTok !(Maybe Wme) deriving Eq

instance Hashable OwnerKey where
  hashWithSalt salt (OwnerKey parent wme) =
    salt `hashWithSalt` parent `hashWithSalt` wme

-- | Parent node of a Partner node.
data PartnerParent = PartnerParent

-- | Child node of a Partner node.
data PartnerChild = PartnerChild

-- | Ncc partner node.
data Partner =
  Partner
  {
    partnerId       :: !Id
  , partnerParent   :: !PartnerParent
  , partnerChildren :: !(TVar (Seq.Seq PartnerChild))

  , partnerNcc      :: !Ncc
  , partnerConjucts :: !Int
  , partnerBuff     :: !(TVar TokSet)
  }

instance HavingId Partner where getId = partnerId
instance Eq       Partner where (==)  = eqOnId

-- | Symbol location describes the binding for a variable within a token.
data Location = Location !Field !Distance

-- | Map of variable bindings for productions.
type Bindings = Map.HashMap Variable Location

-- | Parent node of a Prod node.
data ProdParent = ProdParent

-- | Child node of a Prod node.
data ProdChild = ProdChild

-- | Production node.
data Prod =
  Prod
  {
    prodId           :: !Id
  , prodParent       :: !ProdParent
  , prodToks         :: !(TVar TokSet)
  , prodAction       :: !Action
  , prodRevokeAction :: !(Maybe Action)
  , prodBindings     :: !Bindings
  }

instance HavingId Prod where getId = prodId
instance Eq       Prod where (==)  = eqOnId

-- | Context of a production action.
data Actx =
  Actx
  {
    actxEnv  :: !Env         -- ^ Current Env
  , actxProd :: !Prod        -- ^ Production node
  , actxTok  :: !Tok         -- ^ The matching token
  , actxWmes :: [Maybe Wme]  -- ^ The token Wmes
  }

-- | Action of a production.
type Action = Actx -> STM ()

-- | The user-friendly representation of symbols.
data S = S   !String
       | Sym !Symbol

instance Show S where
  show (S   s) = s
  show (Sym s) = show s

-- | The condition of a production.
data Cond =
  -- Positive conds
    PosStr  !String !String  !String
  | PosS    !S      !S       !S
  | PosCond !Obj    !Attr    !Val -- canonical form

  -- Neg conds
  | NegStr  !String !String  !String
  | NegS    !S      !S       !S
  | NegCond !Obj    !Attr    !Val -- canonical form

  -- Nccs
  | NccCond ![Cond]

instance Show Cond where
  show (PosStr  o a v) =         show o ++ " " ++ show a ++ " " ++ show v
  show (PosS    o a v) =         show o ++ " " ++ show a ++ " " ++ show v
  show (PosCond o a v) =         show o ++ " " ++ show a ++ " " ++ show v

  show (NegStr  o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v
  show (NegS    o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v
  show (NegCond o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v

  show (NccCond conds) = "¬ " ++ show conds
