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
module AI.Rete.Data where

import           Control.Concurrent.STM (STM, TVar)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Sequence as Seq

-- IDENTITY

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

-- SYMBOLIC DATA

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

-- ENVIRONMENT

-- | Environment. Contains a global context for running algorithm.
data Env =
  Env
  {
    -- | State of the Id generator.
    envIdState :: !(TVar Id)

    -- | Registry of (interned) Constants.
  , envConstants:: !(TVar (Map.HashMap String Constant))

    -- | Registry of (interned) Variables.
  , envVariables:: !(TVar (Map.HashMap String Variable))

    -- | All Wmes indexed by their WmeKey.
  , envWmes :: !(TVar (Map.HashMap WmeKey Wme))

    -- 3 Wme indexes by Wme Field value
  , envWmesByObj  :: !(TVar WmesByObj)
  , envWmesByAttr :: !(TVar WmesByAttr)
  , envWmesByVal  :: !(TVar WmesByVal)

    -- | Known alpha memories indexed by their WmeKey.
  , envAmems :: !(TVar (Map.HashMap WmeKey Amem))

    -- | Productions the Env knows about.
  , envProds :: !(TVar (Set.HashSet Prod))

    -- | Dummy Top Node.
  , envDtn :: !Dtn
  }

-- FIELDS AND THEIR VALUES

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

-- WMES

type WmeSet            = Set.HashSet Wme
type WmesIndex a       = Map.HashMap a WmeSet
type WmesByObj         = WmesIndex Obj
type WmesByAttr        = WmesIndex Attr
type WmesByVal         = WmesIndex Val

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

-- TOKS (TOKENS)

type TokSet           = Set.HashSet Tok
type NegJoinResultSet = Set.HashSet NegJoinResult

data TokNode = DtnTokNode     !Dtn
             | BmemTokNode    !Bmem
             | NegTokNode     !Neg
             | NccTokNode     !Ncc
             | PartnerTokNode !Partner
             | ProdTokNode    !Prod

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

    -- | Points to a 'higher' token. Nothing for Dtt.
  , tokParent :: !(Maybe Tok)

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

-- AMEM (ALPHA MEMORY)

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

-- GENERALIZED NODES

-- | Amem successor. May also be used to represent nodes holding a
-- reference to an Amem.
data AmemSuccessor = JoinAmemSuccessor !Join
                   | NegAmemSuccessor  !Neg  deriving Eq

-- | Represents the nodes that occur as results of building the network
-- structure for various types of conditions.
data CondNode = PosCondNode !Join
              | NegCondNode !Neg
              | NccCondNode !Ncc

-- | A special case of CondNode that adds a Dtn extension.
data CondNodeWithDtn = DtnCondNode !Dtn
                     | StdCondNode !CondNode

-- | Type of nodes that are children of CondNodes.
data CondChild = BmemCondChild    !Bmem
               | NegCondChild     !Neg
               | NccCondChild     !Ncc
               | PartnerCondChild !Partner
               | ProdCondChild    !Prod

-- PARTICULAR NODES

-- | A singleton (within Env) Dummy Top Node.
data Dtn =
  Dtn
  {
    dtnTok :: !Tok -- ^ Dummy Top Token

    -- Indices below store Dtn children in a way usable during network
    -- construction. Ordering does not matter cause none of the
    -- children gets ever activated by Dtn.
  , dtnJoins :: !(TVar (Map.HashMap Amem     Join))
  , dtnNegs  :: !(TVar (Map.HashMap Amem     Neg))
  , dtnNccs  :: !(TVar (Map.HashMap CondNode Ncc))
  }

-- | Beta Memory.
data Bmem =
  Bmem
  {
    bmemId          :: !Id
  , bmemParent      :: !CondNode
  , bmemChildren    :: !(TVar (Seq.Seq Join))

  , bmemAllChildren :: !(TVar (Seq.Seq Join))
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
data JoinParent = BmemJoinParent !Bmem | DtnJoinParent !Dtn

-- | Right-unlinked flag.
newtype RightUnlinked = RightUnlinked Bool

-- | Left-unlinked flag.
newtype LeftUnlinked = LeftUnlinked Bool

-- | Join Node.
data Join =
  Join
  {
    joinId              :: !Id
  , joinParent          :: !JoinParent
  , joinChildren        :: !(TVar (Seq.Seq CondChild))

  , joinAmem            :: !Amem
  , joinNearestAncestor :: !(Maybe AmemSuccessor)
  , joinTests           :: ![JoinTest]
  , joinLeftUnlinked    :: !(TVar LeftUnlinked)
  , joinRightUnlinked   :: !(TVar RightUnlinked)
  }

instance HavingId Join where getId = joinId
instance Eq       Join where (==)  = eqOnId

-- | Negative Node.
data Neg =
  Neg
  {
    negId              :: !Id
  , negParent          :: !CondNodeWithDtn
  , negChildren        :: !(TVar (Seq.Seq CondChild))

  , negToks            :: !(TVar TokSet)
  , negAmem            :: !Amem
  , negTests           :: ![JoinTest]
  , negNearestAncestor :: !(Maybe AmemSuccessor)
  , negRightUnlinked   :: !(TVar RightUnlinked)
  }

instance HavingId Neg where getId = negId
instance Eq       Neg where (==)  = eqOnId

-- | Ncc node.
data Ncc =
  Ncc
  {
    nccId       :: !Id
  , nccParent   :: !CondNodeWithDtn
  , nccChildren :: !(TVar (Seq.Seq CondChild))

  , nccToks      :: !(TVar (Map.HashMap OwnerKey Tok))
  , nccPartner  :: !Partner
  }

instance HavingId Ncc where getId = nccId
instance Eq       Ncc where (==)  = eqOnId

-- | Key in nccToks index.
data OwnerKey = OwnerKey !Tok !(Maybe Wme) deriving Eq

instance Hashable OwnerKey where
  hashWithSalt salt (OwnerKey parent wme) =
    salt `hashWithSalt` parent `hashWithSalt` wme

-- | Ncc partner node.
data Partner =
  Partner
  {
    partnerId       :: !Id
  , partnerParent   :: !CondNode

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

-- | Production node.
data Prod =
  Prod
  {
    prodId           :: !Id
  , prodParent       :: !CondNode
  , prodToks         :: !(TVar TokSet)
  , prodAction       :: !Action
  , prodRevokeAction :: !(Maybe Action)
  , prodBindings     :: !Bindings
  }

instance HavingId Prod where getId = prodId
instance Eq       Prod where (==)  = eqOnId

-- ACTIONS

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

-- CONDITIONS

-- | The condition of a production.
data Cond = PosCond !Obj !Attr !Val
          | NegCond !Obj !Attr !Val
          | NccCond [Cond]

instance Show Cond where
  show (PosCond o a v) =         show o ++ " " ++ show a ++ " " ++ show v
  show (NegCond o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v
  show (NccCond conds) = "¬ " ++ show conds
