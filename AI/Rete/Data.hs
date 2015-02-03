{-# LANGUAGE    Trustworthy #-}
{-# OPTIONS_GHC -W -Wall    #-}
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

instance Show Constant where
  show (Constant _ s) = s
  {-# INLINE show #-}

instance HavingId Constant where
  getId (Constant id' _) = id'
  {-# INLINE getId #-}

instance Eq Constant where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Constant where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- | Variable.
data Variable = Variable !Id !String

instance Show Variable where
  show (Variable _ s) = s
  {-# INLINE show #-}

instance HavingId Variable where
  getId (Variable id' _) = id'
  {-# INLINE getId #-}

instance Eq Variable where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Variable where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- | Constant or Variable is a Symbol.
data Symbol = Const !Constant
            | Var   !Variable

instance Show Symbol where
  show (Const c) = show c
  show (Var   v) = show v
  {-# INLINE show #-}

instance HavingId Symbol where
  getId (Const c) = getId c
  getId (Var   v) = getId v
  {-# INLINE getId #-}

instance Eq Symbol where
  (Const c1) == (Const c2) = c1 == c2
  (Var   v1) == (Var   v2) = v1 == v2
  _          ==          _ = False
  {-# INLINE (==) #-}

instance Hashable Symbol where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- ENVIRONMENT

-- | Environment. Contains a global context for running Rete.
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

    -- | The Dummy Top Node.
  , envDtn :: !Dtn
  }

-- FIELDS AND THEIR VALUES

-- | Object (Constant or Variable).
newtype Obj = Obj Symbol deriving Eq

instance Show Obj where
  show (Obj s) = show s
  {-# INLINE show #-}

instance Hashable Obj where
  hashWithSalt salt (Obj s) = salt `hashWithSalt` s
  {-# INLINE hashWithSalt #-}

-- | Attribute (Constant or Variable).
newtype Attr = Attr Symbol deriving Eq

instance Show Attr where
  show (Attr s) = show s
  {-# INLINE show #-}

instance Hashable Attr where
  hashWithSalt salt (Attr s) = salt `hashWithSalt` s
  {-# INLINE hashWithSalt #-}

-- | Value (Constant or Variable).
newtype Val = Val Symbol deriving Eq

instance Show Val where
  show (Val s) = show s
  {-# INLINE show #-}

instance Hashable Val where
  hashWithSalt salt (Val s) = salt `hashWithSalt` s
  {-# INLINE hashWithSalt #-}

-- | Field is a description of a location in Wmes, Conds etc. Its
-- variants correspond with Obj, Attr and Val.
data Field = O | A | V deriving (Show, Eq)

-- WMES

type WmesIndex a       = Map.HashMap a (Set.HashSet Wme)
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
  , wmeToks :: !(TVar (Set.HashSet WmeTok))

    -- | Negative join results in which this Wme participates.
  , wmeNegJoinResults :: !(TVar (Set.HashSet NegJoinResult))
  }

instance Show Wme where
  show Wme { wmeObj = obj, wmeAttr = attr, wmeVal = val } =
    "(" ++ show obj  ++ "," ++ show attr ++ "," ++ show val  ++ ")"
  {-# INLINE show #-}

instance HavingId Wme where
  getId = wmeId
  {-# INLINE getId #-}

instance Eq Wme where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Wme where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- | Key for a Wme.
data WmeKey = WmeKey !Obj !Attr !Val deriving Eq

instance Hashable WmeKey where
  hashWithSalt salt (WmeKey obj attr val) =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val
  {-# INLINE hashWithSalt #-}

-- TOKENS

-- | Dummy Top Token.
data Dtt = Dtt

-- | Beta memory token.
data Btok =
  Btok
  {
    btokId       :: !Id
  , btokWme      :: !Wme
  , btokParent   :: !(Either Dtt Btok)
  , btokNode     :: !Bmem
  , btokChildren :: !(TVar (Set.HashSet WmeTok))
  }

instance HavingId Btok where
  getId = btokId
  {-# INLINE getId #-}

instance Eq Btok where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Btok where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- | Negation node token.
data Ntok =
  Ntok
  {
    ntokId             :: !Id
  , ntokWme            :: !(Maybe Wme)
  , ntokParent         :: !(Either JoinTok Ntok)
  , ntokNode           :: !Neg
  , ntokChildren       :: !(TVar (Set.HashSet (Either Ntok Ptok)))
  , ntokNegJoinResults :: !(TVar (Set.HashSet NegJoinResult))
  }

instance HavingId Ntok where
  getId = ntokId
  {-# INLINE getId #-}

instance Eq Ntok where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Ntok where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- | Negative join result.
data NegJoinResult =
  NegJoinResult { njrOwner :: !Ntok
                , njrWme   :: !Wme } deriving Eq

instance Hashable NegJoinResult where
  hashWithSalt salt (NegJoinResult owner wme) =
    salt `hashWithSalt` owner `hashWithSalt` wme
  {-# INLINE hashWithSalt #-}

-- | Production node token.
data Ptok =
  Ptok
  {
    ptokId     :: !Id
  , ptokWme    :: !(Maybe Wme)
  , ptokParent :: !(Either JoinTok Ntok)
  , ptokNode   :: !Prod
  }

instance HavingId Ptok where
  getId = ptokId
  {-# INLINE getId #-}

instance Eq Ptok where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Ptok where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- | A token potentially holding a Wme.
data WmeTok = BmemWmeTok !Btok
            | NegWmeTok  !Ntok
            | ProdWmeTok !Ptok deriving Eq

instance Hashable WmeTok where
  hashWithSalt salt tok = case tok of
    BmemWmeTok btok -> salt `hashWithSalt` btok
    NegWmeTok  ntok -> salt `hashWithSalt` ntok
    ProdWmeTok ptok -> salt `hashWithSalt` ptok
  {-# INLINE hashWithSalt #-}


type JoinTok = Either Dtt Btok

-- ALPHA MEMORY

-- | Alpha Memory.
data Amem =
  Amem
  {
    -- | Successors must be a list, cause the ordering matters.
    amemSuccessors :: !(TVar (Seq.Seq AmemSuccessor))

    -- | The number of join or negative nodes using this Amem.
  , amemReferenceCount :: !(TVar Int)

    -- | The wmes in this Amem (unindexed).
  , amemWmes :: !(TVar (Set.HashSet Wme))

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
  {-# INLINE (==) #-}

instance Hashable Amem where
  hashWithSalt salt Amem { amemObj = obj, amemAttr = attr, amemVal = val } =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val
  {-# INLINE hashWithSalt #-}

-- | Amem successor. May also be used to represent nodes holding a
-- reference to an Amem.
data AmemSuccessor = JoinSuccessor !Join
                   | NegSuccessor  !Neg deriving Eq

-- BETA NETWORK NODES

-- | Dummy Top Node
data Dtn =
  Dtn
  {
    dtnAllChildren :: !(TVar (Set.HashSet Join))
  }

-- | Beta Memory.
data Bmem =
  Bmem
  {
    bmemId          :: !Id
  , bmemParent      :: !Join
  , bmemChildren    :: !(TVar (Set.HashSet Join))
  , bmemAllChildren :: !(TVar (Set.HashSet Join))
  , bmemToks        :: !(TVar (Set.HashSet Btok))
  }

instance HavingId Bmem where
  getId = bmemId
  {-# INLINE getId #-}

instance Eq Bmem where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Bmem where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

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

-- | Join node.
data Join =
  Join
  {
    joinId              :: !Id
  , joinParent          :: !(Either Dtn Bmem)

  , joinBmems           :: !(TVar (Set.HashSet Bmem))
  , joinNegs            :: !(TVar (Set.HashSet Neg))
  , joinProds           :: !(TVar (Set.HashSet Prod))

  , joinAmem            :: !Amem
  , joinNearestAncestor :: !(Maybe AmemSuccessor)
  , joinTests           :: ![JoinTest]
  , joinLeftUnlinked    :: !(TVar Bool)
  , joinRightUnlinked   :: !(TVar Bool)
  }

instance HavingId Join where
  getId = joinId
  {-# INLINE getId #-}

instance Eq Join where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Join where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- | Negative Node.
data Neg =
  Neg
  {
    negId              :: !Id
  , negParent          :: !(Either Join Neg)

  , negNegs            :: !(TVar (Set.HashSet Neg))
  , negProds           :: !(TVar (Set.HashSet Prod))

  , negToks            :: !(TVar (Set.HashSet Ntok))
  , negAmem            :: !Amem
  , negTests           :: ![JoinTest]
  , negNearestAncestor :: !(Maybe AmemSuccessor)
  , negRightUnlinked   :: !(TVar Bool)
  }

instance HavingId Neg where
  getId = negId
  {-# INLINE getId #-}

instance Eq Neg where
  (==) = eqOnId
  {-# INLINE (==) #-}

instance Hashable Neg where
  hashWithSalt = hashWithId
  {-# INLINE hashWithSalt #-}

-- | Symbol location describes the binding for a variable within a token.
data Location = Location !Field !Distance

-- | Map of variable bindings for productions.
type Bindings = Map.HashMap Variable Location

-- | Production node.
data Prod =
  Prod
  {
    prodId           :: !Id
  , prodParent       :: !(Either Join Neg)
  , prodToks         :: !(TVar (Set.HashSet Ptok))
  , prodAction       :: !Action
  , prodRevokeAction :: !(Maybe Action)
  , prodBindings     :: !Bindings
  }

instance HavingId Prod where
  getId = prodId
  {-# INLINE getId #-}

instance Eq Prod where
  (==) = eqOnId
  {-# INLINE (==) #-}

-- ACTIONS

-- | Context of a production action.
data Actx =
  Actx
  {
    actxEnv  :: !Env         -- ^ Current Env
  , actxProd :: !Prod        -- ^ Production node
  , actxTok  :: !Ptok        -- ^ The matching token
  , actxWmes :: ![Maybe Wme] -- ^ Wmes of the matching token
  }

-- | Action of a production.
type Action = Actx -> STM ()

-- CONDITIONS

-- | Positive condition.
data PosCond = PosCond !Obj !Attr !Val

instance Show PosCond where
  show (PosCond o a v) = show o ++ " " ++ show a ++ " " ++ show v
  {-# INLINE show #-}

-- | Negative condition.
data NegCond = NegCond !Obj !Attr !Val

instance Show NegCond where
  show (NegCond o a v) = "¬ " ++ show o ++ " " ++ show a ++ " " ++ show v
  {-# INLINE show #-}
