{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
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
      -- * Environment
      Env (..)
    , Id

      -- * Symbols
    , Symbol (..)
    , S      (..)

      -- * Production components
    , Action
    , Actx (..)
    , Cond (..)

      -- * (Internal) structure of Rete network
    , Amem     (..)
    , Dtn      (..)
    , Bmem     (..)
    , Join     (..)
    , JoinTest (..)
    , Neg      (..)
    , Ncc      (..)
    , Partner  (..)
    , Prod     (..)

      -- * (Internal) data Rete operates on
    , Dtt           (..)
    , Tok           (..)
    , GTok
    , Wme           (..)
    , NegJoinResult (..)

      -- * Some (Internal) utilities
    , WmesIndex
    , WmeKey   (..)
    , Field    (..)
    , Location (..)
    , Bindings (..)

      -- * (Internal) Environment Tags
    , EnvIdState         (..)
    , EnvSymbolsRegistry (..)
    , EnvVarsRegistry    (..)
    , EnvWmesRegistry    (..)
    , EnvWmesByObj       (..)
    , EnvWmesByAttr      (..)
    , EnvWmesByVal       (..)
    , EnvAmems           (..)
    , EnvProds           (..)

      -- * (Internal) Symbol Tags
    , SymbolId     (..)
    , VariableId   (..)
    , SymbolName   (..)
    , VariableName (..)

      -- * (Internal) Wme Tags
    , WmeId             (..)
    , WmeObj            (..)
    , WmeAttr           (..)
    , WmeVal            (..)
    , WmeAmems          (..)
    , WmeToks           (..)
    , WmeNegJoinResults (..)

      -- * (Internal) Token Tags
    , TokNode           (..)
    , TokId             (..)
    , TokParent         (..)
    , TokWme            (..)
    , TokChildren       (..)
    , TokNegJoinResults (..)
    , TokNccResults     (..)
    , TokOwner          (..)

      -- * (Internal) Negative Join Result Tags
    , NJROwner (..)
    , NJRWme   (..)

      -- * (Internal) Amem Tags
    , AmemSuccessor  (..)
    , AmemSuccessors (..)
    , AmemRefCount   (..)
    , AmemWmes       (..)
    , AmemWmesByObj  (..)
    , AmemWmesByAttr (..)
    , AmemWmesByVal  (..)
    , AmemObj        (..)
    , AmemAttr       (..)
    , AmemVal        (..)

      -- * (Internal) Bmem Tags
    , BmemParent      (..)
    , BmemChild       (..)
    , BmemId          (..)
    , BmemChildren    (..)
    , BmemToks        (..)
    , BmemAllChildren (..)

      -- * (Internal) JoinTest Tags
    , Distance     (..)
    , JoinField1   (..)
    , JoinField2   (..)
    , JoinDistance (..)

      -- * (Internal) Join Tags
    , JoinParent          (..)
    , JoinChild           (..)
    , JoinNearestAncestor (..)
    , JoinId              (..)
    , JoinChildren        (..)
    , JoinAmem            (..)
    , JoinTests           (..)
    , JoinLeftUnlinked    (..)
    , JoinRightUnlinked   (..)

      -- * (Internal) Neg Tags
    , NegParent          (..)
    , NegChild           (..)
    , NegNearestAncestor (..)
    , NegId              (..)
    , NegChildren        (..)
    , NegToks            (..)
    , NegAmem            (..)
    , NegTests           (..)
    , NegRightUnlinked   (..)

      -- * (Internal) Ncc Tags
    , NccParent   (..)
    , NccChild    (..)
    , NccId       (..)
    , NccChildren (..)
    , NccToks     (..)
    , NccPartner  (..)

      -- * (Internal) Partner Tags
    , PartnerParent   (..)
    , PartnerChild    (..)
    , PartnerId       (..)
    , PartnerChildren (..)
    , PartnerNcc      (..)
    , PartnerConjucts (..)
    , PartnerBuff     (..)

      -- * (Internal) Location Tags
    , LocationField    (..)
    , LocationDistance (..)

      -- * (Internal) Prod Tags
    , ProdParent       (..)
    , ProdId           (..)
    , ProdToks         (..)
    , ProdAction       (..)
    , ProdRevokeAction (..)
    , ProdBindings     (..)
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

newtype SymbolId     = SymbolId     Id     deriving Eq
newtype VariableId   = VariableId   Id     deriving Eq
newtype SymbolName   = SymbolName   String deriving Eq
newtype VariableName = VariableName String deriving Eq

instance Show SymbolName   where show (SymbolName   name) = name
instance Show VariableName where show (VariableName name) = name

instance Hashable SymbolId where
  hashWithSalt salt (SymbolId id') = salt `hashWithSalt` id'

instance Hashable VariableId where
  hashWithSalt salt (VariableId id') = salt `hashWithSalt` id'

instance Hashable SymbolName where
  hashWithSalt salt (SymbolName name) = salt `hashWithSalt` name

instance Hashable VariableName where
  hashWithSalt salt (VariableName name) = salt `hashWithSalt` name

-- | Symbols are the atomic pieces of information managed by Rete.
data Symbol = Symbol   !SymbolId   !SymbolName
            | Variable !VariableId !VariableName

instance Show Symbol where
  show (Symbol   _ s) = show s
  show (Variable _ s) = show s

instance Eq Symbol where
  (Symbol   id1 _) == (Symbol   id2 _) = id1 == id2
  (Variable id1 _) == (Variable id2 _) = id1 == id2
  _ == _ = False

instance Hashable Symbol where
  hashWithSalt salt (Symbol   id' _) = salt `hashWithSalt` id'
  hashWithSalt salt (Variable id' _) = salt `hashWithSalt` id'

newtype EnvIdState         = EnvIdState         Id
newtype EnvSymbolsRegistry = EnvSymbolsRegistry (Map.HashMap SymbolName Symbol)
newtype EnvVarsRegistry    = EnvVarsRegistry    (Map.HashMap VariableName Symbol)
newtype EnvWmesRegistry    = EnvWmesRegistry    (Map.HashMap WmeKey Wme)
newtype EnvWmesByObj       = EnvWmesByObj       (WmesIndex   WmeObj)
newtype EnvWmesByAttr      = EnvWmesByAttr      (WmesIndex   WmeAttr)
newtype EnvWmesByVal       = EnvWmesByVal       (WmesIndex   WmeVal)
newtype EnvAmems           = EnvAmems           (Map.HashMap WmeKey Amem)
-- newtype EnvDtn          = EnvDtn             Dtn
-- newtype EnvDtt          = EnvDtt             Dtt
newtype EnvProds           = EnvProds           (Set.HashSet Prod)

-- | Environment
data Env =
  Env
  {
    -- | State of the Env-wide Id generator
    envIdState :: !(TVar EnvIdState)

    -- | Registry of (interned) Symbols
  , envSymbolsRegistry :: !(TVar EnvSymbolsRegistry)

    -- | Registry of (interned) Variables
  , envVarsRegistry :: !(TVar EnvVarsRegistry)

    -- | All Wmes indexed by their WmeKey
  , envWmesRegistry :: !(TVar EnvWmesRegistry)

    -- 3 Wme indexes by Wme Field value
  , envWmesByObj  :: !(TVar EnvWmesByObj)
  , envWmesByAttr :: !(TVar EnvWmesByAttr)
  , envWmesByVal  :: !(TVar EnvWmesByVal)

    -- | Known alpha memories indexed by their WmeKey
  , envAmems :: !(TVar EnvAmems)

    -- Dummies
  -- , envDtn :: !Dtn
  -- , envDtt :: !Dtt

    -- | Productions the Env knows about
  , envProds :: !(TVar EnvProds)
  }

newtype WmeId             = WmeId             Id     deriving Eq
newtype WmeObj            = WmeObj            Symbol deriving Eq
newtype WmeAttr           = WmeAttr           Symbol deriving Eq
newtype WmeVal            = WmeVal            Symbol deriving Eq
newtype WmeAmems          = WmeAmems          [Amem]
newtype WmeToks           = WmeToks           (Set.HashSet Tok)
newtype WmeNegJoinResults = WmeNegJoinResults (Set.HashSet NegJoinResult)

-- | Working Memory Element
data Wme =
  Wme
  {
    wmeId :: !WmeId

  , wmeObj  :: !WmeObj
  , wmeAttr :: !WmeAttr
  , wmeVal  :: !WmeVal

    -- | α-memories this Wme belongs to (8 at most)
  , wmeAmems :: !(TVar WmeAmems)

    -- | Toks with tokenWme = this Wme
  , wmeToks :: !(TVar WmeToks)

    -- | Neg join results in which this Wme participates
  , wmeNegJoinResults :: !(TVar WmeNegJoinResults)
  }

instance Show Wme where
  show Wme { wmeObj = obj, wmeAttr = attr, wmeVal = val } =
    "(" ++ show obj  ++ "," ++ show attr ++ "," ++ show val  ++ ")"

instance Eq Wme where
  wme1 == wme2 = wmeId wme1 == wmeId wme2

instance Hashable Wme where
  hashWithSalt salt wme = salt `hashWithSalt` wmeId wme

instance Hashable WmeId where
  hashWithSalt salt (WmeId id') = salt `hashWithSalt` id'

instance Show WmeObj  where show (WmeObj  s) = show s
instance Show WmeAttr where show (WmeAttr s) = show s
instance Show WmeVal  where show (WmeVal  s) = show s

instance Hashable WmeObj where
  hashWithSalt salt (WmeObj s)  = salt `hashWithSalt` s
instance Hashable WmeAttr where
  hashWithSalt salt (WmeAttr s) = salt `hashWithSalt` s
instance Hashable WmeVal where
  hashWithSalt salt (WmeVal s)  = salt `hashWithSalt` s

data    TokNode           = TokNode
newtype TokId             = TokId             Id deriving Eq
newtype TokParent         = TokParent         GTok
newtype TokWme            = TokWme            (Maybe Wme)
newtype TokChildren       = TokChildren       (Set.HashSet Tok)
newtype TokNegJoinResults = TokNegJoinResults (Set.HashSet NegJoinResult)
newtype TokNccResults     = TokNccResults     (Set.HashSet Tok)
newtype TokOwner          = TokOwner          (Maybe Tok)

-- | Token
data Tok =
  Tok
  {
    -- | An internal identifier of the token
    tokId :: !TokId

    -- | Points to a higher token
  , tokParent :: !TokParent

    -- | i-th Wme, Nothing for some toks
  , tokWme :: !TokWme

    -- | The node the token is in
  , tokNode :: !TokNode

    -- | The Toks with parent = this
  , tokChildren :: !(TVar TokChildren)

    -- | Used only for Toks in negative nodes
  , tokNegJoinResults :: !(TVar TokNegJoinResults)

    -- | Similar to tokNegJoinResults but for NCC nodes
  , tokNccResults :: !(TVar TokNccResults)

    -- | On Toks in NCC partners: toks in whose local memory this
    -- result resides
  , tokOwner :: !(TVar TokOwner)
  }

instance Eq Tok where
  Tok { tokId = id1 } == Tok { tokId = id2 } = id1 == id2

instance Hashable TokId where
  hashWithSalt salt (TokId id') = salt `hashWithSalt` id'

instance Hashable Tok where
  hashWithSalt salt Tok { tokId = id' } = salt `hashWithSalt` id'


-- | Dummy Top Token. Does not hold any internal data at all, just
-- marks effective beginnings of Toks.
data Dtt = Dtt

-- newtype DttNode     = DttNode     Dtn
-- newtype DttChildren = DttChildren (Set.HashSet Tok)
-- -- | Dummy Top Token
-- data Dtt =
--   Dtt
--   {
--     -- | The node the token is in - Dummy Top Node
--     dttNode :: !DttNode

--     -- | Children of this Dtt (Token)
--   , dttChildren :: !(TVar DttChildren)
--   }

-- | Generalized Token
type GTok = Either Dtt Tok

-- | Field
data Field = Obj | Attr | Val deriving (Show, Eq)

-- | Index of Wmes by a specific field values
type WmesIndex a = Map.HashMap a (Set.HashSet Wme)

-- | A key for a Wme consisting of its obj, attr and value
data WmeKey = WmeKey !WmeObj !WmeAttr !WmeVal deriving Eq

instance Hashable WmeKey where
  hashWithSalt salt (WmeKey obj attr val) =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val

newtype NJROwner = NJROwner Tok deriving Eq
newtype NJRWme   = NJRWme   Wme deriving Eq

-- | Negative join result
data NegJoinResult =
  NegJoinResult { njrOwner :: !NJROwner
                , njrWme   :: !NJRWme } deriving Eq

data    AmemSuccessor  = AmemSuccessor
newtype AmemSuccessors = AmemSuccessors (Seq.Seq     AmemSuccessor)
newtype AmemRefCount   = AmemRefCount   Int
newtype AmemWmes       = AmemWmes       (Set.HashSet Wme)
newtype AmemWmesByObj  = AmemWmesByObj  (WmesIndex   WmeObj)
newtype AmemWmesByAttr = AmemWmesByAttr (WmesIndex   WmeAttr)
newtype AmemWmesByVal  = AmemWmesByVal  (WmesIndex   WmeVal)
newtype AmemObj        = AmemObj        Symbol deriving Eq
newtype AmemAttr       = AmemAttr       Symbol deriving Eq
newtype AmemVal        = AmemVal        Symbol deriving Eq

-- | Alpha Memory
data Amem =
  Amem
  {
    -- | Successors must be a list, cause the ordering matters.
    amemSuccessors :: !(TVar AmemSuccessors)

    -- | The number of join or negative node using this Amem
  , amemReferenceCount :: !(TVar AmemRefCount)

    -- | The wmes in this α memory (unindexed)
  , amemWmes :: !(TVar AmemWmes)

    -- | Wmes are indexed by their Field value.
  , amemWmesByObj  :: !(TVar AmemWmesByObj)
  , amemWmesByAttr :: !(TVar AmemWmesByAttr)
  , amemWmesByVal  :: !(TVar AmemWmesByVal)

    -- | Keys to identify the α memory in the α memories registry
  , amemObj  :: !AmemObj
  , amemAttr :: !AmemAttr
  , amemVal  :: !AmemVal
  }

instance Eq Amem where
  Amem   { amemObj = obj1, amemAttr = attr1, amemVal = val1 } ==
    Amem { amemObj = obj2, amemAttr = attr2, amemVal = val2 } =
      obj1 == obj2 && attr1 == attr2 && val1 == val2

instance Hashable Amem where
  hashWithSalt salt Amem { amemObj = obj, amemAttr = attr, amemVal = val} =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val

instance Hashable AmemObj where
  hashWithSalt salt (AmemObj s)  = salt `hashWithSalt` s
instance Hashable AmemAttr where
  hashWithSalt salt (AmemAttr s) = salt `hashWithSalt` s
instance Hashable AmemVal where
  hashWithSalt salt (AmemVal s)  = salt `hashWithSalt` s

-- | Dummy Top Node does not hold any data at all. Just marks a top of
-- the Rete net.
data Dtn = Dtn

-- data    DtnChild       = DtnChild
-- newtype DtnChildren    = DtnChildren    (Seq.Seq     DtnChild)
-- newtype DtnAllChildren = DtnAllChildren (Set.HashSet DtnChild)
-- -- | Dummy Top Node
-- data Dtn =
--   Dtn
--   {
--     dtnChildren    :: !(TVar DtnChildren)
--   , dtnAllChildren :: !(TVar DtnAllChildren)
--   , dtnDtt         :: !Dtt
--   }

data    BmemParent      = BmemParent
data    BmemChild       = BmemChild
newtype BmemId          = BmemId          Id deriving Eq
newtype BmemChildren    = BmemChildren    (Seq.Seq     BmemChild)
newtype BmemToks        = BmemToks        (Set.HashSet Tok)
newtype BmemAllChildren = BmemAllChildren (Set.HashSet BmemChild)

-- | Beta Memory
data Bmem =
  Bmem
  {
    bmemId          :: !BmemId
  , bmemParent      :: !BmemParent
  , bmemChildren    :: !(TVar BmemChildren)

  , bmemAllChildren :: !(TVar BmemAllChildren)
  , bmemToks        :: !(TVar BmemToks)
  }

instance Eq Bmem where
  Bmem { bmemId = id1 } == Bmem { bmemId = id2 } = id1 == id2

newtype Distance     = Distance     Int      deriving Eq
newtype JoinField1   = JoinField1   Field    deriving Eq
newtype JoinField2   = JoinField2   Field    deriving Eq
newtype JoinDistance = JoinDistance Distance deriving Eq

data JoinTest =
  JoinTest
  {
    joinField1   :: !JoinField1
  , joinField2   :: !JoinField2
  , joinDistance :: !JoinDistance
  }
  deriving Eq

data    JoinParent          = JoinParent
data    JoinChild           = JoinChild
data    JoinNearestAncestor = JoinNearestAncestor
newtype JoinId              = JoinId            Id deriving Eq
newtype JoinChildren        = JoinChildren      (Seq.Seq JoinChild)
newtype JoinAmem            = JoinAmem          Amem
newtype JoinTests           = JoinTests         [JoinTest]
newtype JoinLeftUnlinked    = JoinLeftUnlinked  Bool
newtype JoinRightUnlinked   = JoinRightUnlinked Bool

-- | Join Node
data Join =
  Join
  {
    joinId             :: !JoinId
  , joinParent         :: !JoinParent
  , joinChildren       :: !(TVar JoinChildren)

  , joinAmem           :: !JoinAmem
  , joinNearesAncestor :: !JoinNearestAncestor
  , joinTests          :: !JoinTests
  , joinLeftUnlinked   :: !(TVar JoinLeftUnlinked)
  , joinRightUnlinked  :: !(TVar JoinRightUnlinked)
  }

instance Eq Join where
  Join { joinId = id1 } == Join { joinId = id2 } = id1 == id2

data    NegParent          = NegParent
data    NegChild           = NegChild
data    NegNearestAncestor = NegNearestAncestor
newtype NegId              = NegId            Id deriving Eq
newtype NegChildren        = NegChildren      (Seq.Seq     NegChild)
newtype NegToks            = NegToks          (Set.HashSet Tok)
newtype NegAmem            = NegAmem          Amem
newtype NegTests           = NegTests         [JoinTest]
newtype NegRightUnlinked   = NegRightUnlinked Bool

-- | Negative Node
data Neg =
  Neg
  {
    negId              :: !NegId
  , negParent          :: !NegParent
  , negChildren        :: !(TVar NegChildren)

  , negToks            :: !(TVar NegToks)
  , negAmem            :: !NegAmem
  , negTests           :: !NegTests
  , negNearestAncestor :: !NegNearestAncestor
  , negRightUnlinked   :: !(TVar NegRightUnlinked)
  }

instance Eq Neg where
  Neg { negId = id1 } == Neg { negId = id2 } = id1 == id2

data    NccParent   = NccParent
data    NccChild    = NccChild
newtype NccId       = NccId       Id deriving Eq
newtype NccChildren = NccChildren (Seq.Seq     NccChild)
newtype NccToks     = NccToks     (Set.HashSet Tok)
newtype NccPartner  = NccPartner  Partner

data Ncc =
  Ncc
  {
    nccId       :: !NccId
  , nccParent   :: !NccParent
  , nccChildren :: !(TVar NccChildren)

  , nccToks     :: !(TVar NccToks)
  , nccParner   :: !NccPartner
  }

instance Eq Ncc where
  Ncc { nccId = id1 } == Ncc { nccId = id2 } = id1 == id2

data    PartnerParent   = PartnerParent
data    PartnerChild    = PartnerChild
newtype PartnerId       = PartnerId       Id deriving Eq
newtype PartnerChildren = PartnerChildren (Seq.Seq     PartnerChild)
newtype PartnerNcc      = PartnerNcc      Ncc
newtype PartnerConjucts = PartnerConjucts Int
newtype PartnerBuff     = PartnerBuff     (Set.HashSet Tok)

data Partner =
  Partner
  {
    partnerId       :: !PartnerId
  , partnerParent   :: !PartnerParent
  , partnerChildren :: !(TVar PartnerChildren)

  , partnerNcc      :: !PartnerNcc
  , partnerConjucts :: !PartnerConjucts
  , partnerBuff     :: !(TVar PartnerBuff)
  }

instance Eq Partner where
  Partner { partnerId = id1 } == Partner { partnerId = id2 } = id1 == id2

newtype LocationField    = LocationField    Field
newtype LocationDistance = LocationDistance Distance

-- | Symbol location describes the binding for a variable within a token.
data Location = Location !LocationField !LocationDistance

-- | A map of variable bindings for productions
newtype Bindings = Bindings (Map.HashMap Symbol Location)

data    ProdParent       = ProdParent
newtype ProdId           = ProdId           Id deriving Eq
newtype ProdToks         = ProdToks         (Set.HashSet Tok)
newtype ProdAction       = ProdAction       Action
newtype ProdRevokeAction = ProdRevokeAction (Maybe Action)
newtype ProdBindings     = ProdBindings     Bindings

-- | Production Node
data Prod =
  Prod
  {
    prodId           :: !ProdId
  , prodParent       :: !ProdParent
  , prodToks         :: !(TVar ProdToks)
  , prodAction       :: !ProdAction
  , prodRevokeAction :: !ProdRevokeAction
  , prodBindings     :: !ProdBindings
  }

instance Eq Prod where
  Prod { prodId = id1 } == Prod { prodId = id2 } = id1 == id2

-- | Context of a production action
data Actx =
  Actx
  {
    actxEnv  :: !Env         -- ^ Current Env
  , actxProd :: !Prod        -- ^ Production node
  , actxTok  :: !Tok         -- ^ The matching token
  , actxWmes :: [Maybe Wme]  -- ^ The token Wmes
  }

-- | Action of a production
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
  | PosCond !WmeObj !WmeAttr !WmeVal -- canonical form

  -- Neg conds
  | NegStr  !String !String  !String
  | NegS    !S      !S       !S
  | NegCond !WmeObj !WmeAttr !WmeVal -- canonical form

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
