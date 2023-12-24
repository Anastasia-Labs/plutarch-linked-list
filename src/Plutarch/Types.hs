{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Types (
  PNodeAction (..),
  NodeAction (..),
  PConfig (..),
  Config (..),
  PSetNode (..),
  PNodeKey (..),
  PNodeKeyState (..),
  SetNode (..),
  NodeKey (..),
  isEmptySet,
  asPredecessorOf,
  asSuccessorOf,
  getNextPK,
  getCurrentPK,
  isFirstNode,
  isLastNode,
  mkNode,
  isNothing,
  validNode,
  mkBSNode,
)
where

import GHC.Generics ()
import Plutarch.Api.V2 (
  PAddress,
  PPOSIXTime,
  PPubKeyHash (PPubKeyHash),
  PTxOutRef,
 )
import Plutarch.Classes
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (PLifted))
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import PlutusLedgerApi.V2 (
  Address,
  BuiltinByteString,
  POSIXTime,
  PubKeyHash,
  TxOutRef,
 )
import PlutusTx qualified

data Config = Config
  { initUTxO :: TxOutRef
  , deadline :: POSIXTime
  , penaltyAddress :: Address
  }
  deriving stock (Generic, Eq, Show)

PlutusTx.makeIsDataIndexed ''Config [('Config, 0)]

newtype PConfig (s :: S)
  = PConfig
      ( Term
          s
          ( PDataRecord
              '[ "initUTxO" ':= PTxOutRef
               , "deadline" ':= PPOSIXTime
               , "penaltyAddress" ':= PAddress
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)
instance PUnsafeLiftDecl PConfig where type PLifted PConfig = Config
deriving via (DerivePConstantViaData Config PConfig) instance PConstantDecl Config
instance DerivePlutusType PConfig where type DPTStrat _ = PlutusTypeData

data NodeKey = Key BuiltinByteString | Empty
  deriving stock (Show, Eq, Ord, Generic)

PlutusTx.unstableMakeIsData ''NodeKey
PlutusTx.makeLift ''NodeKey

data SetNode = MkSetNode
  { key :: NodeKey
  , next :: NodeKey
  }
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed ''SetNode [('MkSetNode, 0)]
PlutusTx.makeLift ''SetNode

data NodeAction
  = Init
  | Deinit
  | -- | first arg is the key to insert, second arg is the covering node
    Insert PubKeyHash SetNode
  | -- | first arg is the key to remove, second arg is the covering node
    Remove PubKeyHash SetNode
  deriving stock (Show, Eq, Generic)

PlutusTx.makeIsDataIndexed
  ''NodeAction
  [ ('Init, 0)
  , ('Deinit, 1)
  , ('Insert, 2)
  , ('Remove, 3)
  ]
PlutusTx.makeLift ''NodeAction

data PNodeKey (s :: S)
  = PKey (Term s (PDataRecord '["_0" ':= PByteString]))
  | PEmpty (Term s (PDataRecord '[]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

deriving via
  (DerivePConstantViaData NodeKey PNodeKey)
  instance
    PConstantDecl NodeKey

instance PUnsafeLiftDecl PNodeKey where
  type PLifted PNodeKey = NodeKey

deriving anyclass instance
  PTryFrom PData PNodeKey

instance DerivePlutusType PNodeKey where type DPTStrat _ = PlutusTypeData

data PNodeKeyState (s :: S)
  = PKeyScott (Term s PByteString)
  | PEmptyScott
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PNodeKeyState where type DPTStrat _ = PlutusTypeScott

instance ScottConvertible PNodeKey where
  type ScottOf PNodeKey = PNodeKeyState
  toScott nodeKey = pmatch nodeKey $ \case
    PKey kname -> pcon (PKeyScott (pfield @"_0" # kname))
    PEmpty _ -> pcon PEmptyScott
  fromScott nodeKeyScott = pmatch nodeKeyScott $ \case
    PKeyScott bs -> pcon (PKey (pdcons # pdata bs # pdnil))
    PEmptyScott -> pcon (PEmpty pdnil)

data PSetNodeState (s :: S) = PSetNodeState
  { key :: Term s PNodeKeyState
  , next :: Term s PNodeKeyState
  }
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq)

instance DerivePlutusType PSetNodeState where type DPTStrat _ = PlutusTypeScott

newtype PSetNode (s :: S)
  = PSetNode
      ( Term
          s
          ( PDataRecord
              '[ "key" ':= PNodeKey
               , "next" ':= PNodeKey
               -- , "commitment" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PSetNode where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData PSetNode

deriving anyclass instance
  PTryFrom PData (PAsData PSetNode)

instance PUnsafeLiftDecl PSetNode where
  type PLifted PSetNode = SetNode

deriving via
  (DerivePConstantViaData SetNode PSetNode)
  instance
    PConstantDecl SetNode

instance ScottConvertible PSetNode where
  type ScottOf PSetNode = PSetNodeState
  toScott discSetNode' = pmatch discSetNode' $ \(PSetNode discSetNode) -> pletFields @'["key", "next"] discSetNode $ \discSetNodeF ->
    pcon (PSetNodeState {key = toScott discSetNodeF.key, next = toScott discSetNodeF.next})
  fromScott discSetNode =
    pmatch discSetNode $
      \( PSetNodeState
          { key
          , next
          }
        ) ->
          pcon
            ( PSetNode
                ( pdcons @"key"
                    # pdata (fromScott key)
                    #$ (pdcons @"next" # pdata (fromScott next))
                    -- #$ (pdcons @"commitment" # pdata 0)
                    #$ pdnil
                )
            )

mkNode :: Term s (PNodeKey :--> PNodeKey :--> PSetNode)
mkNode = phoistAcyclic $
  plam $ \key next ->
    pcon $
      PSetNode $
        pdcons @"key"
          # pdata key
          #$ pdcons @"next"
          # pdata next
          #$ pdnil

data PNodeAction (s :: S)
  = PInit (Term s (PDataRecord '[]))
  | PDeinit (Term s (PDataRecord '[]))
  | PInsert (Term s (PDataRecord '["keyToInsert" ':= PPubKeyHash, "coveringNode" ':= PSetNode]))
  | PRemove (Term s (PDataRecord '["keyToRemove" ':= PPubKeyHash, "coveringNode" ':= PSetNode]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType PNodeAction where type DPTStrat _ = PlutusTypeData

deriving anyclass instance
  PTryFrom PData (PAsData PNodeAction)

instance PUnsafeLiftDecl PNodeAction where
  type PLifted PNodeAction = NodeAction

deriving via
  (DerivePConstantViaData NodeAction PNodeAction)
  instance
    PConstantDecl NodeAction

-----------------------------------------------
-- Helpers:

mkBSNode :: ClosedTerm (PByteString :--> PByteString :--> PAsData PSetNode)
mkBSNode = phoistAcyclic $
  plam $ \key' next' ->
    let key = pcon $ PKey $ pdcons @"_0" # pdata key' #$ pdnil
        next = pcon $ PKey $ pdcons @"_0" # pdata next' #$ pdnil
     in pdata $ mkNode # key # next

-- | Checks that the node is the empty head node and the datum is empty
isEmptySet :: ClosedTerm (PAsData PSetNode :--> PBool)
isEmptySet = phoistAcyclic $
  plam $ \head -> P.do
    keys <- pletFields @'["key", "next"] head
    isNothing # pfromData keys.key #&& isNothing # pfromData keys.next

-- | Checks that a PubKeyHash does belong to the first Node in the set.
isFirstNode :: ClosedTerm (PByteString :--> PSetNode :--> PBool)
isFirstNode = phoistAcyclic $
  plam $ \key node -> P.do
    keys <- pletFields @'["key", "next"] node
    pmatch (keys.next) $ \case
      PKey n ->
        key #== pfromData (pfield @"_0" # n) #&& isNothing # pfromData keys.key
      _ -> pcon PFalse

-- | Checks that a PubkeyHash does belong to the last Node in a set.
isLastNode :: ClosedTerm (PByteString :--> PSetNode :--> PBool)
isLastNode = phoistAcyclic $
  plam $ \key node -> P.do
    keys <- pletFields @'["key", "next"] node
    pmatch (keys.key) $ \case
      PKey ((pfield @"_0" #) -> n) ->
        key #== pfromData n #&& isNothing # pfromData keys.next
      _ -> pcon PFalse

-- | Checks that node key is absent.
isNothing :: Term s (PNodeKey :--> PBool)
isNothing = phoistAcyclic $
  plam $ \md -> pmatch md $ \case
    PKey _ -> pcon PFalse
    PEmpty _ -> pcon PTrue

{- | @
 node `asPredecessorOf` next
 @ makes @node@ to be a predecessor of a node with *key* @next@
 Seen as if the node between them was removed.
 @node.key@ remains the same, @node.next@ changes to @next@.
-}
asPredecessorOf :: ClosedTerm (PAsData PSetNode :--> PByteString :--> PSetNode)
asPredecessorOf = phoistAcyclic $
  plam $ \node next ->
    let nodeKey = pfromData $ pfield @"key" # node
        nextPK = pcon $ PKey $ pdcons @"_0" # pdata next #$ pdnil
     in mkNode # nodeKey # nextPK

{- | @
   key `asSuccessorOf` node
 @ makes @node@ to be a successor of a node with *next* @key@
 Seen as if the node between them was removed.
 @node.next@ remains the same, @node.key@ changes to @key@.
-}
asSuccessorOf :: ClosedTerm (PByteString :--> PAsData PSetNode :--> PSetNode)
asSuccessorOf = phoistAcyclic $
  plam $ \key node ->
    let nodeNext = pfromData $ pfield @"next" # node
        keyPK = pcon $ PKey $ pdcons @"_0" # pdata key #$ pdnil
     in mkNode # keyPK # nodeNext

-- | Extracts the next node key
getNextPK :: ClosedTerm (PAsData PSetNode :--> PMaybe PPubKeyHash)
getNextPK = phoistAcyclic $
  plam $ \node ->
    let nextNodeKey = pfromData $ pfield @"next" # node
     in pmatch nextNodeKey $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> n) -> pcon $ PJust $ pcon $ PPubKeyHash $ pfromData n

-- | Extracts the node key
getCurrentPK :: ClosedTerm (PAsData PSetNode :--> PMaybe PPubKeyHash)
getCurrentPK = phoistAcyclic $
  plam $ \node ->
    let nodeKey = pfromData $ pfield @"key" # node
     in pmatch nodeKey $ \case
          PEmpty _ -> pcon PNothing
          PKey ((pfield @"_0" #) -> n) -> pcon $ PJust $ pcon $ PPubKeyHash $ pfromData n

{- | Checks whether @SetNode@ key is less than next node key.
 Any valid sequence of nodes MUST follow this property.
-}
validNode :: ClosedTerm (PAsData PSetNode :--> PBool)
validNode = phoistAcyclic $
  plam $ \node -> P.do
    nodeDatum <- pletFields @'["key", "next"] node
    pmatch (nodeDatum.key) $ \case
      PEmpty _ -> pcon PTrue
      PKey ((pfield @"_0" #) -> key) -> pmatch (nodeDatum.next) $ \case
        PEmpty _ -> pcon PTrue
        PKey ((pfield @"_0" #) -> next) ->
          pfromData key #< pfromData next -- nodes ordered incrementally
