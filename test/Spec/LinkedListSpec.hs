{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Spec.LinkedListSpec (
  mkLiquidityNodeMP,
  mkLiquidityNodeMPW,
) where

import Plutarch.Api.V2 (
  PMintingPolicy,
  PScriptContext,
  PTxOutRef,
 )
import Plutarch.Extra.Interval (pafter, pbefore)

--  pRemoveAndDeinit,

import Plutarch.Helpers (
  hasUtxoWithRef,
 )
import Plutarch.LinkedList (
  PPriceDiscoveryCommon (mint, ownCS),
  makeCommon,
  pClaim,
  pDeinit,
  pInit,
  pInsert,
  pRemove,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Unsafe (punsafeCoerce)

import Plutarch.Prelude
import Plutarch.Types (PDiscoveryConfig (..), PDiscoveryNodeAction (..))
import Plutarch.Utils (pand'List, passert, pcond)

--------------------------------
-- FinSet Node Minting Policy:
--------------------------------

mkDiscoveryNodeMP ::
  ClosedTerm
    ( PDiscoveryConfig
        :--> PDiscoveryNodeAction
        :--> PScriptContext
        :--> PUnit
    )
mkDiscoveryNodeMP = plam $ \discConfig redm ctx -> P.do
  configF <- pletFields @'["initUTxO"] discConfig

  (common, inputs, outs, sigs, vrange) <-
    runTermCont $
      makeCommon ctx

  pmatch redm $ \case
    PInit _ -> P.do
      passert "Init must consume TxOutRef" $
        hasUtxoWithRef # configF.initUTxO # inputs
      pInit common
    PDeinit _ ->
      -- TODO deinit must check that reward fold has been completed
      pDeinit common
    PInsert action -> P.do
      act <- pletFields @'["keyToInsert", "coveringNode"] action
      let insertChecks =
            pand'List
              [ pafter # (pfield @"discoveryDeadline" # discConfig) # vrange
              , pelem # act.keyToInsert # sigs
              ]
      pif insertChecks (pInsert common # act.keyToInsert # act.coveringNode) perror
    PRemove action -> P.do
      configF <- pletFields @'["discoveryDeadline"] discConfig
      act <- pletFields @'["keyToRemove", "coveringNode"] action
      discDeadline <- plet configF.discoveryDeadline
      pcond
        [ pbefore # discDeadline # vrange, pClaim common outs sigs # act.keyToRemove
        , pafter # discDeadline # vrange, pRemove common vrange discConfig outs sigs # act.keyToRemove # act.coveringNode
        ]
        perror

mkDiscoveryNodeMPW ::
  ClosedTerm
    ( PDiscoveryConfig
        :--> PMintingPolicy
    )
mkDiscoveryNodeMPW = phoistAcyclic $ plam $ \discConfig redm ctx ->
  let red = punsafeCoerce @_ @_ @PDiscoveryNodeAction redm
   in popaque $ mkDiscoveryNodeMP discConfig # red # ctx
