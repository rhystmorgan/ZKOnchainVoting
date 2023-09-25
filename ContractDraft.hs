--               --
-- ZKVotingDraft --
--               --

{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-matches   #-}
{-# LANGUAGE ViewPatterns             #-}
{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE UndecidableInstances     #-}

module ContractDraft () where

import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.DataRepr
import Plutarch.Lift 
import Plutarch.Api.V1.Value 
import Plutarch.Bool
import Plutarch.Prelude
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import Plutarch.Extra.ScriptContext (ptryFromInlineDatum, pfromPDatum)
import Plutarch.Extra.Value (psymbolValueOf')
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import Utils (pand'List, pcond, ppositiveSymbolValueOf, (#>), (#>=), pand'List)
import PlutusTx qualified 
import PlutusTx.AssocMap qualified as AssocMap 
import PlutusLedgerApi.V1 (BuiltinData(..), Value(..), BuiltinByteString)
import PlutusLedgerApi.V2 (ScriptHash, SerialisedScript)

--                     --
-- Data Types & Params --
--                     --

data PMintAction (s :: S) = PMint | PBurn
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMintAction where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData PMintAction
instance PTryFrom PData (PAsData PMintAction)

data PRegistryAction (s :: S) = PUpdate | PRemove
  deriving stock (Generic, Enum, Bounded)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PRegistryAction where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData PRegistryAction
instance PTryFrom PData (PAsData PRegistryAction)

data DiDatum = DiDatum
  { issueDate :: POSIXTime
  , issuedBy :: Asset
  , verifiedBy :: Asset
  , issedUsing :: BuiltinData
  , acceptedOn :: POSIXTime 
  , lastUpdated :: POSIXTime
  }
PlutusTx.makeLift ''DiDatum 
PlutusTx.makieIsDataIndexed ''DiDatum [('DiDatum, 0)]

data PDiDatum (s :: S) = PDiDatum
  (Term s
    (PDataRecord '[ "issueDate" ':= POSIXTime
                  , "isseudBy" ':= PAsset
                  , "verifiedBy" ':= PAsset
                  , "issuedUsing" ':= PData
                  , "acceptedOn" ':= POSIXTime
                  , "lastUpdated" ':= POSIXTime
                  ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PDiDatum where
  type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData PDiDatum

instance PUnsafeLiftDecl PDiDatum where type PLifted DiDatum = DiDatum
deriving via (DerivePConstantViaData DiDatum PDiDatum)

-- DidParams
{ registry :: ValidatorHash
, issuer :: ValidatorHash??
, verifier :: ValidatorHash
}

--                            --
-- TokenName Helper Functions --
--                            --

-- define tokenName splitting


-- Reference NFT Label
label100 :: Term s PByteString 
label100 = phexByteStr "000643b0" -- 6

-- NFT Label
label222 :: Term s PByteString 
label222 = phexByteStr "000de140" -- 13

ptryLookupValue :: 
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S). 
  Term s 
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts 
        :--> (PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger)))
    )
ptryLookupValue = phoistAcyclic $ plam $ \policyId val ->
  let valItems = pto (pto val)
   in (pfix #$ plam $ \self xs ->
        pelimList
          ( \y ys ->
              pif
                (policyId #== (pfstBuiltin # y))
                (pto (pfromData (psndBuiltin # y)))
                (self # ys)
          )
          perror
          xs
        )
        # valItems 

pbreakTokenName :: Term s PTokenName -> Term s (PPair PByteString PByteString)
pbreakTokenName tn = 
  let tnBS = pto tn 
   in pcon $ PPair (psliceBS # 0 # 4 # tnBS) (psliceBS # 4 # (plengthBS # tnBS) # tnBS)


--                    --
-- DID Minting Policy --
--                    --

pdidMintingPolicy :: Term s (PDidParams :--> PMintAction :--> PScriptContext :--> PUnit)
pdidMintingPolicy = phoistAcyclic $ plam $ \dp r ctx -> unTermCont $ do
  ctxF <- pletFields @'["txInfo", "purpose"] ctx
  infoF <- pletFieldsC @'["mint", "outputs", "inputs", "signatories"] ctxf.txInfo

  PMinting purpose <- pmatchC ctxF.purpose 
  ownCS <- pletC (pfield @"_0" # purpose)

  mintedItem <- pletC $ ptryLookupValue # ownCS # infoF.mint
  refTokenPair <- pletC $ phead # mintedItems
  userTokenPair <- pletC $ ptryIndex 1 mintedItems

  let txOutputs :: Term _ (PBuiltinList PTxOut)
      txOutputs = infoF.outputs
      refTokenName = pfromData $ pfstBuiltin # refTokenPair
      refTokenAmt = pfromData $ psndBuiltin # refTokenPair
      userTokenName = pfromData $ pfstBuiltin # userTokenPair
      userTokenAmt = pfromData $ psndBuiltin # userTokenPair

  PPair userLebel _userTn <- pmatchC (pbreakTokenName userTokenName)
  PPair refLebel _refTn <- pmatchC (pbreakTokenName refTokenName)

  pure $
    pif
      (pmatch redeemer $ \case
        PMint -> 
          -- Must Mint2 assets, user token and reference token
          -- tokenName == signatory && recipient of credential 
            -- (ie user token output PKH must be token name(s))
          userTokenAmt #== pconstant 1
            #&& refTokenAmt #== pconstant 1
            #&& reflabel #== label100
            #&& userLabel #== label222

          
        PBurn -> 
          -- must burn both reference and user credential
          -- must be signed by admin && pkh from token name (asset owner)
      )
      (pcontstant ())
      perror

--                        --
-- DID Registry Validator --
--                        --

pregistryValidator :: Term s (
  PRegistryParams :--> 
  PRegistryDatum :--> 
  PRegistryAction :--> 
  PScriptContext :--> 
  PUnit)
pregistryValidator = phoistAcyclic $ plam $ \rp rd r ctx -> unTermCont $ do
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  infoF <- pletFieldsC @'["inputs", "outputs", "signatories"] ctxF.txInfo
  PSPending ((pfiled @"_0") -> ownRef) <- pmatchC ctxF.purpose
  let ownInput = ptryOwnInput # infoF.inputs # ownRef
  ownInputF <- pletFieldsC @'["value", "address", "datum"] ownInput
  let ownOutput = pheadSingleton #$ pfilter # (paysToCredential # ownValHash)
  ownOutputF <- pletFields @'["value", "datum"] ownOutput
  let refToken
  pure $
    pif
      (pmatch redeemer \case
        PUpdate ->
          pelem #  # infoF.inputs
          #&& onwInputF.value #== ownOutputF.value 
          -- must include the user token in the tx
          -- must remove all signatures from the datum
          -- must spend back to the validator

        PVerify ->
          pany @PBuiltinList
            # plam (\txoF ->
                pmatch (pfield @"credential" # txoF.address) $ \case
                  PPubKeyCredential v -> pfield @"_0" # v == pbreakTokenName
                                          #&& pvalueOf # txoF.value # ptokenCS # puserTokenName
                                          #&& pelem # txoF.signatories # v 
                  PScriptCredential _ -> perror
                )
          #&& ownInputValue #== ownOutput.value
          #&& -- datum fields are the same
          #&& -- datum signatories is updated to the new element

          -- must update datum with assetclass
          -- must include User asset in the transaction
          -- must spend back to validator

        PRemove ->
          -- must burn token and burn the user token

      )
      (pconstant ())
      perror

pVotingRecord :: Term s (P)

{- 

--

Datum structure

---

Credential

{
    issueDate: POSIXTime,               -> as of right now 1691489684
    issuedBy: IssuerPolicyID.IssuerId,
    issuedUsing: "Certi.Fi",            -> this could also be a token credential as there could be multiple platforms for this process in the future
    issuedTo: nameOfRecipient,          -> this could be a name, it could also be a token that holds information about a person, such as name, age, PubKeyHash etc.
    acceptedOn: POSIXTime,
    lastUpdated: POSIXTime,
  }

  potential parties involved:

  Recipient
  Issuer
  Verifier
  
  Issuer / Verifier Credential metadata
  {
    dateCreated:
    issuerName:
    issuingBody:
    url:
    ...
    }
