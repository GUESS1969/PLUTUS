
-- MARKETPLACE SMART CONTRACT
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Contracts.MarketPlace where

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils

-- Data type structure of the marketplace
type NFTIdentifier = TokenName
type Seller = PubKeyHash
type Price = Integer
type MarketplaceOwner = PubKeyHash
type Lovelace = Integer -- Represents ADA values in Lovelaces
type Buyer = PubKeyHash

-- The sale information of a locked NFT
data NFTSale = NFTSale
  { nftId :: NFTIdentifier -- NFT identifier
  , nftImage :: BuiltinByteString -- NFT image
  , nftSymbol :: BuiltinByteString -- NFT symbol
  , seller :: Seller -- The seller public key hash
  , price :: Price -- The price of the NFT
  }
  deriving (Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''NFTSale

-- The Datum, which can hold information about an NFT locked in the contract
data MarketplaceDatum = NFTLocked NFTSale | NoNFTLocked
  deriving (Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''MarketplaceDatum

-- The Redeemer type defines the actions that can be taken by interacting with the smart contract
data MarketplaceAction = Lock | Unlock
  deriving (Show, Generic, FromJSON, ToJSON)

unstableMakeIsData ''MarketplaceAction

-- Placeholder for the marketplace owner's public key hash
marketplaceOwner :: PubKeyHash
marketplaceOwner = PubKeyHash "c1272b677c5d0a894f5dd5472711aab5ca59d856f55d12157670c930"

-- The marketplace fee as 1/20 of the price of asset
marketplaceFee :: Price -> Lovelace
marketplaceFee p = p `divide` 20

-- Verifies that a payment of a certain amount is made to a specific address
{-# INLINEABLE paymentMadeTo #-}
paymentMadeTo :: TxInfo -> PubKeyHash -> Integer -> Bool
paymentMadeTo txInfo recipient amount =
  any (\o -> txOutValue o #== lovelaceValueOf amount && isPaidTo o recipient) (txInfoOutputs txInfo)

-- Helper to check if a TxOut is paid to the given PubKeyHash
{-# INLINEABLE isPaidTo #-}
isPaidTo :: TxOut -> PubKeyHash -> Bool
isPaidTo txOut recipient =
  case toPubKeyHash $ txOutAddress txOut of
    Just pkh -> pkh #== recipient
    Nothing -> False

-- function for checking all the payments
{-# INLINEABLE allPaymentsCorrect #-}
allPaymentsCorrect :: TxInfo -> PubKeyHash -> PubKeyHash -> Integer -> Bool
allPaymentsCorrect txInfo sellerAddr marketplaceOwnerAddr price =
  let isSellerPayment o = txOutValue o #== lovelaceValueOf price && isPaidTo o sellerAddr
      isFeePayment _ = paymentMadeTo txInfo marketplaceOwnerAddr (marketplaceFee price)
   in any isSellerPayment (txInfoOutputs txInfo) && any isFeePayment (txInfoOutputs txInfo)

-- function to check availability of NFT
{-# INLINEABLE inputsContainNFT #-}
inputsContainNFT :: ScriptContext -> AssetClass -> Bool
inputsContainNFT ctx asset =
  any (containsAsset asset 1 . txInInfoResolved) (txInfoInputs $ scriptContextTxInfo ctx)

-- Verifies that the marketplace fee is paid to the marketplace owner
{-# INLINEABLE feePaidToMarketplace #-}
feePaidToMarketplace :: TxInfo -> Price -> Bool
feePaidToMarketplace txInfo p =
  let fee = marketplaceFee p
   in any (\o -> txOutValue o #== lovelaceValueOf fee && isPaidTo o marketplaceOwner) (txInfoOutputs txInfo)

-- Checks if the given TxOut contains the specified amount of a given asset
{-# INLINEABLE containsAsset #-}
containsAsset :: AssetClass -> Integer -> TxOut -> Bool
containsAsset asset amount txOut =
  assetClassValueOf (txOutValue txOut) asset #== amount

-- It assumes that an NFT can be distinguished by the contract's own policy ID
{-# INLINEABLE findNftIdToLock #-}
findNftIdToLock :: ScriptContext -> TxInfo -> Maybe TokenName
findNftIdToLock ctx txInfo =
  listToMaybe
    [ tn | i <- txInfoInputs txInfo, let v = txOutValue $ txInInfoResolved i, (cs, tn, amt) <- flattenValue v,
    -- Ensure the token is under the contract's policy ID
    cs #== ownCurrencySymbol ctx,
    -- Ensure the token amount is 1, indicative of NFT uniqueness
    amt #== 1,
    -- Additional filtering based on TokenName, assuming a naming convention for NFTs
    isNftTokenName tn,
    -- Ensure the NFT is not already locked (this requires implementation)
    isNftUnique ctx tn
    ]

-- function to transfer ownership to the asset buyer
nftTransferred :: TxInfo -> AssetClass -> Bool
nftTransferred txInfo asset =
  any (\o -> assetClassValueOf (txOutValue o) asset #== 1) (txInfoOutputs txInfo)
{-# INLINEABLE nftTransferred #-}

-- function, always returns True to check uniqueness
isNftUnique :: ScriptContext -> TokenName -> Bool
isNftUnique _ _ = True
{-# INLINEABLE isNftUnique #-}

-- function to check if the token name follows NFT naming conventions
isNftTokenName :: TokenName -> Bool
isNftTokenName _ = True
{-# INLINEABLE isNftTokenName #-}

-- The main logic lambda function to validate the transactions
{-# INLINEABLE mkValidator #-}
mkValidator :: Seller -> MarketplaceDatum -> MarketplaceAction -> ScriptContext -> Bool
mkValidator _ datum redeemer ctx = case (datum, redeemer) of
  (NFTLocked _, Lock) -> traceIfFalse "NFT already locked" False
  (NoNFTLocked, Unlock) -> traceIfFalse "No NFT to unlock" False
  (NFTLocked nftSale, Unlock) ->
    let txInfo = scriptContextTxInfo ctx
        asset = AssetClass (ownCurrencySymbol ctx, nftId nftSale)
        sellerAddr = PaymentPubKeyHash $ seller nftSale
        price' = price nftSale
        paymentsCorrect = allPaymentsCorrect txInfo (unPaymentPubKeyHash sellerAddr) marketplaceOwner price'
        assetContained = any (containsAsset asset 1) (txInfoOutputs txInfo)
     in traceIfFalse "Payments incorrect or NFT not transferred or not contained" (paymentsCorrect && assetContained)
  (NoNFTLocked, Lock) ->
    let txInfo = scriptContextTxInfo ctx
        maybeNftId = findNftIdToLock ctx txInfo
        assetContained = maybe False (\nftId -> any (containsAsset (AssetClass (ownCurrencySymbol ctx, nftId)) 1) (txInfoOutputs txInfo)) maybeNftId
     in traceIfFalse "NFT not identified to lock or not contained" assetContained

-- Conversion mkValidator to use BuiltinData
untypedLockUnlockValidator :: Seller -> UntypedValidator
untypedLockUnlockValidator = mkUntypedValidator . mkValidator

-- Pre-compilation of the contract
type LockUnlockValidator = ValidatorContract "nft-marketplace"

compileLockUnlockValidator :: Seller -> LockUnlockValidator
compileLockUnlockValidator pkh = mkValidatorContract ($$(compile [||untypedLockUnlockValidator||]) `applyCode` liftCode pkh)

-- Ange is a seller
sampleSeller :: Seller
sampleSeller = "039f51d0c3f33b9d4c78d4492548d62fdf116f1dbbdfa5e0d6d900dd"

-- robert is the buyer
sampleBuyer :: Buyer
sampleBuyer = "53b4939fa0fef0b160ee80334f179d4251d59e064876ae030749ef66"

-- MarketPlaceOwner ----Cyprien
sampleMarketplaceOwner :: MarketplaceOwner
sampleMarketplaceOwner = "c1272b677c5d0a894f5dd5472711aab5ca59d856f55d12157670c930"

-- Constructing datum for exporting
mkLockUnlockDatum :: NFTIdentifier -> BuiltinByteString -> BuiltinByteString -> Seller -> Price -> MarketplaceDatum
mkLockUnlockDatum nftId nftImage nftSymbol seller price =
  let datum = NFTLocked $ NFTSale {nftId, nftImage, nftSymbol, seller, price}
   in datum

-- Exports for jambala-cli
lockUnlockValidatorExports :: JambExports
lockUnlockValidatorExports =
  export
    (defExports $ compileLockUnlockValidator sampleSeller)
      { dataExports =
          [ mkSampleLockUnlockDatum "NFT1" "ipfs://QmUNt1Ziv2915gN23SVtAswKZQY9cPtMGmcNzgEQq7Pxh9" "IVOCOINS" sampleSeller 1000000 `toJSONfile` "lockedNFT1"
          , Unlock `toJSONfile` "unlockRedeemer"
          , Lock `toJSONfile` "lockRedeemer"
          ]
      , emulatorTest = test
      }
  where
    mkSampleLockUnlockDatum = mkLockUnlockDatum

-- Define the validator endpoints and parameter types

instance ValidatorEndpoints LockUnlockValidator where
  newtype GiveParam LockUnlockValidator = LockNFT NFTSale
    deriving (Generic, FromJSON, ToJSON)

  newtype GrabParam LockUnlockValidator = UnlockNFT NFTIdentifier
    deriving (Generic, FromJSON, ToJSON)

  -- Implement the give function
  give :: GiveParam LockUnlockValidator -> ContractM LockUnlockValidator ()
  give (LockNFT sale) = do
    let datum = NFTLocked sale
    let sellerpkh = seller sale
    let appliedValidator = compileLockUnlockValidator sellerpkh
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor appliedValidator
        , constraints = mustPayScriptWithDatum appliedValidator (Datum $ toBuiltinData datum) (lovelaceValueOf 0)
        }
    logInfo @String "Locked NFT in contract"

  -- Implement the grab function
  grab :: GrabParam LockUnlockValidator -> ContractM LockUnlockValidator ()
  grab (UnlockNFT _) = do
    utxos <- getUtxosAt appliedValidator
    let constraints =
          mconcat [mustSpendScriptOutput oref unitRedeemer | (oref, _) <- Map.toList utxos]
            <> mustPayToPubKey (PaymentPubKeyHash sampleMarketplaceOwner) (lovelaceValueOf 3_000_000)
            <> mustPayToPubKey (PaymentPubKeyHash sampleSeller) (lovelaceValueOf 3_000_000)
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor appliedValidator `andUtxos` utxos
        , constraints = constraints
        }
    logInfo @String "Unlocked NFT from contract"
    where
      appliedValidator = compileLockUnlockValidator sampleSeller

-- Define emulator test
test :: EmulatorTest
test =
  initEmulator @LockUnlockValidator
    3
    [ --LockNFT (NFTSale "NFT1" "ipfs://QmUNt1Ziv2915gN23SVtAswKZQY9cPtMGmcNzgEQq7Pxh9" "IVOCOINS" sampleSeller 1000000) `fromWallet` 1 -- Lock NFT
      UnlockNFT "NFT1" `toWallet` 2 -- Unlock NFT
    ]
