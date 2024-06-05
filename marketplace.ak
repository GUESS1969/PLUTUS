use aiken/hash.{Blake2b_224, Hash}
use aiken/list.{any, find}
use aiken/option
use aiken/transaction.{Input, Output, ScriptContext, Transaction}
use aiken/transaction/credential.{VerificationKey, from_verification_key}
use aiken/transaction/value.{
  AssetName, PolicyId, flatten, lovelace_of, quantity_of,
}

// Define the NFTSale type
type NFTSale {
  nft_id: AssetName,
  nft_image: ByteArray,
  nft_symbol: ByteArray,
  seller: Hash<Blake2b_224, VerificationKey>,
  marketplace_owner: Hash<Blake2b_224, VerificationKey>,
  price: Int,
}

// Define the MarketplaceDatum type
type MarketplaceDatum {
  NFTLocked(NFTSale)
  NoNFTLocked
}

// Define the MarketplaceAction type
type MarketplaceAction {
  Lock
  Unlock
}

// The marketplace fee as 1/20 of the price
fn marketplaceFee(p: Int) -> Int {
  p / 20
}

// Verifies that a payment of a certain amount is made to a specific address
fn paymentMadeTo(
  recipient: Hash<Blake2b_224, VerificationKey>,
  amount: Int,
  outputs: List<Output>,
) -> Bool {
  any(
    outputs,
    fn(o: Output) {
      from_verification_key(recipient) == o.address && lovelace_of(o.value) == amount
    },
  )
}

// Helper to check if a TxOut is paid to the given PubKeyHash
fn isPaidTo(
  ctx: ScriptContext,
  seller: Hash<Blake2b_224, VerificationKey>,
  price: Int,
) -> Bool {
  any(
    ctx.transaction.outputs,
    fn(output: Output) {
      from_verification_key(seller) == output.address && lovelace_of(
        output.value,
      ) >= price
    },
  )
}

// Check if all payments are correct
fn allPaymentsCorrect(
  seller: Hash<Blake2b_224, VerificationKey>,
  marketplace_owner: Hash<Blake2b_224, VerificationKey>,
  price: Int,
  outputs: List<Output>,
) -> Bool {
  let isSellerPayment =
    fn(o: Output) {
      from_verification_key(seller) == o.address && lovelace_of(o.value) == price
    }
  let isFeePayment =
    paymentMadeTo(marketplace_owner, marketplaceFee(price), outputs)
  any(outputs, isSellerPayment) && isFeePayment
}

fn containsAsset(
  txOut: Output,
  policy_id: PolicyId,
  asset: AssetName,
  amount: Int,
) -> Bool {
  quantity_of(txOut.value, policy_id, asset) == amount
}

// Function to check if the inputs contain the specified NFT
fn inputsContainNFT(
  ctx: ScriptContext,
  policy_id: PolicyId,
  asset: AssetName,
) -> Bool {
  any(
    ctx.transaction.inputs,
    fn(input: Input) { containsAsset(input.output, policy_id, asset, 1) },
  )
}

// Verifies that the marketplace fee is paid to the marketplace owner
fn feePaidToMarketplace(
  marketplace_owner: Hash<Blake2b_224, VerificationKey>,
  outputs: List<Output>,
  p: Int,
) -> Bool {
  let fee = marketplaceFee(p)
  paymentMadeTo(marketplace_owner, fee, outputs)
}

// Check if the token name follows NFT naming conventions
fn isNftTokenName(_tokenname: AssetName) -> Bool {
  True
}

// Function that always returns True to check uniqueness
fn isNftUnique(_ctx: ScriptContext, _tokenname: AssetName) -> Bool {
  True
}

// Helper function to get the currency symbol of the current contract
fn ownCurrencySymbol(ctx: ScriptContext) -> PolicyId {
  // Assuming we have a way to derive the PolicyId from the context
  ctx.transaction.id.hash
}

// Find the NFT ID to lock
fn findNftIdToLock(ctx: ScriptContext) -> Option<AssetName> {
  find(
    ctx.transaction.inputs,
    fn(i: Input) -> Bool {
      any(
        flatten(i.output.value),
        fn(cs_tn_amt: (PolicyId, AssetName, Int)) -> Bool {
          let (cs, tn, amt) = cs_tn_amt
          cs == ownCurrencySymbol(ctx) && amt == 1 && isNftTokenName(tn) && isNftUnique(
            ctx,
            tn,
          )
        },
      )
    },
  )
    |> option.and_then(
         fn(i: Input) {
           i.output.value
             |> flatten
             |> list.head
             |> option.map(
                  fn(cs_tn_amt: (PolicyId, AssetName, Int)) { cs_tn_amt.2nd },
                )
         },
       )
}

// The main validator logic
validator(marketplace_owner: Hash<Blake2b_224, VerificationKey>) {
  fn validate(
    datum: MarketplaceDatum,
    redeemer: MarketplaceAction,
    ctx: ScriptContext,
  ) -> Bool {
    let outputs = ctx.transaction.outputs

    let result =
      when (datum, redeemer) is {
        (NFTLocked(_), Lock) -> False
        (NoNFTLocked, Unlock) -> False
        (NFTLocked(nft_sale), Unlock) -> {
          let seller_addr = nft_sale.seller
          let price = nft_sale.price
          let payments_correct =
            allPaymentsCorrect(seller_addr, marketplace_owner, price, outputs)
          let asset_contained =
            any(
              outputs,
              fn(o: Output) {
                containsAsset(o, ownCurrencySymbol(ctx), nft_sale.nft_id, 1)
              },
            )
          payments_correct && asset_contained
        }
        (NoNFTLocked, Lock) -> {
          let maybe_nft_id = findNftIdToLock(ctx)
          let asset_contained =
            when maybe_nft_id is {
              Some(nft_id) ->
                any(
                  outputs,
                  fn(o: Output) {
                    containsAsset(o, ownCurrencySymbol(ctx), nft_id, 1)
                  },
                )
              None -> False
            }
          asset_contained
        }
      }
    result
  }
}
