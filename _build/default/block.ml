open Core
open Sha256
open Utils
open Transaction
open Transactions

type t = {
  index: int;
  nonce: int;
  previous_hash: string;
  hash: string;
  time_stamp: Time.t;
  coinbase: Transaction.t;
  transactions: Transaction.t list; 
}

let blank_hash = "0000000000000000000000000000000000000000000000000000000000000000"
let miner = "0457df8160a94d0c46ac530aa26b3bd8504c7b1311eafef15d23954a7958381f50ab5aad62a22ad296f7cf50a2f618ba8f78cbcbf301e5cae645cff550dabf6d1e"

let miner_key = "94552477888986636199915020373038064382336777228033613343116201654206953645482"
let miners_reward = 1


let hash_block {index; previous_hash; time_stamp; nonce; coinbase; transactions} =
  Printf.sprintf "%i%s%s%i%s%s" index previous_hash (Time.to_string time_stamp) nonce (coinbase |> Transaction.to_string) (transactions |> Transactions.to_string)
  |> Sha256.string 
  |> Sha256.to_hex

let rec mine block =
  let this_hash = hash_block block in
  match String.to_list this_hash with
  '0' :: '0' :: '0' :: _ -> { block with hash = this_hash}
  | _ -> {block with nonce = Utils.increment block.nonce} |> mine

let new_block ?(previous_hash=blank_hash) index transactions =
  mine { index = index;
         nonce = 0;
         previous_hash = previous_hash; 
         hash = ""; 
         time_stamp = Time.now ();
         transactions = transactions;
         coinbase = Transaction.reward_miner miner miners_reward miner_key }

let create optional_previous_block ~transactions =
  match optional_previous_block with
    None -> new_block 0 []
  | Some { index; hash } -> new_block (Utils.increment index) transactions ~previous_hash:hash

let to_string { index; time_stamp; previous_hash; nonce; hash; coinbase; transactions} =
  Printf.sprintf 
    "BLOCK - Index: %i, Timestamp:%s, Previous Block Hash: %s, Nonce: %i, Hash: %s, Coinbase: %s, Transactions: %s\n" 
    index (time_stamp |> Time.to_string) previous_hash nonce hash (coinbase |> Transaction.to_string) (transactions |> Transactions.to_string)


let verify block =
  hash_block block = block.hash
