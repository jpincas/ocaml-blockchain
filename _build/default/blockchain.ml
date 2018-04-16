open Core
open Stdio
open Block
open Transaction

let jon = "04bc9865402bc41313de96febec0078302e3d184932d118deb607f966505e448f2ae0a6ee96df954aa99c87af739ebaede58f023ee104bfaf3f16e01e23efd0632"
let jon_key = "53247997263117442929260004267783043342898584171298065171308541626819238572348"

let jessi = "043e51fa2003b3f40429f1d11020e29a46b22019dd92624cc1b345607ad89b5412216866286c1c044f415b8b294cc46ed4afe17358583a45d440a8b6c8dfee1840"

let jessi_key = "96361439401356539239289603481947133023947578293036640307435575628746143601129"

let jon_sends_to_jessi = Transaction.create ~sender:jon ~recipient:jessi 100
let jessi_sends_to_jon = Transaction.create ~sender:jessi ~recipient:jon 100

let transactions = 
  [] 
  |> Transaction.execute jon_sends_to_jessi ~with_key: jon_key
  |> Transaction.execute jessi_sends_to_jon ~with_key: jessi_key

let to_string blockchain =
  blockchain |> List.map ~f:Block.to_string |> String.concat

let verify_indices (this,head) =
  phys_equal this.index (Utils.increment head.index)

let verify_hashes (this,head) =
  phys_equal head.hash this.previous_hash

let verify_blocks bs =
  verify_indices bs 
  && verify_hashes bs

let verify_incoming_block blockchain this_block =
  match blockchain with
    [] -> false
  | head_block :: _ -> 
    Block.verify this_block && verify_blocks (this_block, head_block)

let add_block blockchain this_block =
  match verify_incoming_block blockchain this_block with
    false -> blockchain
  | true -> this_block :: blockchain

let rec add_n_blocks n blockchain =
  match blockchain with
    _ when n = 0 -> blockchain
  | [] -> blockchain
  | head_block :: _ -> 
    let add_to_blockchain = add_block blockchain in
    (Block.create (Some head_block) transactions) 
    |> add_to_blockchain
    |> add_n_blocks (n-1)

let create_n n =
  let genesis = Block.create None ~transactions:[] in
  let genesis_list = genesis :: [] in
  add_n_blocks (n-1) genesis_list

