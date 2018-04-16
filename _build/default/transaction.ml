open Core
open Nocrypto.Dsa

type t = {
  sender: string option;
  recipient: string;
  amount: int;
  signature: Cstruct.t;
}

let create ~sender ~recipient amount =
  { sender = Some sender; recipient; amount; signature = ""}

let sign transaction with_key =
  let key = Nocrypto.Dsa.priv_of_sexp with_key in
  let (r,s) = Nocrypto.Dsa.sign key transaction in
  {transaction with signature = r}

let reward_miner address amount with_key =
  let tx = { sender = None; recipient = address; amount = amount; signature = "" } in
  sign tx with_key

let to_string {sender; recipient; amount; signature} =
  match sender with
    None -> Printf.sprintf "Recipient:%s, Amount:%i, Signature:%s" recipient amount signature
  | Some s -> Printf.sprintf "Recipient:%s, Sender:%s, Amount:%i, Signature:%s" recipient s amount signature

let execute this_transaction pending_transactions ~with_key =
  sign this_transaction with_key :: pending_transactions