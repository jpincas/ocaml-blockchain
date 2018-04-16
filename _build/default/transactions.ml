open Core

let to_string list_of_transactions =
  list_of_transactions 
  |> List.map ~f:Transaction.to_string 
  |> String.concat
  |> Printf.sprintf "TRANSACTIONS: %s"