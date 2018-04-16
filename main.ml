open Core
open Blockchain
open Transaction


let () =
  Blockchain.create_n 10 
  |> Blockchain.to_string 
  |> printf "\n%s"
