(* simple config via json ------------------------------------------- *)

type quad = {
  ip:string;
  port:int
} [@@deriving yojson]

type config = {
  client:quad;
  server: quad;
  log_everything: bool;
}  [@@deriving yojson]


let test () = { 
  client={ip="client_ip"; port=1234 };
  server={ip="server_ip"; port=1234 };
  log_everything=false;
} |> config_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline


open Unix

let quad_to_addr q =
  ADDR_INET(Unix.inet_addr_of_string q.ip, q.port)


let get_config ~filename = 
  let c = 
    Tjr_file.read_file "config.json" 
    |> Yojson.Safe.from_string
    |> config_of_yojson
    |> function
    | Ok c -> c
    | Error _ -> failwith __LOC__
  in
  let open Unix in
  let c' = c.client |> quad_to_addr in
  let s = c.server |> quad_to_addr in

  let client = Tjr_connection.{ local=c'; remote=s } in
  let server = Tjr_connection.{ local=s; remote=c' } in
  let log_everything = c.log_everything in 
  fun k -> k ~client ~server ~log_everything
