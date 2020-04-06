(* simple config via json ------------------------------------------- *)

module S = struct
  type conf_endpt = {
    inet_addr:string;
    port:int
  } [@@deriving yojson]

  type config = {
    client: conf_endpt;
    server: conf_endpt;
    log_everything: bool;
  }  [@@deriving yojson]

  let default_config = Some {
      client={ inet_addr="127.0.0.1"; port=8001 };
      server={ inet_addr="127.0.0.1"; port=8002 };
      log_everything=true;
    }

  let filename="minifs_config.json"
end
open S

module Runtime_config_ = Tjr_config.Make(S)

let config = Runtime_config_.config
               
let conf_endpt_to_endpt (e:conf_endpt) : Net_intf.endpt =
  ADDR_INET(Unix.inet_addr_of_string e.inet_addr, e.port)

let client_endpt_pair = 
  let f = conf_endpt_to_endpt in
  Net_intf.{local=f config.client; remote=f config.server}

let server_endpt_pair = 
  let f = conf_endpt_to_endpt in
  Net_intf.{local=f config.server; remote=f config.client}



(*
let test () = { 
  client={ip="client_ip"; port=1234 };
  server={ip="server_ip"; port=1234 };
  log_everything=false;
} |> config_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline


open Unix


let get_config ?(filename="minifs_config.json") () = 
  let c = 
    Tjr_file.read_file filename
    |> Yojson.Safe.from_string
    |> config_of_yojson
    |> function
    | Ok c -> c
    | Error _ -> failwith __LOC__
  in
  (* let open Unix in *)
  let c' = c.client |> quad_to_addr in
  let s = c.server |> quad_to_addr in

  let client = Tjr_connection.{ local=c'; remote=s } in
  let server = Tjr_connection.{ local=s; remote=c' } in
  let log_everything = c.log_everything in 
  fun k -> k ~client ~server ~log_everything
*)
