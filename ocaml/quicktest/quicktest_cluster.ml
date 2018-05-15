
module C = Client.Client

let is_empty = function | [] -> true | _ -> false
let rpc = !Quicktest_common.rpc

(** --- Helpers for reconfiguration --- *)

let reconfigure_ipv4 ~session_id ~self ~dNS =
  let netmask = C.PIF.get_netmask ~session_id ~rpc ~self in
  let iP = C.PIF.get_IP ~session_id ~rpc ~self in
  let gateway = C.PIF.get_gateway ~session_id ~rpc ~self in
  let mode = C.PIF.get_ip_configuration_mode ~session_id ~rpc ~self in
  C.PIF.reconfigure_ip ~session_id ~rpc ~self ~iP ~dNS ~gateway ~netmask ~mode

let reconfigure_ipv6 ~session_id ~self ~dNS =
  (* confirm valid IPv6 strings exist *)
  let iPv6_lst = (C.PIF.get_IPv6 ~session_id ~rpc ~self) |> List.filter ((<>) "") in
  if is_empty iPv6_lst
  then Alcotest.fail "No valid IPv6 strings exist.";

  let gateway = C.PIF.get_ipv6_gateway ~session_id ~rpc ~self in
  let mode = C.PIF.get_ipv6_configuration_mode ~session_id ~rpc ~self in
  let iPv6 = List.hd iPv6_lst in
  C.PIF.reconfigure_ipv6 ~session_id ~rpc ~self ~iPv6 ~dNS ~gateway ~mode

(** --- Test skeleton, receives environment params before running  --- *)
let test_reconfigure_ip ~ipv6 ~session_id ~(self : API.ref_PIF) ~(cluster_host : API.ref_Cluster_host) =
  let ip_string = if ipv6 then "IPv6" else "IPv4" in
  Printf.printf "Testing reconfiguring %s with clustering.\n" ip_string;
  try
    let dNS = C.PIF.get_DNS ~session_id ~rpc ~self in
    let (reconfig_function : unit -> unit) =
      if ipv6
      then (fun () -> reconfigure_ipv6 ~session_id ~self ~dNS)
      else (fun () -> reconfigure_ipv4 ~session_id ~self ~dNS)
    in
    Alcotest.check_raises
      "test_reconfigure_ip should fail when clustering is enabled"
      Api_errors.(Server_error (clustering_enabled_on_network, [ Ref.string_of cluster_host ]))
      reconfig_function
  with
  | Api_errors.(Server_error(code,_)) when code <> Api_errors.clustering_enabled_on_network -> ()
  (* Don't fail on other API errors, only test clustering *)

(** --- Check environment before calling test --- *)
let test session_id () =
  print_endline "Testing IP reconfiguration with and without clustering.";
  let enabled_cluster_hosts =
    List.filter
      (fun self -> C.Cluster_host.get_enabled ~session_id ~rpc ~self)
      (C.Cluster_host.get_all ~session_id ~rpc)
  in
  if is_empty enabled_cluster_hosts
  then print_endline "No cluster objects on this PIF, skipping tests."
  else begin
    enabled_cluster_hosts
    |> List.map  (fun cluster_host         -> cluster_host, C.Cluster_host.get_PIF ~session_id ~rpc ~self:cluster_host)
    |> List.iter (fun (cluster_host, self) -> test_reconfigure_ip ~ipv6:false ~session_id ~self ~cluster_host)
    (* IPv6 clusters not yet supported, can run this test once that changes:
       test_reconfigure_ip ~ipv6:true ~session_id ~self ~cluster_host *)
  end

let tests session_id =
  [ "IP reconfiguration test", `Slow, test session_id
  ]
