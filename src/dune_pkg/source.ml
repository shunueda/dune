open Import

type t =
  { url : Loc.t * OpamUrl.t
  ; checksums : (Loc.t * Checksum.t) list
  }

let remove_locs { url = _loc, url; checksums } =
  { url = Loc.none, url
  ; checksums = List.map checksums ~f:(fun (_loc, checksum) -> Loc.none, checksum)
  }
;;

let equal
      { url = loc, url; checksums }
      { url = other_loc, other_url; checksums = other_checksums }
  =
  Loc.equal loc other_loc
  && OpamUrl.equal url other_url
  && List.equal
       (fun (loc, checksum) (other_loc, other_checksum) ->
          Loc.equal loc other_loc && Checksum.equal checksum other_checksum)
       checksums
       other_checksums
;;

let to_dyn { url = _loc, url; checksums } =
  Dyn.record
    [ "url", Dyn.string (OpamUrl.to_string url)
    ; "checksums", Dyn.list (fun (_loc, checksum) -> Checksum.to_dyn checksum) checksums
    ]
;;

let hash { url; checksums } =
  Tuple.T2.hash
    (Tuple.T2.hash Loc.hash OpamUrl.hash)
    (List.hash (Tuple.T2.hash Loc.hash Checksum.hash))
    (url, checksums)
;;

let digest_feed = Dune_digest.Feed.generic

let fetch_archive_cached =
  let cache = Single_run_file_cache.create () in
  fun (url_loc, url) ->
    Single_run_file_cache.with_ cache ~key:(OpamUrl.to_string url) ~f:(fun target ->
      Fetch.fetch_without_checksum ~unpack:false ~target ~url:(url_loc, url))
;;

let fetch_and_hash_archive_cached (url_loc, url) =
  let open Fiber.O in
  fetch_archive_cached (url_loc, url)
  >>| function
  | Ok target ->
    Some
      (match Md5.file target with
       | Ok digest -> Checksum.of_md5 digest
       | Error exn ->
         User_error.raise
           ~loc:url_loc
           [ Pp.textf "failed to fetch %s" (OpamUrl.to_string url); Exn.pp exn ])
  | Error message_opt ->
    let message =
      Option.value
        ~default:
          (User_message.make
             [ Pp.textf
                 "Failed to retrieve source archive from: %s"
                 (OpamUrl.to_string url)
             ])
        message_opt
    in
    User_warning.emit_message message;
    None
;;

let compute_missing_checksum
      ({ url = url_loc, url; checksums } as fetch)
      package_name
      ~pinned
  =
  let open Fiber.O in
  match checksums with
  | _ :: _ -> Fiber.return fetch
  | [] when OpamUrl.is_local url || OpamUrl.is_version_control url -> Fiber.return fetch
  | [] ->
    if
      not pinned
      (* No point in warning this about pinned packages. The user explicitly
          asked for the pins *)
    then
      User_message.print
        (User_message.make
           [ Pp.textf
               "Package %S has source archive which lacks a checksum."
               (Package_name.to_string package_name)
           ; Pp.textf
               "The source archive will be downloaded from: %s"
               (OpamUrl.to_string url)
           ; Pp.text "Dune will compute its own checksum for this source archive."
           ]);
    fetch_and_hash_archive_cached (url_loc, url)
    >>| Option.map ~f:(fun checksum ->
      { url = url_loc, url; checksums = [ Loc.none, checksum ] })
    >>| Option.value ~default:fetch
;;

module Fields = struct
  let copy = "copy"
  let fetch = "fetch"
  let url = "url"
  let checksum = "checksum"
end

let decode_fetch =
  let open Decoder in
  let+ url_loc, url = field Fields.url OpamUrl.decode_loc
  and+ checksums = field_o Fields.checksum (repeat (located string)) in
  let checksums =
    Option.value ~default:[] checksums
    |> List.map ~f:(fun ((loc, _) as checksum) ->
      let checksum = Checksum.of_string_user_error checksum |> User_error.ok_exn in
      loc, checksum)
  in
  { url = url_loc, url; checksums }
;;

let external_copy (loc, path) =
  let path = Path.External.to_string path in
  let url : OpamUrl.t = { transport = "file"; path; hash = None; backend = `rsync } in
  { url = loc, url; checksums = [] }
;;

let decode =
  let open Decoder in
  sum
    [ ( Fields.copy
      , located string
        >>| fun (loc, source) path ->
        let path =
          if Filename.is_relative source
          then Path.External.relative path source
          else Path.External.of_string source
        in
        external_copy (loc, path) )
    ; ( Fields.fetch
      , let+ fetch = fields decode_fetch in
        fun _ -> fetch )
    ]
;;

let encode_fetch_field { url = _loc, url; checksums } =
  let open Encoder in
  [ field Fields.url string (OpamUrl.to_string url)
  ; field_l Fields.checksum Checksum.encode (List.map checksums ~f:snd)
  ]
;;

let encode t =
  let open Encoder in
  named_record_fields Fields.fetch (encode_fetch_field t)
;;

let kind t =
  let _, url = t.url in
  if OpamUrl0.is_local url && url.backend = `rsync
  then `Directory_or_archive (Path.External.of_string url.path)
  else `Fetch
;;
