let mirror = "https://mirror.rackspace.com/archlinux"
let arch = "x86_64"
let repos = [ "core"; "extra"; "multilib" ]
let tmp_dir = "archlinux.tmp"
let out_dir = "archlinux"
let path = String.concat "/"

let mkdir_p s =
  String.split_on_char '/' s
  |> List.fold_left
       (fun acc d ->
         let p = if String.length acc > 0 then path [ acc; d ] else d in
         let () = if not (Sys.file_exists p) then Sys.mkdir p 0o755 in
         p)
       ""

let () =
  List.iter
    (fun repo ->
      let dest = mkdir_p (path [ tmp_dir; repo ]) in
      let () = Printf.printf "%s\n" dest in
      let file = repo ^ ".tar.gz" in
      let _ =
        if Sys.file_exists file then 0
        else Sys.command (Filename.quote_command "curl" [ "-L"; path [ mirror; repo; "os"; arch; repo ^ ".db.tar.gz" ]; "-o"; file ])
      in
      if Sys.command (Filename.quote_command "tar" [ "-xzf"; file; "-C"; dest ]) <> 0 then assert false)
    repos

let read_input file =
  let ic = open_in file in
  let rec loop input lines =
    try
      let line = input_line input in
      loop input (line :: lines)
    with
    | End_of_file ->
        close_in input;
        lines
  in
  loop ic [] |> List.rev

type pac = {
  filename : string;
  name : string;
  base : string;
  version : string;
  sha256 : string;
  deps : string list;
  provides : string list;
  conflicts : string list;
}

module List = struct
  include List

  let rec split_on_element e = function
    | [] -> ([], [])
    | hd :: tl ->
        if hd = e then ([], tl)
        else
          let a, b = split_on_element e tl in
          (hd :: a, b)
end

module String = struct
  include String

  let strstr haystack needle =
    let nlen = String.length needle in
    let rec loop = function
      | i when i < 0 -> -1
      | i -> if String.sub haystack i nlen = needle then i else loop (i - 1)
    in
    loop (String.length haystack - nlen)
end

let normalise invalid s = List.fold_left (fun acc ch -> if String.contains acc ch then String.split_on_char ch acc |> String.concat "_" else acc) s invalid

let rec process p = function
  | "%FILENAME%" :: filename :: tl -> process { p with filename } tl
  | "%NAME%" :: name :: tl -> process { p with name = normalise [ '.' ] name } tl
  | "%BASE%" :: base :: tl -> process { p with base } tl
  | "%VERSION%" :: version :: tl -> process { p with version = normalise [ ':' ] version } tl
  | "%SHA256SUM%" :: sha256 :: tl -> process { p with sha256 } tl
  | "%CONFLICTS%" :: tl ->
      let conflicts, rest = List.split_on_element "" tl in
      process { p with conflicts } rest
  | "%PROVIDES%" :: tl ->
      let provides, rest = List.split_on_element "" tl in
      process { p with provides } rest
  | "%DEPENDS%" :: tl ->
      let deps, rest = List.split_on_element "" tl in
      process { p with deps } rest
  | _ :: tl -> process p tl
  | [] -> p

let quoted_string lst = List.map (fun x -> "\"" ^ x ^ "\"") lst |> String.concat " "

let triple_of_version s =
  let name, equality, version =
    List.fold_left
      (fun (name, equality, version) n ->
        let l = String.length n in
        let i = String.strstr s n in
        if name = None && i >= 0 then (Some (String.sub s 0 i), Some (String.sub s i l), Some (String.sub s (i + l) (String.length s - l - i)))
        else (name, equality, version))
      (None, None, None) [ ">="; "<="; "<"; ">"; "=" ]
  in
  let name = normalise [ '.' ] (Option.value ~default:s name) in
  (name, equality, version)

let string_of_version v =
  let name, equality, version = v in
  match (equality, version) with
  | Some equality, Some version -> quoted_string [ name ] ^ " {" ^ equality ^ " " ^ quoted_string [ version ] ^ "}"
  | _, _ -> quoted_string [ name ]

let () =
  let _ = mkdir_p out_dir in
  let oc = open_out (path [ out_dir; "/repo" ]) in
  let () = Printf.fprintf oc "opam-version: \"2.0\"\n" in
  close_out oc

let () =
  List.iter
    (fun repo ->
      Sys.readdir (path [ tmp_dir; repo ])
      |> Array.to_list
      |> List.iter (fun folder ->
             let desc = path [ tmp_dir; repo; folder; "desc" ] in
             let content = if Sys.is_regular_file desc then read_input desc else [] in
             let d = process { filename = ""; name = ""; base = ""; version = ""; sha256 = ""; deps = []; provides = []; conflicts = [] } content in
             let () = Printf.printf "%s\n%s\n%s\n%s\n" d.filename d.name d.base d.version in
             let () = List.iter (Printf.printf "%s,") d.deps in
             let () = Printf.printf "\n\n" in
             if d.name <> "opam" then
               let opam = mkdir_p (path [ out_dir; "packages"; d.name; d.name ^ "." ^ d.version ]) in
               let oc = open_out (path [ opam; "opam" ]) in
               let () = Printf.fprintf oc "opam-version: \"2.0\"\n" in
               let () = Printf.fprintf oc "build: [%s]\n" (quoted_string [ "/usr/bin/pacman"; "-U"; "--noconfirm"; d.filename ]) in
               let () = Printf.fprintf oc "remove: [%s]\n" (quoted_string [ "/usr/bin/pacman"; "-R"; d.filename ]) in
               let () =
                 if List.length d.deps > 0 then
                   let () = Printf.fprintf oc "depends: [\n" in
                   let () = List.iter (fun e -> Printf.fprintf oc "  %s\n" (string_of_version (triple_of_version e))) d.deps in
                   Printf.fprintf oc "]\n"
               in
               let () =
                 if List.length d.conflicts > 0 then
                   let () = Printf.fprintf oc "conflicts: [\n" in
                   let () = List.iter (fun e -> Printf.fprintf oc "  %s\n" (string_of_version (triple_of_version e))) d.conflicts in
                   Printf.fprintf oc "]\n"
               in
               let () = Printf.fprintf oc "extra-source \"%s\" {\n" d.filename in
               let () = Printf.fprintf oc "  src: \"%s/%s/os/%s/%s\"\n" mirror repo arch d.filename in
               let () = if d.sha256 <> "" then Printf.fprintf oc "  checksum: [ \"sha256=%s\" ]\n" d.sha256 in
               let () = Printf.fprintf oc "}\n" in
               let () = close_out oc in
               List.iter
                 (fun p ->
                   let name, _, version = triple_of_version p in
                   let opam = mkdir_p (path [ out_dir; "packages"; name; name ^ "." ^ Option.value ~default:"1" version ]) in
                   let oc = open_out (path [ opam; "opam" ]) in
                   let () = Printf.fprintf oc "opam-version: \"2.0\"\n" in
                   let () = Printf.fprintf oc "depends: [ %s ]\n" (string_of_version (d.name, Some "=", Some d.version)) in
                   close_out oc)
                 d.provides))
    repos
