let is_specific =
  let module S = Info.Common.StringSet in
  let all = S.of_list
      [ "ggp"; "fuchsia"; "xlib";  "xcb";  "wl"; "android"; "wayland"; "mir"; "win"; "win32";
        "directfb" ] in
  let unsupported = S.diff all @@ S.of_list Econfig.supported_systems in
  let check x = S.mem x unsupported in
  fun name -> List.exists (List.exists check )
    Info.Linguistic.[name.prefix;name.postfix;name.main]
