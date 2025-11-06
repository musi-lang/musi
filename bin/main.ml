let () =
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  let exit_code =
    try
      match Cli.parse_args () with
      | Cli.Compile { input; output; search_paths } ->
        Commands.compile input output search_paths
      | Cli.Check { input; search_paths } -> Commands.check input search_paths
      | Cli.Run { input; search_paths } -> Commands.run input search_paths
      | Cli.Disasm { input } -> Commands.disasm input
      | Cli.Help -> Commands.help ()
      | Cli.Version -> Commands.version ()
    with
    | Sys_error msg ->
      Output.error msg;
      1
    | exn ->
      Output.error (Printexc.to_string exn);
      1
  in
  exit exit_code
