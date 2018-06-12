let parseArgs =
  Minimist.parse(
    ~alias=[("h", "help"), ("V", "version")],
    ~presence=["help", "version"],
  );

let parsed =
  Node.Process.argv |> Js.Array.sliceFrom(2) |> Array.to_list |> parseArgs;

switch (parsed) {
| Minimist.Error(err) =>
  Print.printMiministError(err);
  exit(1);
| Ok(options) =>
  if (Minimist.has("help", options.presence)) {
    Print.printHelp();
    exit(0);
  } else if (Minimist.has("version", options.presence)) {
    Print.printVersion();
    exit(0);
  } else {
    switch (List.hd(options.rest)) {
    | fileName =>
      print_endline(fileName);
      exit(0);
    | exception Not_found => exit(1)
    };
  }
};
