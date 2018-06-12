let printHelp = () =>
  print_endline(
    {|
  # border-types - a type generator for different language targets

  Usage: border-types types_definitions.json

    -V, --version
        print the version
    -h, --help
        print this help
  |},
  );

let printMiministError = err => print_endline(Minimist.report(err));

let printVersion = () => print_endline("TODO");
