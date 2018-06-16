module R = Belt.Result;

module Primitive = {
  type value =
    | Str(string)
    | List(list(Js.Json.t))
    | Dict(Js.Dict.t(Js.Json.t));

  type error =
    | ParseError(string)
    | UnknownType(string);

  type valueDecoder = Json.Decode.decoder(value);

  let decodeString: valueDecoder =
    Json.Decode.map(x => Str(x), Json.Decode.string);
  let decodeList: valueDecoder =
    Json.Decode.map(x => List(x), Json.Decode.list(Misc.identity));
  let decodeDict: valueDecoder =
    Json.Decode.map(x => Dict(x), Json.Decode.dict(Misc.identity));

  let rec run = (json: Js.Json.t) : R.t(Types.primitive, error) => {
    let decoded =
      Json.Decode.oneOf([decodeString, decodeList, decodeDict], json);
    switch (decoded) {
    | exception (Json.ParseError(message)) => R.Error(ParseError(message))
    | Str(str) =>
      switch (str) {
      | "string" => R.Ok(Types.String)
      | "int" => R.Ok(Types.Int)
      | "float" => R.Ok(Types.Float)
      | "bool" => R.Ok(Types.Bool)
      | "unit" => R.Ok(Types.Unit)
      | invalid => R.Error(UnknownType(invalid))
      }
    | List(list) =>
      switch (list) {
      | [] => R.Ok(Types.Unit)
      | [listType] =>
        let listTypeDecoded = run(listType);
        Misc.mapResult(x => Types.List(x), listTypeDecoded);
      | tupleTypes =>
        let tupleTypesDecoded = Misc.traverseResults(run, tupleTypes);
        Misc.mapResult(x => Types.Tuple(x), tupleTypesDecoded);
      }
    | Dict(dict) =>
      let (keys, values) =
        dict |> Js.Dict.entries |> Array.to_list |> List.split;
      let valuesDecoded = Misc.traverseResults(run, values);
      Misc.mapResult(
        actual =>
          List.combine(keys, actual)
          |> Misc.listToStringMap
          |> (x => Types.Record(x)),
        valuesDecoded,
      );
    };
  };
};
