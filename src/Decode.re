module R = Belt.Result;

type decoder('result) = Json.Decode.decoder('result);

type jsonType('result) =
  | Str(string)
  | List(list('result))
  | Dict(Js.Dict.t('result));

type jsonTypeDecoder('result) = decoder(jsonType('result));

let stringDecoder: jsonTypeDecoder('result) =
  Json.Decode.map(x => Str(x), Json.Decode.string);
let listDecoder = (subDecoder: decoder('result)) : jsonTypeDecoder('result) =>
  Json.Decode.map(x => List(x), Json.Decode.list(subDecoder));
let dictDecoder = (subDecoder: decoder('result)) : jsonTypeDecoder('result) =>
  Json.Decode.map(x => Dict(x), Json.Decode.dict(subDecoder));

module Primitive = {
  type error =
    | ParseError(string)
    | UnknownTypeError(string)
    | ListError(error)
    | RecordError(error);

  let rec decode = (json: Js.Json.t) : R.t(Types.primitive, error) => {
    let decoded =
      Json.Decode.oneOf(
        [
          stringDecoder,
          listDecoder(Misc.identity),
          dictDecoder(Misc.identity),
        ],
        json,
      );

    switch (decoded) {
    | exception (Json.ParseError(message)) => R.Error(ParseError(message))
    | Str(str) =>
      switch (str) {
      | "string" => R.Ok(Types.String)
      | "int" => R.Ok(Types.Int)
      | "float" => R.Ok(Types.Float)
      | "bool" => R.Ok(Types.Bool)
      | "unit" => R.Ok(Types.Unit)
      | invalid => R.Error(UnknownTypeError(invalid))
      }
    | List(list) =>
      switch (list) {
      | [] => R.Ok(Types.Unit)
      | [listType] =>
        let listTypeDecoded = decode(listType);
        Misc.mapResult(x => Types.List(x), listTypeDecoded);
      | tupleTypes =>
        tupleTypes
        |> Misc.traverseResults(decode)
        |> Misc.mapResult(x => Types.Tuple(x))
        |> Misc.mapErrorResult(x => ListError(x))
      }
    | Dict(dict) =>
      let (keys, values) =
        dict |> Js.Dict.entries |> Array.to_list |> List.split;
      let valuesDecoded = Misc.traverseResults(decode, values);
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

module Custom = {
  type error =
    | Parse(string)
    | MissingName
    | MissingKind
    | InvalidName(string, string)
    | InvalidKind(string, string);

  let rec decode = (json: Js.Json.t) : R.t(Types.custom, error) => {
    let rootDecoded =
      Json.Decode.dict(
        Json.Decode.oneOf([
          stringDecoder,
          listDecoder(decode),
          dictDecoder(Primitive.decode),
        ]),
      );

    switch (rootDecoded) {
    | exception (Json.ParseError(message)) => R.Error(Parse(message))
    | root =>
      let nameDecoded = Js.Dict.get(root, "name");
      let kindDecoded = Js.Dict.get(root, "kind");
      switch (nameDecoded, kindDecoded) {
      | (None, _) => R.Error(MissingName)
      | (_, None) => R.Error(MissingKind)
      | _ => R.Ok()
      };
    };
  };
};
