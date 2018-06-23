module R = Belt.Result;
module O = Belt.Option;

type decoder('result) = Json.Decode.decoder('result);

let decodeAsResult =
    (decoder: decoder('result))
    : decoder(R.t('result, string)) =>
  (json: Js.Json.t) =>
    switch (decoder(json)) {
    | exception (Json.ParseError(message)) => R.Error(message)
    | value => R.Ok(value)
    };

module Primitive = {
  type decodeType('result) =
    | Str(string)
    | List(list('result))
    | Dict(Js.Dict.t('result));

  type decodeTypeDecoder('result) = decoder(decodeType('result));

  let stringDecoder: decodeTypeDecoder('result) =
    Json.Decode.map(x => Str(x), Json.Decode.string);
  let listDecoder =
      (subDecoder: decoder('result))
      : decodeTypeDecoder('result) =>
    Json.Decode.map(x => List(x), Json.Decode.list(subDecoder));
  let dictDecoder =
      (subDecoder: decoder('result))
      : decodeTypeDecoder('result) =>
    Json.Decode.map(x => Dict(x), Json.Decode.dict(subDecoder));

  let stringValue = "string";
  let intValue = "int";
  let floatValue = "float";
  let boolValue = "bool";

  type error =
    | ParseError(string)
    | UnknownTypeError(string)
    | ListError(error)
    | RecordError(error);

  let rec decodeInternal = (json: Js.Json.t) : R.t(Types.primitive, error) => {
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
      if (str == stringValue) {
        R.Ok(Types.String);
      } else if (str == intValue) {
        R.Ok(Types.Int);
      } else if (str == floatValue) {
        R.Ok(Types.Float);
      } else if (str == boolValue) {
        R.Ok(Types.Bool);
      } else {
        R.Error(UnknownTypeError(str));
      }
    | List(list) =>
      switch (list) {
      | [] => R.Ok(Types.Unit)
      | [listType] =>
        let listTypeDecoded = decodeInternal(listType);
        Misc.mapResult(x => Types.List(x), listTypeDecoded);
      | tupleTypes =>
        tupleTypes
        |> Misc.traverseListResults(decodeInternal)
        |> Misc.mapResult(x => Types.Tuple(x))
        |> Misc.mapErrorResult(x => ListError(x))
      }
    | Dict(dict) =>
      let (keys, values) =
        dict |> Js.Dict.entries |> Array.to_list |> List.split;
      let valuesDecoded = Misc.traverseListResults(decodeInternal, values);
      Misc.mapResult(
        actual =>
          List.combine(keys, actual)
          |> Misc.listToStringMap
          |> (x => Types.Record(x)),
        valuesDecoded,
      );
    };
  };

  let rec errorToString = (error: error) : string =>
    switch (error) {
    | ParseError(message) => message
    | UnknownTypeError(message) =>
      "I was expecting one of \""
      ++ stringValue
      ++ "\", \""
      ++ intValue
      ++ "\", \""
      ++ floatValue
      ++ "\", \""
      ++ boolValue
      ++ "\", an object, or an array but got \""
      ++ message
      ++ "\" instead."
    | ListError(subError) =>
      "I ran into an error decoding a list or x-tuple: "
      ++ errorToString(subError)
    | RecordError(subError) =>
      "I ran into an error decoding a record: " ++ errorToString(subError)
    };

  let decode = (json: Js.Json.t) : R.t(Types.primitive, string) =>
    json |> decodeInternal |> Misc.mapErrorResult(errorToString);
};

module Custom = {
  /* HELPERS */

  let startsWithCapitalLetter = (str: string) : bool =>
    switch (str.[1]) {
    | exception (Invalid_argument(_)) => false
    | firstChar =>
      let ascii = Char.code(firstChar);
      ascii > 64 && ascii < 91;
    };

  let containsInvalidChars = (str: string) : bool => {
    let chars = Misc.stringToList(str);
    List.fold_right(
      (cur, acc) => acc ? acc : cur == ' ' || cur == '\n' || cur == '\r',
      chars,
      false,
    );
  };

  /* DECODING */

  type decodeType =
    | Str(string)
    | PrimitiveType(R.t(Types.primitive, string));

  type decodeTypeDecoder = decoder(decodeType);

  let stringDecoder: decodeTypeDecoder =
    Json.Decode.map(x => Str(x), Json.Decode.string);
  let primitiveDecoder: decodeTypeDecoder =
    Json.Decode.map(x => PrimitiveType(x), Primitive.decode);

  type error =
    | ParseError(string)
    | ParseValueError(option(string), string)
    | ParseConstructorError(option(string), error)
    | InvalidNameError(string, string)
    | InvalidKindError(option(string), string);

  let nameKey = "name";
  let kindKey = "kind";
  type kindValue =
    | Union
    | Alias;
  let kindUnionValue = "union";
  let kindAliasValue = "alias";
  let valueKey = "value";
  let constructorsKey = "constructors";

  let nameDecoder = (json: Js.Json.t) : R.t(string, error) =>
    json
    |> decodeAsResult(Json.Decode.field(nameKey, Json.Decode.string))
    |> Misc.mapErrorResult(x => ParseError(x))
    |> Misc.andThenResult((decodedName: string) =>
         if (String.length(decodedName) == 0) {
           R.Error(InvalidNameError(decodedName, "not be an empty string"));
         } else if (! startsWithCapitalLetter(decodedName)) {
           R.Error(
             InvalidNameError(decodedName, "start with a capital letter"),
           );
         } else if (containsInvalidChars(decodedName)) {
           R.Error(
             InvalidNameError(
               decodedName,
               "contain any space or new line characters",
             ),
           );
         } else {
           R.Ok(decodedName);
         }
       );

  let kindDecoder =
      (kind: kindValue, json: Js.Json.t)
      : R.t(unit, option(string) => error) => {
    let expectedKind =
      switch (kind) {
      | Union => kindUnionValue
      | Alias => kindAliasValue
      };

    json
    |> decodeAsResult(Json.Decode.field(nameKey, Json.Decode.string))
    |> Misc.mapErrorResult((x, _) => ParseError(x))
    |> Misc.andThenResult((decodedKind: string) =>
         if (decodedKind != expectedKind) {
           R.Error(
             (optionName: option(string)) =>
               InvalidKindError(optionName, decodedKind),
           );
         } else {
           R.Ok();
         }
       );
  };

  let aliasDecoder: decoder(R.t(Types.custom, error)) =
    json => {
      let decodedName = nameDecoder(json);
      let decodedKind =
        json
        |> kindDecoder(Alias)
        |> Misc.mapErrorResult(makeError =>
             decodedName |> Misc.toOptionResult |> makeError
           );
      let decodedValue =
        json
        |> Json.Decode.field(valueKey, Primitive.decode)
        |> Misc.mapErrorResult(x =>
             ParseValueError(Misc.toOptionResult(decodedName), x)
           );

      Misc.map3Result(
        (name: string, _kind: unit, value: Types.primitive) =>
          Types.Alias({name, value}),
        decodedName,
        decodedKind,
        decodedValue,
      );
    };

  let constructorsDecoder =
      (customDecoder: decoder(R.t(Types.custom, error)))
      : decoder(R.t(Types.map(list(Types.custom)), error)) =>
    (json: Js.Json.t) => {
      let listDecoder: decoder(R.t(list(Types.custom), error)) =
        Json.Decode.list(Misc.identity)
        |> decodeAsResult
        |> Json.Decode.map(result =>
             result
             |> Misc.mapErrorResult(x => ParseError(x))
             |> Misc.andThenResult(Misc.traverseListResults(customDecoder))
           );

      let dictDecoder =
        Json.Decode.dict(Misc.identity)
        |> decodeAsResult
        |> Json.Decode.map(result =>
             result
             |> Misc.mapErrorResult(x => ParseError(x))
             |> Misc.andThenResult(dict =>
                  dict
                  |> Misc.dictToStringMap
                  |> Misc.traverseMapResults(listDecoder)
                )
           );

      dictDecoder(json);
    };

  let unionDecoder =
      (customDecoder: decoder(R.t(Types.custom, error)))
      : decoder(R.t(Types.custom, error)) =>
    json => {
      let decodedName = nameDecoder(json);
      let decodedKind =
        json
        |> kindDecoder(Union)
        |> Misc.mapErrorResult(makeError =>
             decodedName |> Misc.toOptionResult |> makeError
           );

      let decodedConstructors =
        json
        |> Json.Decode.field(
             constructorsKey,
             constructorsDecoder(customDecoder),
           )
        |> Misc.mapErrorResult(x =>
             ParseConstructorError(Misc.toOptionResult(decodedName), x)
           );

      Misc.map3Result(
        (
          name: string,
          _kind: unit,
          constructors: Types.map(list(Types.custom)),
        ) =>
          Types.Union({name, constructors}),
        decodedName,
        decodedKind,
        decodedConstructors,
      );
    };

  let rec decodeInternal: decoder(R.t(Types.custom, error)) =
    json =>
      Json.Decode.oneOf([unionDecoder(decodeInternal), aliasDecoder], json);

  let rec errorToString = (error: error) : string =>
    switch (error) {
    | ParseError(message) => message
    | ParseValueError(optionName, message) =>
      let prefix =
        O.mapWithDefault(optionName, "a union type", name =>
          "the union type \"" ++ name ++ "\""
        );
      "While parsing "
      ++ prefix
      ++ ", I ran into the following error: "
      ++ message;
    | ParseConstructorError(optionName, error) =>
      let prefix =
        O.mapWithDefault(optionName, "a union type", name =>
          "the union type \"" ++ name ++ "\""
        );
      "While parsing "
      ++ prefix
      ++ ", I ran into the following error: "
      ++ errorToString(error);
    | InvalidNameError(name, message) =>
      "While parsing the union type \""
      ++ name
      ++ "\" , I expected then \"name\" field to "
      ++ message
      ++ "."
    | InvalidKindError(optionName, kind) =>
      let prefix =
        O.mapWithDefault(optionName, "a union type", name =>
          "the union type \"" ++ name ++ "\""
        );
      "While parsing "
      ++ prefix
      ++ " and the field \"kind\", I was expecting either \"alias\" or \"union\" "
      ++ "but got \""
      ++ kind
      ++ "\"";
    };

  let decode = (json: Js.Json.t) : R.t(Types.custom, string) =>
    json |> decodeInternal |> Misc.mapErrorResult(errorToString);
};
