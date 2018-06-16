module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

type map('subType) = StringMap.t('subType);

type primitive =
  | String
  | Int
  | Float
  | Bool
  | Unit
  | List(primitive)
  | Tuple(list(primitive))
  | Record(map(primitive));

type aliasMetaData = {
  name: string,
  value: primitive,
};

type unionMetaData = {
  name: string,
  constructors: list(map(primitive)),
};

type custom =
  | Alias(aliasMetaData)
  | Union(unionMetaData);

type languageMetaData = {outputPath: string};

type language =
  | ElmConfig(languageMetaData)
  | ReasonConfig(languageMetaData)
  | TypeScriptConfig(languageMetaData)
  | FlowTypeConfig(languageMetaData);

type config = {
  languages: list(language),
  types: list(custom),
};
