module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

type map('subType) = StringMap.t('subType);

type primitiveType =
  | String
  | Int
  | Float
  | Bool
  | Unit
  | List(primitiveType)
  | Tuple(primitiveType)
  | Record(string, map(primitiveType));

type aliasMetaData = {
  name: string,
  value: primitiveType,
};

type unionMetaData = {
  name: string,
  constructors: list(map(primitiveType)),
};

type customType =
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
  types: list(customType),
};
