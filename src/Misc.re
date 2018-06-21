module R = Belt.Result;
module D = Js.Dict;

/* CORE */

let identity = (a: 'a) : 'a => a;
let (<<) = (f, g, x) => g(f(x));
let (>>) = (f, g, x) => f(g(x));

/* STRING */
let stringToList = (s: string) : list(char) => {
  let rec helper = (i: int, l: list(char)) =>
    if (i < 0) {
      l;
    } else {
      helper(i - 1, [s.[i], ...l]);
    };

  helper(String.length(s) - 1, []);
};

/* LIST */

let listToString = (chars: list(char)) : string => {
  let strings = List.map(x => String.make(1, x), chars);
  String.concat("", strings);
};

let listToStringMap = (l: list((string, 'value))) : Types.map('value) =>
  List.fold_right(
    ((key, value), map) => Types.StringMap.add(key, value, map),
    l,
    Types.StringMap.empty,
  );

/* Js.Dict */

let dictToStringMap = (d: Js.Dict.t('a)) : Types.StringMap.t('a) => {
  let pairs = d |> Js.Dict.entries |> Array.to_list;

  List.fold_right(
    ((key, value), acc) => Types.StringMap.add(key, value, acc),
    pairs,
    Types.StringMap.empty,
  );
};

/* RESULT */

let toOptionResult = (result: R.t('a, 'e)) : option('a) =>
  switch (result) {
  | R.Ok(value) => Some(value)
  | R.Error(_) => None
  };

let mapResult = (func: 'a => 'b, value: R.t('a, 'e)) : R.t('b, 'e) =>
  R.map(value, func);

let andThenResult =
    (func: 'a => R.t('b, 'e), value: R.t('a, 'e))
    : R.t('b, 'e) =>
  switch (value) {
  | R.Error(error) => R.Error(error)
  | R.Ok(result) => func(result)
  };

let mapErrorResult = (func: 'e1 => 'e2, value: R.t('a, 'e1)) : R.t('a, 'e2) =>
  switch (value) {
  | R.Error(error) => R.Error(func(error))
  | R.Ok(result) => R.Ok(result)
  };

let map2Result =
    (func: ('a, 'b) => 'c, a: R.t('a, 'e), b: R.t('b, 'e))
    : R.t('c, 'e) =>
  switch (a, b) {
  | (R.Error(error), _) => R.Error(error)
  | (_, R.Error(error)) => R.Error(error)
  | (R.Ok(aValue), R.Ok(bValue)) => R.Ok(func(aValue, bValue))
  };

let map3Result =
    (
      func: ('a, 'b, 'c) => 'd,
      a: R.t('a, 'e),
      b: R.t('b, 'e),
      c: R.t('c, 'e),
    )
    : R.t('d, 'e) =>
  switch (a, b, c) {
  | (R.Error(error), _, _) => R.Error(error)
  | (_, R.Error(error), _) => R.Error(error)
  | (_, _, R.Error(error)) => R.Error(error)
  | (R.Ok(aValue), R.Ok(bValue), R.Ok(cValue)) =>
    R.Ok(func(aValue, bValue, cValue))
  };

let traverseListResults =
    (func: 'a => R.t('b, 'e), l: list('a))
    : R.t(list('b), 'e) =>
  List.fold_right(
    (cur, acc) =>
      map2Result(
        (curValue, accValue) => [curValue, ...accValue],
        func(cur),
        acc,
      ),
    l,
    R.Ok([]),
  );

let traverseMapResults =
    (func: 'a => R.t('b, 'e), m: Types.StringMap.t('a))
    : R.t(Types.StringMap.t('b), 'e) =>
  Types.StringMap.fold(
    (key, cur, acc) =>
      map2Result(
        (curValue, accValue) => Types.StringMap.add(key, curValue, accValue),
        func(cur),
        acc,
      ),
    m,
    R.Ok(Types.StringMap.empty),
  );
