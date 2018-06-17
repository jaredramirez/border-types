module R = Belt.Result;
module D = Js.Dict;

/* CORE */

let identity = (a: 'a) : 'a => a;
let (<<) = (f, g, x) => g(f(x));
let (>>) = (f, g, x) => f(g(x));

/* RESULT */

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
  | (_, R.Error(error)) => R.Error(error)
  | (R.Error(error), _) => R.Error(error)
  | (R.Ok(aValue), R.Ok(bValue)) => R.Ok(func(aValue, bValue))
  };

let traverseResults =
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

let listToStringMap = (l: list((string, 'value))) : Types.map('value) =>
  List.fold_right(
    ((key, value), map) => Types.StringMap.add(key, value, map),
    l,
    Types.StringMap.empty,
  );
