module R = Belt.Result;
module D = Js.Dict;

let identity = (a: 'a) : 'a => a;

let mapResult = (func: 'a => 'b, value: R.t('a, 'e)) : R.t('b, 'e) =>
  R.map(value, func);

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
