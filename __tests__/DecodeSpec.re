module R = Belt.Result;

open Jest;

/* This function a hacky workaround. If you try to use this code:

       let decoded = ...;
       let result = R.Ok(Types.String);
       expect(decoded) |> toBe(result);

     The compiled JS throws an error. For some reason Jest cannot compare
     nested varients. So to circumvent this, I wrote the following function
     `expectToBe` to do the comparison with normal operators, and if the
     objects are the same then make a true assertion.  If not, throw a custom
     exception with a message of the two values compared
   */
exception NotEqual(string);
let expectToBe = (value: 'a, expected: 'a) : 'assertion =>
  Expect.(
    if (value == expected) {
      expect(true) |> toEqual(true);
    } else {
      raise(
        NotEqual(
          "Expected "
          ++ Js.String.make(value)
          ++ " to equal "
          ++ Js.String.make(expected),
        ),
      );
    }
  );

describe("Decode", () =>
  describe("Primitive", () =>
    describe("should success", () => {
      test("to decode primitve string type", () => {
        let decoded = "string" |> Js.Json.string |> Decode.Primitive.decode;
        let expected = R.Ok(Types.String);
        expectToBe(decoded, expected);
      });

      test("to decode primitve int type", () => {
        let decoded = "int" |> Js.Json.string |> Decode.Primitive.decode;
        let expected = R.Ok(Types.Int);
        expectToBe(decoded, expected);
      });

      test("to decode primitve float type", () => {
        let decoded = "float" |> Js.Json.string |> Decode.Primitive.decode;
        let expected = R.Ok(Types.Float);
        expectToBe(decoded, expected);
      });

      test("to decode primitve bool type", () => {
        let decoded = "bool" |> Js.Json.string |> Decode.Primitive.decode;
        let expected = R.Ok(Types.Bool);
        expectToBe(decoded, expected);
      });

      test("to decode primitve unit type", () => {
        let decoded = [||] |> Js.Json.array |> Decode.Primitive.decode;
        let expected = R.Ok(Types.Unit);
        expectToBe(decoded, expected);
      });

      test("to decode primitve list type", () => {
        let decoded =
          [|Js.Json.string("string")|]
          |> Js.Json.array
          |> Decode.Primitive.decode;
        let expected = R.Ok(Types.List(Types.String));
        expectToBe(decoded, expected);
      });
    })
  )
);
