module R = Belt.Result;

open Jest;

describe("Decode", () =>
  describe("Primitive", () =>
    Expect.(
      test("to decode string type", () =>
        "string"
        |> Js.Json.string
        |> Decode.Primitive.run
        |> expect
        |> toEqual(R.Ok(Types.String))
      )
    )
  )
);
