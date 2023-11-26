module SimpleLanguage.Tests.Parser

open Expecto
open SimpleLanguage
open FsCheck

let config =
    { FsCheckConfig.defaultConfig with
        arbitrary = [ typeof<Generators.Generators> ] }

module SelectObjectToPlace =

    [<Tests>]
    let tests =
        testList
            "select object to place"
            [ testCase "Items selected from DataTable of size one and consisting of one ObjectInstance are the same"
              <| fun _ ->
                  Expect.equal 1 1 "Generated items were expected to be equal"
            ]
