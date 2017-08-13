module RingBufferTests exposing (..)

import Expect exposing (Expectation)
import Internal exposing (Internal)
import RingBuffer exposing (RingBuffer(..))
import Test exposing (Test, describe, test, todo)


suite : Test
suite =
    describe "RingBuffer"
        [ describe "RingBuffer.fromList"
            [ describe "with size of 0"
                [ test "fits no items" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 0 [ "a", "b", "c" ]
                        in
                        Expect.equalLists [] (RingBuffer.toList buf)
                ]
            , describe "with size of 1 and 3 insertions"
                [ test "is last item" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 1 [ "a", "b", "c" ]
                        in
                        Expect.equalLists [ "c" ] (RingBuffer.toList buf)
                ]
            , describe "with size of 3 and 3 insertions"
                [ test "holds full array" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 [ "a", "b", "c" ]
                        in
                        Expect.equalLists [ "a", "b", "c" ] (RingBuffer.toList buf)
                ]
            , describe "with size of 3 and 6 insertions"
                [ test "holds last 3 items" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 [ "a", "b", "c", "d", "e", "f" ]
                        in
                        Expect.equalLists [ "d", "e", "f" ] (RingBuffer.toList buf)
                ]
            ]
        , describe "RingBuffer.first"
            [ describe "with size of 0"
                [ test "returns Nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 0 [ "a", "b", "c", "d", "e", "f" ]
                        in
                        Expect.equal Nothing (RingBuffer.first buf)
                ]
            , describe "with size > 0 but empty"
                [ test "returns Nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 []
                        in
                        Expect.equal Nothing (RingBuffer.first buf)
                ]
            , describe "with size > 0 but not empty"
                [ test "returns (Just firstValue)" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 [ "a", "b", "c" ]
                        in
                        Expect.equal (Just "a") (RingBuffer.first buf)
                ]
            ]
        , describe "RingBuffer.last"
            [ describe "with size of 0"
                [ test "returns Nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 0 [ "a", "b", "c", "d", "e", "f" ]
                        in
                        Expect.equal Nothing (RingBuffer.last buf)
                ]
            , describe "with size > 0 but empty"
                [ test "returns Nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 []
                        in
                        Expect.equal Nothing (RingBuffer.last buf)
                ]
            , describe "with size > 0 but not empty"
                [ test "returns (Just lastValue)" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 [ "a", "b", "c" ]
                        in
                        Expect.equal (Just "c") (RingBuffer.last buf)
                ]
            ]
        , describe "RingBuffer.get"
            [ test "works" <|
                \_ ->
                    let
                        buf =
                            RingBuffer.fromList 3 [ "a", "b", "c" ]
                    in
                    Expect.all
                        [ -- Negative range wraps backwards
                          Expect.equal (Just "c") << RingBuffer.get -1
                        , Expect.equal (Just "b") << RingBuffer.get -2
                        , Expect.equal (Just "a") << RingBuffer.get -3

                        -- Index in range
                        , Expect.equal (Just "a") << RingBuffer.get 0
                        , Expect.equal (Just "b") << RingBuffer.get 1
                        , Expect.equal (Just "c") << RingBuffer.get 2

                        -- Overflows to beginning
                        , Expect.equal (Just "a") << RingBuffer.get 3
                        , Expect.equal (Just "b") << RingBuffer.get 4
                        ]
                        buf
            ]
        , describe "RingBuffer.set"
            [ test "works" <|
                \_ ->
                    let
                        buf =
                            RingBuffer.fromList 3 [ "a", "b", "c" ]
                    in
                    Expect.all
                        [ -- Negative range wraps backwards
                          Expect.equalLists [ "a", "b", "x" ] << RingBuffer.toList << RingBuffer.set -1 "x"
                        , Expect.equalLists [ "a", "x", "c" ] << RingBuffer.toList << RingBuffer.set -2 "x"
                        , Expect.equalLists [ "x", "b", "c" ] << RingBuffer.toList << RingBuffer.set -3 "x"

                        -- Index in range
                        , Expect.equalLists [ "x", "b", "c" ] << RingBuffer.toList << RingBuffer.set 0 "x"
                        , Expect.equalLists [ "a", "x", "c" ] << RingBuffer.toList << RingBuffer.set 1 "x"
                        , Expect.equalLists [ "a", "b", "x" ] << RingBuffer.toList << RingBuffer.set 2 "x"

                        -- Overflows to beginning
                        , Expect.equalLists [ "x", "b", "c" ] << RingBuffer.toList << RingBuffer.set 3 "x"
                        , Expect.equalLists [ "a", "x", "c" ] << RingBuffer.toList << RingBuffer.set 4 "x"
                        , Expect.equalLists [ "a", "b", "x" ] << RingBuffer.toList << RingBuffer.set 5 "x"
                        ]
                        buf
            ]
        , describe "RingBuffer.empty"
            [ describe "negative size"
                [ test "will get floored at 0" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.empty -1
                        in
                        Expect.equal 0 (RingBuffer.capacity buf)
                ]
            ]
        , describe "RingBuffer.pop"
            [ describe "0 size"
                [ test "returns Nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 0 [ "a" ]
                        in
                        Expect.equal ( Nothing, buf ) (RingBuffer.pop buf)
                ]
            , describe "> 0 size but empty"
                [ test "returns Nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 []
                        in
                        Expect.equal ( Nothing, buf ) (RingBuffer.pop buf)
                ]
            , describe "non-empty buffer"
                [ test "works" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 [ "a", "b", "c" ]
                        in
                        Expect.all
                            [ -- 1 pop
                              Expect.equal ( Just "c", RingBuffer.fromList 3 [ "a", "b" ] )
                                << RingBuffer.pop

                            -- 2 pops
                            , Expect.equal ( Just "b", RingBuffer.fromList 3 [ "a" ] )
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop

                            -- 3 pops
                            , Expect.equal ( Just "a", RingBuffer.fromList 3 [] )
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop

                            -- 4 pops (last pop should work)
                            , Expect.equal ( Nothing, RingBuffer.fromList 3 [] )
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop

                            -- 5 pops (popping empty buffer should work)
                            , Expect.equal ( Nothing, RingBuffer.fromList 3 [] )
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop
                                << Tuple.second
                                << RingBuffer.pop
                            ]
                            buf
                ]
            ]
        , describe "RingBuffer.popLeft"
            [ describe "0 size"
                [ test "returns Nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 0 [ "a" ]
                        in
                        Expect.equal ( Nothing, buf ) (RingBuffer.popLeft buf)
                ]
            , describe "> 0 size but empty"
                [ test "returns Nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 []
                        in
                        Expect.equal ( Nothing, buf ) (RingBuffer.popLeft buf)
                ]
            , describe "non-empty buffer"
                [ test "works" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 3 [ "a", "b", "c" ]
                        in
                        Expect.all
                            [ -- 1x
                              \buf1 ->
                                let
                                    ( maybeValue, buf2 ) =
                                        RingBuffer.popLeft buf1
                                in
                                Expect.equal
                                    ( Just "a", [ "b", "c" ] )
                                    ( maybeValue, RingBuffer.toList buf2 )

                            -- 2x
                            , \buf1 ->
                                let
                                    ( maybeValue, buf2 ) =
                                        buf1
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                in
                                Expect.equal
                                    ( Just "b", [ "c" ] )
                                    ( maybeValue, RingBuffer.toList buf2 )

                            -- 3x
                            , \buf1 ->
                                let
                                    ( maybeValue, buf2 ) =
                                        buf1
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                in
                                Expect.equal
                                    ( Just "c", [] )
                                    ( maybeValue, RingBuffer.toList buf2 )

                            -- 4x
                            , \buf1 ->
                                let
                                    ( maybeValue, buf2 ) =
                                        buf1
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                in
                                Expect.equal
                                    ( Nothing, [] )
                                    ( maybeValue, RingBuffer.toList buf2 )

                            -- 5x
                            , \buf1 ->
                                let
                                    ( maybeValue, buf2 ) =
                                        buf1
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                            |> Tuple.second
                                            |> RingBuffer.popLeft
                                in
                                Expect.equal
                                    ( Nothing, [] )
                                    ( maybeValue, RingBuffer.toList buf2 )
                            ]
                            buf
                ]
            ]
        , describe "RingBuffer.pushLeft"
            [ describe "0 size"
                [ test "changes nothing" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 0 []
                        in
                        Expect.equalLists [] (RingBuffer.toList (RingBuffer.pushLeft "a" buf))
                ]
            , test "works" <|
                \_ ->
                    let
                        buf =
                            RingBuffer.fromList 3 [ "x", "y", "z" ]
                    in
                    Expect.all
                        [ --  1x
                          Expect.equalLists [ "a", "x", "y" ]
                            << RingBuffer.toList
                            << RingBuffer.pushLeft "a"

                        -- 2x
                        , Expect.equalLists [ "a", "b", "x" ]
                            << RingBuffer.toList
                            << RingBuffer.pushLeft "a"
                            << RingBuffer.pushLeft "b"

                        -- 3x
                        , Expect.equalLists [ "a", "b", "c" ]
                            << RingBuffer.toList
                            << RingBuffer.pushLeft "a"
                            << RingBuffer.pushLeft "b"
                            << RingBuffer.pushLeft "c"

                        -- 4x
                        , Expect.equalLists [ "a", "b", "c" ]
                            << RingBuffer.toList
                            << RingBuffer.pushLeft "a"
                            << RingBuffer.pushLeft "b"
                            << RingBuffer.pushLeft "c"
                            << RingBuffer.pushLeft "d"
                        ]
                        buf
            ]
        , describe "RingBuffer.rotate{Left,Right}"
            [ describe "0 length"
                [ test "works" <|
                    \_ ->
                        let
                            buf =
                                RingBuffer.fromList 0 [ 'a' ]
                        in
                        Expect.all
                            [ RingBuffer.rotateLeft 1 >> (\buf2 -> Expect.equal buf buf2)
                            , RingBuffer.rotateRight 1 >> (\buf2 -> Expect.equal buf buf2)
                            ]
                            buf
                ]
            , describe "1 length"
                [ test "does not update pointers" <|
                    \_ ->
                        let
                            ring =
                                RingBuffer.fromList 10 [ 'a' ]
                        in
                        case ring of
                            RingBuffer buf ->
                                Expect.all
                                    [ RingBuffer.rotateLeft 1
                                        >> (\ring2 ->
                                                case ring2 of
                                                    RingBuffer buf2 ->
                                                        Expect.equal ( buf.start, buf.end ) ( buf2.start, buf2.end )
                                           )
                                    , RingBuffer.rotateRight 1
                                        >> (\ring2 ->
                                                case ring2 of
                                                    RingBuffer buf2 ->
                                                        Expect.equal ( buf.start, buf.end ) ( buf2.start, buf2.end )
                                           )
                                    ]
                                    ring
                ]
            ]
        , describe "RingBuffer.rotateLeft"
            [ describe "0 size"
                [ test "nothing happens" <|
                    \_ ->
                        Expect.equalLists
                            []
                            (RingBuffer.fromList 0 []
                                |> RingBuffer.rotateLeft 1
                                |> RingBuffer.toList
                            )
                ]
            , test "partially-full buffer rotates" <|
                \_ ->
                    Expect.equalLists
                        [ "c", "a", "b" ]
                        (RingBuffer.fromList 100 [ "a", "b", "c" ]
                            |> RingBuffer.rotateLeft 1
                            -- [b, c, a]
                            |> RingBuffer.rotateLeft 1
                            -- [c, a, b]
                            |> RingBuffer.toList
                        )
            , test "full buffer rotates" <|
                \_ ->
                    Expect.equalLists
                        [ "c", "a", "b" ]
                        (RingBuffer.fromList 3 [ "a", "b", "c" ]
                            |> RingBuffer.rotateLeft 2
                            |> RingBuffer.toList
                        )
            ]
        , describe "RingBuffer.rotateRight"
            [ describe "0 size"
                [ test "nothing happens" <|
                    \_ ->
                        Expect.equalLists
                            []
                            (RingBuffer.fromList 0 []
                                |> RingBuffer.rotateRight 1
                                |> RingBuffer.toList
                            )
                ]
            , test "partially-full buffer rotates" <|
                \_ ->
                    Expect.equalLists
                        [ "b", "c", "a" ]
                        (RingBuffer.fromList 100 [ "a", "b", "c" ]
                            |> RingBuffer.rotateRight 1
                            |> RingBuffer.rotateRight 1
                            |> RingBuffer.toList
                        )
            , test "full buffer rotates" <|
                \_ ->
                    Expect.equalLists
                        [ "b", "c", "a" ]
                        (RingBuffer.fromList 3 [ "a", "b", "c" ]
                            |> RingBuffer.rotateRight 2
                            |> RingBuffer.toList
                        )
            ]

        -- TODO: Add rotate{Left,Right}
        , describe "kitchen sink"
            [ test "works" <|
                \_ ->
                    let
                        buf =
                            RingBuffer.fromList 3 []
                    in
                    Expect.all
                        [ RingBuffer.push "x"
                            -- [x]
                            >> RingBuffer.push "y"
                            -- [x, y]
                            >> RingBuffer.push "z"
                            -- [x, y, _]
                            >> RingBuffer.push "_"
                            -- [x, y]
                            >> RingBuffer.pop
                            >> Tuple.second
                            -- [x, y, z]
                            >> RingBuffer.pushLeft "c"
                            -- [c, x, y]
                            >> RingBuffer.pushLeft "b"
                            -- [b, c, x]
                            >> RingBuffer.pushLeft "a"
                            -- [a, b, c]
                            >> RingBuffer.rotateRight 10
                            >> RingBuffer.rotateLeft 10
                            >> RingBuffer.toList
                            >> Expect.equalLists [ "a", "b", "c" ]
                        ]
                        buf
            ]
        ]
