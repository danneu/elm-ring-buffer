module RingBuffer
    exposing
        ( -- Change RingBuffer to RingBuffer(..) for testing
          RingBuffer
        , capacity
        , empty
        , first
        , fromList
        , get
        , isEmpty
        , last
        , length
        , pop
        , popLeft
        , push
        , pushLeft
        , rotateLeft
        , rotateRight
        , set
        , toArray
        , toList
        )

{-| A library for circular buffers <https://en.wikipedia.org/wiki/Circular_buffer>.

Live demo: <https://www.danneu.com/elm-ring-buffer/>.


# Ring Buffers

@docs RingBuffer


# Create

@docs empty, fromList


# Basics

@docs get, isEmpty, first, last, toList, toArray, capacity, length


# Modify

@docs pop, popLeft, push, pushLeft, set, rotateLeft, rotateRight

-}

import Array.Hamt as Array exposing (Array)
import Internal exposing (Internal)
import Util


-- RING BUFFERS


{-| Representation of a fixed-size collection that
efficiently overflows.
-}
type RingBuffer a
    = RingBuffer (Internal a)



-- CREATING RING BUFFERS


{-| Create an empty ring buffer that rolls over at
the given capacity.
-}
empty : Int -> RingBuffer a
empty maxSize =
    RingBuffer
        { array = Array.initialize maxSize (always Nothing)
        , start = 0
        , end = maxSize - 1
        , length = 0
        }


{-| Create a ring buffer from a list with the given capacity.
-}
fromList : Int -> List a -> RingBuffer a
fromList maxSize values =
    if maxSize == 0 then
        empty 0
    else
        List.foldl push (empty maxSize) values



-- BASICS


{-| Determine if the ring buffer has zero items.
-}
isEmpty : RingBuffer a -> Bool
isEmpty (RingBuffer buf) =
    buf.length == 0


{-| Get the static max size of the ring buffer.
-}
capacity : RingBuffer a -> Int
capacity (RingBuffer buf) =
    Array.length buf.array


{-| Get the current size of the ring buffer.
-}
length : RingBuffer a -> Int
length (RingBuffer buf) =
    buf.length


{-| Get the first (left-hand-most) item in the ring buffer.
-}
first : RingBuffer a -> Maybe a
first (RingBuffer buf) =
    if buf.length == 0 then
        Nothing
    else
        Array.get buf.start buf.array
            |> Util.unwrapMaybe


{-| Get the last (right-hand-most) item in the ring buffer.
-}
last : RingBuffer a -> Maybe a
last (RingBuffer buf) =
    if buf.length == 0 then
        Nothing
    else
        Array.get buf.end buf.array
            |> Util.unwrapMaybe


{-| Remove the element from the end (right side) of the ring buffer.
-}
pop : RingBuffer a -> ( Maybe a, RingBuffer a )
pop ((RingBuffer buf) as ring) =
    if isEmpty ring then
        ( Nothing, ring )
    else
        let
            maxSize =
                capacity ring

            value =
                Array.get buf.end buf.array |> Util.unwrapMaybe

            array_ =
                Array.set buf.end Nothing buf.array

            end_ =
                (buf.end - 1 + maxSize) % maxSize

            length_ =
                buf.length - 1

            buf_ =
                { buf
                    | array = array_
                    , end = end_
                    , length = length_
                }
        in
        ( value, RingBuffer buf_ )


{-| Push a value onto the end (right side) of the ring buffer.

If the ring buffer is at capacity, then the first item
is pushed out.

-}
push : a -> RingBuffer a -> RingBuffer a
push value ((RingBuffer buf) as ring) =
    let
        maxSize =
            capacity ring

        index =
            (buf.end + 1) % maxSize

        array_ =
            Array.set index (Just value) buf.array

        length_ =
            min maxSize (buf.length + 1)

        end_ =
            (buf.end + 1) % maxSize

        start_ =
            (maxSize + end_ - length_ + 1) % maxSize
    in
    RingBuffer
        { array = array_
        , length = length_
        , start = start_
        , end = end_
        }


{-| Get a list from a ring buffer.
-}
toList : RingBuffer a -> List a
toList =
    Array.toList << toArray


{-| Get an array from a ring buffer.
-}
toArray : RingBuffer a -> Array a
toArray ((RingBuffer buf) as ring) =
    let
        start =
            0

        end =
            buf.length

        size =
            if start < end then
                end - start
            else
                0

        accumulate i accum =
            let
                internalIndex =
                    (buf.start + start + i) % capacity ring

                value =
                    Array.get internalIndex buf.array
                        |> Util.unwrapMaybe
                        |> Util.unwrapMaybe
            in
            Array.push value accum
    in
    List.foldl accumulate Array.empty (List.range 0 (size - 1))


{-| Get the item at the given index.

Index 0 is the first item.

-}
get : Int -> RingBuffer a -> Maybe a
get externalIdx ((RingBuffer buf) as ring) =
    let
        internalIdx =
            (buf.start + externalIdx) % capacity ring
    in
    Array.get internalIdx buf.array
        |> Maybe.map Util.unwrapMaybe


{-| Set a value at a given index.

Index 0 is the first item. If index is out of bounds,
the ring buffer is returned unchanged.

-}
set : Int -> a -> RingBuffer a -> RingBuffer a
set externalIdx value ((RingBuffer buf) as ring) =
    let
        internalIdx =
            (buf.start + externalIdx) % capacity ring
    in
    RingBuffer
        { buf
            | array = Array.set internalIdx (Just value) buf.array
        }


{-| Push a value into the left-hand side of the ring buffer. aka `Array#unshift` in Javascript.
-}
pushLeft : a -> RingBuffer a -> RingBuffer a
pushLeft value ((RingBuffer buf) as ring) =
    if capacity ring == 0 then
        ring
    else
        let
            maxSize =
                capacity ring

            index =
                (maxSize + buf.start - (0 % maxSize) - 1) % maxSize

            array_ =
                Array.set index (Just value) buf.array

            end_ =
                if maxSize - buf.length - 1 < 0 then
                    let
                        tmpEnd =
                            buf.end + maxSize - buf.length - 1
                    in
                    if tmpEnd < 0 then
                        maxSize + (buf.end % maxSize)
                    else
                        tmpEnd
                else
                    buf.end

            length_ =
                if buf.length < maxSize then
                    if buf.length + 1 > maxSize then
                        maxSize
                    else
                        buf.length + 1
                else
                    buf.length

            start_ =
                (buf.start - 1) % maxSize
        in
        RingBuffer
            { array = array_
            , length = length_
            , start = start_
            , end = end_
            }


rotateLeft1 : RingBuffer a -> RingBuffer a
rotateLeft1 ((RingBuffer buf) as ring) =
    if buf.length == 1 then
        ring
    else
        let
            ( maybeValue, ring_ ) =
                popLeft ring
        in
        case maybeValue of
            Nothing ->
                ring_

            Just value ->
                push value ring_


rotateRight1 : RingBuffer a -> RingBuffer a
rotateRight1 ((RingBuffer buf) as ring) =
    if buf.length == 1 then
        ring
    else
        let
            ( maybeValue, ring_ ) =
                pop ring
        in
        case maybeValue of
            Nothing ->
                ring_

            Just value ->
                pushLeft value ring_


{-| Rotate the elements such that the first element
becomes the last and the second becomes the first.
-}
rotateLeft : Int -> RingBuffer a -> RingBuffer a
rotateLeft times ring =
    Util.applyRepeatedly times rotateLeft1 ring


{-| Rotate the elements such that the last element
becomes the first and the first becomes the second.
-}
rotateRight : Int -> RingBuffer a -> RingBuffer a
rotateRight times ring =
    Util.applyRepeatedly times rotateRight1 ring


{-| Remove and return first item. aka `Array#shift` in Javascript.
-}
popLeft : RingBuffer a -> ( Maybe a, RingBuffer a )
popLeft ((RingBuffer buf) as ring) =
    if buf.length == 0 then
        ( Nothing, ring )
    else
        let
            maybeValue =
                first ring

            array_ =
                Array.set buf.start Nothing buf.array

            start_ =
                (buf.start + 1) % capacity ring

            length_ =
                buf.length - 1
        in
        ( maybeValue
        , RingBuffer
            { buf
                | array = array_
                , start = start_
                , length = length_
            }
        )
