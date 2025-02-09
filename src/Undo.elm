module Undo exposing
    ( Stack
    , canRedo
    , canUndo
    , init
    , insert
    , redo
    , undo
    , value
    )


type Stack a
    = Stack ( List a, a, List a )


init : a -> Stack a
init v =
    Stack ( [], v, [] )


insert : a -> Stack a -> Stack a
insert newV (Stack ( before, oldV, _ )) =
    Stack ( oldV :: before, newV, [] )


value : Stack a -> a
value (Stack ( _, v, _ )) =
    v


undo : Stack a -> Stack a
undo ((Stack ( before, v, after )) as stack) =
    case before of
        [] ->
            stack

        b :: rest ->
            Stack ( rest, b, v :: after )


redo : Stack a -> Stack a
redo ((Stack ( before, v, after )) as stack) =
    case after of
        [] ->
            stack

        a :: rest ->
            Stack ( v :: before, a, rest )


canUndo : Stack a -> Bool
canUndo (Stack ( before, _, _ )) =
    before /= []


canRedo : Stack a -> Bool
canRedo (Stack ( _, _, after )) =
    after /= []
