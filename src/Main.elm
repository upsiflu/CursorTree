module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)



-- helpers


type alias Map a =
    a -> a


type Ambiguation a
    = Or a (Ambiguation a)
    | Of a


{-| Map a function over all members.
-}
map_ambiguation : (a -> b) -> Ambiguation a -> Ambiguation b
map_ambiguation fu ambi =
    case ambi of
        Or x y ->
            Or (fu x) (map_ambiguation fu y)

        Of x ->
            Of (fu x)



{- The CursorTree is a zipper with three dimensions, over a single type.
   By analogy, think of a cursor in a text editor, plus a hierarchy dimension,
   plus an ambiguity dimension.
   (TODO: add a history dimension? Or keep history per-item?)
   It is a **rosetree** with nested **windows**,
   and a single **pivot** for each layer,
   where you can manage **variant layers**, and either **choose** one or
   let the choice **undecided**. Each edge is **tagged* with an `a`.
   Variants are untagged. So paths can't single out variants.
   It is noteworthy that nodes don't carry data. Only edges contain instances of `a`.
   By convention, sequences surround the "down" edge, not the "up" edge.
   Such that a fold will converge a sequence with the down edge.

   These features are inspired by the list representation of a tree as drawn on paper,
   where the indentation represents nesting layer,
   font sizes (or outlines) represent hierarchical windows,
   names are the tags,
   successive identical names are variant forms.

   We get these features for free:
   - each node (as including its variants) has a unique path;
   - each node with a unique path can be visited through a zipper;
   - nodes you leave remember their pivot.

       locus                    --unique path to the current node
       focus        : Locus     --go there

   ## Hierarchy
   In the Parent/Kid dimension, you can insert hierarchical "windows".
   Applications include code folding, open/close nodes, outline hierarchy.

       up
       down

       add_window               --put kid in a window if kid exists.
       remove_window            --only possible if kid exists and is `inside`.

       window_depth    : Int    --returns 0 if no parent is `outside`.
       inside                   --the current tree with ancestry up to the first `outside` parent.

   ## Sequence

       before
       after


   ## Ambivalence
   You can make multiple sequences variant to each other. Once you have such an ambivalence,
   you can `choose` a preference.
   Important: you can only go into a node where you have chosen a preference. (This is why
   the `Up` type has no notion of undecided variant).

       left
       right
       choose                    --always chooses the first option
       unchoose                  --discards any beforeious choice

    ## TODO: Selection
    When you select text in a modern editor, you can hold down Shift and click on another
    character. If that character is outside of your selection range, the range will automatically
    be extended up to it. If it is inside, the closest limit of the range will be shrunk to
    exclude it. In our model, we have an additional 'midpoint', the current node, so we can
    will decide how to shrink based on that node. If it is 'beforeious' to the current node,
    the beforeious node will extend resp. shrink. This is trivially mapped to all dimensions.

    ## TODO: Multiselection



-}


type CursorTree a
    = CursorTree
        { composition : Composition a
        , sequence : Sequence a
        , variation : Variation a
        , editing : Bool
        }


type Locus
    = Locus (List String)



-- Edges


type Edge a
    = RootEdge
    | Composed { windowing : Bool }
    | LeafEdge
    | UndecidedEdge
    | Decided { windowing : Bool }
    | ImpossibleEdge
    | Left
    | Right
    | OutsideEdge
    | Before a
    | After a


type alias Fold a acc =
    { composition : 
        { root : acc
        , up : { windowing : Bool } -> Map acc
        , down : { windowing : Bool } -> Map acc
        , undecide : { possibilities: Ambiguation Possibility} -> Map acc
        , leaf : acc
        , merge : Map acc
        }  
    , variation :
        { init : acc
        , succ : Map acc
        , merge : acc -> Map acc
        }
    , sequence:
        { init : acc
        , succ : a -> Map acc
        , merge : acc -> Map acc
        }
    }



-- Composition (Up and Down)


type alias Composition a =
    { up : Up a
    , down : Down a
    }


type Up a
    = Root
    | Up
        { variation : Variation a
        , sequence : Sequence a
        , windowing : Bool
        , succ : Up a
        }


type Down a
    = Leaf
    | Undecided
        (Ambiguation Possibility )
    | Down
        { windowing : Bool
        , variation : Variation a
        , sequence : Sequence a
        , succ : Down a
        }

type alias Possibility =
    { windowing : Bool
            , sequence : Sequence a
            , succ : Down a
            }


-- Variation (alternative Downs)


type alias Variation a =
    { left : Variant a
    , right : Variant a
    }


type Variant a
    = Impossible
    | Variant
        { down : Down a
        , sequence : Sequence a
        , succ : Variant a
        }



-- Sequence (Next; carries a datum of type a)


type alias Sequence a =
    { before : Next a
    , after : Next a
    }


type Next a
    = Outside
    | Next
        a
        { down : Down a
        , succ : Next a
        }



-- Maps


{-| map each datum `a` in the zipper.
-}
map : (a -> b) -> CursorTree a -> CursorTree b
map fu (CursorTree z) =
    CursorTree
        { z
            | composition = map_composition fu z.composition
            , sequence = map_sequence fu z.sequence
            , variation = map_variation fu z.variation
        }


map_composition : (a -> b) -> Composition a -> Composition b
map_composition fu c =
    { up = map_up fu c.up
    , down = map_down fu c.down
    }


map_sequence : (a -> b) -> Sequence a -> Sequence b
map_sequence fu s =
    { before = map_next fu s.before
    , after = map_next fu s.after
    }


map_variation : (a -> b) -> Variation a -> Variation b
map_variation fu a =
    { left = map_alt fu a.left
    , right = map_alt fu a.right
    }


map_up : (a -> b) -> Up a -> Up b
map_up fu p =
    case p of
        Root ->
            Root

        Up x ->
            Up
                { up = map_up fu x.up
                , windowing = x.windowing
                , sequence = map_sequence fu x.sequence
                , variation = map_variation fu x.variation
                }


map_down : (a -> b) -> Down a -> Down b
map_down fu k =
    case k of
        Leaf ->
            Leaf

        Undecided aa ->
            aa
                |> map_ambiguation
                    (\a ->
                        { down = map_down fu a.down
                        , windowing = a.windowing
                        , sequence = map_sequence fu a.sequence
                        }
                    )
                |> Variants

        Down x ->
            Down
                { down = map_down fu x.down
                , windowing = x.windowing
                , sequence = map_sequence fu x.sequence
                , variation = map_variation fu x.variation
                }


map_alt : (a -> b) -> Variant a -> Variant b
map_alt fu p =
    case p of
        Impossible ->
            Impossible

        Variant x ->
            Variant
                { down = map_down fu x.down
                , succ = map_alt fu x.succ
                , sequence = map_sequence fu x.sequence
                }


map_next : (a -> b) -> Next a -> Next b
map_next fu p =
    case p of
        Outside ->
            Outside

        Next a x ->
            Next (fu a)
                { down = map_down fu x.down
                , succ = map_next fu x.succ
                }


empty : CursorTree a
empty =
    CursorTree null


null =
    { composition = { up = Root, down = Leaf }
    , sequence = { after = Outside, before = Outside }
    , variation = { left = Impossible, right = Impossible }
    , editing = False
    }


up : CursorTree a -> Maybe (CursorTree a)
up (CursorTree z) =
    case z.composition.up of
        Root ->
            Nothing

        Up u ->
            (CursorTree >> Just)
                { z
                    | composition =
                        { up = u.up
                        , down =
                            Down
                                { down = z.composition.down
                                , sequence = z.sequence
                                , variation = z.variation
                                , windowing = u.windowing
                                }
                        }
                    , sequence = u.sequence
                    , variation = u.variation
                }


down : CursorTree a -> Maybe (CursorTree a)
down (CursorTree z) =
    case z.composition.down of
        Leaf ->
            Nothing

        Variants aa ->
            Nothing

        Down d ->
            (CursorTree >> Just)
                { z
                    | composition =
                        { up =
                            Up
                                { up = z.composition.up
                                , sequence = z.sequence
                                , variation = z.variation
                                , windowing = d.windowing
                                }
                        , down = d.down
                        }
                    , sequence = d.sequence
                    , variation = d.variation
                }


before : CursorTree a -> Maybe (CursorTree a)
before (CursorTree z) =
    case z.sequence.before of
        Outside ->
            Nothing

        Next a p ->
            (CursorTree >> Just)
                { z
                    | composition = { up = z.composition.up, down = p.down }
                    , sequence =
                        { before = p.succ
                        , after =
                            Next a
                                { down = z.composition.down
                                , succ = z.sequence.after
                                }
                        }
                    , variation = p.variation
                }


after : CursorTree a -> Maybe (CursorTree a)
after (CursorTree z) =
    case z.sequence.after of
        Outside ->
            Nothing

        Next a n ->
            (CursorTree >> Just)
                { z
                    | composition = { up = z.composition.up, down = n.down }
                    , sequence =
                        { after = n.succ
                        , before =
                            Next a
                                { down = z.composition.down
                                , succ = z.sequence.before
                                }
                        }
                    , variation = n.variation
                }


left : CursorTree a -> Maybe (CursorTree a)
left (CursorTree z) =
    case z.variation.left of
        Impossible ->
            Nothing

        Variant l ->
            (CursorTree >> Just)
                { z
                    | composition =
                        { up = z.composition.up, down = l.down }
                    , sequence = l.sequence
                    , variation =
                        { left = l.succ
                        , right =
                            Variant
                                { down = z.composition.down
                                , sequence = z.sequence
                                , succ = z.variation.right
                                }
                        }
                }


right : CursorTree a -> Maybe (CursorTree a)
right (CursorTree z) =
    case z.variation.right of
        Impossible ->
            Nothing

        Variant r ->
            (CursorTree >> Just)
                { z
                    | composition =
                        { down = r.down, up = z.composition.up }
                    , sequence = r.sequence
                    , variation =
                        { right = r.succ
                        , left =
                            Variant
                                { down = z.composition.down
                                , sequence = z.sequence
                                , succ = z.variation.left
                                }
                        }
                }


insert_up : Map (CursorTree a)
insert_up (CursorTree z) =
    CursorTree
        { z
            | composition =
                { down = z.composition.down
                , up =
                    Up
                        { sequence = null.sequence
                        , variation = null.variation
                        , windowing = False
                        , succ = z.composition.up
                        }
                }
        }


insert_down : Map (CursorTree a)
insert_down (CursorTree z) =
    CursorTree
        { z
            | composition =
                { down =
                    Down
                        { sequence = null.sequence
                        , variation = null.variation
                        , windowing = False
                        , succ = z.composition.down
                        }
                , up = z.composition.up
                }
        }


insert_before : a -> Map (CursorTree a)
insert_before x (CursorTree z) =
    CursorTree
        { z
            | sequence =
                { before =
                    Next x
                        { down = Leaf
                        , succ = z.sequence.before
                        }
                , after = z.sequence.after
                }
        }


insert_after : a -> Map (CursorTree a)
insert_after x (CursorTree z) =
    CursorTree
        { z
            | sequence =
                { before = z.sequence.before
                , after =
                    Next x
                        { down = Leaf
                        , succ = z.sequence.after
                        }
                }
        }


insert_left : Map (CursorTree a)
insert_left (CursorTree z) =
    CursorTree
        { z
            | variation =
                { left =
                    Variant
                        { down = Leaf
                        , sequence = null.sequence
                        , succ = z.variation.left
                        }
                , right = z.variation.right
                }
        }


insert_right : Map (CursorTree a)
insert_right (CursorTree z) =
    CursorTree
        { z
            | variation =
                { left = z.variation.left
                , right =
                    Variant
                        { down = Leaf
                        , sequence = null.sequence
                        , succ = z.variation.right
                        }
                }
        }


safe : (CursorTree a -> Maybe (CursorTree a)) -> Map (CursorTree a)
safe fu =
    fu >> Maybe.withDefault empty



-- Folding


empty 
    |> insert_before 3
    |> insert_before 2
    |> fold
        \edge ->
            case edge of
                Before ( Next { sequence } ) ->



fold :
    Fold a acc
    -> CursorTree a 
    -> acc


fold directive ( CursorTree z ) =
    fold_sequence z.sequence directive.sequence
        |> fold_variation z.variation directive.variation
        |> fold_composition z.composition directive.composition

fold_sequence : 
    Sequence a -> 
    Fold a acc
    -> acc
fold_sequence sequence directive  =
    let
        fold_side : Next a -> acc
        fold_side n =
            case n of
                Next a { down, Outside } ->
                    directive.sequence.init
                        |> fold_lower down directive
                Next a { down, Next nn } ->
                    fold_side (Next nn)
                        |> fold_lower down directive
                        |> directive.sequence.succ a

    in
        merge
            fold_side sequence.before
            fold_side sequence.after


fold_lower :
    Down a ->
    Fold a acc -> 
    Map acc
fold_lower lower directive =
    let
        fold_side : Down a -> acc
        fold_side d =
            case d of
                Leaf ->








































type alias Acc acc bcc =
    { branch : acc
    , merge : acc -> acc -> bcc
    }


type alias Folder a acc result =
    { before : { initial : acc, accumulate : acc -> a -> acc }
    , after : { initial : acc, accumulate : acc -> a -> acc }
    , merge : acc -> a -> acc -> result
    }


fold_sequence : Folder sum acc result -> (Next a -> sum) -> Sequence a -> result
fold_sequence { before, after, merge } read current =
    let
        fold_successors : { initial : acc, accumulate : acc -> a -> acc } -> Next a -> acc
        fold_successors direction succ =
            case succ of
                Outside ->
                    read Outside

                Next x ->
                    direction.accumulate (fold_successors direction x.succ) (read (Next x))
    in
    merge
        (fold_successors before current.before)
        read
        current
        (fold_successors after current.after)


fold_variation : Folder sum acc result -> (Variant a -> sum) -> Variation a -> result
fold_variation { before, after, merge } read current =
    let
        fold_variants : { initial : acc, accumulate : acc -> a -> acc } -> Variant a -> acc
        fold_variants direction succ =
            case succ of
                Impossible ->
                    read Impossible

                Variant x ->
                    direction.accumulate (fold_variants direction x.variant) (read (Variant x))
    in
    merge
        (fold_variants before current.left)
        read
        current
        (fold_variants after current.right)


type alias Fold a sequence_acc sequence_result variation_acc variation_result acc result =
    { compose : Folder variation_result acc result
    , alternate : Folder sequence_result variation_acc variation_result
    , sequence : Folder a sequence_acc sequence_result
    }


{-| first folds the sequence dimension, then the variants, and finally the composition.
-}
fold :
    Fold a sequence_acc sequence_result variation_acc variation_result acc result
    -> CursorTree a
    -> result
fold { compose, alternate, sequence } (CursorTree z) =
    let
        fold_upper : Up a -> acc
        fold_upper upper =
            case upper of
                Root ->
                    compose.before.initial

                Up x ->
                    compose.before.accumulate (fold_upper x.up) (fold_variation alternate sequence x.variation)

        fold_lower : Down a -> acc
        fold_lower lower =
            case lower of
                Leaf ->
                    compose.after.initial

                Down x ->
                    compose.after.accumulate (fold_lower x.down) (fold_variation alternate sequence x.variation)
    in
    compose.merge
        (fold_upper s.up)
        (fold_variation alternate sequence z.variation)
        (fold_lower s.down)


viewer : Fold String (Html msg)
viewer =
    let
        view_edge : String -> Html msg
        view_edge str =
            span [ class "edge" ] [ button [] [ text str ] ]

        view_line : List String -> Html msg
        view_line edges =
            List.map view_edge edges
    in
    { compose =
        { before =
            { initial = [], accumulate = \acc a -> a :: acc }
        , after =
            { initial = acc, accumulate = \acc a -> acc ++ [ a ] }
        , merge =
            \upper a lower ->
                div []
                    List.map
                    view_line
                    (upper ++ [ a ] ++ lower)
        }
    , alternate =
        { before =
            { initial = 0, accumulate = \acc a -> acc + 1 }
        , after =
            { initial = 0, accumulate = \acc a -> acc + 1 }
        , merge =
            \left a right ->
                if left + right > 0 then
                    a ++ [ " (" ++ String.fromInt (left + right) ++ " variants" ]

                else
                    a
        }
    , sequence =
        { before =
            { initial = [], accumulate = \acc a -> a :: acc }
        , after =
            { initial = [], accumulate = \acc a -> acc ++ [ a ] }
        , merge =
            \before a after -> before ++ [ "->" ++ a ++ "<--" ] ++ after
        }
    }


view : CursorTree String -> Html msg
view =
    fold viewer


side_by_side : (a -> Html msg) -> (a -> Html msg) -> a -> Html msg
side_by_side fi fu a =
    div [] [ fi a, fu a ]


main =
    empty
        |> insert_next 0
        |> insert_down
        |> safe down
        |> insert_before 1
        |> insert_before 2
        |> insert_next 4
        |> insert_down
        |> insert_next 3
        |> safe after
        |> insert_down
        |> side_by_side
            (fold adder >> String.fromInt >> (\t -> div [ class "result" ] [ text t ]))
            (map String.fromInt >> view)
