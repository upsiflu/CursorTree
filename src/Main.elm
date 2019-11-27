module Main exposing (main)

import Html exposing (Html, div, text, span, button)
import Html.Attributes exposing (class)



-- helpers


type alias Map a =
    a -> a


type Ambiguation a
    = Of a
    | Or a (Ambiguation a)


{-| Map a function over all members.
-}
map_ambiguation : (a -> b) -> Ambiguation a -> Ambiguation b
map_ambiguation fu ambi =
    case ambi of
        Of x ->
            Of (fu x)
        Or x y ->
            Or (fu x) (map_ambiguation fu y)

fold_ambiguation : ( a -> b -> b) -> b -> Ambiguation a -> b
fold_ambiguation f acc ambi =
    case ambi of
        Of x ->
            f x acc
        Or x rest ->
            fold_ambiguation f ( f x acc ) rest
                   

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
        { editing : Bool }
        { composition : Composition a
        , sequence : Sequence a
        , variation : Variation a
        }


type Locus
    = Locus (List String)




-- Composition (Up and Down)


type alias Composition a =
    { up : Up a
    , down : Down a
    }


type Up a
    = Root
    | Up
        { windowing : Bool }
        { variation : Variation a
        , sequence : Sequence a
        , succ : Up a
        }


type Down a
    = Leaf
    | Undecided
        { windowing : Bool }
        ( Ambiguation ( Possibility a ) )
    | Decided
        { windowing : Bool }
        { sequence : Sequence a
        , variation : Variation a
        , succ : Down a
        }

type alias Possibility a =
    { sequence : Sequence a
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
        { sequence : Sequence a
        , down : Down a
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
map fu (CursorTree parameters z) =
    CursorTree parameters
        { composition = map_composition fu z.composition
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

        Up parameters u ->
            Up
                parameters
                { succ = map_up fu u.succ
                , sequence = map_sequence fu u.sequence
                , variation = map_variation fu u.variation
                }


map_down : (a -> b) -> Down a -> Down b
map_down fu k =
    case k of
        Leaf ->
            Leaf

        Undecided parameters possibilities ->
            Undecided
                parameters <|
                map_ambiguation
                    ( \possibility ->
                        { succ = map_down fu possibility.succ
                        , sequence = map_sequence fu possibility.sequence
                        }
                    )
                    possibilities
        Decided parameters d ->
            Decided parameters
                { succ = map_down fu d.succ
                , sequence = map_sequence fu d.sequence
                , variation = map_variation fu d.variation
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
    CursorTree { editing = False } null


null =
    { composition = { up = Root, down = Leaf }
    , sequence = { after = Outside, before = Outside }
    , variation = { left = Impossible, right = Impossible }
    }


up : CursorTree a -> Maybe (CursorTree a)
up (CursorTree parameters z) =
    case z.composition.up of
        Root ->
            Nothing

        Up u_parameters u ->
            (CursorTree parameters >> Just)
                { composition =
                        { up = u.succ
                        , down =
                            Decided u_parameters
                                { succ = z.composition.down
                                , sequence = z.sequence
                                , variation = z.variation
                                }
                        }
                , sequence = u.sequence
                , variation = u.variation
                }


down : CursorTree a -> Maybe (CursorTree a)
down (CursorTree parameters z) =
    case z.composition.down of
        Leaf ->
            Nothing

        Undecided u_parameters possibilities ->
            Nothing

        Decided d_parameters d ->
            (CursorTree parameters >> Just)
                { z
                    | composition =
                        { up =
                            Up d_parameters
                                { succ = z.composition.up
                                , sequence = z.sequence
                                , variation = z.variation
                                }
                        , down = d.succ
                        }
                    , sequence = d.sequence
                    , variation = d.variation
                }


before : CursorTree a -> Maybe (CursorTree a)
before (CursorTree parameters z ) =
    case z.sequence.before of
        Outside ->
            Nothing

        Next a p ->
            (CursorTree parameters >> Just)
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
                }


after : CursorTree a -> Maybe (CursorTree a)
after (CursorTree parameters z) =
    case z.sequence.after of
        Outside ->
            Nothing

        Next a n ->
            (CursorTree parameters >> Just)
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
                }


left : CursorTree a -> Maybe (CursorTree a)
left (CursorTree parameters z) =
    case z.variation.left of
        Impossible ->
            Nothing

        Variant l ->
            (CursorTree parameters >> Just)
                { composition =
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
right (CursorTree parameters z) =
    case z.variation.right of
        Impossible ->
            Nothing

        Variant r ->
            (CursorTree parameters >> Just)
                    { composition =
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
insert_up (CursorTree parameters z) =
    CursorTree parameters
        { z | composition =
                { down = z.composition.down
                , up =
                    Up
                        { windowing = False }
                        { sequence = null.sequence
                        , variation = null.variation
                        , succ = z.composition.up
                        }
                }
        }


insert_down : Map (CursorTree a)
insert_down (CursorTree parameters z) =
    CursorTree parameters
        { z | composition =
                { down =
                    Decided
                        { windowing = False }
                        { sequence = null.sequence
                        , variation = null.variation
                        , succ = z.composition.down
                        }
                , up = z.composition.up
                }
        }


insert_before : a -> Map (CursorTree a)
insert_before x (CursorTree parameters z) =
    CursorTree parameters
        { z | sequence =
                { before =
                    Next x
                        { down = Leaf
                        , succ = z.sequence.before
                        }
                , after = z.sequence.after
                }
        }


insert_after : a -> Map (CursorTree a)
insert_after x (CursorTree parameters z) =
    CursorTree parameters
        { z | sequence =
                { before = z.sequence.before
                , after =
                    Next x
                        { down = Leaf
                        , succ = z.sequence.after
                        }
                }
        }


insert_left : Map (CursorTree a)
insert_left (CursorTree parameters z) =
    CursorTree parameters
        { z | variation =
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
insert_right (CursorTree parameters z) =
    CursorTree parameters
        { z | variation =
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


type alias Fold edge node =
    { compose :
        { editing : Bool }
            ->{ from_root : node, from_leaf : node, variation : node, sequence : node }
            -> node
    , from_leaf :
        { leaf : node
        , consider : { windowing : Bool } -> { possibilities : node } -> Map node
        , cover : { windowing : Bool } -> { variation : node, sequence : node } -> Map node
        }
    , from_root :
        { root : node
        , cover : { windowing : Bool } -> { variation : node, sequence : node } -> Map node
        }
    , possibilities :
        { initial : node
        , onward : { sequence : node, from_leaf : node } -> Map node
        }
    , variation :
        { fringe : node
        , inward : { sequence : node, from_leaf : node } -> Map node
        , center : { left : node, right : node } -> node
        }
    , sequence:
        { fringe : node
        , inward : { edge : edge, from_leaf : node } -> Map node
        , center : { before : node, after : node } -> node
        }
    }



{-| turns the CursorTree into a single node.
Define functions to reduce each dimension from the fringe to the center.
-}
fold :
    Fold edge node
    -> CursorTree edge
    -> node

fold f ( CursorTree parameters z ) =
    f.compose parameters
        { from_root = fold_from_root f z.composition.up
        , from_leaf = fold_from_leaf f z.composition.down
        , variation = fold_variation f z.variation
        , sequence = fold_sequence f z.sequence
        }




        
fold_sequence :  
    Fold edge node ->
    Sequence edge ->
    node
fold_sequence f sequence =
    let
        inward : Next edge -> node
        inward edge =
            case edge of
                Outside ->
                    f.sequence.fringe
                Next a n ->
                    f.sequence.inward
                        { edge = a, from_leaf = fold_from_leaf f n.down }
                        ( inward n.succ )
    in
        f.sequence.center
            { before = inward sequence.before, after = inward sequence.after}
        
        
fold_variation :  
    Fold edge node ->
    Variation edge ->
    node
fold_variation f variation =
    let
        inward : Variant edge -> node
        inward edge =
            case edge of
                Impossible ->
                    f.variation.fringe
                Variant v ->
                    f.variation.inward
                        { sequence = fold_sequence f v.sequence
                        , from_leaf = fold_from_leaf f v.down }
                        ( inward v.succ )
    in
        f.variation.center
            { left = inward variation.left, right = inward variation.right }
        


fold_from_root :
    Fold edge node -> 
    Up edge ->
    node
fold_from_root f edge =
    case edge of
        Root ->
            f.from_root.root
        Up parameters { sequence, variation, succ } ->
            f.from_root.cover parameters
                { variation = fold_variation f variation, sequence = fold_sequence f sequence }
                ( fold_from_root f succ )
        

fold_from_leaf :
    Fold edge node -> 
    Down edge ->
    node
fold_from_leaf f edge =
    case edge of
        Leaf ->
            f.from_leaf.leaf
        Undecided parameters possibilities ->
            f.from_leaf.consider parameters
                { possibilities = fold_possibilities f possibilities }
                ( f.from_leaf.leaf )
        Decided parameters { sequence, variation, succ } ->
            f.from_leaf.cover parameters
                { variation = fold_variation f variation, sequence = fold_sequence f sequence }
                ( fold_from_leaf f succ )
        
fold_possibilities :
    Fold edge node ->
    Ambiguation ( Possibility edge ) ->
    node
fold_possibilities f =
    let
        fold_help : Possibility edge -> Map node
        fold_help possibility =
            f.possibilities.onward
              { sequence = fold_sequence f possibility.sequence
              , from_leaf = fold_from_leaf f possibility.succ
              }
    in
        fold_ambiguation
            fold_help
            f.possibilities.initial

            {--
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

        fold_lower : Downa -> acc
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
--}

viewer : Fold String (Html msg)
viewer =
    let
      compose_ =
        \parameters { from_root, from_leaf, variation, sequence } ->
            div [ class "composed" ] [ from_root, variation, sequence, from_leaf ]
      from_leaf_ =
        { leaf = span [ class "from_leaf leaf" ] [ text "🍂" ]
        , consider =
              \parameters { possibilities } node ->
                  div [ class "from_leaf consider" ] [ possibilities, node ]
        , cover =
              \parameters { variation , sequence } node ->
                  div [ class "from_leaf cover" ] [ variation, sequence, node ]
        }
      from_root_ =
        { root = span [ class "from_root" ] [ text "ROOT" ]
        , cover =
              \parameters { variation, sequence } node ->
                  div [ class "from_root cover" ] [ node, variation, sequence ]
        }
      possibilities_ =
        { initial = span [ class "possibilities" ] [ text "CHOOSE ONE:" ]
        , onward =
              \{ sequence, from_leaf } node ->
                  div [ class "sequence" ] [ node, text "|", sequence, from_leaf ]
        }
      variation_ =
        { fringe = span [ class "impossible fringe" ] [ text "-" ]
        , inward =
              \{ sequence, from_leaf } node ->
                  button [ class "variants" ] [ text "..." ]
        , center =
              \sides ->
                  div [ class "variation" ] [ sides.left, text "|||", sides.right ]
        }
      sequence_ =
        { fringe = span [ class "outside fringe" ] [ text "[]" ]
        , inward =
              \{ edge, from_leaf } node ->
                  div [ class "next" ] [ node, span [ class "edge" ] [ text edge ], from_leaf ]
        , center =
              \sides ->
                  div [ class "sequence" ] [ sides.before, span [] [ text "<>" ], sides.after ]
        }
    in
      Fold
        compose_
        from_leaf_
        from_root_
        possibilities_
        variation_
        sequence_
    

view : CursorTree String -> Html msg
view =
    fold viewer


side_by_side : (a -> Html msg) -> (a -> Html msg) -> a -> Html msg
side_by_side fi fu a =
    div [] [ fi a, fu a ]


main =
    empty
        |> insert_after 0
        |> insert_down
        |> safe down
        |> insert_before 1
        |> insert_before 2
        |> insert_after 4
        |> insert_down
        |> insert_after 3
        |> safe after
        |> insert_down
        |> map String.fromInt >> view
