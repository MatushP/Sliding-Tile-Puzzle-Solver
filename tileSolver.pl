% file path: "Documents/MRU/3649/Asg/sliding-tile-solver/3short.txt"

:- dynamic
    solved/1,
    size/1.

generateSolution(Last, Last, List) :- List = [Last|[0]].
generateSolution(First, Last, NewList) :- Second is First + 1, generateSolution(Second, Last, List), NewList = [First|List], !.

initializeBoard(Size) :-
    Size2 is Size*Size - 1,
    generateSolution(1, Size2, Solved),
    assertz(size(Size)),
    assertz(solved(Solved)).

endProgram :-
    retractall(size(_)),
    retractall(solved(_)).

printBoard(L) :- size(Size), printBoard(L, Size, 0).

printBoard([], _, _).
printBoard([H|T], Size, Count) :- 
    (H == 0, write('_'); write(H)), 
    (H >= 10, write(' '); write('  ')), 
    NewCount = Count + 1, 
    (NewCount =:= Size, write('\n'), 
    printBoard(T, Size,0); 
    printBoard(T, Size,NewCount)), !.

findEmptyTile([0|_], 0) :- !.
findEmptyTile([_|T], Index) :- findEmptyTile(T, NewIndex), !, Index is NewIndex + 1.

getValidMoves(Layout, MoveList2, PrevMove) :-
    setof(Move, getValidMove(Layout, Move, PrevMove), MoveList1),
    heuristicFunction(Layout, MoveList1, MoveList2).

getValidMove(Layout, Move, PrevMove) :-
    size(Size),
    findEmptyTile(Layout, EmptyTile),
    EmptyTileX is EmptyTile mod Size, 
    EmptyTileY is (EmptyTile - EmptyTileX) / Size,
    ((EmptyTileY < (Size - 1), PrevMove \= 'DOWN', Move = 'UP');
    (EmptyTileY > 0, PrevMove \= 'UP', Move = 'DOWN');
    (EmptyTileX < (Size - 1), PrevMove \= 'RIGHT', Move = 'LEFT');
    (EmptyTileX > 0, PrevMove \= 'LEFT', Move = 'RIGHT')).

heuristicFunction(_, [], []).
heuristicFunction(Layout, [Move|List], NewOutput) :-
    heuristicFunction(Layout, List, Output),
    findEmptyTile(Layout, EmptyTile),
    findNeighbouringTile(EmptyTile, Move, Tile),
    nth0(Tile, Layout, Index),
    RelativePos is Tile - Index - 1, 
    (
        (RelativePos < 0, (Move == 'UP'; Move == 'LEFT'), NewOutput = [Move|Output]);
        (RelativePos > 0, (Move == 'DOWN'; Move == 'RIGHT'), NewOutput = [Move|Output]);
        append(Output, [Move], NewOutput)
    ),!.

findNeighbouringTile(EmptyTile, Move, Result) :-
    size(Size),
    (
        Move = 'UP', !, Result is EmptyTile + Size;
        Move = 'DOWN', !, Result is EmptyTile - Size;
        Move = 'LEFT', !, Result is EmptyTile + 1;
        Move = 'RIGHT', !, Result is EmptyTile - 1
    ).

makeMove(Layout, Move, Result) :-
    findEmptyTile(Layout, EmptyTile),
    findNeighbouringTile(EmptyTile, Move, SwapTile),
    swapTiles(Layout, EmptyTile, SwapTile, Result).

attemptMove(Layout, [], _, _) :-
    solved(Layout), !.

attemptMove(Layout, NewOutputMoveList, MovesRemaining, PrevMove) :-
    (MovesRemaining =< 0, !, fail)
    ; 
    (
        getValidMoves(Layout, MoveList, PrevMove),
        member(Move, MoveList),
        makeMove(Layout, Move, Result),
        NewMovesRemaining is MovesRemaining - 1,
        attemptMove(Result, OutputMoveList, NewMovesRemaining, Move),
        NewOutputMoveList = [Move|OutputMoveList]
    ).

solveBoard(Layout, MaxMoves, SolutionMovelist) :-
    write('Attempting with maximum '), write(MaxMoves), write(' moves.'), write('\n'), 
    (
        attemptMove(Layout, SolutionMovelist, MaxMoves, ''), !, write('Solved in '), length(SolutionMovelist, Len), write(Len), write(' moves.'), write('\n'), write(SolutionMovelist), write('\n')
        ;
        NewMaxMoves is MaxMoves + 1,
        solveBoard(Layout, NewMaxMoves, SolutionMovelist)
    ).

indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1), !, Index is Index1+1.

swapTiles(List, EmptyTile, SwapTile, Result) :-
    same_length(List,Result),
    append(BeforeI,[AtI|PastI],List),
    append(BeforeI,[AtJ|PastI],Bs),
    append(BeforeJ,[AtJ|PastJ],Bs),
    append(BeforeJ,[AtI|PastJ],Result),
    length(BeforeI,EmptyTile),
    length(BeforeJ,SwapTile).

printSolution(_, []).
printSolution(Layout, [Move|List]) :-
    write('Move: '), write(Move), write('\n'),
    makeMove(Layout, Move, Result),
    printBoard(Result),
    printSolution(Result, List).

getInput(Path,Size,Input) :-
    catch(
    open(Path, read, Stream),
          _,
          throw("file not found")),
    readInput(Stream, Size, Input),
    close(Stream).

readInput(Stream, Size, Input) :-
    read(Stream, Size),
    (integer(Size); throw("invalid input argument found")),
    readValues(Stream, Input),!.

readValues(Stream, Input) :-
    read(Stream, In),
    In \= end_of_file,
    (integer(In); throw('invalid input argument found')),
    Input = [In|In1],
    readValues(Stream, In1);
    Input = [].

inputCheck(Size, EmptyTile, Inversions) :-
            EmptyTileRow is (EmptyTile - (EmptyTile mod Size)) / Size,
            %Check if grid is even
            %First "if"
            (Size mod 2 =:= 0,!,
            %grid is even
            %Second "if" Inversions + EmptyTileRow is even
            ((((Inversions + EmptyTileRow) mod 2) =\= 0),!,
              %true and solvable
              write('Solvable\n')
              %false and unsolvable - Throw Error
            ; throw('Provided List is unsolvable\n'))

            %Else grid is odd
           ;
            %"If" Inversions are even then List is solvable
            (Inversions mod 2 =:= 0,!, write('Solvable\n')
            %Else grid is unsolvable - Throw Error
            ; throw('Provided List is unsolvable\n'))).

%Assumption - SearchIndex must start as 0 and CurrNum must start as 1
checkSolvable(Size,SearchIndex, CurrNum, List, Inversions) :-
    NextNum is SearchIndex + 1,
    nth0(ListIndex, List, CurrNum),
    nth0(SearchIndex,List,PrevNum),

    %Checks each individual number that appears before the current number
    ListIndex =\= SearchIndex,!,
     (PrevNum > CurrNum,!,
      NewInversions is (Inversions + 1),
      checkSolvable(Size,NextNum,CurrNum,List,NewInversions)
    ;
      checkSolvable(Size,NextNum,CurrNum,List,Inversions))
    ;
   %Increment current number
   %Current number starts at lowest number 1 and stops after highest number "MaxSize" 
    CheckNext is (CurrNum + 1), MaxSize is Size * Size,
    (CheckNext =\= MaxSize,!,NewSearch is 0,
    checkSolvable(Size,NewSearch,CheckNext,List,Inversions)
    ;
    nth0(EmptyTile,List,0),
    %Call inputCheck
    inputCheck(Size,EmptyTile,Inversions)). 

main :-
    write('Please input the file path:'),
    read(Path),
    catch((
    getInput(Path,Size,Input),
    checkSolvable(Size,0,1,Input,0),
    initializeBoard(Size),
    Layout = Input,
    solveBoard(Layout, 5, SolutionMovelist),
    printBoard(Layout),
    printSolution(Layout, SolutionMovelist),
    endProgram),
          Message,
          write(Message)).
          
