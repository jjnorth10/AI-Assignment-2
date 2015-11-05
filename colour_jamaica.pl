% Author:
% Date: 03/11/2015

goal_state([]).

empty([]).
new_state(S,P) :-
        not(member(S,P)).
        
new_color(S,P) :-
        not(member(S,P)).

find_move(State,Prev,red) :-
                no_red_neighbour(State,Prev).

find_move(State,Prev,blue) :-
                no_blue_neighbour(State,Prev).

find_move(State,Prev,green) :-
                no_green_neighbour(State,Prev).
                
find_move(State,Prev,yellow) :-
                no_yellow_neighbour(State,Prev).
                
no_red_neighbour(State,L):-
                empty(L).
no_red_neighbour(State,[First|Tail]):-
                not(color_red(First)),
                no_red_neighbour(State,Tail).
                

no_blue_neighbour(State,L):-
                (State,[]).
no_blue_neighbour(State,[First|Tail]):-
                not(color_blue(First)),
                no_blue_neighbour(State,Tail).
                
no_green_neighbour(State,L):-
                (State,[]).
no_green_neighbour(State,[First|Tail]):-
                not(color_green(First)),
                no_green_neighbour(State,Tail).


no_yellow_neighbour(State,L):-
                (State,[]).
no_yellow_neighbour(State,[First|Tail]):-
                not(color_yellow(First)),
                no_yellow_neighbour(State,Tail).


color_red([_,"red"]).
color_blue([_,"blue"]).
color_green([_,"green"]).
color_yellow([_,"yellow"]).

apply_move([X,Y],red,[X,"red"]).
apply_move([X,Y],blue,[X,"blue"]).
apply_move([X,Y],green,[X,"green"]).
apply_move([X,Y],yellow,[X,"yellow"]).

        
adjacent(["Hanover","Westmoreland"]).
adjacent(["Hanover","Saint James"]).
adjacent(["Westmoreland","Saint Elizabeth"]).
adjacent(["Westmoreland","Saint James"]).
adjacent(["Saint Elizabeth"," Saint James"]).
adjacent(["Saint Elizabeth","Trelawny"]).
adjacent(["Saint Elizabeth","Manchester"]).
adjacent(["Manchester","Trelawny"]).
adjacent(["Manchester","Clarendon"]).
adjacent(["Manchester","Saint Ann"]).
adjacent(["Clarendon","Trelawny"]).
adjacent(["Clarendon","Saint Ann"]).
adjacent(["Clarendon","Saint Catherine"]).
adjacent(["Saint Catherine","Saint Ann"]).
adjacent(["Saint Catherine","Saint Mary"]).
adjacent(["Saint Catherine","Saint Andrew"]).
adjacent(["Saint Andrew","Saint Mary"]).
adjacent(["Saint Andrew","Portland"]).
adjacent(["Saint Andrew","Saint Thomas"]).
adjacent(["Saint Thomas","Portland"]).
adjacent(["Portland","Saint Mary"]).
adjacent(["Saint Mary","Saint Ann"]).
adjacent(["Saint Ann","Trelawny"]).
adjacent(["Trelawny","Saint James"]).


%find_solution(State,Solution) :-
%        solve([State], Solution, [["Hanover",""],["Westmoreland",""],["Saint Elizabeth",""],["Manchester",""],["Clarendon",""],["Saint Catherine",""],
%        ["Saint Andrew",""],["Saint Thomas",""],["Portland",""],["Saint Mary",""],["Saint Ann",""],["Trelawny",""],["Saint James",""]] ).
        
find_solution(State,Solution) :-
        solve([ [State, [], []] ], Solution,["Hanover","Westmoreland","Saint Elizabeth","Manchester","Clarendon","Saint Catherine","Saint Andrew","Saint Thomas","Portland","Saint Mary","Saint Ann","Trelawny","Saint James"]).
        
        
solve([ [State,Sol,_]|_],Sol,NotVisited) :-
        goal_state(NotVisited).


solve([APath|RestPaths],Sol,NotVisited) :-
        extend(APath,NewPaths,NewNotVisited),
        conc(RestPaths,NewPaths,PathQ),
        solve(PathQ,Sol,NewNotVisited).

extend([State,Moves,Previous],NewPaths,NotVisited,NewNotVisited):-
        bagof(X,acceptable_move(State,Moves,Previous,NotVisited,NewNotVisited,X),NewPaths).
        
neighbours([Parish|_],Prev,ApplicableNeighbours):-
        bagof(X,adjacent(Parish,X),FirstList),
        bagof(Y,adjacent(Y,Parish),SecondList),
        append(FirstList,SecondList,Neighbours),
        bagof(X,member(X,Neighbours),not(member([X,_],Prev)),ApplicableNeighbours).
        
        




acceptable_move(State, Moves, Prev,NotVisited,NewNotVisited,[NewState, NewMoves, NewPrev]) :-
        neighbours(State,Prev,Neighbours),
        find_move(State, Neighbours, NewMove),
        apply_move(State, NewMove, NewState),
        new_state(NewState, Prev),
        remove_from_not_visited(NewState,NotVisited,NewNotVisited).
        conc(Moves,[NewMove],NewMoves),
        conc(Prev,[State],NewPrev).
        
remove_from_not_visited([Parish|Colour],NotVisited,NewList):-
        del(Parish,NotVisited,NewList).
        
conc([],L,L).

conc([X|L1],L2,[X|L3]) :-
        conc(L1,L2,L3).
