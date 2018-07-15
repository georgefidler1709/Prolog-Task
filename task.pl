%Q1
%Squares all the negative numbers in a given list and returns their sum.
sumsq_neg([], 0).
sumsq_neg([H|T], Sum) :-
	H < 0,
	sumsq_neg(T, Rest),
	Sum is (H * H) + Rest.
sumsq_neg([H|T], Sum) :-
	H >= 0,
	sumsq_neg(T, Rest),
	Sum is Rest.

%Q2
%Returns true if all the people in the who list (1st argument) like all the fruits in the what list (2nd argument).
%Returns false otherwise.
all_like_all([], []) :-
!.
all_like_all(A, []).
all_like_all([], A).
all_like_all([Person|People], [Subject|Fruits]) :-
	likes(Person, Subject),
	all_like_all(People, [Subject|Fruits]),
	all_like_all([Person|People], Fruits).

%Q3
%Returns a descending list of the numbers between (and including) Upper and Lower appended to the square root of each number.
sqrt_table(Upper, Lower, []) :-
	Lower > Upper.
sqrt_table(Upper, Lower, Result) :-
	Upper >= Lower,
	NewUpper is Upper - 1,
	sqrt_table(NewUpper, Lower, Rest),
	Root is sqrt(Upper),
	Result = [[Upper, Root]|Rest].

%Q4
%Condenses every sequence of successive increasing numbers in a list into a new list element containing only the first and last element in the sequence. 
chop_up([],[]).
chop_up([A], [A]).
chop_up([H1, H2 | T], Result) :-
	H2 =:= H1 + 1,
	ascending([H1, H2|T], FrontBack, LeftOver, _),
	chop_up(LeftOver, Rest),
	Result = [FrontBack|Rest].
chop_up([H1, H2 | T], Result) :-
	H2 =\= H1 + 1,
	chop_up([H2 | T],Rest),
	Result = [H1 | Rest].
	
%Used in chop_up
%Returns a condenses version of a sequence of successive increasing numbers if one exists at the start of the list
%and returns the remaining elements in the original list.
ascending([], [], [], Counter).
ascending([A], [A], [], Counter).
ascending([H1, H2|T], Result, LeftOver, 0) :-
	Counter = 1,
	ascending([H2|T], Rest, LeftOver, Counter),
	Result = [H1|Rest], !.

ascending([H1, H2|T], Result, LeftOver, Counter) :-
	H2 =:= H1 + 1,
	Counter2 is Counter + 1,
	ascending([H2|T], Result, LeftOver, Counter2).

ascending([H1, H2|T], Result, LeftOver, Counter) :-
	H2 =\= H1 + 1,
	Result = [H1],
	LeftOver = [H2|T].

%Q5
%Sums a binary expression tree where every leaf node is a number and every root node is an arithmetic operator.
%If a leaf node contains 'z' it will be given the value 'Value'.
tree_eval(Value, tree(Left, '+', Right), Eval) :-
	tree_eval(Value, Left, LTotal),
	tree_eval(Value, Right, RTotal),
	Eval is LTotal + RTotal,!.
tree_eval(Value, tree(Left, '-', Right), Eval) :-
	tree_eval(Value, Left, LTotal),
	tree_eval(Value, Right, RTotal),
	Eval is LTotal - RTotal,!.
tree_eval(Value, tree(Left, '*', Right), Eval) :-
	tree_eval(Value, Left, LTotal),
	tree_eval(Value, Right, RTotal),
	Eval is LTotal * RTotal,!.
tree_eval(Value, tree(Left, '/', Right), Eval) :-
	tree_eval(Value, Left, LTotal),
	tree_eval(Value, Right, RTotal),
	Eval is LTotal / RTotal,!.
tree_eval(Value, tree(Left, 'z', Right), Eval) :-
	Eval = Value,!.
tree_eval(Value, tree(Left, Num, Right), Eval) :-
	Eval = Num.
