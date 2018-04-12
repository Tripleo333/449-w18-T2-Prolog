% X = list of chars to take from
% Y = list where fisrt element is ascii code of machine, second element is ascii code of task
getPair(X,Y) :- nth(2, X, M), nth(4, X, T), Y = [M,T], !.

removeSpaces([], [], [], []) :- [], !.
removeSpaces([X|Xs], Y, Z, F) :- X #> 32, append(Y,[X],R), removeSpaces(Xs, R, Z, F) , !.
removeSpaces([X|Xs], Y, Z, F) :- X #=< 32, append(Z, [Y], R), removeSpaces(Xs, [], R, F), !.
removeSpaces([X], Y, Z, F) :- X #> 32, append(Y,[X],R), append(Z,[R],F), !.
removeSpaces([X], Y, Z, F) :- X #=< 32, append(Z, [Y], F), !.

% X = reversed removeSpaces list
% Y = list after removing blanks from end (unreversed)
removeBlanks([X|Xs], Y) :- length(X, F), F #=< 0, removeBlanks(Xs, Y), !.
% Otherwise...
removeBlanks(X, Y) :- reverse(X, Y), !.

% X = machine penalties line
% Y = list of integers from the mach penalties line
getPenaltiesLine(X, Y) :- removeSpaces(X, [], [], F), reverse(F, R), removeBlanks(R, S), % S is now a list of lists of character codes.  Each list within S can be converted to a number via number_codes

% X = list of chars to take from
% Y = list where first element = ascii code of first task, second element = ascii code of second task, third element is list of ascii character codes which represent an integer
getTriple(X,Y) :- nth(2, X, TONE), nth(4, X, TTWO), subtract(X, ["A", "B", "C", "D", "E", "F", "G", "H", ",", "(", ")", " ", "\t", "\n"], F), Y = [TONE, TTWO, F], !.