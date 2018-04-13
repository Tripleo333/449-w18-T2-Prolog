:- [constraints].

% Only redoes soft constraints
 bnb(S,P) :- fd_minimize(constraints(S,P), P) , !.

 constraints(S,P) :- 
    S = [_1,_2,_3,_4,_5,_6,_7,_8], 
    fd_domain(S, 65, 72) , 
    checkFPA(S,1) ,
    fd_all_different(S) ,
    fd_labeling(S) ,
    hard_constr(S) ,
    getSOFT(S, P) , 
    fd_labeling(P).


% T parameter has to receive a list of variables for this to make sense
% values of predicates are values of ASCII characters
 hard_constr([H|T]) :- 
    checkFM([H|T],1) -> checkTNT([H|T],H).

