%min_p(P, Old) :- g_read(min(P,Old), P).

%get_min(

add_p(Var, Old, Num) :- g_read(Var, Old) , N is Old + Num , g_assignb(Var, N).
init_p(Var, Old, New) :- Old is 0, g_read(Var, Old) , g_assignb(Var, New).

% Only redoes soft constraints
bnb(T) :- hard_constr(T) , ! , init_p(x,P,0) , fd_minimize(soft_constr(T, P), P) , !.

soft_constr([_1,_2,_3,_4,_5,_6,_7,_8], P) :- 
    (_1 #\= 65 ; add_p(x,P,4)) ,   % machine penalties
    (_7 #\= 65 ; add_p(y,P,6)) ,   % machine penalties
    (_2 #\= _1-1 ; add_p(z,P,3)) ,   % too near penalties
    (_2 #\= 67; add_p(w,P,7)) ,
    fd_labeling(P).

% T parameter has to receive [A,B,C] for this to make sense
% values of predicates are values of ASCII characters
hard_constr(T) :- 
    T = [_1,_2,_3,_4,_5,_6,_7,_8], 
    fd_domain(T, 65, 72) , 
    fpa([_1,_2,_3,_4,_5,_6,_7,_8]) , 
    fm([_1,_2,_3,_4,_5,_6,_7,_8]) , 
    tnt([_1,_2,_3,_4,_5,_6,_7,_8]) , 
    fd_all_different(T) , 
    fd_labeling(T).

fpa([_1,_,_,_4,_,_,_,_]) :- _1 #= 65, _4 #= 69.
fm([_,_2,_,_,_,_,_,_]) :- _2 #\= 66.
tnt([_,_,_,_4,_5,_,_,_]) :- _5 #\= _4+1, _4 #\= _5+1.
% [[0,0,1],[0,0,0],[0,0,0]]

