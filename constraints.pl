:- [rules_machpen2]. 
    
    %checkFPA(Task List, Current Machine)
    % first call is with current machine 1
checkFPA([],_).
checkFPA([H|T],Mach) :- fpa(Mach,H) , 
    NewMach is Mach + 1, 
    checkFPA(T,NewMach), !; 
    NewMach is Mach + 1, 
    checkFPA(T,NewMach).

    %checkFM(Task List, Current Machine)
    % first call is with current machine 1
checkFM([],_).
checkFM([H|T],Mach) :- \+ fm(Mach,H), 
    NewMach is Mach + 1, 
    checkFM(T,NewMach).

    %checkTNT(Task List, Task on Machine 1)
    % first call has to send the head of the list in order to check wrap around
checkTNT([],_).
checkTNT([Last|[]],Head) :- \+ tnt(Last,Head), 
    checkTNT([],_).
checkTNT([H|[N|T]],Head) :- \+ tnt(H,N), 
    checkTNT(T,Head).

    %checkHARD(Task List)
    % simply send the task list
    % will return true or false
checkHARD([H|T]) :- checkFPA([H|T],1) -> checkFM([H|T],1) -> checkTNT([H|T],H).
      

    %getMP(Current Machine, Task List, Penalty)
    % total penalty value is returned through the third parameter
getMP(_,[],0).
getMP(Mach,[Task|T],Total) :- mp(Mach,Task,Z) -> NewMach is Mach + 1,
    getMP(NewMach,T,NewTotal),
    Total is NewTotal + Z, !;
    NewMach is Mach + 1,
    getMP(NewMach, T, Total), !.

    %getTNP(Currrent Machine, Task List, Penalty)
    % total penalty value is returned through the third parameter
getTNP(_,[],0).
getTNP(Head,[Last|[]],Total) :- tnp(Last,Head,Z) -> Total is Z ; !,
    Total is 0, !.
getTNP(Head,[T1|[T2|T]],Total) :- tnp(T1,T2,Z) -> getTNP(Head,[T2|T],NewTotal),
    Total is NewTotal + Z, !;
    getTNP(Head,[T2|T],NewTotal),
    Total is NewTotal.

    %getSOFT(Task List, Penalty Total)
    % total penalty value is returned through the second parameter
getSOFT([H|T],Total) :- getMP(1,[H|T],MPtotal), 
    getTNP(H,[H|T],TNPtotal), 
    Total is MPtotal + TNPtotal.
