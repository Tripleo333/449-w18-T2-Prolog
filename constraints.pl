    %Example asserts in the db
fpa(0,'X').
fpa(1,'A').
%fpa(2,'B').
fpa(3,'C').
fpa(4,'D').
fpa(5,'E').
%fpa(6,'F').
%fpa(7,'G').
%fpa(8,'H').

fm(0,'X').
fm(6,'F').

tnt('X','Y').
tnt('A','B').
 
mp(0,'X',0).
mp(1,'A',10).
mp(2,'B',11).
mp(3,'C',12).
mp(4,'D',13).

tnp('X','Y',0).
tnp('E','F',17).
tnp('F','G',3).
tnp('H','A',1).
   
    
    %checkFPA(Task List, Current Machine)
    % first call is with current machine 1
checkFPA([],_).
checkFPA([H|T],Mach) :- fpa(Mach,Z) -> H==Z, 
    NewMach is Mach + 1, 
    checkFPA(T,NewMach), !; 
    NewMach is Mach + 1, 
    checkFPA(T,NewMach).

    %checkFM(Task List, Current Machine)
    % first call is with current machine 1
checkFM([],_).
checkFM([H|T],Mach) :- fm(Mach,Z) -> H\=Z, 
    NewMach is Mach + 1, 
    checkFM(T,NewMach), !; 
    NewMach is Mach + 1,
    checkFM(T,NewMach).

    %checkTNT(Task List, Task on Machine 1)
    % first call has to send the head of the list in order to check wrap around
checkTNT([],_).
checkTNT([Last|[]],Head) :- tnt(Last,Z) -> Head\=Z, !; 
    checkTNT([],_).
checkTNT([H|[N|T]],Head) :- tnt(H,N) -> false, !; 
    checkTNT(T,Head).

    %checkHARD(Task List)
    % simply send the task list
    % will return true or false
checkHARD([H|T]) :- checkFPA([H|T],1), 
    checkFM([H|T],1), 
    checkTNT([H|T],H).
      

    %getMP(Current Machine, Task List, Penalty)
    % total penalty value is returned through the third parameter
getMP(_,[],0).
getMP(Mach,[Task|T],Total) :- mp(Mach,Task,Z) -> NewMach is Mach + 1,
    getMP(NewMach,T,NewTotal),
    Total is NewTotal + Z, !;
    NewMach is Mach + 1,
    getMP(NewMach, T, Total).

    %getTNP(Currrent Machine, Task List, Penalty)
    % total penalty value is returned through the third parameter
getTNP(_,[],0).
getTNP(Head,[Last|[]],Total) :- tnp(Last,Head,Z) -> Total is Z, !;
    Total is 0.
getTNP(Head,[T1|[T2|T]],Total) :- tnp(T1,T2,Z) -> getTNP(Head,[T2|T],NewTotal),
    Total is NewTotal + Z, !; 
    getTNP(Head,[T2|T],NewTotal),
    Total is NewTotal.

    %getSOFT(Task List, Penalty Total)
    % total penalty value is returned through the second parameter
getSOFT([H|T],Total) :- getMP(1,[H|T],MPtotal), 
    getTNP(H,[H|T],TNPtotal), 
    Total is MPtotal + TNPtotal.