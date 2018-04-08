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
   
    
    
checkFPA([],_).
checkFPA([H|T],Mach) :- fpa(Mach,Z) -> H==Z, 
    NewMach is Mach + 1, 
    checkFPA(T,NewMach), !; 
    NewMach is Mach + 1, 
    checkFPA(T,NewMach).

checkFM([],_).
checkFM([H|T],Mach) :- fm(Mach,Z) -> H\=Z, 
    NewMach is Mach + 1, 
    checkFM(T,NewMach), !; 
    NewMach is Mach + 1,
    checkFM(T,NewMach).


checkTNT([],_).
checkTNT([Last|[]],Head) :- tnt(Last,Z) -> Head\=Z, !; 
    checkTNT([],_).
checkTNT([H|[N|T]],Head) :- tnt(H,N) -> false, !; 
    checkTNT(T,Head).

checkHARD([H|T]) :- checkFPA([H|T],1), 
    checkFM([H|T],1), 
    checkTNT([H|T],H).
      

getMP(_,[],_).
getMP(Mach,[Task|T],Total) :- mp(Mach,Task,Z) -> NewTotal is Total + Z, 
    NewMach is Mach + 1,
    getMP(NewMach,T,NewTotal), !; 
    NewMach is Mach + 1, 
    getMP(NewMach, T, Total).
    
getTNP(_,[],_).
getTNP(Head,[Last|[]],Total) :- tnp(Last,Head,Z) -> NewTotal is Total + Z, 
    getTNP(_,[],NewTotal), !;
    NewTotal is Total + Z,
    getTNP(_,[],NewTotal).
getTNP(Head,[T1|[T2|T]],Total) :- tnp(T1,T2,Z) -> NewTotal is Total + Z, 
    getTNP(Head,[T2|T],NewTotal), !; 
    NewTotal is Total + Z,
    getTNP(Head,[T2|T],NewTotal).

getSOFT([H|T],Total) :- getMP(1,[H|T],MPtotal), 
    getTNP(H,[H|T],TNPtotal), 
    Total is MPtotal + TNPtotal.
