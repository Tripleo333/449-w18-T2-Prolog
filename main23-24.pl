:- initialization(commandline).
    commandline :- argument_value(1, InputFileName), argument_value(2, OutputFileName),
    main(InputFileName,OutputFileName).

    :- dynamic(fpa/2).
    :- dynamic(fm/2).
    :- dynamic(tnt/2).
    :- dynamic(mp/3).
    :- dynamic(tnp/3).
    

    isEndOfFile(FileHandle, CharCode, CurrentLine, FileContent) :-
    CharCode == -1,
    FileContent = CurrentLine,
    close(FileHandle), !.


    appendChar(FileHandle, CharCode, CurrentLine, FileContent) :-
    atom_codes(CurrentLine, CurrentLineCode),
    append(CurrentLineCode, [CharCode], NextCurrentLineCode),
    atom_codes(NextCurrentLine, NextCurrentLineCode),
    read_loop(FileHandle, NextCurrentLine, FileContent), !.

    

    read_file(FileName, FileContent) :-
    open(FileName, read, FileHandle),
    read_loop(FileHandle, '', FileContent), !.

    read_loop(FileHandle, CurrentLine, FileContent) :-
    get_code(FileHandle, CharCode),
    ( isEndOfFile(FileHandle, CharCode, CurrentLine, FileContent)
      ;
		  appendChar(FileHandle, CharCode, CurrentLine, FileContent)
		  ).

    main(InputFile, OutputFile) :-
    open(OutputFile, write, OS),
    read_file(InputFile,InputLines),
    % this gets the number of lines read in and outputs it to the screen.

    atom_codes(InputLines, InputLinesCodes),
    append(InputLinesCodes, [-1], InputLinesCodesTwo),
    splitAtNewLines(InputLinesCodesTwo, [], [], OutputCodes),
    parser(OutputCodes,[],Error),
    length(Error,ErrorLen),
    name(ErrorStr,Error),
     ((ErrorLen #> 0) ->					      		%if Error.size() > 0
      (write(OS, ErrorStr),nl(OS));    						%else
      (bnb([A,B,C,D,E,F,G,H],P)->
     (name(S,[A,B,C,D,E,F,G,H]), 
      char_code(One,A),char_code(Two, B),char_code(Three,C),char_code(Four,D),char_code(Five,E),
      char_code(Six,F),char_code(Seven,G),char_code(Eight,H),
      number_atom(P,Ps),
      write(OS,'Solution '),
      write(OS,One), write(OS,' '),write(OS,Two), write(OS,' '),write(OS,Three), write(OS,' '),write(OS,Four), write(OS,' '),write(OS,Five), write(OS,' '),write(OS,Six), write(OS,' '),write(OS,Seven), write(OS,' '),write(OS,Eight), write(OS,'; Quality: '),write(OS,Ps),nl(OS));
       (write(OS,'No valid solution possible!'),nl(OS)))),
/*      length(OutputLine,OutputLineLen),
      (OutputLineLen #> 0 ) ->
       (write(OS,OutputLine),nl(OS));
%      write(OS, OutputLine),nl(OS);*/
     close(OS),

    halt.


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%Parser%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%splitAtNewLines([X|Xs], CurrentList, Total, Output) :-
%      X == 10,
  
% X = list of lists of character codes
% Y = empty list
% F = output
convertCodeListCodes([X|Xs], Y, F) :- Xs = [], atom_codes(Z, X), append(Y, [Z], S), F = S, !
    ;
      atom_codes(Z, X), append(Y, [Z], S), convertCodeListCodes(Xs, S, F), !.

      
splitAtNewLines([-1|_], CurrentList, Total, Output) :-
      append(Total, CurrentList, Output), !.
      
splitAtNewLines([10|Xs], CurrentList, Total, Output) :-
      append(Total, [CurrentList], Appended),
      splitAtNewLines(Xs, [], Appended, Output), !.
      
splitAtNewLines([X|Xs], CurrentList, Total, Output) :-
      append(CurrentList, [X], Appended),
      splitAtNewLines(Xs, Appended, Total, Output), !.


      
      
    
parser(InputLines, OutputLine, Error) :-
    getNameKeyWord(InputLines, OutputLines, Error).

getNameKeyWord(InputLines, OutputLines, Error) :-
      % Skip past whitespace lines until we encounter a non-whitespace line
    skipWhitespaceLines(InputLines, [CheckNameLine|RestOfTheLines]),
      % Check to see if CheckNameLine is "Name:" followed by whitespace
%    atom_codes(CheckNameLine, CurrentCodesLine),
      % If it matches the DCG, then set RestOfTheLines where (NonWhitespaceLines = [CheckNameLine|RestOfTheLines]), and then call next part. (doesnt include the line "Name:")
    phrase(checkNameStr, CheckNameLine) -> (getName(RestOfTheLines, OutputLines, Error)), !
      ;
      % Otherwise, something here broke, which means we return Error while parsing input file.
    OutputLines = [], Error = "Error while parsing input file", !.



getName(InputLines, OutputLines, Error) :-
    % Current line is right after "Name:", so clear whitespace until we hit something, which we will save (actually ignore) as the name, then move to the next function
    skipWhitespaceLines(InputLines, [NameLine|RestOfTheLines]),
    % Dont even bother reading the first non-whitespace line, its supposed to be the name.  Skip past it and call the next function
    getFPAKeyWord(RestOfTheLines, OutputLines, Error), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getFPAKeyWord(InputLines, OutputLines, Error) :-
    skipWhitespaceLines(InputLines, [FPAKeyWordLine|RestOfTheLines]),
%    atom_codes(FPAKeyWordLine, FPAKeyWordCodesLine),
    phrase(checkFPAStr, FPAKeyWordLine) -> ( getFPAPairs(RestOfTheLines, OutputLines, Error) ), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getFPAPairs(InputLines, OutputLines, Error) :-
    skipWhitespaceLines(InputLines, [PairLine|RestOfTheLines]),
%    atom_codes(PairLine, PairCodeLine),
    phrase(machTaskParseError, PairLine) -> ( getFPAPairsOneAndAHalf([PairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    skipWhitespaceLines(InputLines, [PairLine|RestOfTheLines]),
    getFMKeyWord([PairLine|RestOfTheLines], OutputLines, Error), !.

getFPAPairsOneAndAHalf([PairLine|RestOfTheLines], OutputLines, Error) :-
%    atom_codes(PairLine, PairCodeLine),
    phrase(machTaskInvalid, PairLine) -> checkPartialAssignmentError([PairLine|RestOfTheLines], OutputLines, Error), !
    ;
    OutputLines = [], Error = "invalid machine/task", !.

checkPartialAssignmentError([PairLine|RestOfTheLines], OutputLines, Error) :-
      %    atom_codes(PairLine, PairCodeLine),
      getPair(PairLine, [Mach|[Task]]),
      NewMach is Mach - 48,
      \+(fpa(NewMach,_)),
      \+(fpa(_,Task)),
      asserta(fpa(NewMach,Task)),
    getFPAPairs(RestOfTheLines, OutputLines, Error), !
    ;
    OutputLines = [], Error = "partial assignment error", !.


getFMKeyWord([FMKeyWordLine|RestOfTheLines], OutputLines, Error) :-
      %    atom_codes(FMKeyWordLine, FMKeyWordCodeLine),
    phrase(checkFMKeyWord, FMKeyWordLine) -> ( getFMPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getFMPairs(InputLines, OutputLines, Error) :-
    skipWhitespaceLines(InputLines, [FMPairLine|RestOfTheLines]),
      %    atom_codes(FMPairLine, FMPairCodeLine),
    phrase(machTaskParseError, FMPairLine) -> ( getFMPairsTwo([FMPairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    skipWhitespaceLines(InputLines, [FMPairLine|RestOfTheLines]),
    getTNTKeyWord([FMPairLine|RestOfTheLines], OutputLines, Error), !.

getFMPairsTwo([FMPairLine|RestOfTheLines], OutputLines, Error) :-
      %    atom_codes(FMPairLine, FMPairCodeLine),
    phrase(machTaskInvalid, FMPairLine) -> ( getPair(FMPairLine, [Mach|[Task]]), NewMach is Mach - 48, assertz(fm(NewMach,Task)), getFMPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "invalid machine/task", !.

getTNTKeyWord([TNTKeyWordLine|RestOfTheLines], OutputLines, Error) :-
      %    atom_codes(TNTKeyWordLine, TNTKeyWordCodeLine),
    phrase(checkTNTKeyWord, TNTKeyWordLine) -> ( getTNTPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getTNTPairs(InputLines, OutputLines, Error) :-
    skipWhitespaceLines(InputLines, [TNTPairLine|RestOfTheLines]),
%    atom_codes(TNTPairLine, TNTPairCodeLine),
    phrase(taskTaskParseError, TNTPairLine) -> ( getTNTPairsTwo([TNTPairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    skipWhitespaceLines(InputLines, [TNTPairLine|RestOfTheLines]),
    getMPKeyWord([TNTPairLine|RestOfTheLines], OutputLines, Error), !.

getTNTPairsTwo([TNTPairLine|RestOfTheLines], OutputLines, Error) :-
%    atom_codes(TNTPairLine, TNTPairCodeLine),
    phrase(taskTaskInvalid, TNTPairLine) -> ( getPair(TNTPairLine, [TaskOne|[TaskTwo]]), assertz(tnt(TaskOne,TaskTwo)), getTNTPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "invalid machine/task", !.



getMPKeyWord([MPKeyWordLine|RestOfTheLines], OutputLines, Error) :-
      %    atom_codes(MPKeyWordLine, MPKeyWordCodeLine),
    phrase(checkMPKeyWord, MPKeyWordLine) -> ( getMPs(RestOfTheLines, OutputLines, Error, 65)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getMPs([MPLine|RestOfTheLines], OutputLines, Error, 73) :-
%    atom_codes(MPLine, MPCodeLine),
    phrase(machPenaltiesParseError, MPLine) -> (OutputLines = [], Error = "machine penalty error"), !
    ;
    skipWhitespaceLines([MPLine|RestOfTheLines], InputLines),
    getTNPKeyWord(InputLines, OutputLines, Error), !.

getMPs(InputLines, OutputLines, Error, NumLines) :-
    skipWhitespaceLines(InputLines, [MPLine|RestOfTheLines]),
%    atom_codes(MPLine, MPCodeLine),
    phrase(machPenaltiesParseError, MPLine) -> (getMPsOneAndAHalf([MPLine|RestOfTheLines], OutputLines, Error, NumLines)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getMPsOneAndAHalf([MPLine|RestOfTheLines], OutputLines, Error, NumLines) :-
%    atom_codes(MPLine, MPCodeLine),
    phrase(machPenaltiesPenaltyError, MPLine) -> (getMPsTwo([MPLine|RestOfTheLines], OutputLines, Error, NumLines)), !
    ;
    OutputLines = [], Error = "machine penalty error", !.

getMPsTwo([MPLine|RestOfTheLines], OutputLines, Error, NumLines) :-
%    atom_codes(MPLine, MPCodeLine),
    phrase(machPenaltiesInvalidPenalty, MPLine) -> (getPenaltiesLine(MPLine, IntegerList), nth(1, IntegerList, FirstNum), assertz(mp(1, NumLines, FirstNum)), nth(2, IntegerList, SecondNum), assertz(mp(2, NumLines, SecondNum)), nth(3, IntegerList, ThirdNum), assertz(mp(3, NumLines, ThirdNum)), nth(4, IntegerList, FourthNum), assertz(mp(4, NumLines, FourthNum)), nth(5, IntegerList, FifthNum), assertz(mp(5, NumLines, FifthNum)), nth(6, IntegerList, SixthNum), assertz(mp(6, NumLines, SixthNum)), nth(7, IntegerList, SeventhNum), assertz(mp(7, NumLines, SeventhNum)), nth(8, IntegerList, EighthNum), assertz(mp(8, NumLines, EighthNum)), NewNumLines is NumLines + 1, getMPs(RestOfTheLines, OutputLines, Error, NewNumLines)), !
    ;
    OutputLines = [], Error = "invalid penalty", !.


getTNPKeyWord([TNPKeyWordLine|RestOfTheLines], OutputLines, Error) :-
      %    atom_codes(TNPKeyWordLine, TNPKeyWordCodeLine),
    phrase(checkTNPKeyWord, TNPKeyWordLine) -> (getTNPPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getTNPPairs(InputLines, OutputLines, Error) :-
    skipWhitespaceLinesTNP(InputLines, OutputLines, Error),
    OutputLines = [],
    Error = [], !
    ;
    skipWhitespaceLinesTNP(InputLines, [TNPPairLine|RestOfTheLines], Error),
%    atom_codes(TNPPairLine, TNPPairCodeLine),
    phrase(tooNearPenalitiesParseError, TNPPairLine) -> (getTNPPairsTwo([TNPPairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getTNPPairsTwo([TNPPairLine|RestOfTheLines], OutputLines, Error) :-
%    atom_codes(TNPPairLine, TNPPairCodeLine),
    phrase(tooNearPenalitiesInvalidTask, TNPPairLine) -> (getTNPPairsThree([TNPPairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    OutputLines = [], Error = "invalid task", !.

getTNPPairsThree([TNPPairLine|RestOfTheLines], OutputLines, Error) :-
%    atom_codes(TNPPairLine, TNPPairCodeLine),
    phrase(tooNearPenalitiesInvalidPenalty, TNPPairLine) -> (getTriple(TNPPairLine, [First|[Second|[Third]]]), assertz(tnp(First,Second,Third)), getTNPPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "invalid penalty", !.



skipWhitespaceLinesTNP([], OutputLines, Error) :- OutputLines = [], Error = [], !. % statically write file before halt

skipWhitespaceLinesTNP([CurrentLine|RestOfInput], OutputLines, Error) :-
    % CurrentCodesLine = the actual current line (character codes instead of atom)
%    atom_codes(CurrentLine, CurrentCodesLine),
    % If the current line is whitespace, recursively call skipWhitespaceLines ignoring the current line.
    phrase(isWhitespace, CurrentLine) -> skipWhitespaceLinesTNP(RestOfInput, OutputLines, Error), !
    ;
    % Otherwise, the current line is not whitespace, so append it back to the rest of the input lines then return that as the output.
    append([CurrentLine], RestOfInput, OutputLines), !.


skipWhitespaceLines([], OutputLines) :- write('halt'), !. % statically write file before halt

skipWhitespaceLines([CurrentLine|RestOfInput], OutputLines) :-
    % CurrentCodesLine = the actual current line (character codes instead of atom)
%    atom_codes(CurrentLine, CurrentCodesLine),
    % If the current line is whitespace, recursively call skipWhitespaceLines ignoring the current line.
    phrase(isWhitespace, CurrentLine) -> skipWhitespaceLines(RestOfInput, OutputLines), !
    ;
    % Otherwise, the current line is not whitespace, so append it back to the rest of the input lines then return that as the output.
    append([CurrentLine], RestOfInput, OutputLines), !.





checkFMKeyWord --> "forbidden machine:", isWhitespace, !.

checkNameStr --> "Name:", isWhitespace,!.

checkFPAStr --> "forced partial assignment:", isWhitespace, !.

checkTNTKeyWord --> "too-near tasks:", isWhitespace, !.

checkMPKeyWord --> "machine penalties:", isWhitespace, !.

checkTNPKeyWord --> "too-near penalities", isWhitespace, !.

machTaskParseError --> "(", notWhitespaceStringNoComma, ",", notWhitespaceStringNoBracket, ")", isWhitespace, !.


startIsAnyNumber --> "0", isAnyNumber ; "1", isAnyNumber ; "2", isAnyNumber ; "3", isAnyNumber ; "4", isAnyNumber ; "5", isAnyNumber ; "6", isAnyNumber ; "7", isAnyNumber ; "8", isAnyNumber ; "9", isAnyNumber.
isAnyNumber --> "0", isAnyNumber ; "1", isAnyNumber ; "2", isAnyNumber ; "3", isAnyNumber ; "4", isAnyNumber ; "5", isAnyNumber ; "6", isAnyNumber ; "7", isAnyNumber ; "8", isAnyNumber ; "9", isAnyNumber ; [].


isAnyLetter --> "A"; "B" ; "C" ; "D" ; "E" ; "F" ; "G" ; "H" ; "I" ; "J" ; "K" ; "L" ; "M" ; "N" ; "O" ; "P" ; "Q" ; "R" ; "S" ; "T" ; "U" ; "V" ; "W" ; "X" ; "Y" ; "Z".

isWhitespace --> " ", isWhitespace ; "\t", isWhitespace ; "\n", isWhitespace ; "\r", isWhitespace ; [],!.



machTaskInvalid --> "(", isValidMach, ",", isValidTask, ")", isWhitespace, !.

isValidMach --> "1" ; "2" ; "3" ; "4" ; "5" ; "6" ; "7" ; "8".

isValidTask --> "A" ; "B" ; "C" ; "D" ; "E" ; "F" ; "G" ; "H".



taskTaskParseError --> "(", isAnyLetter, ",", isAnyLetter, ")", isWhitespace, !.



taskTaskInvalid --> "(", isValidTask, ",", isValidTask, ")", isWhitespace, !.



machPenaltiesParseError --> spaceOrNoSpace, (spaceOrNoSpace ; []), (spaceOrNoSpace ; []), (spaceOrNoSpace ; []), (spaceOrNoSpace ; []), (spaceOrNoSpace ; []), (spaceOrNoSpace ; []), (spaceOrNoSpace ; []), isWhitespace, !.

spaceOrNoSpace --> notWhitespaceStringSpace, ! ; notWhitespaceString, !.

notWhitespaceStringSpace --> isNotWhitespaceChar, (notWhitespaceStringSpace ; " "), !.

notWhitespaceString --> isNotWhitespaceChar, (notWhitespaceString ; []), !.


isNotWhitespaceChar --> [Y], {isNotWhitespaceCharacter(Y)}, !.


isNotWhitespaceCharacter(X) :-  X #> 32, !.

notWhitespaceStringNoBracket --> isNotWhitespaceCharNoBracket, (notWhitespaceStringNoBracket ; []), !.


isNotWhitespaceCharNoBracket --> [Y], {isNotWhitespaceCharacterNoBracket(Y)}, !.


isNotWhitespaceCharacterNoBracket(X) :-  X #> 32, X #\= 41, !.

notWhitespaceStringNoComma --> isNotWhitespaceCharNoComma, (notWhitespaceStringNoComma ; []), !.


isNotWhitespaceCharNoComma --> [Y], {isNotWhitespaceCharacterNoComma(Y)}, !.


isNotWhitespaceCharacterNoComma(X) :-  X #> 32, X #\= 44, !.




machPenaltiesPenaltyError --> notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceString, isWhitespace, !.


% Test everything below here!
machPenaltiesInvalidPenalty --> naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumber, isWhitespace, !.

naturalNumberSpace  --> isNaturalNumber, " ",!.
naturalNumber       --> isNaturalNumber,!.

isNaturalNumber     --> ("0", isNaturalNumberRest) ; ("1", isNaturalNumberRest) ; ("2", isNaturalNumberRest) ; ("3", isNaturalNumberRest) ; ("4", isNaturalNumberRest) ; ("5", isNaturalNumberRest) ; ("6", isNaturalNumberRest) ; ("7", isNaturalNumberRest) ; ("8", isNaturalNumberRest) ; ("9", isNaturalNumberRest), !.
isNaturalNumberRest --> ("0", isNaturalNumberRest) ; ("1", isNaturalNumberRest) ; ("2", isNaturalNumberRest) ; ("3", isNaturalNumberRest) ; ("4", isNaturalNumberRest) ; ("5", isNaturalNumberRest) ; ("6", isNaturalNumberRest) ; ("7", isNaturalNumberRest) ; ("8", isNaturalNumberRest) ; ("9", isNaturalNumberRest) ; [], !.



tooNearPenalitiesParseError --> "(", isAnyLetter, ",", isAnyLetter, ",", notWhitespaceStringNoBracket, ")", isWhitespace, !.



tooNearPenalitiesInvalidTask --> "(", isValidTask, ",", isValidTask, ",", notWhitespaceStringNoBracket, ")", isWhitespace, !.



tooNearPenalitiesInvalidPenalty --> "(", isValidTask, ",", isValidTask, ",", naturalNumber, ")", isWhitespace, !.





% X = list of chars to take from
% Y = list where fisrt element is ascii code of machine, second element is ascii code of task
getPair(X,Y) :- nth(2, X, M), nth(4, X, T), Y = [M,T], !.



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
getPenaltiesLine(X, Y) :- removeSpaces(X, [], [], F), reverse(F, R), removeBlanks(R, S), convertCodeList(S, [], Y), !.

% X = list of lists of character codes
% Y = empty list
% F = output
convertCodeList([X|Xs], Y, F) :- Xs = [], number_codes(Z, X), append(Y, [Z], S), F = S, !
    ;
      number_codes(Z, X), append(Y, [Z], S), convertCodeList(Xs, S, F), !.





% X = list of chars to take from
% Y = list where first element = ascii code of first task, second element = ascii code of second task, third element is list of ascii character codes which represent an integer
getTriple(X,Y) :- nth(2, X, TONE), nth(4, X, TTWO), clearList(X, F), number_codes(G, F), Y = [TONE, TTWO, G], !.

% X = list of chars to clear
% Y = cleared list
clearList(X,Y) :- delete(X, 65, One), delete(One, 66, Two), delete(Two, 67, Three), delete(Three, 68, Four), delete(Four, 69, Five), delete(Five, 70, Six), delete(Six, 71, Seven), delete(Seven, 72, Eight), delete(Eight, 44, Nine), delete(Nine, 40, Ten), delete(Ten, 41, Eleven), delete(Eleven, 32, Twelve), delete(Twelve, 9, Thirteen), delete(Thirteen, 10, Fourteen), delete(Fourteen, 13, Y), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%Constraints%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %checkFPA(Task List, Current Machine)
    % first call is with current machine 1
      checkFPA([],_).
      
checkFPA([H|T],Mach) :- fpa(Mach,H),
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
    checkTNT([N|T],Head).

    %checkHARD(Task List)
    % simply send the task list
    % will return true or false
% checkHARD([H|T]) :- checkFPA([H|T],1) -> checkFM([H|T],1) -> checkTNT([H|T],H).
      

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%BNB%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%Ben #\= Rules%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
      
      dynamic(fpa), fpa(0,0).
      dynamic(fm),
      fm(0,0).
      dynamic(tnt),
      tnt(0,0).
      dynamic(mp),
      mp(0,0,0).
      dynamic(tnp),
      tnp(0,0,0).
*/

      
/*
      fpa(0,0).
      fm(0,0).
      tnt(0,0).
      mp(0,0,0).
      tnp(0,0,0).

*/
