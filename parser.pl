:- initialization(commandline).
    commandline :- argument_value(1, InputFileName), argument_value(2, OutputFileName),
    main(InputFileName,OutputFileName).

    is_eof(FileHandle, CharCode, CurrentLine, FileString, FileContent) :-
    CharCode == -1,
    append(FileString, [CurrentLine], FileContent),
    close(FileHandle), !.


    is_newline(FileHandle, CharCode, CurrentLine, FileString, FileContent) :-
    CharCode == 10,
    append(FileString, [CurrentLine], NextFileString),
    read_loop(FileHandle, '', NextFileString, FileContent).


    append_char(FileHandle, CharCode, CurrentLine, FileString, FileContent) :-
    char_code(Char, CharCode),
    atom_concat(CurrentLine, Char, NextCurrentLine),
    read_loop(FileHandle, NextCurrentLine, FileString, FileContent).


    read_file(FileName, FileContent) :-
    open(FileName, read, FileHandle),
    read_loop(FileHandle, '', [], FileContent), !.


    read_loop(FileHandle, CurrentLine, FileString, FileContent) :-
    get_code(FileHandle, CharCode),
    ( is_eof(FileHandle, CharCode, CurrentLine, FileString, FileContent)
      ; is_newline(FileHandle, CharCode, CurrentLine, FileString, FileContent)
      ; append_char(FileHandle, CharCode, CurrentLine, FileString, FileContent)
    ).


    main(InputFile, OutputFile) :-
    open(OutputFile, write, OS),
    (read_file(InputFile,InputLines),
     % this gets the number of lines read in and outputs it to the screen.
     parser(InputLines, OutputLine),
     write(OS, OutputLine),nl(OS);
     close(OS)
    ),
    halt.

parser(InputLines, OutputLine, Error) :-
    getNameKeyWord(InputLines, OutputLines, Error).

getNameKeyWord(InputLines, OutputLines, Error) :-
    % Skip past whitespace lines until we encounter a non-whitespace line
    skipWhitespaceLines(InputLines, [CheckNameLine|RestOfTheLines]),
    % Check to see if CheckNameLine is "Name:" followed by whitespace
    atom_codes(CheckNameLine, CurrentCodesLine),
    % If it matches the DCG, then set RestOfTheLines where (NonWhitespaceLines = [CheckNameLine|RestOfTheLines]), and then call next part. (doesnt include the line "Name:")
    phrase(checkNameStr, CurrentCodesLine) -> (getName(RestOfTheLines, OutputLines, Error)), !
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
    atom_codes(FPAKeyWordLine, FPAKeyWordCodesLine),
    phrase(checkFPAStr, FPAKeyWordCodesLine) -> ( getFPAPairs(RestOfTheLines, OutputLines, Error) ), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getFPAPairs(InputLines, OutputLines, Error) :-
    skipWhitespaceLines(InputLines, [PairLine|RestOfTheLines]),
    atom_codes(PairLine, PairCodeLine),
    phrase(machTaskParseError, PairCodeLine) -> ( getFPAPairsOneAndAHalf([PairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    skipWhitespaceLines(InputLines, [PairLine|RestOfTheLines]),
    getFMKeyWord([PairLine|RestOfTheLines], OutputLines, Error), !.

getFPAPairsOneAndAHalf([PairLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(PairLine, PairCodeLine),
    phrase(machTaskInvalid, PairCodeLine) -> checkPartialAssignmentError([PairLine|RestOfTheLines], OutputLines, Error), !
    ;
    OutputLines = [], Error = "invalid machine/task", !.

checkPartialAssignmentError([PairLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(PairLine, PairCodeLine),
    getPair(PairCodeLine, [Mach|[Task]]),
    NewMach is Mach - 48,
    \+(fpa(NewMach,_)),
    \+(fpa(_,Task)),
    assertz(fpa(NewMach,Task)),
    getFPAPairs(RestOfTheLines, OutputLines, Error), !
    ;
    OutputLines = [], Error = "partial assignment error", !.


getFMKeyWord([FMKeyWordLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(FMKeyWordLine, FMKeyWordCodeLine),
    phrase(checkFMKeyWord, FMKeyWordCodeLine) -> ( getFMPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getFMPairs(InputLines, OutputLines, Error) :-
    skipWhitespaceLines(InputLines, [FMPairLine|RestOfTheLines]),
    atom_codes(FMPairLine, FMPairCodeLine),
    phrase(machTaskParseError, FMPairCodeLine) -> ( getFMPairsTwo([FMPairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    skipWhitespaceLines(InputLines, [FMPairLine|RestOfTheLines]),
    getTNTKeyWord([FMPairLine|RestOfTheLines], OutputLines, Error), !.

getFMPairsTwo([FMPairLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(FMPairLine, FMPairCodeLine),
    phrase(machTaskInvalid, FMPairCodeLine) -> ( getPair(FMPairCodeLine, [Mach|[Task]]), write(Mach), write(' '), write(Task), write('\n'), assertz(fm(Mach,Task)), getFMPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "invalid machine/task", !.

getTNTKeyWord([TNTKeyWordLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(TNTKeyWordLine, TNTKeyWordCodeLine),
    phrase(checkTNTKeyWord, TNTKeyWordCodeLine) -> ( getTNTPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getTNTPairs(InputLines, OutputLines, Error) :-
    skipWhitespaceLines(InputLines, [TNTPairLine|RestOfTheLines]),
    atom_codes(TNTPairLine, TNTPairCodeLine),
    phrase(taskTaskParseError, TNTPairCodeLine) -> ( getTNTPairsTwo([TNTPairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    skipWhitespaceLines(InputLines, [TNTPairLine|RestOfTheLines]),
    getMPKeyWord([TNTPairLine|RestOfTheLines], OutputLines, Error), !.

getTNTPairsTwo([TNTPairLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(TNTPairLine, TNTPairCodeLine),
    phrase(taskTaskInvalid, TNTPairCodeLine) -> ( getPair(TNTPairCodeLine, [TaskOne|[TaskTwo]]), assertz(tnt(TaskOne,TaskTwo)), getTNTPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "invalid machine/task", !.



getMPKeyWord([MPKeyWordLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(MPKeyWordLine, MPKeyWordCodeLine),
    phrase(checkMPKeyWord, MPKeyWordCodeLine) -> ( getMPs(RestOfTheLines, OutputLines, Error, 65)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getMPs([MPLine|RestOfTheLines], OutputLines, Error, 73) :-
    atom_codes(MPLine, MPCodeLine),
    phrase(machPenaltiesParseError, MPCodeLine) -> (OutputLines = [], Error = "machine penalty error"), !
    ;
    skipWhitespaceLines([MPLine|RestOfTheLines], InputLines),
    getTNPKeyWord(InputLines, OutputLines, Error), !.

getMPs(InputLines, OutputLines, Error, NumLines) :-
    skipWhitespaceLines(InputLines, [MPLine|RestOfTheLines]),
    atom_codes(MPLine, MPCodeLine),
    phrase(machPenaltiesParseError, MPCodeLine) -> (getMPsOneAndAHalf([MPLine|RestOfTheLines], OutputLines, Error, NumLines)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getMPsOneAndAHalf([MPLine|RestOfTheLines], OutputLines, Error, NumLines) :-
    atom_codes(MPLine, MPCodeLine),
    phrase(machPenaltiesPenaltyError, MPCodeLine) -> (getMPsTwo([MPLine|RestOfTheLines], OutputLines, Error, NumLines)), !
    ;
    OutputLines = [], Error = "machine penalty error", !.

getMPsTwo([MPLine|RestOfTheLines], OutputLines, Error, NumLines) :-
    atom_codes(MPLine, MPCodeLine),
    phrase(machPenaltiesInvalidPenalty, MPCodeLine) -> (getPenaltiesLine(MPCodeLine, IntegerList), nth(1, IntegerList, FirstNum), assertz(mp(1, NumLines, FirstNum)), nth(2, IntegerList, SecondNum), assertz(mp(2, NumLines, SecondNum)), nth(3, IntegerList, ThirdNum), assertz(mp(3, NumLines, ThirdNum)), nth(4, IntegerList, FourthNum), assertz(mp(4, NumLines, FourthNum)), nth(5, IntegerList, FifthNum), assertz(mp(5, NumLines, FifthNum)), nth(6, IntegerList, SixthNum), assertz(mp(6, NumLines, SixthNum)), nth(7, IntegerList, SeventhNum), assertz(mp(7, NumLines, SeventhNum)), nth(8, IntegerList, EighthNum), assertz(mp(8, NumLines, EighthNum)), NewNumLines is NumLines + 1, getMPs(RestOfTheLines, OutputLines, Error, NewNumLines)), !
    ;
    OutputLines = [], Error = "invalid penalty", !.


getTNPKeyWord([TNPKeyWordLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(TNPKeyWordLine, TNPKeyWordCodeLine),
    phrase(checkTNPKeyWord, TNPKeyWordCodeLine) -> (getTNPPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getTNPPairs(InputLines, OutputLines, Error) :-
    skipWhitespaceLinesTNP(InputLines, OutputLines, Error),
    OutputLines = [],
    Error = [], !
    ;
    skipWhitespaceLinesTNP(InputLines, [TNPPairLine|RestOfTheLines], Error),
    atom_codes(TNPPairLine, TNPPairCodeLine),
    phrase(tooNearPenalitiesParseError, TNPPairCodeLine) -> (getTNPPairsTwo([TNPPairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    OutputLines = [], Error = "Error while parsing input file", !.

getTNPPairsTwo([TNPPairLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(TNPPairLine, TNPPairCodeLine),
    phrase(tooNearPenalitiesInvalidTask, TNPPairCodeLine) -> (getTNPPairsThree([TNPPairLine|RestOfTheLines], OutputLines, Error)), !
    ;
    OutputLines = [], Error = "invalid task", !.

getTNPPairsThree([TNPPairLine|RestOfTheLines], OutputLines, Error) :-
    atom_codes(TNPPairLine, TNPPairCodeLine),
    phrase(tooNearPenalitiesInvalidPenalty, TNPPairCodeLine) -> (getTriple(TNPPairCodeLine, [First|[Second|[Third]]]), assertz(tnp(First,Second,Third)), getTNPPairs(RestOfTheLines, OutputLines, Error)), !
    ;
    OutputLines = [], Error = "invalid penalty", !.





skipWhitespaceLinesTNP([], OutputLines, Error) :- OutputLines = [], Error = [], !. % statically write file before halt

skipWhitespaceLinesTNP([CurrentLine|RestOfInput], OutputLines, Error) :-
    % CurrentCodesLine = the actual current line (character codes instead of atom)
    atom_codes(CurrentLine, CurrentCodesLine),
    % If the current line is whitespace, recursively call skipWhitespaceLines ignoring the current line.
    phrase(isWhitespace, CurrentCodesLine) -> skipWhitespaceLinesTNP(RestOfInput, OutputLines, Error), !
    ;
    % Otherwise, the current line is not whitespace, so append it back to the rest of the input lines then return that as the output.
    append([CurrentLine], RestOfInput, OutputLines), !.


skipWhitespaceLines([], OutputLines) :- write('halt'), !. % statically write file before halt

skipWhitespaceLines([CurrentLine|RestOfInput], OutputLines) :-
    % CurrentCodesLine = the actual current line (character codes instead of atom)
    atom_codes(CurrentLine, CurrentCodesLine),
    % If the current line is whitespace, recursively call skipWhitespaceLines ignoring the current line.
    phrase(isWhitespace, CurrentCodesLine) -> skipWhitespaceLines(RestOfInput, OutputLines), !
    ;
    % Otherwise, the current line is not whitespace, so append it back to the rest of the input lines then return that as the output.
    append([CurrentLine], RestOfInput, OutputLines), !.





checkFMKeyWord --> "forbidden machine:", isWhitespace, !.

checkNameStr --> "Name:", isWhitespace,!.

checkFPAStr --> "forced partial assignment:", isWhitespace, !.

checkTNTKeyWord --> "too-near tasks:", isWhitespace, !.

checkMPKeyWord --> "machine penalties:", isWhitespace, !.

checkTNPKeyWord --> "too-near penalities", isWhitespace, !.

machTaskParseError --> "(", startIsAnyNumber, ",", isAnyLetter, ")", isWhitespace, !.


startIsAnyNumber --> "0", isAnyNumber ; "1", isAnyNumber ; "2", isAnyNumber ; "3", isAnyNumber ; "4", isAnyNumber ; "5", isAnyNumber ; "6", isAnyNumber ; "7", isAnyNumber ; "8", isAnyNumber ; "9", isAnyNumber.
isAnyNumber --> "0", isAnyNumber ; "1", isAnyNumber ; "2", isAnyNumber ; "3", isAnyNumber ; "4", isAnyNumber ; "5", isAnyNumber ; "6", isAnyNumber ; "7", isAnyNumber ; "8", isAnyNumber ; "9", isAnyNumber ; [].


isAnyLetter --> "A"; "B" ; "C" ; "D" ; "E" ; "F" ; "G" ; "H" ; "I" ; "J" ; "K" ; "L" ; "M" ; "N" ; "O" ; "P" ; "Q" ; "R" ; "S" ; "T" ; "U" ; "V" ; "W" ; "X" ; "Y" ; "Z".

isWhitespace --> " ", isWhitespace ; "\t", isWhitespace ; "\n", isWhitespace ; [],!.



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
clearList(X,Y) :- delete(X, 65, One), delete(One, 66, Two), delete(Two, 67, Three), delete(Three, 68, Four), delete(Four, 69, Five), delete(Five, 70, Six), delete(Six, 71, Seven), delete(Seven, 72, Eight), delete(Eight, 44, Nine), delete(Nine, 40, Ten), delete(Ten, 41, Eleven), delete(Eleven, 32, Twelve), delete(Twelve, 9, Thirteen), delete(Thirteen, 10, Y), !.
