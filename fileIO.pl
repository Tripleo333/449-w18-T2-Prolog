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

    parser(InputLines,OutputLine) :-
    skipWhite(InputLines,InputLines2),
    getHead(InputLines2,Head, List),
    atom_codes(Head,CurrLineAtoms),
    phrase(checkNameStr, CurrLineAtoms) ->
    OutputLine = 'successfully read the name',
    write('if currline is Name: \n'),

    skipWhite(List, InputLines4),
    write('\nhere\n'),
    getHead(InputLines4, Head4, InputLines4New),
    write('Actual Name: '),
    write(Head4),
    skipWhite(InputLines4New,InputLines5),
    write('welost AF'),
    getHead(InputLines5,Head5,InputLines5New),
    write(Head5),
    atom_codes(Head5,CurrLineAtoms2),
    write('hey'),
    phrase(checkFPAStr, CurrLineAtoms2),
    write('Currline is forced partial assignment: \n'),
    checkFPAparse(InputLines5New,InputLines6),
    write('it all work\n'),
    skipWhite(InputLines6,InputLines7),
    getHead(InputLines7,Head7,InputLines7New),
    atom_codes(Head7,CurrLineAtoms7),
    phrase(checkFMStr,CurrLineAtoms7),
    write(Head7),
    !;
 % else
    write('else Currline is NOT name: \n'),
    OutputLine = 'Error while parsing', !.


    getHead([Head|List],Head2,List2) :-
    Head2 = Head,
    List2 = List, !.

    checkFPAparse(InputLinesFPA, ReturnLines) :-
    getHead(InputLinesFPA, HeadFPA, InputLinesNewFPA),
    write(HeadFPA),
    atom_codes(HeadFPA,CurrLineAtom),
    write('atom codes worked1\n'),
    phrase(machTaskParseError,CurrLineAtom) ->
    %if
    checkFPA(CurrLineAtom,InputLinesNewFPA,ReturnLines),
    !;
%else
    write('not in if\n'),
    !.

    checkFPA(CurrLineAtom,InputLines,ReturnLines) :-
    phrase(machTaskInvalid,CurrLineAtom) ->
    write('is this work?\n'),
    getPair(CurrLineAtom,FpaPair),
    write(FpaPair),
    checkFPAparse(InputLines,ReturnLines),!;
    write('checkFPA\n'),!.


    skipWhite([CurrLine|InputLines], ReturnLines) :-
    write('skip whites current line: '),
    write(CurrLine),
    write('\n'),
    atom_codes(CurrLine,CurrLineAtom),
%if
    phrase(isWhitespace,CurrLineAtom) ->
    skipWhite(InputLines,ReturnLines),!;
%else
    write('\nnot a white line: '),
    write(CurrLine),
    write('\n'),
    append([CurrLine],InputLines,ReturnLines),
    write('length of InputLines: '),
    length(InputLines, InputLinesLen),
    write(InputLinesLen),
    write('\nLength of ReturnLines: '),
    length(ReturnLines, ReturnLinesLen),
    write(ReturnLinesLen),
    write('\n\n'),
    !.

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Parser
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    
    
    checkNameStr --> "Name:", isWhitespace,!.
    
    checkFPAStr --> "forced partial assignment:", isWhitespace,!.
    
    machTaskParseError --> "(", startIsAnyNumber, ",", isAnyLetter, ")", isWhitespace, !.


    startIsAnyNumber --> "0", isAnyNumber ; "1", isAnyNumber ; "2", isAnyNumber ; "3", isAnyNumber ; "4", isAnyNumber ; "5", isAnyNumber ; "6", isAnyNumber ; "7", isAnyNumber ; "8", isAnyNumber ; "9", isAnyNumber.
    isAnyNumber --> "0", isAnyNumber ; "1", isAnyNumber ; "2", isAnyNumber ; "3", isAnyNumber ; "4", isAnyNumber ; "5", isAnyNumber ; "6", isAnyNumber ; "7", isAnyNumber ; "8", isAnyNumber ; "9", isAnyNumber ; [].


    isAnyLetter --> "A"; "B" ; "C" ; "D" ; "E" ; "F" ; "G" ; "H" ; "I" ; "J" ; "K" ; "L" ; "M" ; "N" ; "O" ; "P" ; "Q" ; "R" ; "S" ; "T" ; "U" ; "V" ; "W" ; "X" ; "Y" ; "Z".

    isWhitespace --> [" "], isWhitespace ; ["\t"], isWhitespace ; ["\n"], isWhitespace ; [],!.



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



    machPenaltiesPenaltyError --> notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceStringSpace, notWhitespaceString, isWhitespace, !.


    % Test everything below here!
    machPenaltiesInvalidPenalty --> naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumberSpace, naturalNumber, isWhitespace, !.

    naturalNumberSpace  --> isNaturalNumber, " ",!.
    naturalNumber       --> isNaturalNumber,!.

    isNaturalNumber     --> ("0", isNaturalNumberRest) ; ("1", isNaturalNumberRest) ; ("2", isNaturalNumberRest) ; ("3", isNaturalNumberRest) ; ("4", isNaturalNumberRest) ; ("5", isNaturalNumberRest) ; ("6", isNaturalNumberRest) ; ("7", isNaturalNumberRest) ; ("8", isNaturalNumberRest) ; ("9", isNaturalNumberRest), !.
    isNaturalNumberRest --> ("0", isNaturalNumberRest) ; ("1", isNaturalNumberRest) ; ("2", isNaturalNumberRest) ; ("3", isNaturalNumberRest) ; ("4", isNaturalNumberRest) ; ("5", isNaturalNumberRest) ; ("6", isNaturalNumberRest) ; ("7", isNaturalNumberRest) ; ("8", isNaturalNumberRest) ; ("9", isNaturalNumberRest) ; [], !.



    tooNearPenalitiesParseError --> "(", isAnyLetter, ",", isAnyLetter, notWhitespaceString, ")", isWhitespace, !.



    tooNearPenalitiesInvalidTask --> "(", isValidTask, ",", isValidTask, notWhitespaceString, ")", isWhitespace, !.



    tooNearPenalitiesInvalidPenalty --> "(", isValidTask, ",", isValidTask, naturalNumber, ")", isWhitespace, !.





    % X = list of chars to take from
    % Y = list where fisrt element is ascii code of machine, second element is ascii code of task
    getPair(X,Y) :- nth(2, X, M), nth(4, X, T), Y = [M,T], !.
/*    
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
    getPenaltiesLine(X, Y) :- removeSpaces(X, [], [], F), reverse(F, R), removeBlanks(R, S), !.% S is now a list of lists of character codes.  Each list within S can be converted to a number via number_codes
    
    % X = list of chars to take from
    % Y = list where first element = ascii code of first task, second element = ascii code of second task, third element is list of ascii character codes which represent an integer
    getTriple(X,Y) :- nth(2, X, TONE), nth(4, X, TTWO), subtract(X, ["A", "B", "C", "D", "E", "F", "G", "H", ",", "(", ")", " ", "\t", "\n"], F), Y = [TONE, TTWO, F], !.
*/




