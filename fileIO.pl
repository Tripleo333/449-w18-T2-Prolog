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

    parser([CurrLine|InputLines],OutputLine) :-
    phrase(checkNameStr, CurrLine) ->  % if Name: is correct eat next line
    skipWhite(InputLines,InputLines2),
    phrase(checkFPAStr,CurrLine), ->  % if "forced partial assignment:" is correct
    checkFPAparse(InputLines,InputLines2),
    !; % else
    OutputLine = 'Error while parsing input file.',!.
    !; % else
    OutputLine = 'Error while parsing', !.

    checkFPAparse([CurrLine|InputLines], ReturnLines) :-
    phrase(machTaskParseError,CurrLine) ->  %check if is a valid format (X,Y)
    checkFPA(CurrLine,ReturnLines), !;
    ReturnLines = [CurrLine|InputLines],!.

	checkFPA(CurrLine,ReturnLines) :-
    phrase(machTaskInvalid,CurrLine)->  %check if is a valid pair ie (1,B)
    assertz(fpa(CurrLine)),

    skipWhite([CurrLine|InputLines], ReturnLines) :-
    phrase(CurrLine,isWhitespace) ->
    skipWhite(InputLines,ReturnLines) ,!;
ReturnLines = [CurrLine|InputLines],!.

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Parser
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    
    
    checkNameStr --> "Name:", isWhitespace,!.
    
    checkFPAStr --> "forced partial assignment:", isWhitespace,!.
    
    machTaskParseError --> "(", startIsAnyNumber, ",", isAnyLetter, ")", isWhitespace, !.


    startIsAnyNumber --> "0", isAnyNumber ; "1", isAnyNumber ; "2", isAnyNumber ; "3", isAnyNumber ; "4", isAnyNumber ; "5", isAnyNumber ; "6", isAnyNumber ; "7", isAnyNumber ; "8", isAnyNumber ; "9", isAnyNumber.
    isAnyNumber --> "0", isAnyNumber ; "1", isAnyNumber ; "2", isAnyNumber ; "3", isAnyNumber ; "4", isAnyNumber ; "5", isAnyNumber ; "6", isAnyNumber ; "7", isAnyNumber ; "8", isAnyNumber ; "9", isAnyNumber ; [].


    isAnyLetter --> "A"; "B" ; "C" ; "D" ; "E" ; "F" ; "G" ; "H" ; "I" ; "J" ; "K" ; "L" ; "M" ; "N" ; "O" ; "P" ; "Q" ; "R" ; "S" ; "T" ; "U" ; "V" ; "W" ; "X" ; "Y" ; "Z".

    isWhitespace --> " ", isWhitespace ; "\t", isWhitespace ; "\n", isWhitespace ; [].



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
