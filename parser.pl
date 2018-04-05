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

    naturalNumberSpace  --> isNaturalNumber, " "
    naturalNumber       --> isNaturalNumber

    isNaturalNumber     --> ("0", isNaturalNumberRest) ; ("1", isNaturalNumberRest) ; ("2", isNaturalNumberRest) ; ("3", isNaturalNumberRest) ; ("4", isNaturalNumberRest) ; ("5", isNaturalNumberRest) ; ("6", isNaturalNumberRest) ; ("7", isNaturalNumberRest) ; ("8", isNaturalNumberRest) ; ("9", isNaturalNumberRest)
    isNaturalNumberRest --> ("0", isNaturalNumberRest) ; ("1", isNaturalNumberRest) ; ("2", isNaturalNumberRest) ; ("3", isNaturalNumberRest) ; ("4", isNaturalNumberRest) ; ("5", isNaturalNumberRest) ; ("6", isNaturalNumberRest) ; ("7", isNaturalNumberRest) ; ("8", isNaturalNumberRest) ; ("9", isNaturalNumberRest) ; []



tooNearPenalitiesParseError --> "(", isAnyLetter, ",", isAnyLetter, notWhitespaceString, ")", isWhitespace, !.



tooNearPenalitiesInvalidTask --> "(", isValidTask, ",", isValidTask, notWhitespaceString, ")", isWhitespace, !.



tooNearPenalitiesInvalidPenalty --> "(", isValidTask, ",", isValidTask, naturalNumber, ")", isWhitespace, !.
