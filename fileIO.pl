:- initialization(commandline).
    commandline :- argument_value(1, X), argument_value(2, Y), write(X), write(Y), write('\n'), main(X,Y).
    is_eof(FlHndl, CharCode, CurrentLine, FileAkku, FileContent) :-
    CharCode == -1,
    append(FileAkku, [CurrentLine], FileContent),
    close(FlHndl), !.
    
    is_newline(FlHndl, CharCode, CurrentLine, FileAkku, FileContent) :-
    CharCode == 10,
    append(FileAkku, [CurrentLine], NextFileAkku),
    read_loop(FlHndl, '', NextFileAkku, FileContent).
    
    append_char(FlHndl, CharCode, CurrentLine, FileAkku, FileContent) :-
    char_code(Char, CharCode),
    atom_concat(CurrentLine, Char, NextCurrentLine),
    read_loop(FlHndl, NextCurrentLine, FileAkku, FileContent).
    
    read_file(FileName, FileContent) :-
    open(FileName, read, FlHndl),
    read_loop(FlHndl, '', [], FileContent), !.
    
    read_loop(FlHndl, CurrentLine, FileAkku, FileContent) :-
    get_code(FlHndl, CharCode),
    ( is_eof(FlHndl, CharCode, CurrentLine, FileAkku, FileContent)
      ; is_newline(FlHndl, CharCode, CurrentLine, FileAkku, FileContent)
      ; append_char(FlHndl, CharCode, CurrentLine, FileAkku, FileContent)).
    
    main(InputFile, OutputFile) :-
    open(OutputFile, write, OS),
    (   read_file(InputFile,InputLines),
	member(Line, InputLines),
	write(Line), nl,
	write(OS,Line),nl(OS),
	false
	;
	close(OS)
    ).
    
