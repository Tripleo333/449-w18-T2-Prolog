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
     length(InputLines, InputLinesLen),
     write(OS,InputLinesLen),nl(OS);
     close(OS)
    ),
    halt.
    
