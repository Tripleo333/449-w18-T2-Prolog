% does NOT like dual penalties, eg, penaltyTooNear([['A','B',10],['E','A',11]]) where A1,B2,E5.
taskCount(8).  % locking here for testing purposes to keep data and results simpler and fewer.
forcedMachines([['A',1],['B',2],['C',3],['D',4],['E',5]]).
forbiddenMachines([]).
forbiddenTooNear([]).
penaltyMachines([]).
penaltyTooNear([['A','C',X]]).
%forcedMachines([['C',4]]).
%forbiddenMachines([['A',1],['E',1],['E',2],['E',5]]).
%forbiddenTooNear([['A','B'],['C','D']]).
%penaltyMachines([['A',1,10],['A',2,90],['A',3,30],['A',4,40],['A',5,50],['B',2,11],['C',3,12],['D',4,13],['E',5,14]]).
%penaltyTooNear([['A','B',10],['B','C',11],['C','D',12],['D','E',13],['B','E',13],['B','D',13]]).

%  machine penalty grid layout
%  a b c d
% 1
% 2
% 3
% 4

printList([]).
printList([X|XS]):-write(X),nl,printList(XS).

doForcedMachines(_,[]):-!.
doForcedMachines(Tasks,Forced):-
    select([L,N],Forced,Rest),
    member(task(L,D,_,_),Tasks),
    D#=N,
    doForcedMachines(Tasks,Rest).

doForbidMachines(_,[]):-!.
doForbidMachines(Tasks,Forbid):-
    select([L,N],Forbid,Rest),
    member(task(L,D,_,_),Tasks),
    D#\=N,
    doForbidMachines(Tasks,Rest).

doForbidTooNear(_,[]):-!.
doForbidTooNear(Tasks, Forbid):-
    select([L,L2],Forbid,Rest),
    member(task(L,D,_,_),Tasks),
    member(task(L2,D2,_,_),Tasks),
    taskCount(TaskCount),
    D2 #\= D+1,
    D2 #\= D-TaskCount+1,
    doForbidTooNear(Tasks,Rest).

doMachinePenalty(_,[]):-!.
doMachinePenalty(Tasks,Pen):-
    select([L,N,P],Pen,Rest),
    member(task(L,D,P2,_),Tasks),
    D #= N #==> P2 #= P,
    doMachinePenalty(Tasks,Rest).
    
doNearPenalty(_,[]):-!.
doNearPenalty(Tasks, Pen):-
    select([L,L2,P],Pen,Rest),
    member(task(L,D,_,_),Tasks),
    member(task(L2,D2,_,P2),Tasks),
    D2 #= D+1 #==> P2 #= P,
    taskCount(TaskCount),
    D2 #= D-TaskCount+1 #==> P2 #= P,
    doNearPenalty(Tasks,Rest).
    
zeroPenalties([]).
zeroPenalties([task(_,_,P1,P2)|Rest]):-
    fd_min(P1,P1min),
    fd_min(P2,P2min),
    fd_max(P1,P1max),
    fd_max(P2,P2max),
    P1min#\=P1max #==> P1#=1,
    P2min#\=P2max #==> P2#=0,
    zeroPenalties(Rest).
        
sumPenalties([],0).
sumPenalties([task(_,_,P1a,P2a)|Rest],N):-
    sumPenalties(Rest,N2),
    N #= P1a + P2a + N2.
    
%scheduler demo
% assumes data is prepared for it
% applies hard and soft constraints to generate
sched:-
    taskCount(TaskCount),

    % prepare lists for domain assignment
    length(TaskDomains,TaskCount),
    length(MachinePenalties,TaskCount),
    length(NearPenalties,TaskCount),

    % assign domains to said lists
    fd_domain(TaskDomains,1,TaskCount),
    fd_max_integer(Max),
    fd_domain(MachinePenalties,0,Max),
    fd_domain(NearPenalties,0,Max),

    % assemble into tasks
    genTaskList(Tasks,TaskDomains,MachinePenalties,NearPenalties,1),       % produces list of task structures with (Id, machine Domain, Penalty Domain)

    % get hard constraint data
    forcedMachines(ForcedMachines),
    forbiddenMachines(ForbiddenMachines),
    forbiddenTooNear(ForbiddenTooNear),

    % apply hard constraints
    doForcedMachines(Tasks,ForcedMachines),
    doForbidMachines(Tasks,ForbiddenMachines),
    doForbidTooNear(Tasks,ForbiddenTooNear),

    % get penalties
    penaltyMachines(PenaltyMachines),
    penaltyTooNear(PenaltyTooNear),

    % apply penalties
    doMachinePenalty(Tasks,PenaltyMachines),
    doNearPenalty(Tasks,PenaltyTooNear),

    % force no duplicates
    fd_all_different(TaskDomains),

    % solve for cheapest schedule
    fd_minimize(
        (
            fd_labeling(TaskDomains),
            zeroPenalties(Tasks),
            sumPenalties(Tasks,Cost)
        ),
        Cost),!,% dont allow it to backtrack,its endless, if it answers at all, that schedule is optimal.
    
    % once here, either optimal schedule, or none exists.    
    write('task,id,machine domain, penalty Machine, penalty Near\n'),
    printList(Tasks),
    nl,write('total penalty= '),write(Cost),nl.    
    
genTaskList(T2,[],[],[],_):-T2=[].    % base case, return empty
genTaskList(T,[M|MS],[P1|PS1],[P2|PS2],Id):-          % expand a domain to a task structure containing the domain and a penalty
    nth(Id,['A','B','C','D','E','F','G','H'],L),
    NewTask=[task(L,M,P1,P2)],          % task(Id, Machine,Machine Penalty, Near Penalty), Id assigned to a letter, Machine is a domain, Penalty will be assigned zero unless found otherwise.
                                    % fd_minimises will be called on a function summing Penalty later on to find the lowest penalty score that obeys the hard constraints.
    Id2 is Id+1,
    genTaskList(T2,MS,PS1,PS2,Id2),
    append(NewTask,T2,T).
