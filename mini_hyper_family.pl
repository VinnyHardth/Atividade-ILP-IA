%———————————————————————————————————————————————
% Learning from family relations
% prove(Goal, Hypo, Ans)
%   Ans = yes …

prove(Goal, Hypo, Answer):-
    max_proof_length(D),
    prove(Goal, Hypo, D, RestD),
    (RestD >= 0, Answer = yes		% Proved
     ;				     
     RestD < 0, Answer = maybe).	% Maybe, but it looks like inf. loop
prove(Goal, _, no).			% Otherwise goal definitely cannot be proved


%———————————————————————————————————————————————
% prove(Goal, Hypo, MaxD, RestD)

prove(G, H, D, D):-
    D <0, !.
prove([], _, D, D):- !.
prove([G1|Gs],Hypo,D0,D):-
    prove(G1,Hypo,D0,D1),
    prove(Gs,Hypo,D1,D).
prove(G,_,D,D):-
    prolog_predicate(G),
    call(G).
prove(G,Hypo,D0,D):-
    D0 =< 0, !,
    D is D0-1
    ;
    D1 is D0 - 1,
    member(Clause/Vars, Hypo),
    copy_term(Clause,[Head|Body]),
    G = Head,
    prove(Body, Hypo,D1,D).

%——————————————————————————————————————————————-——————————————-
induce(Hyp):-
    iter_deep(Hyp,0).

iter_deep(Hyp,MaxD):-
    write('MaxD= '), write(MaxD), nl,
    start_hyp([[predecessor(X1,Y1)]/[X1,Y1], [predecessor(X2,Y2)]/[X2,Y2]]),
    complete(Hyp0),
    depth_first(Hyp0,Hyp,MaxD)
    ;
    NewMaxD is MaxD+1,
    iter_deep(Hyp, NewMaxD).

depth_first(Hyp,Hyp,_):-
    consistent(Hyp).
depth_first(Hyp0,Hyp,MaxD0):-
    MaxD0 > 0,
    MaxD1 is MaxD0-1,
    refine_hyp(Hyp0, Hyp1),
    complete(Hyp1),
    depth_first(Hyp1,Hyp,MaxD1).

complete(Hyp):-
    not(ex(E),				% A positive example
        once(prove(E, Hyp, Answer)),	% Prove it with Hyp
        Answer \== yes).		% possibly provable

consistent(Hyp):-
    not(nex(E),				% A negative example
        once(prove(E, Hyp, Answer)),	% Prove it with Hyp
        Answer \== no).			% possibly provable

refine_hyp(Hyp0,Hyp):-
    conc(Clauses1,[Clause0/Vars0 | Clauses2], Hyp0),
    conc(Clauses1,[Clause/Vars | Clauses2], Hyp),
    refine(Clause0, Vars0, Clause, Vars).

refine(Clause, Args, Clause, NewArgs):-
    conc(Args1, [A | Args2], Args),
    member(A, Args2),
    conc(Args1, Args2, NewArgs).
refine(Clause,Args,NewClause, NewArgs):-
    length(Clause, L),
    max_clause_length(MaxL),
    L < MaxL,
    backliteral(Lit, Vars),
    conc(Clause,[Lit],NewClause),
    conc(Args, Vars, NewArgs).

max_proof_length(10).

max_clause_length(3).

conc([],L,L).
conc([X|T],L,[X|L1]):-
    conc(T,L,L1).

%———————————————————————————————————————————————
% exercise 21.1
% Positive examples
% ex(predecessor(pam, ann)).
% ex(predecessor(tom, liz)).
% ex(predecessor(bob, liz)).

% Negative examples
% nex(predecessor(liz, pam)).
% nex(predecessor(tom, jim)).
% nex(predecessor(pam, tom)).

% Background parent relationships
% parent(pam, ann).
% parent(tom, liz).
% parent(bob, liz).
% parent(pam, bob).
% parent(tom, jim).

% backliteral(predecessor(X, Y), [X, Y]).
% backliteral(parent(X, Y), [X, Y]).
% prolog_predicate(parent(X, Y)).

%———————————————————————————————————————————————
% exercise 21.2

% Positive examples
ex(predecessor(pam, bob)).
ex(predecessor(pam, jim)).
ex(predecessor(tom, ann)).
ex(predecessor(tom, jim)).
ex(predecessor(tom, liz)).

% Negative examples
nex(predecessor(liz, bob)).
nex(predecessor(pat, bob)).
nex(predecessor(pam, liz)).
nex(predecessor(liz, jim)).
nex(predecessor(liz, liz)).


% Constrained background literals
backliteral([atom(X), parent(X, Y)], [X, Y]).
backliteral([atom(X), predecessor(X, Y)], [X, Y]).

% Declare 'parent' and 'atom' as Prolog predicates
prolog_predicate(parent(X, Y)).
prolog_predicate(atom(X)).
