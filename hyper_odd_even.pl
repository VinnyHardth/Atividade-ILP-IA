% Program HYPER

:- op(500, xfx, :).

% induce(Hyp):
% induce a consistent and complete hypothesis Hyp by gradually refining start hypothesis

induce(Hyp):-
    init_counts, !,                     % initialize counts of hypothesis
    start_hyps(Hyps),                   % get starting hypothesis
    best_search(Hyps, _:Hyp).           % specialized best-first search


% best_search(CandidateHyps, FinalHypothesis):

best_search([Hyp|Hyps], Hyp):-
    show_counts,                        % show counts of hypothesis
    Hyp = 0:H,                          % cost = 0:H doesn't cover any negative examples
    complete(H).                        % complete hypothesis

best_search([C0:H0 | Hyps0], H) :-
    write('Refining hypo with cost '), write(C0),
    write(':'), nl, show_hyp(H0), nl,
    all_refinements(H0, NewHs),         % get all refinements of H0
    add_hyps(NewHs, Hyps0, Hyps), !,    % add new hypotheses to Hyps0
    add1(refined),                      % increment count of refined hypotheses
    best_search(Hyps, H).

all_refinements(H0, Hyps) :-
    findall(C:H,
            (refine_hyp(H0, H),
             once((add1(generated),
             complete(H),
             add1(complete),
             eval(H, C)
             ))),
            Hyps).


% add_hyps(Hyps1, Hyps2, Hyps):
% merge Hyps1 and Hyps2 in order of cost, giving Hyps

add_hyps(Hyps1, Hyps2, Hyps) :-
    mergesort(Hyps1, OrderedHyps1),
    merge(Hyps2, OrderedHyps1, Hyps).

complete(Hyp) :-                        % Hyp covers all positive examples
    \+ (ex(P),                          % A positive example
        once(prove(P, Hyp, Answ)),      % Prove it with Hyp
        Answ \== yes).                  % possibly not proved
    

% eval(Hyp, Cost):
% Cost of Hyp = Size + 10 * # covered negative examples

eval(Hyp, Cost) :-
    size(Hyp, Size),
    covers_neg(Hyp, Negs),
    (Negs = 0, !, Cost is 0; Cost is Size + 10 * Negs).


% size(Hyp, Size):
% Size = k1+#literals + k2 * #variables in hypothesis; Settings of parameters: k1=10, k2=1

size([], 0).

size([Cs0/Vs0 | RestHyp], Size) :-
    length(Cs0, LO),
    length(Vs0, NO),
    size(RestHyp, SizeRest),
    Size is 10 * LO + NO + SizeRest.


% covers_neg(H, N):
% N is number of neg. examples possibly covered by H
% Example possibly covered if prove/3 returns 'yes' or 'maybe'

covers_neg(Hyp, N) :-
    findall(1, (nex(E), once(prove(E, Hyp, Answ)), Answ \== no), L),
    length(L, N).


% unsatisfiable(Clause, Hyp):
% Clause can never be used in any proof, i.e., Clause's body cannot be proved from Hyp

unsatisfiable([Head | Body], Hyp) :-
    once(prove(Body, Hyp, Answ)), Answ = no.

start_hyps(Hyps) :-
    max_clauses(M),
    setof(C:H,
          (start_hyp(H, M), add1(generated),
           complete(H), add1(complete), eval(H, C)),
          Hyps).


% start_hyp(Hyp, MaxClauses):
% Set of starting hypotheses, limited by MaxClauses

start_hyp([], _).
start_hyp([C | Cs], M) :-
    M > 0, M1 is M - 1,
    start_clause(C),
    start_hyp(Cs, M1).


% refine_hyp(Hypo, Hyp):
% refine hypothesis Hypo into Hyp

refine_hyp(Hypo, Hyp) :-
    choose_clause(Hypo, Clause0/Vars0, Clauses1, Clauses2),
    append(Clauses1, [Clause/Vars | Clauses2], Hyp),
    refine(Clause0, Vars0, Clause, Vars),
    non_redundant(Clause),
    \+ unsatisfiable(Clause, Hyp).

choose_clause(Hyp, Clause, Clauses1, Clauses2) :-
    append(Clauses1, [Clause | Clauses2], Hyp),
    nex(E),
    prove(E, [Clause], yes), !;
    append(Clauses1, [Clause | Clauses2], Hyp).


% refine(Clause, Args, NewClause, NewArgs):
% refine Clause with variables Args giving NewClause with NewArgs

refine(Clause, Args, Clause, NewArgs) :-
    append(Args1, [A | Args2], Args),
    member(A, Args2),
    append(Args1, Args2, NewArgs).

% Refine a variable to a term

refine(Clause, Args0, Clause, Args) :-
    select(Var:Type, Args0, Args1),
    term(Type, Var, Vars),
    append(Args1, Vars, Args).

% Refine by adding a literal

refine(Clause, Args, NewClause, NewArgs) :-
    length(Clause, L),
    max_clause_length(MaxL),
    L < MaxL,
    backliteral(Lit, InArgs, RestArgs),
    append(Clause, [Lit], NewClause),
    connect_inputs(Args, InArgs),
    append(Args, RestArgs, NewArgs).


% non_redundant(Clause): Clause has no obviously redundant literals

non_redundant([]).
non_redundant([Lit1 | Lits]):-
    \+ literal_member(Lit1, Lits),
    non_redundant(Lits).

literal_member(X, [X1 | Xs]) :-
    (X == X1, !; literal_member(X, Xs)).


% show_hyp(Hypothesis):
% write out Hypothesis in readable form

show_hyp([]) :- nl.
show_hyp([C/Vars | Cs]) :- nl,
    copy_term(C/Vars, C1/Vars1),
    name_vars(Vars1, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N']),
    show_clause(C1), show_hyp(Cs), !.

show_clause([Head | Body]) :-
    write(Head),
    (Body = [] ; write(':'), nl, write_body(Body)).

write_body([]):- write('.'), !.
write_body([G | Gs]) :- !, tab(2), write(G),
    (Gs = [], !, write('.'), nl;
     write(','), nl, write_body(Gs)).

name_vars([], _).
name_vars([Name:Type | Xs], [Name | Names]) :- name_vars(Xs, Names).


% connect_inputs(Vars, Inputs):
% Match each variable in list Inputs with a variable in list Vars

connect_inputs(_, []).
connect_inputs(S, [X | Xs]):-
    member(X, S),
    connect_inputs(S, Xs).


% merge(L1, L2, L3): all lists sorted
merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([X1 | L1], [X2 | L2], [X1 | L3]) :-
    X1 @=< X2, !,
    merge(L1, [X2 | L2], L3).
merge(L1, [X2 | L2], [X2 | L3]) :- merge(L1, L2, L3).

% mergesort(L1, L2): sort L1 giving L2

mergesort([], []) :- !.
mergesort([X], [X]) :- !.
mergesort(L, S) :-
    split(L, L1, L2),
    mergesort(L1, S1), mergesort(L2, S2),
    merge(S1, S2, S).

% split(L, L1, L2): split L into lists of approx. equal length

split([], [], []).
split([X], [X], []).
split([X1, X2 | L], [X1 | L1], [X2 | L2]) :- split(L, L1, L2).


% Counters of generated, complete, and refined hypotheses

init_counts :-
    retractall(counter(_, _)),          % Clear old counters
    assert(counter(generated, 0)),      % Initialize counter 'generated'
    assert(counter(complete, 0)),       % Initialize counter 'complete'
    assert(counter(refined, 0)).        % Initialize counter 'refined'

add1(Counter) :-
    retract(counter(Counter, N)), !, N1 is N + 1, assert(counter(Counter, N1)).

show_counts :-
    counter(generated, NG), counter(refined, NR), counter(complete, NC),
    nl, write('Hypotheses generated: '), write(NG),
    nl, write('Hypotheses refined: '), write(NR),
    ToBeRefined is NC - NR,
    nl, write('To be refined: '), write(ToBeRefined), nl.


% Parameter settings

max_proof_length(6).       % Max proof length, counting calls to preds in hypothesis
max_clauses(4).            % Max number of clauses in hypothesis
max_clause_length(5).       % Max number of literals in a clause

%——————————————————————————————————————————————————————————————————————————————

% Inducing odd and even length for lists

% Defining backliterals for even and odd length properties
backliteral(even(L), [L:list], []).
backliteral(odd(L), [L:list], []).

% Defining terms for list construction
term(list, [X, L], [X:item, L:list]).  % corrected term definition
term(list, [], []).

% Defining a Prolog predicate (example)
prolog_predicate(fail).

% Starting clauses for even and odd list length hypotheses
start_clause([odd(L)]/[L:list]).
start_clause([even(L)]/[L:list]).

% Positive examples (ex) for even and odd lists
ex(even([])).                % empty list is even
ex(even([a, b])).            % list with 2 elements is even
ex(odd([a])).                % list with 1 element is odd
ex(odd([b, c, d])).          % list with 3 elements is odd
ex(odd([a, b, c, d, e])).    % list with 5 elements is odd
ex(even([a, b, c, d])).      % list with 4 elements is even

% Negative examples (nex) for even and odd lists
nex(even([a])).              % list with 1 element is not even
nex(even([a, b, c])).        % list with 3 elements is not even
nex(odd([])).                % empty list is not odd
nex(odd([a, b])).            % list with 2 elements is not odd
nex(odd([a, b, c, d])).      % list with 4 elements is not odd
