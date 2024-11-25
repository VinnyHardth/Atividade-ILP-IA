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
% Função auxiliar para exibir a hipótese com variáveis renomeadas
write_hypothesis(Label, Hypothesis) :-
    copy_term(Hypothesis, HypothesisCopy),
    numbervars(HypothesisCopy, 0, _),  % Converte variáveis para _A, _B, etc.
    write(Label), write(': '), write(HypothesisCopy), nl.

induce(Hyp):-
    iter_deep(Hyp, 0, 0, 0, 0, Generated, Refined, ToBeRefined),
    nl,
    write('Hypotheses generated: '), write(Generated), nl,
    write('Hypotheses refined: '), write(Refined), nl,
    write('To be refined: '), write(ToBeRefined), nl.

iter_deep(Hyp, MaxD, Gen, Ref, ToRef, FinalGen, FinalRef, FinalToRef):-
    write('MaxD= '), write(MaxD), nl,
    start_hyp(Hyp0),
    write_hypothesis('Hipótese Inicial', Hyp0),  % Exibe a hipótese inicial
    complete(Hyp0),
    NewGen is Gen + 1,  % Incrementa o contador de hipóteses geradas
    depth_first(Hyp0, Hyp, MaxD, NewGen, Ref, ToRef, FinalGen, FinalRef, FinalToRef)
    ;
    NewMaxD is MaxD + 1,
    iter_deep(Hyp, NewMaxD, Gen, Ref, ToRef, FinalGen, FinalRef, FinalToRef).

depth_first(Hyp, Hyp, _, Gen, Ref, ToRef, Gen, NewRef, ToRef):-
    consistent(Hyp),
    NewRef is Ref + 1,  % Incrementa o contador de hipóteses refinadas quando consistente
    write_hypothesis('Hipótese Consistente', Hyp).  % Exibe a hipótese se for consistente

depth_first(Hyp0, Hyp, MaxD0, Gen, Ref, ToRef, FinalGen, FinalRef, FinalToRef):-
    MaxD0 > 0,
    MaxD1 is MaxD0 - 1,
    refine_hyp(Hyp0, Hyp1),
    write_hypothesis('Hipótese Refinada', Hyp1),  % Exibe cada hipótese refinada
    complete(Hyp1),
    NewGen is Gen + 1,  % Incrementa o contador de hipóteses geradas
    NewToRef is ToRef + 1,  % Incrementa o contador de hipóteses a serem refinadas
    depth_first(Hyp1, Hyp, MaxD1, NewGen, Ref, NewToRef, FinalGen, FinalRef, FinalToRef).


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
not(A,B,C):-
    A,
    B,
    C, !, fail.
not(_,_,_).
