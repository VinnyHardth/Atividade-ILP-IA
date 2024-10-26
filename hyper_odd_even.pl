% Program HYPER

:- op(500, xfx, :).

% induce(Hyp):
% induce a consistent and complete hypothesis Hyp by gradually refining start hypothesis

induce(Hyp):-
    init_counts, !,                       % initialize counts of hypothesis
    start_hyp(Hyps),                      % get starting hypothesis
    best_search(Hyps, _:Hyp).             % specialized best-first search


% best_search(CandidateHyps, FinalHypothesis):

best_search([Hyp|Hyps], Hyp):-
    show_counts,                          % show counts of hypothesis
    Hyp = 0:H,                            % cost = 0:H dosent cover any negative examples
    complete(H),                          % complete hypothesis

best_search([C0:H0 | Hyps0], H) :-
   write('Refining hypo with cost '), write(C0), 
   write(':'), nl, show_hypo(H0), nl,
   all_refinements(H0, NewHs),            % get all refinements of H0
   add_hyps(NewHs, Hyps0, Hyps), !,       % add new hypotheses to Hyps0
   add1(refined),                         % increment count of refined hypotheses
   best_search(Hyps, H).                  

all_refinements(H0, Hyps) :-
    findall(C:h,
            (refine_hyp(H0, H), 
                once((add1(generated),
                complete(H),
                add1(complete),
                eval(H,C)
                ) )),
            Hyps).
    	