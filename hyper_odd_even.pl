% Wrapper para compatibilidade com backliteral/2
backliteral(Literal, Type) :-
    backliteral(Literal, Type, []).

% Literais de fundo (backliteral/3) para listas ímpares e pares
backliteral(even(L), [L:list], []).
backliteral(odd(L), [L:list], []).

% Termos para listas
term(list, [X|L], [X:item, L:list]). % Definição de lista não-vazia
term(list, [], []).                  % Definição de lista vazia

% Predicado para representar falha no Prolog
prolog_predicate(fail).

% Cláusulas iniciais para listas ímpares e pares
start_clause([odd(L)] / [L:list]).
start_clause([even(L)] / [L:list]).

% Hipóteses iniciais de partida para HYPER
start_hyp([[odd(L)]/[L:list], [even(L)]/[L:list]]).

% Exemplos positivos (ex)
ex(even([])).             % Lista vazia (par)
ex(even([a, b])).         % Lista com dois elementos (par)
ex(odd([a])).             % Lista com um elemento (ímpar)
ex(odd([b, c, d])).       % Lista com três elementos (ímpar)
ex(odd([a, b, c, d, e])). % Lista com cinco elementos (ímpar)
ex(even([a, b, c, d])).   % Lista com quatro elementos (par)

% Exemplos negativos (nex)
nex(even([a])).           % Um elemento (ímpar) não é par
nex(even([a, b, c])).     % Três elementos (ímpar) não é par
nex(odd([])).             % Lista vazia não é ímpar
nex(odd([a, b])).         % Dois elementos (par) não é ímpar
nex(odd([a, b, c, d])).   % Quatro elementos (par) não é ímpar

% Definições de alternância para pares e ímpares
even([]).
even([_, _ | L]) :- even(L).
odd([_ | L]) :- even(L).

