% Conhecimento de Fundo
backliteral(parent(X,Y), [X,Y]).
backliteral(predecessor(X,Y), [X,Y]).

prolog_predicate(parent(_, _)).
prolog_predicate(predecessor(_, _)).

% Definições de relacionamento (parentesco)
parent(pam, bob).
parent(pam, jim).
parent(pam, ann).
parent(tom, jim).
parent(tom, liz).

% Definição de predecessor como regra para experimentação
predecessor(X, Y) :- parent(X, Y).
predecessor(X, Y) :- parent(X, Z), predecessor(Z, Y).

% Exemplos Positivos de predecessor
ex(predecessor(pam, bob)).
ex(predecessor(pam, jim)).
ex(predecessor(pam, ann)).
ex(predecessor(tom, jim)).
ex(predecessor(tom, liz)).

% Exemplos Negativos de predecessor
nex(predecessor(liz, bob)).
nex(predecessor(pat, bob)).
nex(predecessor(pam, liz)).
nex(predecessor(liz, jim)).
nex(predecessor(liz, liz)).

% Hipótese inicial
start_hyp([[predecessor(X1, Y1)]/[X1, Y1], [predecessor(X2, Y2)]/[X2, Y2]]).

