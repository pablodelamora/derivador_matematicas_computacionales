%usage example:
% simplifiedDerivative([^, [/, [*, x, x], [-, 3, x]], 3], D).
% D = [*, 3, [^, [/, [*, x, x], [-, 3, x]], 2], [/, [-, [*, [-, 3, x], [+, x, x]], [*, -1, [*, x, x]]], [^, [-, 3, x], 2]]].
%verifications of operations:
isAddition(F) :- F = [+|_], !.
isAddition(+) :- !.
isPower(F) :- F = [^|_], !.
isPower(^) :- !.
isMultiplication(F) :- F = [*|_], !.
isMultiplication(*) :- !.
isSubtraction(F) :- F = [-|_], !.
isSubtraction(-) :- !.
isDivision(F) :- F = [/|_], !.
isDivision(/) :- !.
isOperator(X) :-
    isAddition(X), !;
    isPower(X), !;
    isMultiplication(X), !;
    isSubtraction(X), !;
    isDivision(X), !.
isSymbolOperator(X) :-
    X = +, !;
    X = *, !;
    X = -, !;
    X = ^, !;
    X = /, !.
%trailing list (cuts list L at position N to give T, eg: trailingList(2, [a, b, c], [c])):
trailingList(0, L, L) :- !.
trailingList(N, L, T) :-
    length(L, Len),
    Len = N,
    T = [],
    !.
trailingList(N, L, T) :-
    N > 0,
    length(L, Len),
    N < Len,
    L = [_|SL],
    NN is N-1,
    trailingList(NN, SL, T),
    !.
%leading list (cuts list L at position N to give T, eg: leadingList(2, [a, b, c], [a, b])):
leadingList(N, L, T) :-
    trailingList(N, L, TL),
    append(T, TL, L),
    !.
%immediate derivatives:
derivative([x], 1) :- !.
derivative(x, 1):- !.
derivative(F, D) :- number(F), D is 0, !.
derivative([F], D) :- number(F), D is 0, !.
derivative(F, D) :-
    F = [H],
    derivative(H, D),
    !.
%recursive derivatives:
derivative(F, D) :-
    isAddition(F),
    length(F, Len),
    Len = 3,
    nth0(1, F, G),
    nth0(2, F, H),
    derivative(G, DG),
    derivative(H, DH),
    D = [+, DG, DH],
    !.
derivative(F, D) :-
    isAddition(F),
    nth0(1, F, G),
    derivative(G, DG),
    trailingList(2, F, TL),
    NTL = [+|TL],
    derivative(NTL, DNTL),
    D = [+, DG, DNTL],
    !.
derivative(F, D) :-
    isPower(F),
    nth0(2, F, P),
    NP is P-1,
    nth0(1, F, G),
    derivative(G, DG),
    D = [*, P, [^, G, NP], DG],
    !.
derivative(F, D) :-
    isSubtraction(F),
    nth0(1, F, G),
    nth0(2, F, H),
    derivative(H, DH),
    derivative(G, DG),
    D = [-, DG, DH],
    !.
derivative(F, D) :-
    isDivision(F),
    nth0(1, F, G),
    nth0(2, F, H),
    derivative(G, DG),
    derivative(H, DH),
    D = [/, [-, [*, H, DG], [*, G, DH]], [^, H, 2]],
    !.
derivative(F, D) :-
    isMultiplication(F),
    length(F, Len),
    Len = 3,
    nth0(1, F, G),
    trailingList(2, F, H),
    derivative(G, DG),
    derivative(H, DH),
    D = [+, [*, DG, H], [*, G, DH]],
    !.
derivative(F, D) :-
    isMultiplication(F),
    nth0(1, F, G),
    trailingList(2, F, TL),
    H = [*|TL],
    derivative(G, DG),
    derivative(H, DH),
    D = [+, [*, DG, H], [*, G, DH]],
    !.
%tells if elements in list are numbers:
numberList([X]) :- number(X), !.
numberList(L) :-
    L = [H|T],
    number(H),
    numberList(T),
    !.
%adds all elements of a number list:
reduceAdd([], 0) :- !.
reduceAdd([X], X) :- number(X), !.
reduceAdd([X, Y], R) :- number(X), number(Y), R is X + Y, !.
reduceAdd(L, R) :-
    numberList(L),
    length(L, Len),
    Len > 2,
    L = [H|T],
    reduceAdd(T, PR),
    R is PR + H,
    !.
%multiplies all elements of a number list:
%empty numbers reduces to multiplicative identity
reduceMultiply([], 1) :- !.
%unitary list of number X reduces to X
reduceMultiply([X], X) :- number(X), !.
%list of numbers X and Y reduces to X times Y
reduceMultiply([X, Y], R) :- number(X), number(Y), R is X * Y, !.
%list of numbers reduces to multiplication of each
reduceMultiply(L, R) :-
    numberList(L),
    length(L, Len),
    Len > 2,
    L = [H|T],
    reduceMultiply(T, PR),
    R is PR * H,
    !.
%get numbers from list:
getNumbers([], []) :- !.
getNumbers([X], [X]) :- number(X), !.
getNumbers(L, N) :-
    L = [H|T],
    number(H),
    getNumbers(T, PN),
    N = [H|PN],
    !.
getNumbers(L, N) :-
    L = [H|T],
    not(number(H)),
    getNumbers(T, PN),
    N = PN,
    !.
%get not numbers from list:
getNotNumbers([], []) :- !.
getNotNumbers([X], [X]) :- not(number(X)), !.
getNotNumbers(L, NN) :-
    L = [H|T],
    number(H),
    getNotNumbers(T, PN),
    NN = PN,
    !.
getNotNumbers(L, NN) :-
    L = [H|T],
    not(number(H)),
    getNotNumbers(T, PN),
    NN = [H|PN],
    !.
%split numbers, operator and not numbers:
split([OP|T], N, NN, OP) :-
    isOperator(OP),
    getNumbers(T, N),
    getNotNumbers(T, NN),
    !.
%simplification:
%immediate simplifications:
%the simplification of the empty list is the empty list
simplify([], []) :- !.
%the simplification of an atom is the atom itself
simplify(X, X) :- not(is_list(X)), !.
%the simplification of a unitary list is the simplification of its element
simplify([X], SX) :- simplify(X, SX), !.
%the simplification of a list with multiple elements and not operation itself and simplification of tail of original list is empty, is the simplification of the first element
simplify(L, S) :-
    L = [H|T],
    not(isSymbolOperator(H)),
    simplify(H, HS),
    simplify(T, TS),
    TS = [],
    S = HS,
    !.
%the simplification of a list with multiple elements and not operation itself and simplification of tail of original list is a non empty list and an operation itself, is the simplification of each of the head and the simplification of tail
simplify(L, S) :-
    L = [H|T],
    not(isSymbolOperator(H)),
    simplify(H, HS),
    simplify(T, TS),
    not(TS = []),
    is_list(TS),
    isOperator(TS),
    S = [HS, TS],
    !.
%the simplification of a list with multiple elements and not operation itself and simplification of tail of original list is a non empty list and not an operation itself, is the simplification of each of the elements
simplify(L, S) :-
    L = [H|T],
    not(isSymbolOperator(H)),
    simplify(H, HS),
    simplify(T, TS),
    not(TS = []),
    is_list(TS),
    not(isOperator(TS)),
    S = [HS|TS],
    !.
%the simplification of a list with multiple elements and not operation itself and simplification of tail of original list is an atom, is the simplification of the first element and the atom
simplify(L, S) :-
    L = [H|T],
    not(isSymbolOperator(H)),
    simplify(H, HS),
    simplify(T, TS),
    not(TS = []),
    not(is_list(TS)),
    S = [HS, TS],
    !.
%simplification for power:
%when power is zero and base is zero
simplify(F, _) :-
    nth0(0, F, OP),
    isPower(OP),
    nth0(1, F, Base),
    simplify(Base, SBase),
    nth0(2, F, Power),
    simplify(Power, SPower),
    SPower = 0,
    SBase = 0,
    !,
    fail.
%when power is zero and base is not zero
simplify(F, S) :-
    nth0(0, F, OP),
    isPower(OP),
    nth0(1, F, Base),
    simplify(Base, SBase),
    nth0(2, F, Power),
    simplify(Power, SPower),
    SPower = 0,
    not(SBase = 0),
    S is 1,
    !.
%when base is zero and power is not zero
simplify(F, S) :-
    nth0(0, F, OP),
    isPower(OP),
    nth0(1, F, Base),
    simplify(Base, SBase),
    nth0(2, F, Power),
    simplify(Power, SPower),
    not(SPower = 0),
    SBase = 0,
    S is 0,
    !.
%when power is 1
simplify(F, S) :-
    nth0(0, F, OP),
    isPower(OP),
    nth0(1, F, Base),
    simplify(Base, SBase),
    nth0(2, F, Power),
    simplify(Power, SPower),
    SPower = 1,
    S = SBase,
    !.
%general case with numbers
simplify(F, S) :-
    nth0(0, F, OP),
    isPower(OP),
    nth0(1, F, Base),
    simplify(Base, SBase),
    nth0(2, F, Power),
    simplify(Power, SPower),
    number(SBase),
    number(SPower),
    S is SBase^SPower,
    !.
%general case
simplify(F, S) :-
    nth0(0, F, OP),
    isPower(OP),
    nth0(1, F, Base),
    simplify(Base, SBase),
    nth0(2, F, Power),
    simplify(Power, SPower),
    S = [^, SBase, SPower],
    !.
%simplification for subtraction:
%only numbers operate
simplify(F, S) :-
    nth0(0, F, OP),
    isSubtraction(OP),
    nth0(1, F, Minuend),
    simplify(Minuend, SMinuend),
    nth0(2, F, Subtrahend),
    simplify(Subtrahend, SSubtrahend),
    number(SMinuend),
    number(SSubtrahend),
    S is SMinuend - SSubtrahend,
    !.
%subtrahend and minuend simplifications are zero
simplify(F, S) :-
    nth0(0, F, OP),
    isSubtraction(OP),
    nth0(1, F, Minuend),
    simplify(Minuend, SMinuend),
    nth0(2, F, Subtrahend),
    simplify(Subtrahend, SSubtrahend),
    SMinuend = 0,
    SSubtrahend = 0,
    S is 0,
    !.
%subtrahend simplification is zero
simplify(F, S) :-
    nth0(0, F, OP),
    isSubtraction(OP),
    nth0(1, F, Minuend),
    simplify(Minuend, SMinuend),
    nth0(2, F, Subtrahend),
    simplify(Subtrahend, SSubtrahend),
    SSubtrahend = 0,
    S = SMinuend,
    !.
%minuend simplification is zero
simplify(F, S) :-
    nth0(0, F, OP),
    isSubtraction(OP),
    nth0(1, F, Minuend),
    simplify(Minuend, SMinuend),
    nth0(2, F, Subtrahend),
    simplify(Subtrahend, SSubtrahend),
    SMinuend = 0,
    S = [*, -1, SSubtrahend],
    !.
%equal operands
simplify(F, S) :-
    nth0(0, F, OP),
    isSubtraction(OP),
    nth0(1, F, Minuend),
    simplify(Minuend, SMinuend),
    nth0(2, F, Subtrahend),
    simplify(Subtrahend, SSubtrahend),
    SMinuend = SSubtrahend,
    S is 0,
    !.
%general case (neither subtrahend nor minuend simplify to zero)
simplify(F, S) :-
    nth0(0, F, OP),
    isSubtraction(OP),
    nth0(1, F, Minuend),
    simplify(Minuend, SMinuend),
    nth0(2, F, Subtrahend),
    simplify(Subtrahend, SSubtrahend),
    S = [-, SMinuend, SSubtrahend],
    !.
%simplification for division:
%when divisor simplification is equal to 0
simplify(F, _) :-
    nth0(0, F, OP),
    isDivision(OP),
    nth0(2, F, Divisor),
    simplify(Divisor, SDivisor),
    SDivisor = 0,
    !,
    fail.
%when only numbers operate
simplify(F, S) :-
    nth0(0, F, OP),
    isDivision(OP),
    nth0(1, F, Dividend),
    simplify(Dividend, SDividend),
    nth0(2, F, Divisor),
    simplify(Divisor, SDivisor),
    number(SDividend),
    number(SDivisor),
    S is SDividend/SDivisor,
    !.
%when divisor simplification is equal to 1
simplify(F, S) :-
    nth0(0, F, OP),
    isDivision(OP),
    nth0(1, F, Dividend),
    simplify(Dividend, SDividend),
    nth0(2, F, Divisor),
    simplify(Divisor, SDivisor),
    SDivisor = 1,
    S = SDividend,
    !.
%when dividend simplification is equal to 0
simplify(F, S) :-
    nth0(0, F, OP),
    isDivision(OP),
    nth0(1, F, Dividend),
    simplify(Dividend, SDividend),
    SDividend = 0,
    S is 0,
    !.
%equal operands
simplify(F, S) :-
    nth0(0, F, OP),
    isDivision(OP),
    nth0(1, F, Dividend),
    simplify(Dividend, SDividend),
    nth0(2, F, Divisor),
    simplify(Divisor, SDivisor),
    SDividend = SDivisor,
    S is 1,
    !.
%general case
simplify(F, S) :-
    nth0(0, F, OP),
    isDivision(OP),
    nth0(1, F, Dividend),
    simplify(Dividend, SDividend),
    nth0(2, F, Divisor),
    simplify(Divisor, SDivisor),
    S = [/, SDividend, SDivisor],
    !.
%simplification for addition:
%when nothing is being added:
simplify(F, S) :-
    length(F, 2),
    nth0(0, F, OP),
    isAddition(OP),
    nth0(1, F, G),
    simplify(G, S),
    !.
%when NNS is [], S is R
simplify(F, S) :-
    split(F, N, NN, OP),
    isAddition(OP),
    reduceAdd(N, R),
    simplify(NN, NNS),
    NNS = [],
    S is R,
    !.
%when NNS is a non empty list and an operation and R is zero, S is NNS
simplify(F, S) :-
    split(F, N, NN, OP),
    isAddition(OP),
    reduceAdd(N, R),
    simplify(NN, NNS),
    is_list(NNS),
    isOperator(NNS),
    R = 0,
    S = NNS,
    !.
%when NNS is a non empty list and not an operation and R is zero, S is [+|NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isAddition(OP),
    reduceAdd(N, R),
    simplify(NN, NNS),
    is_list(NNS),
    not(isOperator(NNS)),
    R = 0,
    S = [+|NNS],
    !.
%when NNS is a non empty list and not an operation itself and R is not zero, S is [+, R|NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isAddition(OP),
    reduceAdd(N, R),
    simplify(NN, NNS),
    is_list(NNS),
    not(isOperator(NNS)),
    not(R = 0),
    S = [+, R|NNS],
    !.
%when NNS is a non empty list and an operation itself and R is not zero, S is [+, R, NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isAddition(OP),
    reduceAdd(N, R),
    simplify(NN, NNS),
    is_list(NNS),
    isOperator(NNS),
    not(R = 0),
    S = [+, R, NNS],
    !.
%when NNS is an atom and R is zero, S is [+, NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isAddition(OP),
    reduceAdd(N, R),
    simplify(NN, NNS),
    R = 0,
    S = [+, NNS],
    !.
%when NNS is an atom and R is not zero, S is [+, R, NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isAddition(OP),
    reduceAdd(N, R),
    simplify(NN, NNS),
    not(R = 0),
    S = [+, R, NNS],
    !.
%simplification for multiplication:
%when nothing is being multiplied:
simplify(F, S) :-
    length(F, 2),
    nth0(0, F, OP),
    isMultiplication(OP),
    nth0(1, F, G),
    simplify(G, S),
    !.
%when numbers (N) reduce to zero, S is zero
simplify(F, S) :-
    split(F, N, _, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    R = 0,
    S is 0,
    !.
%when not numbers reduction (NNS) are [], S is number reduction
simplify(F, S) :-
    split(F, N, NN, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    simplify(NN, NNS),
    NNS = [],
    S is R,
    !.
%when N reduce to 1 or are [] and simplification of NN (NNS) is not [] but is list and an operation itself, S is NNS
simplify(F, S) :-
    split(F, N, NN, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    R = 1,
    simplify(NN, NNS),
    not(NNS = []),
    is_list(NNS),
    isOperator(NNS),
    S = NNS,
    !.
%when N reduce to 1 or are [] and simplification of NN (NNS) is not [] but is list and not an operation itself, S is [*|NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    R = 1,
    simplify(NN, NNS),
    not(NNS = []),
    is_list(NNS),
    not(isOperator(NNS)),
    S = [*|NNS],
    !.
%when N reduce to 1 or are [] and simplification of NN (NNS) is not [] and is not list, S is NNS
simplify(F, S) :-
    split(F, N, NN, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    R = 1,
    simplify(NN, NNS),
    not(NNS = []),
    not(is_list(NNS)),
    S = NNS,
    !.
%when N do not reduce to 1 or are not [] and simplification of NN (NNS) is [], S is R
simplify(F, S) :-
    split(F, N, NN, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    not(R = 1),
    simplify(NN, NNS),
    NNS = [],
    S is R,
    !.
%when N do not reduce to 1 or are not [] and simplification of NN (NNS) is not [] but it is a list and an operation, S is [*, R, NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    not(R = 1),
    simplify(NN, NNS),
    not(NNS = []),
    is_list(NNS),
    isOperator(NNS),
    S = [OP, R, NNS],
    !.
%when N do not reduce to 1 or are not [] and simplification of NN (NNS) is not [] but it is a list and not an operation, S is [*, R, NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    not(R = 1),
    simplify(NN, NNS),
    not(NNS = []),
    is_list(NNS),
    not(isOperator(NNS)),
    S = [OP, R|NNS],
    !.
%when N do not reduce to 1 or are not [] and simplification of NN (NNS) is not [] and it is not a list, S is [*, R, NNS]
simplify(F, S) :-
    split(F, N, NN, OP),
    isMultiplication(OP),
    reduceMultiply(N, R),
    not(R = 1),
    simplify(NN, NNS),
    not(NNS = []),
    not(is_list(NNS)),
    S = [OP, R, NNS],
    !.
%simplified derivative:
simplifiedDerivative(F, SD) :- derivative(F, D), deepSimplify(D, SD), !.
%deep simplify:
deepSimplify(F, S) :- simplify(F, S), F=S, !.
deepSimplify(F, S) :- simplify(F, SF), not(F=SF), deepSimplify(SF, S), !.
