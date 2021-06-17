	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%
	% The ten-day rule
	% 
	% We  maintain  a 'purchaseStructure'  which  tracks what  has
	% happened in the last 10  days, together with a pool of units
	% which were bought more than then days ago.
	%
	% This makes  it easy to  unroll the 10  day rule to a  set of
	% explicit rules.

	% We  need to  be able  to  generate an  initial value  tuple,
	% containing an initial purchaseStructure.

initVal(D, FS, RPIH, val(FS, InitPurchaseStructure, 0, RPIH, [])) :-
	initPurchaseStructure(D, InitPurchaseStructure).

initPurchaseStructure(D, purchaseStruct(T, T, T, T, T, T, T, T, T, T, r(D,0,0,0))) :-
	T = nil(nil).

        % We also  need to be able  to shift a  purchase structure one
        % day into the future (by rolling the contents of T-9, if any,
        % into the pool.

shift1(RPIH, Fund, purchaseStruct(T0,       T1, T2, T3, T4, T5, T6, T7, T8, T9, Acc0),
                   purchaseStruct(nil(nil), T0, T1, T2, T3, T4, T5, T6, T7, T8, Acc1)) :-
	rollIntoPool(RPIH, Fund, T9, Acc0, Acc1).

rollIntoPool(_, _, nil(nil), r(D, U, BV, IBV), r(D, U, BV, IBV)).
rollIntoPool(RPIH, Fund, nil(D), r(D0, U, BV, IBV0), r(D, U, BV, IBV1)) :-
	calcAI(Fund, D, D0, RPIH, AI),
	IBV1 is IBV0 * AI.
rollIntoPool(RPIH, Fund, p(D, U, BV), r(D0, U0, BV0, IBV0), r(D, U1, BV1, IBV1)) :-
	U1   is U + U0,
	BV1  is BV + BV0,
	calcAI(Fund, D, D0, RPIH, AI),
	IBV1 is IBV0 * AI + BV.
 
        % Given  this, it  is easy  to implement  an  arbitrary shift.

shiftN(0, _RPIH, _Fund, PurchaseStructure, PurchaseStructure).
        % N = 0
shiftN(N, RPIH, Fund, PurchaseStructure1, PurchaseStructureN) :-
	(0 < N, N =< 10),
	M is N - 1,
	shift1(RPIH, Fund, PurchaseStructure1, PurchaseStructure2),
	shiftN(M, RPIH, Fund, PurchaseStructure2, PurchaseStructureN).
shiftN(N, RPIH, Fund, PurchaseStructure0, PurchaseStructureN) :-
	N > 10,
	shiftN(10, RPIH, Fund, PurchaseStructure0, PurchaseStructureN).

        % And from this, given code to map dates onto ordinals, we can
        % abstract  things to the  level of  dates: rolling  forward a
        % purchaseStructure from date D0 to date D1.

moveForward(RPIH, Fund, (D0, PurchaseStructure0), (D1, PurchaseStructure1)) :-
	dateToOrd(D0, O0),
	dateToOrd(D1, O1),
	Diff is O1 - O0,
	shiftN(Diff, RPIH, Fund, PurchaseStructure0, PurchaseStructure1).

	%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%
	% The following encodes the  10-day rule.
	%
	% Purchases are easy, since  they only affect the current day.
	% We just update the details.

purchase(D, U, V, purchaseStruct(T00, T1, T2, T3, T4, T5, T6, T7, T8, T9, Rest),
	          purchaseStruct(T01, T1, T2, T3, T4, T5, T6, T7, T8, T9, Rest)) :-
	purchase1(D, U, V, T00, T01).

purchase1(D, U, V, nil(_), p(D, U, V)).
purchase1(D, U, V, p(D, U0, V0), p(D, U1, V1)) :-
	U1 is U + U0,
	V1 is V + V0.

	% Sales  are where  the complexity  is.  I  have  unrolled the
	% loop, since  this makes things  clearer (and is  the natural
	% way to do things in Prolog).
	% 
	% base case: U is 0: make sure that an event is registered for
	% the current day.

sale(D, U, _, _, purchaseStruct(nil(nil), T1, T2, T3, T4, T5, T6, T7, T8, T9, Pool),
	   purchaseStruct(nil(D), T1, T2, T3, T4, T5, T6, T7, T8, T9, Pool)) :- U =:= 0, !.
sale(_, U, _, _, purchaseStruct(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, Pool),
	   purchaseStruct(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, Pool)) :- U =:= 0, !.

	% first try to net out sales first against purchases today, if there are any.

sale(D, U0, Fund, RPIH, purchaseStruct(B0, T1, T2, T3, T4, T5, T6, T7, T8, T9, Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(B1, T1, T2, T3, T4, T5, T6, T7, T8, T9, Pool), PurchaseStructure).

	% then work back from T-9 to the present

sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, T5, T6, T7, T8, B0, Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, T5, T6, T7, T8, B1, Pool), PurchaseStructure).
sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, T5, T6, T7, B0, nil(T9), Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, T5, T6, T7, B1, nil(T9), Pool), PurchaseStructure).
sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, T5, T6, B0, nil(T8), nil(T9), Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, T5, T6, B1, nil(T8), nil(T9), Pool), PurchaseStructure).
sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, T5, B0, nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, T5, B1, nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure).
sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, B0, nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, T4, B1, nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure).
sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, B0, nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, T3, B1, nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure).
sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, B0, nil(T4), nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), T1, T2, B1, nil(T4), nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure).
sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), T1, B0, nil(T3), nil(T4), nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), T1, B1, nil(T3), nil(T4), nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure).
sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), B0, nil(T2), nil(T3), nil(T4), nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), B1, nil(T2), nil(T3), nil(T4), nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), Pool), PurchaseStructure).

	% and finally, if necessary, take out of the pool.

sale(D, U0, Fund, RPIH, purchaseStruct(nil(T0), nil(T1), nil(T2), nil(T3), nil(T4), nil(T5), nil(T6), nil(T7), nil(T8), nil(T9),   B0), PurchaseStructure) :-
	sale1(D, Fund, RPIH, U0, B0, B1, Rest),
	sale(D, Rest, Fund, RPIH, purchaseStruct(nil(T0), nil(T1), nil(T2), nil(T3), nil(T4), nil(T5), nil(T6), nil(T7), nil(T8), nil(T9), B1), PurchaseStructure).

        % sale1 provides, essentially, the body of the loop: there are
        % three possibilities
	%
	% Either  the number of  units is  less than  or equal  to the
	% number in the bucket we are looking at

sale1(_, _F, _RPIH, U, p(D, U0, _), nil(D), U1) :-
	U0 =< U,
	U1 is U - U0.

	% or the number of units is more than the number in the bucket

sale1(_, _F, _RPIH, U, p(D0, U0, V0), p(D0, U1, V1), 0) :-
	not(U0 =< U),
	U1 is U0 - U,
	V1 is V0 * U1 / U0.

	% or we  have fallen  through to the  pool, in which  case the
	% number  of units available  is assumed  always to  be larger
	% than the sale units.

sale1(D, F, RPIH, U, r(D0, U0, BV0, IBV0), r(D, U1, BV1, IBV1), 0) :-
	U1 is U0 - U,
	BV1 is BV0 * U1 / U0,
	calcAI(F, D, D0, RPIH, AI),
	IBV1 is IBV0 * AI * U1 / U0.

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
	% We need  to be able to  roll up the purchase  structure to a
	% single report.
	% --------------------
	% rollupToReport/4
	% rollupToReport(+Fund, +RPIH, +PurchaseStructure, -Report)

rollupToReport(Fund, RPIH, PurchaseStructure, Report) :-
	PurchaseStructure =.. [_ | Tail],
	rollupToReport1(Fund, RPIH, Tail, Report).

rollupToReport1(Fund, RPIH, [nil(nil) | Rest], r(D, U, BV, IBV)) :-
	rollupToReport1(Fund, RPIH, Rest, r(D, U, BV, IBV)).
rollupToReport1(Fund, RPIH, [nil(D) | Rest], r(D0, U0, BV0, IBV1)) :-
	rollupToReport1(Fund, RPIH, Rest, r(D0, U0, BV0, IBV0)),
	calcAI(Fund, D, D0, RPIH, AI),
	IBV1 is IBV0 * AI.
rollupToReport1(Fund, RPIH, [p(D, U, V) | Rest], r(D, U1, BV1, IBV1)) :-
	rollupToReport1(Fund, RPIH, Rest, r(D0, U0, BV0, IBV0)),
	BV1 is BV0 + V,
	U1 is U0 + U,
	calcAI(Fund, D, D0, RPIH, AI),
	IBV1 is IBV0 * AI + V.
rollupToReport1(_, _, [r(D, U, BV, IBV)], r(D, U, BV, IBV)).

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %

moveToNextEventDate(RPIH, (D0, val((F,S), PurchaseStructure0, _, _, _)), (D1, val((F,S), PurchaseStructure1, 0, RPIH, []))) :-
	moveForward(RPIH, F, (D0, PurchaseStructure0), (D1, PurchaseStructure1)).

        %
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
