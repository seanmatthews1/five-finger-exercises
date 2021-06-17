% put in the visible RPI history at the beginning.

:- [transactions].

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %

loadCsvFiles :-

	% corporateDate(Date, Event)
	read_csv_file('OutCorporateDates.csv', [string, date], corporateDate),

	% externalPrice(Fund, Date, Price)
	read_csv_file('OutExternalPrices.csv', [string, date, number], externalPrice),

	% deal(Fund, Series, Date, VisibleDate, Type, Amount, Units)
	read_csv_file('OutDeals.csv', [string, string, date, date, string, number, number], deal),
	
	% rpi(date, VisibleDate, rpi)
	read_csv_file('OutRPI.csv', [date, date, number], rpi),

	% fundSeries(CitiFund, Fund, Series, Equity/Bond, ValidFrom)
	read_csv_file('OutFundSeries.csv', [string, string, number, string, date], fundSeries).

loadDavidsFilesSmall :-
	['TP4Small-deals', 'TP4Small-corporateDates', 'TP4Small-rpi',
	 'TP4Small-fundSeries', 'TP4Small-externalPrices'].

loadDavidsFilesFull :-
	['TP4Full-deals', 'TP4Full-corporateDates', 'TP4Full-rpi',
	 'TP4Full-fundSeries', 'TP4Full-externalPrices'].

loadSMfiles :-
	['sm_deals', 'sm_corporateDates', 'sm_rpi',
	 'sm_fundSeries', 'sm_externalPrices'].

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%
	% Here we present the core of the program.
	%
	% generateReport/8
	% generateReport(+FundSeries, +Dates, +Events, +FutureEventDates,
	%                +EventsVisible, +CurrentReported, +CumulativeReported,
	%                -Result)
	%
	% this implements  the basic  loop to generate  and accumulate
	% reports.  Note that we have a cut before the final tail call
	% to allow tail call optimisation.
	%
	% - FundSeries: a (Fund, Series) number pair
	% 
	%   Dates: a list of dates for  which we want to report - this
	%   stretches into the future.
	%
	% - Events: a  list of (timepoint, value)  pairs which contain
	%   the current  'core values' for a date  point.  Time points
	%   are  allocated  not  only  for operative  events  such  as
	%   purchases and  sales, but also  for when new  retail price
	%   index information becomes usable.  This stretches into the
	%   past (it is what we elsewhere define as a DateMap).
	%
	% - FutureEventDates: a list future event dates.
	%
	% - EventsVisible:  a  DateMap   telling  when  events  become
	%   visible.  A date maps onto a further datemap which in turn
	%   maps onto  sets of events  that occur on  particular days;
	%   i.e. for  a date D1,  we get back  a map from dates  D2 to
	%   sets of  events E  which happened on  D2, but  only became
	%   visible on D1.
	%
	% - CurrentReported: is the most up to date set of reports, on
	%   the basis of currently  vislble information.  We use it to
	%   decide when we need to produce a restatement.
	%
	% - CumulativeReports:  A cumulative  set  of actual  reported
	%   values generated  on particular days.  On  most days, this
	%   grows by  one entry, however on days  when new information
	%   becomes visible, it also be extended with restatements.
	%
	% ============================================================
	% 
	% The case analysis is as follows:
	%
	% If the current date for which  we want to report is after an
	% event date,

generateReport(FS, [D1 | R], Events, [D2 | RestFutureEventDates],
	       EventsVisible, Reported, Cumulative, Return) :-
	not(compare_Date(<, D1, D2)),

	% then  we first place  a new  (empty) event  corresponding to
	% that  date on  the  Events  list, with  a  RPH history  that
	% reflects what  can be seen  at that time, pop  the EventDate
	% List,

	findall((D, V), (rpi(D, VD, V), not(compare_Date(>, VD, D2))), L),
	dmAddMappingSet([], L, RPIH),
	Events = [(D0, Val0) | _],
	moveToNextEventDate(RPIH, (D0, Val0), (D2, Val1)),

	% and repeat (i.e. loop).

	!,
	generateReport(FS, [D1 | R], [(D2, Val1) | Events], RestFutureEventDates,
	               EventsVisible, Reported, Cumulative, Return).

	%--------------------
	% If  (after  having dealt  with  event  dates),  we see  that
	% transactions for some event date become visible today,

generateReport(FS, [D1 | R], Events, FutureEventDates, [(D2, T) | RestEventsVisible],
	Reported, Cumulative, Return) :-
	not(compare_Date(<, D1, D2)),

	% update the list of events with these new transactions

	updateValueSequenceTransactions(Events, T, ExtendedEvents),

	% update the known values for these events (we only update the
	% values that could have been affected by the updates),

	last(T, (DateEarliest, _)),
	updateValueSequence(ExtendedEvents, DateEarliest, UpdatedEvents),

	% calculate the new reports for the relevant dates

	dmGetFrom(Reported, DateEarliest, CurrentReported, _),

	% Identify which reports have changed;

	dmDom(CurrentReported, DomCRR),
	xGetDateValues(DomCRR, UpdatedEvents, getReport, NewReported),
	dmDiff('==', CurrentReported, NewReported, DifferenceReports),

	% Update these reports

	dmAddMappingSet(Reported, NewReported, UpdatedReports),

	% and restate them (append tuples to the Cumulative list)

	addNewCumulativeReports(D2, DifferenceReports, Cumulative, NewCumulative),

	% then loop

	!,
	generateReport(FS, [D1 | R], UpdatedEvents, FutureEventDates,
	               RestEventsVisible, UpdatedReports, NewCumulative, Return).

	%--------------------
	% If neither of the above  holds,

generateReport(FS, [D1 | R], [(D2, V) | RestEvents], FutureEventDates, EventsVisible,
	       Reported, Cumulative, Return) :-

	% then we simply calculate the
	% report  for the  first date  on the  date list,

	getReport(D1, V, NewReport),

	% and  place these results  on the  Current reported,  and the
	% Cumulative reported  lists, delete  the date from  the list,
	% and repeat.

	!,
	generateReport(FS, R, [(D2, V) | RestEvents], FutureEventDates, EventsVisible,
	              [(D1, NewReport)|Reported], [(D1, (D1, NewReport)) | Cumulative], Return).

        % Finally, of course, the loop will bottom out when it reaches
        % the last date for which we want to report, at which point we
        % are finished, so  we return the complete list  of reports we
        % have generated.

generateReport(_, [], _, _, _, _, Return, Return) :- !.

	%====================
	%We need the following secondary predicates here
	%
        % addNewCumulativeReports(D, DifferenceReports, Cumulative, NewCumulative)

addNewCumulativeReports(_, [], Cumulative, Cumulative).
addNewCumulativeReports(D, [H | R], Cumulative, [(D, H) | NewCumulative]) :-
        addNewCumulativeReports(D, R, Cumulative, NewCumulative).

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
	% It's  convenient   to  have   a  predicate  which   sets  up
	% generateReport actually to generate a report.

runGenerateReport(FS, Report) :-
	getAllEventDates(FS, Dates),
	getAllReportingDates(FS, ReportingDates),
	getAllEventsVisibility(FS, EV1),
	reverse(EV1, EV),
	getStartingPoint(FS, D0, RPIH),
	initVal(D0, FS, RPIH, V),
	!,
	generateReport(FS, ReportingDates, [(D0, V)], Dates, EV, [], [], Report).

        %--------------------
	% getStartingPoint/3
	% getStartingPoint(+FundSeries, -StartingDate, -RPIHistory)
	%
	% For a fund series, find  the first relevant date then return
	% the retail  price index  history visible _previous_  to this
	% date, together with the date of the newest timepoint in this
	% history.

getStartingPoint(FS, D_init, RPIH) :-
	getAllEventDates(FS, [D1 | _]),
	getAllReportingDates(FS, [D2 | _]),
	predsort(compare_Date, [D1, D2], [D0 | _]),
	findall((D, V), (rpi(D, DV, V), compare_Date(<, DV, D0)), RPIreports),
	predsort(compare_DMpoint, RPIreports, RPIH),
	[(D_init, _) | _] = RPIH.

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
	% Here we  have the basic  predicates we need to  generate the
	% inptuts to generateReport.
	%--------------------
	% getAllFunds/1
	% getAllFunds(-FundSet)
	% 
	% Generate a list of all  the Fund, Series pairs in the loaded
	% data. (You can backtrack into setof, hence the cut.)

getAllFunds(Funds) :-
	findall((F, S), deal(F, S, _, _, _, _, _), F1),
	sort(F1, Funds).

        %--------------------
	% getAllEventDates/2
	% getAllEvents(+Fund, -DM)
	%
	% Generate a  list of  all the dates  when events  take place.
	% Note that these  are _not_ the dates when  the events become
	% visible.  The list is ordered earliest to latest.
	% 
	% Return a list of all event dates, newest last.

getAllEventDates((F, S), EventDates) :-
	getDate_rng((F, S), First, Last),
	findall(D1, (corporateDate(_, D1), inDate_rng(First, D1, Last)), ED1, EDR1),
	findall(D2, deal(F, S, D2, _, _, _, _), EDR1, EDR2),
	findall(D3, (rpi(D3, VD3, _), inDate_rng(First, VD3, Last)), EDR2, []),
	predsort(compare_Date, ED1, EventDates).

        %--------------------
	% getAllEventsVisibility/2
	% getAllEventsVisibility(+Fund, -DM)
	%
	% Return a  map from event date visibility  to event occurance
	% date to  sets of events  (events include updates to  the RPI
	% history).

getAllEventsVisibility((F, S), EventsVisibility) :-
	getDate_rng((F, S), First, Last),
	findall((D1, corporateDate((F, S), E1, D1)),
	        (corporateDate(E1, D1), inDate_rng(First, D1, Last)),
		EV1, EVR1),
	findall((VD2, deal(F, S, D2, VD2, T2, U2, A2)),
	        deal(F, S, D2, VD2, T2, U2, A2),
		EVR1, EVR2),
	findall((VD3, rpi(D3, VD3, RPI)),
	        (rpi(D3, VD3, RPI), inDate_rng(First, VD3, Last)),
		EVR2, []),
	predsort(compare_DMpoint, EV1, EV2),
	xRelToMap(EV2, EV3),
	maplist(predGetAllEventsVisibility, EV3, EventsVisibility).

	% where

predGetAllEventsVisibility((D, TList), (D, TDM)) :-
	eventSetToDM(TList, TDM).

        %--------------------
	% eventSetToDM/2
	% eventSetToDM(+S, -DM)
	%
	% given set of events S, return DM: date -> event set.

eventSetToDM(S, DM) :-
	maplist(predEventSetToDM, S, DM1),
	predsort(compare_DMpoint, DM1, DM2),
	xRelToMap(DM2, DM).

predEventSetToDM(corporateDate(FS, E, D), (D, corporateDate(FS, E, D))).
predEventSetToDM(deal(Fund, Series, D, VD, T, U, A), (D, deal(Fund, Series, D, VD, T, U, A))).
predEventSetToDM(rpi(D, VD, RPI), (D, rpi(D, VD, RPI))).

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
	% We have  a basic  set of values  which we maintain  for each
	% event  timepoint, and  which are  enough to  calculate other
	% information we need.  We organise this data as a datemap.
	% 
	% for details  of the internal  structure of the  value tuple,
	% see other notes
	% --------------------
	% updateValuseSequenceTransactions/3
	% updateValuseSequenceTransactions(+ValueDM1, +TransactionDM, -ValueDM2)
	%
	% The value  tuple contains the  set of transactions  for that
	% day  that  are  currently  visible, and  the  RPI  currently
	% visible.  We need to update both of these.

updateValueSequenceTransactions(VS1, DM, VS2) :-
	xPartialMap(VS1, DM, predUpdateValueSequenceTransactions, VS2).

        % where
	% --------------------
	% Split the transaction set  into retail price index info, and
	% operational  events.   Stick  the  RPI  info  into  the  RPI
	% history,  and  the  operational  events  into  the  list  of
	% transactions (making sure to order this properly).

predUpdateValueSequenceTransactions(val(F, PurchaseStructure, S, RPIH0, Transactions0),
	                      NewTransactions,
			      val(F, PurchaseStructure, S, RPIH1, Transactions1)) :-
	partition(isRPI, NewTransactions, In, Out),
	maplist(rpiToPair, In, In_inter),
	dmAddMappingSet(RPIH0, In_inter, RPIH1),
	append(Out, Transactions0, Transactions_inter),
	predsort(compare_Event, Transactions_inter, Transactions1).

        % where

isRPI(rpi(_,_,_)).
rpiToPair(rpi(D,_,V), (D, V)).

        %--------------------
	% updateValueSequence/3
	% updateValueSequence(VS1, Date, VS2)
	% 
	% Having updated the RPI  and the transactions associated with
	% an event  date, we  have to update  the other values  in the
	% value tuple.  A  map won't work here, because  to update the
	% value for some point, we need to be able to see the previous
	% value.  Thus we use a custom combinator, which looks by back
	% in  Datemap to  the point  just before  the  earliest point,
	% Date, affected  by the newly visible  information, and works
	% forward from there, returning an updated sequence VS2.

updateValueSequence(VS1, Date, VS2) :-
	xUpdateFrom(VS1, Date, predUpdateValueSequence, VS2),

	(isDM(VS1) -> true ; error(updateValueSequence)).

predUpdateValueSequence((D0, V0), (D1, V1), V2) :-
	getInitialValueForFold((D0, V0), (D1, V1), V_inter),
	val(_,_,_,_, Transactions) = V_inter,
	rightFold(pred1UpdateValueSequence, V_inter, Transactions, V2).

% ASSERT TRANSACTIONS ORDERED

pred1UpdateValueSequence(VPrev, Transaction, VUpdated) :-
	doTransaction(Transaction, VPrev, VUpdated).

        %--------------------
	%
	% We  cannot simply  carry forward  the val  tuple  across day
	% boundaries (we  need to reset the  sale value to  0, and the
	% set of events changes).

getInitialValueForFold((D0, val((F,S), PurchaseStructure0, _, RPIH0, _)),
	               (D1, val(_,                      _ , _, RPIH1, T)), 
		            val((F,S), PurchaseStructure1, 0, RPIH2, T)) :-
	dmAddMappingSet(RPIH0, RPIH1, RPIH2),
	moveForward(RPIH2, F, (D0, PurchaseStructure0), (D1, PurchaseStructure1)).

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %

getFundPrice((F, _S), Date, Price) :-
%	fundSeries(CitiF, F, S, _, _),
	externalPrice(F, Date, Price).

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %

% ** TO BE FIXED **

getReport(D, val(FS, PurchaseStructure, S0, RPIH, _),
	     report(F, S, U, BV, IBV1, S1, LOE)) :-
	FS = (F, S),
	rollupToReport(F, RPIH, PurchaseStructure, r(LOE, U, BV, IBV0)),
	calcAI(F, D, LOE, RPIH, AI),
	IBV1 is IBV0 * AI,
	(LOE = D -> S1 = S0; S1 = 0).

calcAI(F, D, LOE, RPImap, AI) :-
	member((D1, DRPI),   RPImap), not(compare_Date(> ,D1, D)),
	member((D2, LOERPI), RPImap), not(compare_Date(> ,D2, LOE)),
	!,
	getAILimit(F, AILimit),
	AI is min(max(DRPI / LOERPI, 1), AILimit).

getAILimit(F, 100) :- fundSeries(_, F, _, 'Equity', _), !.
getAILimit(F,   0) :- fundSeries(_, F, _, 'Bond',   _), !.

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%
	% The first and last relevant dates for a Fund, etc.
        %--------------------
	%getDate_rng/3

getDate_rng(FS, First, Last) :-
	getFirstDeal(FS, First),
	getLastMarketPrice(FS, Last).
	
getFirstDeal((F, S), First) :-
  	findall(D, deal(F, S, D, _, _, _, _), L),
	predsort(compare_Date, L, [First | _]).

getLastMarketPrice((F, S), Last) :-
	findall(Date, getFundPrice((F, S), Date, _), L1),
	predsort(compare_Date, L1, L2),
	last(L2, Last).

        %--------------------
        % getAllReportingDates/2
	% getAllReportingDates(+Fund, -Dates)

getAllReportingDates((Fund, Series), Dates) :-
	getFirstDeal((Fund, Series), First),
	findall(D, (getFundPrice((Fund, Series), D, _), not(compare_Date(<, D, First))), Dates1),
	!,
	predsort(compare_Date, Dates1, Dates).


        %
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %

rightFold(_, S, [], S).
rightFold(P, S, [H|T], E) :-
	call(P, S, H, S1),
	rightFold(P, S1, T, E).

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
	% Date to value maps.
	% 
	% We work with Date to value maps, which we model as sequences
	% of date-value pairs, ordered descending on the date values.
	%--------------------

compare_DMpoint(R, (D1, E1), (D2, E2)) :-
	(D1 = D2 -> compare(R, E2, E1); compare_Date(R, D2, D1)).

	%--------------------
	% isDM/1

isDM([]).
isDM([(_/_/_, _)]).
isDM([P1, P2 | R]) :-
	compare_DMpoint(<, P1, P2),
	isDM([P2 | R]).

	%--------------------
	% dmDom/2
	% dmDom(+DM, -Dates)

dmDom(DM, Dates) :-
	maplist(dmDomPred, DM, Dates).

        % where

dmDomPred((D, _), D).

	%--------------------
	% dmGetFrom/4
        % dmGetFrom(+Date, +DM1, -DM2, -DM3)
	%
	% Split DM  at Date, so that  the Date point itself  is in DM2
	% and the tail is in DM3
        %
	% - DM2 union DM3 = DM1
	%
	% - dom DM2 = {d : dom DM1 | not d < Date};
	%
	% - dom DM3 = {d : dom DM1 | d < Date };

dmGetFrom([], _, [], []).
dmGetFrom([(D1, V) | R1], D2, [(D1, V) | R3], R4) :-
	not(compare_Date(<, D1, D2)),
	dmGetFrom(R1, D2, R3, R4).
dmGetFrom([(D1, V) | R1], D2, [], [(D1, V) | R1]) :-
	compare_Date(<, D1, D2).

        %--------------------
	% dmAddMap/3
	% dmAddMap(+DM1, +(D, V), -DM2)
	%
	% - dom DM2 = dom DM1 + {D}
	% - if d \= D then DM1(d) = DM2(d)
	% - if d = D then DM2(d) = V

dmAddMap([(D2, V2) | R2], (D1, V1), [(D2, V2) | R3]) :-
	compare_Date(>, D2, D1),
	dmAddMap(R2, (D1, V1), R3).
dmAddMap([(D, _) | R], (D, V), [(D, V) | R]).
dmAddMap([(D2, V2) | R], (D1, V1), [(D1, V1), (D2, V2) | R]) :-
	compare_Date(<, D2, D1).
dmAddMap([], M, [M]).

        %--------------------
	% dmAddMappingSet/3
	% dmAddMappingSet(+DM1, +DM2, -DM3)
	%
	% Update mapping set DM1 with DM2, and return in DM3.

dmAddMappingSet(DM1, DM2, DM3) :-
	rightFold(dmAddMap, DM1, DM2, DM3).

        %--------------------
	% dmApply/3
	% dmApply(+D, +DM, -V)
	%
	% V = DM(D)

dmApply(D, [(D, V)|_], V).
dmApply(D1, [(D2, _) | R], V) :-
        compare_Date(>, D1, D2),
	dmApply(D1, R, V).

        %--------------------
	% dmDiff/4
	% dmDiff(P, +DM1, +DM2, -DM3)
	% 
	% DM3 = {(d, DM2(d)) | d in dom DM2 - dom DM1 or not P(DM1(d), DM2(d))}
	%
	% ** Note asymmetry **

dmDiff(_, [], _, []) :-
	!.
dmDiff(P, [(D, V1) | R1], [(D, V2) | R2], Diff) :-
	call(P, V1, V2),
	!,
	dmDiff(P, R1, R2, Diff).
dmDiff(P, [(D, V1) | R1], [(D, V2) | R2], [(D, V2) | Diff]) :-
	not(call(P, V1, V2)),
	!,
	dmDiff(P, R1, R2, Diff).
dmDiff(P, [(D1, V1) | R1], [(D2, V2) | R2], [(D2, V2) | Diff]) :-
	compare_Date(<, D1, D2),
	!,
	dmDiff(P, [(D1, V1) | R1], R2, Diff).
dmDiff(P, [(D1, _) | R1], [(D2, V2) | R2], Diff) :-
	compare_Date(>, D1, D2),
	!,
	dmDiff(P, R1, [(D2, V2) | R2], Diff).
dmDiff(_, _, _, _) :-
	error('dmDiff failed').

        %--------------------
	% xPartialMap/4
	% xPartialMap(+DM1, +DM2, +P, -DM3)
	%
	% given P(+,+,-)
	%
	% - dom DM3 = dom DM1 union dom DM2;
	%
	% - for d:dom DM1 inter dom DM2, P(DM1(d), DM2(d), DM3(d));
	%
	% - for d:dom DM1 minus dom DM2, DM3(d) = DM1(d);
	% 
	% - for d:dom DM2 minus dom DM1, P(null, DM2(d), DM3(d)).

xPartialMap(L, [], _, L).
xPartialMap([], [(D, V2) | R2], P, [(D, X) | R3]) :-
	call(P, null, V2, X),
	xPartialMap([], R2, P, R3).
xPartialMap([(D, V1) | R1], [(D, V2) | R2], P, [(D, X) | R3]) :-
	call(P, V1, V2, X),
	xPartialMap(R1, R2, P, R3).
xPartialMap([(D1, V1) | R1], [(D2, V2) | R2], P, [(D1, V1) | R3]) :-
	compare_Date(>, D1, D2),
	xPartialMap(R1, [(D2, V2) | R2], P, R3).
xPartialMap([(D1, V1) | R1], [(D2, V2) | R2], P, [(D2, X) | R3]) :-
	compare_Date(<, D1, D2),
	call(P, null, V2, X),
	xPartialMap([(D1, V1) | R1], R2, P, R3).

        %--------------------
	% xGetDateValues/4
	% xGetDateValues(+Dates, +DM1, +P, -DM2).
	%
	% Dates is a set of dates modeled as a descending list.
	%
	% given  P(+,+,-)
	%
	% - dom DM2 = {d: Dates | not d < inf dom DM1}
	% 
	% - for d: dom DM2, P(d,  DM1(d1),  DM2(d))
	% 
	%   where
	% 
	%   d1 is sup {d2: dom DM1 | not d < d2}.

xGetDateValues([], _, _, []).
xGetDateValues([D1 | R1],  [(D2, V2) | R2], P, [(D1, V3) | R3]) :-
	compare_Date(>, D1, D2),
	!,
	call(P, D1, V2, V3),
        xGetDateValues(R1,  [(D2, V2) | R2], P, R3).
xGetDateValues([D1 | R1],  [(D1, V2) | R2], P, [(D1, V3) | R3]) :-
	!,
	call(P, D1, V2, V3),
        xGetDateValues(R1,  R2, P, R3).
xGetDateValues([D1 | R1],  [(_D2, _V2) | R2], P, R3) :-
	% compare_Date(<, D1, _D2)),
        xGetDateValues([D1 | R1],  R2, P, R3).

xGetDateValues([_|_], [], _, []) :- error(xgetDateValues1).

        %--------------------
	% xUpdateFrom/4
	% xUpdateFrom(+DM1, +Date, +P, -DM2)
	%
	% P  is a  predicate  where P(V1,  V2,  V3) where  V1  is a  A
	% 'previous' value, V2 is  the current 'current' value, and V3
	% is the updated 'current' value. predicate essentially runs P
	% along the date map DM1  from Date, so that the values mapped
	% to  Date  and all  later  time  points  are updated  (it  is
	% essentially an instantiation of leftReduce).

xUpdateFrom([(D1, V) | R], D2, _, [(D1, V) | R]) :-
	compare_Date(<, D1, D2).
xUpdateFrom([(D1, V1) | R1], D2, P, [(D1, V3) | R3]) :-
	not(compare_Date(<, D1, D2)),
        xUpdateFrom(R1, D2, P, R3),
	[Prev | _] = R3,
	call(P, Prev, (D1, V1), V3).

        %--------------------
	% xRelToMap/2
	% xRelToMap(+DRel, -DM)
	%
	% - dom DRel = dom DM
	%
	% - for d:dom DRel, DRel{d} = DM(d)
	%
	% Assumes that DRel is like a DateMap except that there may be
	% more than one copy of a date in the domain.

xRelToMap([], []).
xRelToMap([(D, V)], [(D, [V])]).
xRelToMap([(D, V1) | R1], [(D, [V1 | S]) | R2]) :-
	xRelToMap(R1, [(D, S) | R2]).
xRelToMap([(D1, V1) | R1], [(D1, [V1]), (D2, A) | R2]) :-
	xRelToMap(R1, [(D2, A) | R2]),
	D1 \= D2.
	
        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
        % Event ordering functions

compare_Event(R, E1, E2) :-
	event_to_Ordinal(E1, O1),
	event_to_Ordinal(E2, O2),
	compare(R, O1, O2).


        % We need a map from event  types onto ordinals so that we can
        % sort events inside a day:
	% 

event_to_Ordinal(rpi(_, _, _)                          ,    0).
event_to_Ordinal(deal(_, _, _, _, 'D', _, _)           ,    5).
event_to_Ordinal(deal(_, _, _, _, 'P', _, _)           ,   10).
event_to_Ordinal(deal(_, _, _, _, 'R', _, _)           ,   20).
%event_to_Ordinal(distribution(_, _)                   ,   30).
event_to_Ordinal(deal('S', _, _, _, _, _, _)           ,   40).
event_to_Ordinal(corporateDate(_, 'Deemed disposal', _),   60).
event_to_Ordinal(corporateDate(_, 'Year end', _)       ,   80).

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
	% Date ordering functions
	%
	%--------------------
	% compare_Date/3
	% compare_date(?R, +D1, +D2)
	%
	% Specialised compare predicate for date format values

compare_Date(R, D1, D2) :-
	dateToOrd(D1, O1),
	dateToOrd(D2, O2),
	compare(R, O1, O2).

        %--------------------
        % we need to  be able to identify when a date  is in a certain
        % range
        %
	% inDate_rng/3
	% inDate_rng(+DateLeft, +Date, +DateRight)

inDate_rng(D, D, _).
inDate_rng(_, D, D).
inDate_rng(DL, D, DR) :-
	compare_Date(<, DL, D),
	compare_Date(<, D, DR).

	%--------------------
	% Code to map dates onto ordinals: 1/1/1990 -> 1
	%
	% Defined for the years 1990 - 2033.
	% Verified against Excel: Ord = Date - 1/1/1990 + 1.
	%
	% dateToOrd/2.
	% dateToOrd(+Date, -Ord).
	%
	% WOULD BE NICE TO REWORK THIS SO THAT IT IS ALGORITHMIC, AND
	% MEMOIZE INSTEAD.

dateToOrd(D/M/Y, O) :-
	yearToOrd(Y, Ny),
	monthToOrd(M, Y, Nm),
	O is Ny + Nm + D.

yearToOrd(1990,0).     yearToOrd(1991,365).   yearToOrd(1992,730).   yearToOrd(1993,1096).
yearToOrd(1994,1461).  yearToOrd(1995,1826).  yearToOrd(1996,2191).  yearToOrd(1997,2557).
yearToOrd(1998,2922).  yearToOrd(1999,3287).  yearToOrd(2000,3652).  yearToOrd(2001,4018).
yearToOrd(2002,4383).  yearToOrd(2003,4748).  yearToOrd(2004,5113).  yearToOrd(2005,5479).
yearToOrd(2006,5844).  yearToOrd(2007,6209).  yearToOrd(2008,6574).  yearToOrd(2009,6940).
yearToOrd(2010,7305).  yearToOrd(2011,7670).  yearToOrd(2012,8035).  yearToOrd(2013,8401).
yearToOrd(2014,8766).  yearToOrd(2015,9131).  yearToOrd(2016,9496).  yearToOrd(2017,9862).
yearToOrd(2018,10227). yearToOrd(2019,10592). yearToOrd(2020,10957). yearToOrd(2021,11323).
yearToOrd(2022,11688). yearToOrd(2023,12053). yearToOrd(2024,12418). yearToOrd(2025,12784).
yearToOrd(2026,13149). yearToOrd(2027,13514). yearToOrd(2028,13879). yearToOrd(2029,14245).
yearToOrd(2030,14610). yearToOrd(2031,14975). yearToOrd(2032,15340). yearToOrd(2033,15706).

isLeapYear(1992). isLeapYear(1996). isLeapYear(2000). isLeapYear(2004). isLeapYear(2008).
isLeapYear(2012). isLeapYear(2016). isLeapYear(2020). isLeapYear(2024). isLeapYear(2028).
isLeapYear(2032).

monthToOrd( 1,_Y,  0).
monthToOrd( 2,_Y, 31).
monthToOrd( 3,Y,  59 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd( 4,Y,  90 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd( 5,Y, 120 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd( 6,Y, 151 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd( 7,Y, 181 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd( 8,Y, 212 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd( 9,Y, 243 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd(10,Y, 273 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd(11,Y, 304 + X) :- (isLeapYear(Y) -> X = 1; X = 0).
monthToOrd(12,Y, 334 + X) :- (isLeapYear(Y) -> X = 1; X = 0).

        %
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %
	% Read and assert data provided in simple CSV files.
	%
	% Form specifies  the type informatin  for the columns  of the
	% file and varies (currently) over
	%
	%   {date, other}
	%
	% We take advantage of the name predicate to deal with strings
	% and numbers indifferently as 'other'.
	%
	% Very  simple code  that makes  strong assumptions  about the
	% input - it is not general purpose.
	%  
	% - makes    use    of     the    SWI    specific    predicate
	%   read_line_to_codes,  which reads the  current line  on the
	%   input stream as a sequence of character codes.

	%--------------------
	% read_csv_file/3
	% read_csv_file(+Filename, +Form, +Name)
	% 

read_csv_file(Filename, Form, Name) :-
	open(Filename, read, S),
	read_csv_file1(S, Form, Name),
	close(S).

read_csv_file1(S, Form, Name) :-
	repeat,
	    read_line_to_codes(S, Code_seq),
            (    Code_seq  \= end_of_file,
	         getField_seq(Code_seq, FieldCode_seq_seq),
	         getFieldName_seq(FieldCode_seq_seq, Form, FieldName_seq),
	         Tup =.. [Name | FieldName_seq],
	         assert(Tup),
	         fail
            ;    Code_seq  == end_of_file),
	    !,
        Tup =.. [Name | Form],
	assert(Tup :- false).

        %
	% given a list of character codes corresponding to a line of a
	% csv file (currently separated  with comma=44), break up into
	% a sequence of subsequences corresponding to the fields.
	%
	% getField_seq/2
	% getField_seq(+Code_seq, -FieldCode_seq_seq)
	% 
	  

getField_seq([], []) :- !.
getField_seq(Code_seq, [FieldCode_seq | RFieldCode_seq_seq]) :-
	getField(Code_seq, FieldCode_seq, TailCode_seq),
	getField_seq(TailCode_seq, RFieldCode_seq_seq).
	
getField([], [], []).
getField([44 | RCode_seq], [], RCode_seq).
getField([A | RCode_seq], [A|T], Rest) :-
	A \= 44,
	getField(RCode_seq, T, Rest).

        %
	% Same as above,  only for dates - and it  returns a YMD list,
	% rather than a Code_seq.
	% 

getDate_seq([], []) :- !.
getDate_seq(Code_seq, [Field | RField_seq]) :-
	getDate(Code_seq, Field_Code_seq, TailCode_seq),
	name(Field, Field_Code_seq),
	getDate_seq(TailCode_seq, RField_seq).

getDate([], [], []).
getDate([47 | RCode_seq], [], RCode_seq).
getDate([A | RCode_seq], [A|T], Rest) :-
	A \= 47,
	getDate(RCode_seq, T, Rest).

        %
	% Given a sequence of  fields, convert it into names following
	% the typing information in the Type_seq parameter.
	%
	% getFieldName_seq/3
	% getFieldName_seq(+Char_seq_seq, Type_seq, Name_seq)
	% 

getFieldName_seq([], [], []).
getFieldName_seq([FieldCode_seq | RFieldCode_seq], [date | RType_seq], [D/M/Y | Name_seq ]) :-
	getDate_seq(FieldCode_seq, [D, M, Y]),
	getFieldName_seq(RFieldCode_seq, RType_seq, Name_seq).
getFieldName_seq([FieldCode_seq | RFieldCode_seq], [Type | RType_seq], [Name  | Name_seq ]) :-
	Type \= date,
	name(Name, FieldCode_seq),
	getFieldName_seq(RFieldCode_seq, RType_seq, Name_seq).

        %
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%
	% Some simple error reporting code.

error(Msg) :-
	write('************************************************************\n* '),
	write('ERROR: '),
	write(Msg),
	write('\n************************************************************\n'),
	fail.

	%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
