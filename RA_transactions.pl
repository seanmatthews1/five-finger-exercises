        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%
	% Here we can specify the specific transactions we have.
	%
	% A transaction has the following form:
	% 
	% doTransaction/4
	% doTransaction(+Fund, +E, +Val_prev, -Val)
	%
	%  where
	%  
	%         E == (EventDate, Event)
	% 
	% We can define the effect of an event on the current
	% portfolio as follows:
        % 
	% val(Units, BookValue, IndexedBookValue, LastOperativeEvent, RPIHistory, Events).
        % 
        %--------------------
	% deal(Fund, Series, Date, VisibleDate, Type, Amount, Units)

:- [purchaseStructure].

doTransaction(deal(_Fund, _, D, _, 'P', Amount, Units),
              val(F, PurchaseStructure0, S, H, E),
              val(F, PurchaseStructure1, S, H, E)) :-

	% write(purchase(D, Units, Amount)), write('\n'),
	purchase(D, Units, Amount, PurchaseStructure0, PurchaseStructure1).

doTransaction(deal(_Fund, _, D1, _, 'S', Amount, Units),
              val((F,S), PurchaseStructure0, S0, H, E),
              val((F,S), PurchaseStructure1, S1, H, E)) :-

	% write(sale(D1, Units, F, H)), write('\n'),
	sale(D1, Units, F, H, PurchaseStructure0, PurchaseStructure1),
	S1 is S0 + Amount.

        %--------------------
	% corporateDate((Fund, Series), EventType, Date).

doTransaction(corporateDate( _, 'Year end', _), V, V).
doTransaction(corporateDate(_, 'Deemed disposal', D), V0, V1) :-

        val((F, S), PurchaseStructure, _S, H, _E) = V0,
	rollupToReport(F, H, PurchaseStructure, r(_D, U0, _BV, _IBV)),

        getFundPrice((F,S), D, Price),
        Amount is Price * U0,
	doTransaction(deal(F, S, D, _, 'S', Amount, U0), V0, V_inter),
	doTransaction(deal(F, S, D, _, 'P', Amount, U0), V_inter, V1).

        %
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
