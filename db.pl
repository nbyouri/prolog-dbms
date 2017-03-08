%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					   %
% Facts: Client, Commande, Detail, Produit %
%					   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% client table
:- dynamic(client_ncli/2).
:- dynamic(client_nom/2).
:- dynamic(client_adresse/2).
:- dynamic(client_localite/2).
:- dynamic(client_cat/2).
:- dynamic(client_compte/2).

client_ncli("B062","B062").
client_ncli("B112","B112").
client_ncli("B332","B332").
client_ncli("B512","B512").
client_ncli("C003","C003").
client_ncli("C123","C123").
client_ncli("C400","C400").
client_ncli("D063","D063").
client_ncli("F010","F010").
client_ncli("F400","F400").
client_ncli("K111","K111").
client_ncli("L422","L422").
client_ncli("S127","S127").
client_ncli("S712","S712").
client_ncli("F011","F011").
client_ncli("K729","K729").

client_nom("B062","Goffin").
client_nom("B112","Hansenne").
client_nom("B332","Monti").
client_nom("B512","Gillet").
client_nom("C003","Avron").
client_nom("C123","MERCIER").
client_nom("C400","Ferard").
client_nom("D063","Mercier").
client_nom("F010","Toussaint").
client_nom("F400","Jacob").
client_nom("K111","Vanbist").
client_nom("L422","Franck").
client_nom("S127","Vanderka").
client_nom("S712","Guillaume").
client_nom("F011","PONCELET").
client_nom("K729","NEUMAN").

client_adresse("B062", "72, rue de la Gare").
client_adresse("B112", "23, rue Dumont").
client_adresse("B332", "112, rue Neuve").
client_adresse("B512", "14, rue de l'Eté").
client_adresse("C003", "8, rue de la Cure").
client_adresse("C123", "25, rue Lemaitre").
client_adresse("C400", "65, rue du Tertre").
client_adresse("D063", "201, boulevard du Nord").
client_adresse("F010", "5, rue Godefroid").
client_adresse("F400", "78, chemin du Moulin").
client_adresse("K111", "180, rue Florimont").
client_adresse("L422", "60, rue de Wépion").
client_adresse("S127", "3, avenue des Roses").
client_adresse("S712", "14a, chemin des Roses").
client_adresse("F011", "17, Clos des Erables").
client_adresse("K729", "40, rue Bransart").

client_localite("B062","Namur").
client_localite("B112","Poitiers").
client_localite("B332","Genève").
client_localite("B512","Toulouse").
client_localite("C003","Toulouse").
client_localite("C123","Namur").
client_localite("C400","Poitiers").
client_localite("D063","Toulouse").
client_localite("F010","Poitiers").
client_localite("F400","Bruxelles").
client_localite("K111","Lille").
client_localite("L422","Namur").
client_localite("S127","Namur").
client_localite("S712","Paris").
client_localite("F011","Toulouse").
client_localite("K729","Toulouse").

client_cat("B062","B2").
client_cat("B112","C1").
client_cat("B332","B2").
client_cat("B512","B1").
client_cat("C003","B1").
client_cat("C123","C1").
client_cat("C400","B2").
client_cat("D063","B2").
client_cat("F010","C1").
client_cat("F400","C2").
client_cat("K111","B1").
client_cat("L422","C1").
client_cat("S127","C1").
client_cat("S712","B1").
client_cat("F011","B2").
client_cat("K729","B2").

client_compte("B062","-3200").
client_compte("B112","1250").
client_compte("B332","0").
client_compte("B512","-8700").
client_compte("C003","-1700").
client_compte("C123","-2300").
client_compte("C400","350").
client_compte("D063","-2250").
client_compte("F010","0").
client_compte("F400","0").
client_compte("K111","720").
client_compte("L422","0").
client_compte("S127","-4580").
client_compte("S712","0").
client_compte("F011","0").
client_compte("K729","0").

% Commande table
:- dynamic(commande_ncom_co/2).
:- dynamic(commande_ncli_co/2).
:- dynamic(commande_date/2).

commande_ncom_co("30178","30178").
commande_ncom_co("30179","30179").
commande_ncom_co("30182","30182").
commande_ncom_co("30184","30184").
commande_ncom_co("30185","30185").
commande_ncom_co("30186","30186").
commande_ncom_co("30188","30188").

commande_ncli_co("30178","K111").
commande_ncli_co("30179","C400").
commande_ncli_co("30182","S127").
commande_ncli_co("30184","C400").
commande_ncli_co("30185","F011").
commande_ncli_co("30186","C400").
commande_ncli_co("30188","B512").

commande_date("30178","2008-12-22").
commande_date("30179","2008-12-22").
commande_date("30182","2008-12-23").
commande_date("30184","2008-12-23").
commande_date("30185","2009-01-02").
commande_date("30186","2009-01-02").
commande_date("30188","2009-01-02").

% Detail table
:- dynamic(detail_ncom_de/2).
:- dynamic(detail_npro_de/2).
:- dynamic(detail_qcom/2).

detail_ncom_de("30178","30178").
detail_ncom_de("30179","30179").
detail_ncom_de("30179","30179").
detail_ncom_de("30182","30182").
detail_ncom_de("30184","30184").
detail_ncom_de("30185","30185").
detail_ncom_de("30185","30185").
detail_ncom_de("30185","30185").
detail_ncom_de("30186","30186").
detail_ncom_de("30188","30188").
detail_ncom_de("30188","30188").
detail_ncom_de("30188","30188").
detail_ncom_de("30184","30184").
detail_ncom_de("30188","30188").

detail_npro_de("30178","CS464").
detail_npro_de("30179","CS262").
detail_npro_de("30179","PA60").
detail_npro_de("30182","PA60").
detail_npro_de("30184","CS464").
detail_npro_de("30185","CS464").
detail_npro_de("30185","PA60").
detail_npro_de("30185","PS222").
detail_npro_de("30186","PA45").
detail_npro_de("30188","CS464").
detail_npro_de("30188","PA60").
detail_npro_de("30188","PH222").
detail_npro_de("30184","PA45").
detail_npro_de("30188","PA45").

detail_qcom("30178","25").
detail_qcom("30179","60").
detail_qcom("30179","20").
detail_qcom("30182","30").
detail_qcom("30184","120").
detail_qcom("30185","260").
detail_qcom("30185","15").
detail_qcom("30185","600").
detail_qcom("30186","3").
detail_qcom("30188","180").
detail_qcom("30188","70").
detail_qcom("30188","92").
detail_qcom("30184","20").
detail_qcom("30188","22").

% Produit table
:- dynamic(produit_npro/2).
:- dynamic(produit_libelle/2).
:- dynamic(produit_prix/2).
:- dynamic(produit_qstock/2).

produit_npro("CS262","CS262").
produit_npro("CS264","CS264").
produit_npro("CS464","CS464").
produit_npro("PA60","PA60").
produit_npro("PS222","PS222").
produit_npro("PA45","PA45").
produit_npro("PH222","PH222").

produit_libelle("CS262","Chev. Sapin 200*6*2").
produit_libelle("CS264","Chev. Sapin 200*6*4").
produit_libelle("CS464","Chev. Sapin 400*6*4").
produit_libelle("PA60","Pointe Acier 60 (10K)").
produit_libelle("PS222","PL. Sapin 200*20*2").
produit_libelle("PA45","POINTE ACIER 45 (20K)").
produit_libelle("PH222","PL. HETRE 200x20x2").

produit_prix("CS262","75").
produit_prix("CS264","120").
produit_prix("CS464","220").
produit_prix("PA60","95").
produit_prix("PS222","185").
produit_prix("PA45","105").
produit_prix("PH222","185").

produit_qstock("CS262","45").
produit_qstock("CS264","2690").
produit_qstock("CS464","450").
produit_qstock("PA60","134").
produit_qstock("PS222","1220").
produit_qstock("PA45","580").
produit_qstock("PH222","1220").

% Table columnns
:- dynamic(client/1).
:- dynamic(commande/1).
:- dynamic(detail/1).
:- dynamic(produit/1).

client([ncli, nom, adresse, localite, cat, compte]).
commande([ncom_co, ncli_co, date]).
detail([ncom_de, npro_de, qcom]).
produit([npro, libelle, prix, qstock]).

% Table index
:- dynamic(table_index/1).

table_index([client, commande, detail, produit]).

%%%
% Rules
%%%
% Select columns from a table
select(X,C) :-
        table_index(L),member(X,L)
        -> select(X,C,_),!
        ; write("No such table exists."),nl.

select(_,[],TL,L) :- L = TL.
select(X,[H|T],TL,L) :- 
	column_as_list(X,H,CL),
	append(TL,[CL],TL1),
	select(X,T,TL1,L).

select(X,C,_) :-
	validate_columns(X,C,[],C1),
	select(X,C1,[],L), combine_lists(L,R), print_table(C1,R), !.

is_column_in_table(Table,Column) :- 
        get_table_columns(Table,L),member(Column, L), !.

column_as_list(Table,Column,R) :-
        column_get_name(Table,Column,X),
        G =.. [X,_,L], findall(L,G,R).

column_as_list(Table,Column,ID,R) :-
        column_get_name(Table,Column,X),
        G =.. [X,ID,L], findall(L,G,R).

%% Create columns
create_column(Table,Column) :-
        get_table_columns(Table,L),
        append(L,[Column],NL),
        rule_replace_list(Table,NL).

create_columns(_,[]).
create_columns(Table,[H|T]) :-
        create_column(Table,H),
        column_get_name(Table,H,X),
        add_rule_two_args(X),
        create_columns(Table, T).

%% Create table
register_table_in_index(Table) :-
        table_index(L), append(L, [Table], NL),
        rule_replace_list(table_index, NL).

create_table(Table, Columns) :- add_rule_empty_list(Table),
        register_table_in_index(Table),
        create_columns(Table,Columns),!.

%% Drop a column
drop_column(Table,X) :-
        column_get_name(Table,X,Column),
	is_column_in_table(Table, X) ->
        get_table_columns(Table,L), delete(L,X,NL),
        rule_replace_list(Table,NL),
	rm_rule_two_args(Column)
        ; write("No such column in table "), write(Table), nl.

drop_columns(_,[]).
drop_columns(Table, [H|T]) :- drop_column(Table, H),
        drop_columns(Table, T).
        
%% Drop a table
delete_table_from_index(Table) :-
        table_index(L), delete(L,Table,NL),
        rule_replace_list(table_index, NL).

drop_table(Table) :- get_table_columns(Table, L),
        drop_columns(Table, L),
        rm_rule_one_arg(Table),
        delete_table_from_index(Table),!.

drop_tables([]).
drop_tables([H|T]) :- drop_table(H), drop_tables(T).

%% Insert values into a table
insert_row(_,_,_,[]).
insert_row(Table,ID,[H|T],[H2|T2]) :-
	atom_string(H,S),
	atom_string(H2,S2),
        column_set_value(Table,S,ID,S2),
        insert_row(Table,ID,T,T2).

insert_rows(_,[]).
insert_rows(Table,[H|T]) :- insert(Table,H), insert_rows(Table,T).

insert(Table,[ID|Values]) :- get_table_columns(Table,Columns),
        length(Columns,L1), length([ID|Values],L2),
        L1 = L2 -> insert_row(Table,ID,Columns,[ID|Values]),!
        ; write("Arguments do not match the table "), write(Table), nl.

%% Check if a string contains only a number
is_number([],_).
is_number([H|T],_) :- char_type(H,digit), is_number(T,_).

is_number(S) :- atom_chars(S,X), length(X,L), L>0, is_number(X,_).

%% Remove duplicates from list
remove_duplicates([],[],_).
remove_duplicates([H|T],[H|Out],Seen) :-
	not(member(H,Seen)), remove_duplicates(T,Out, [H|Seen]).
remove_duplicates([H|T],Out,Seen) :-
	member(H,Seen), remove_duplicates(T,Out,Seen).
remove_duplicates(L1,L2) :- remove_duplicates(L1, L2, []).

%% Remove sub-lists from a lsit
remove_list([],_,[]).
remove_list([H|T],L,R) :-
	member(H, L), !, remove_list(T, L, R). 
remove_list([H|T],L,[H|R]) :-
	remove_list(T,L,R).

%% Filter a list based on a predicate
filter(Table,Column,Op,Val,L) :-
	is_number(Val) ->
	column_as_list(Table,Column,ICN),
	G =.. [Op,Y,Val],
	findall(Y,(member(X,ICN),atom_number(X,Y), G),L1),
	remove_duplicates(L1,L)
	; column_as_list(Table,Column,ICN),
 	G =.. [Op,X,Val],
	findall(X,(member(X,ICN), G),L1),
	remove_duplicates(L1,L).

%% Get column ID matching values
where_id(_,_,[],CL,L) :- L = CL.
where_id(Table,Column,[H|T],CL,L) :-
	column_get_name(Table,Column,X),
	atom_string(H,S),
	G =.. [X,Y,S],
	findall(Y,G,CL1),
	append(CL,CL1,CL2),
	where_id(Table,Column,T,CL2,L).
where_id(T,C,I,L) :- where_id(T,C,I,[],L).

%% Select in table based on ID
select_columns_id(_,[],_,CL,L) :- L = CL.
select_columns_id(Table,[H|T],ID,CL,L) :-
	column_as_list(Table,H,ID,CL1),
	append(CL,CL1,CL2),
	select_columns_id(Table,T,ID,CL2,L).

select_id(_,_,[],CL,L) :- L = CL.
select_id(Table,Columns,[H|T],CL,L) :-
	select_columns_id(Table,Columns,H,[],CL1),
	append(CL,[CL1],CL2),
	select_id(Table,Columns,T,CL2,L).

select_id(T,C,I,L) :- select_id(T,C,I,[],L),!.

%% Select rows based on a predicate
select_where(Table,Columns,WhereColumn,Op,Val) :-
	validate_columns(Table,[WhereColumn],[],[WC|_]),
	validate_columns(Table,Columns,[],C),
	filter(Table,WC,Op,Val,L),
	where_id(Table,WC,L,L2),
	select_id(Table,C,L2,L3),
	print_table(C,L3),!.

%% Inverse of select
select_where_not(Table,Columns,WhereColumn,Op,Val) :-
	validate_columns(Table,[WhereColumn],[],[WC|_]),
	validate_columns(Table,Columns,[],C),
	select(Table,C,[],List),
	combine_lists(List,List2),

	filter(Table,WC,Op,Val,L),
	where_id(Table,WC,L,L2),
	select_id(Table,C,L2,L3),

	remove_list(List2,L3,L4),
	print_table(C,L4),!.

%% Delete row from table
delete_column_row(_,[],_).
delete_column_row(Table,[H|T],ID) :-
	column_get_name(Table,H,C),
	atom_string(ID,SID),
	G =.. [C,SID,_],
	retract(G),
	delete_column_row(Table,T,ID).
	
delete_id(_,[],_).
delete_id(Table,[H|T],C) :-
	delete_column_row(Table,C,H),
	delete_id(Table,T,C).

%% Delete rows based a predicate
delete_where(Table,Column,Op,Val) :-
	validate_columns(Table,[Column],[],[WC|_]),
	validate_columns(Table,*,[],C),
	filter(Table,WC,Op,Val,L),
	where_id(Table,WC,L,L2),
	delete_id(Table,L2,C),!.

%% Update row from table
update_row(_,__,[],[]).
update_row(Table,ID,[H|T],[H2|T2]) :-
	atom_string(H2,S2),
	atom_string(ID,SI),
	column_get_name(Table,H,C),
	G =.. [C,SI,_],
	retract(G),
        column_set_value(Table,H,SI,S2),
        update_row(Table,ID,T,T2).

update_rows(_,[],[],_,_).
update_rows(Table,[OH|OT],[H|T],Cols,Vals) :-
	insert(Table,OH),
	update_row(Table,H,Cols,Vals),
	update_rows(Table,OT,T,Cols,Vals).
	
%% Update rows based on a predicate
update_where(Table,UpdateColumns,NewValues,WhereColumn,Op,Val) :-
	% Validate
	validate_columns(Table,UpdateColumns,[],UC),
	validate_columns(Table,[WhereColumn],[],[WC|_]),
	validate_columns(Table,*,[],AC),
	list_symmetric([UpdateColumns,NewValues],_),

	% Get matching rows
	filter(Table,WC,Op,Val,L),
	where_id(Table,WC,L,L1),

	% First remove and insert back rows
	select_id(Table,AC,L1,Orig),
	delete_where(Table,WhereColumn,Op,Val),

	% Update the appropriate columns
	update_rows(Table,Orig,L1,UC,NewValues),!.

%% Cartesian product of two tables
pairs(_,[],[]).
pairs(H,[H2|T2],[[H,H2]|L]) :- pairs(H,T2,L).

product([],_,[]).
product([H|T],T2,L) :- pairs(H,T2,X), product(T,T2,Y), append(X,Y,L).

%% Cross join of two tables
cross_join(Table1,Table2) :-
	% get table columns
	validate_columns(Table1,*,[],PC),
	validate_columns(Table2,*,[],CC),

	% select columns into lists
	select(Table1,PC,[],PL),
	select(Table2,CC,[],CL),

	% combine lists into tables
	combine_lists(PL,CPL),
	combine_lists(CL,CCL),

	% cartesian product of the two tables
	product(CPL,CCL,RL),
	print_table([PC,CC],RL),!.
	
%% Get the index of an item in a list
indexOf([E|_],E,0) :- !.
indexOf([_|T],E,I) :-
	indexOf(T,E,I1),
	!,
	I is I1+1.

%% Check whether list1 at position i matches list2 at position j
matches(L1,L2,I,J) :-
	nth0(I,L1,V1),
	nth0(J,L2,V2),
	V1 = V2.

%% Find matching columns in a list of two lists
list_matches([H|[T|_]],I,J) :-
	matches(H,T,I,J).

inner_join(Table1,Table2,Col1,Col2) :-
	% get table columns
	validate_columns(Table1,*,[],PC),
	validate_columns(Table2,*,[],CC),
	validate_columns(Table1,[Col1],[],[ICol1|_]),
	validate_columns(Table2,[Col2],[],[ICol2|_]),

	% select columns into lists
	select(Table1,PC,[],PL),
	select(Table2,CC,[],CL),

	% combine lists into tables
	combine_lists(PL,CPL),
	combine_lists(CL,CCL),
	
	% Cartesian product of the two tables
	product(CPL,CCL,RL),
	
	% Get index of column in the two tables
	indexOf(PC,ICol1,I1),
	indexOf(CC,ICol2,I2),

	% Get matching rows and print them
	findall(X,(member(X,RL),list_matches(X,I1,I2)),L),
	print_table([PC,CC],L),!.

%% Verify all lists have the same size
list_symmetric([],_).
list_symmetric([H|T],LE) :- length(H,LE1), LE1=LE
	-> list_symmetric(T,LE1)
        ; false.

%% Get nth element of each list in list of lists
lists_nth([],_,CL,R) :- R=CL.
lists_nth([H|T],I,CL,R) :- nth0(I,H,X), append(CL,[X],CL1),
	lists_nth(T,I,CL1,R).

%% Combine a list of n lists of n elements
combine_lists(L,I,LE,CR,R) :- I=LE -> R = CR
        ; lists_nth(L,I,[],L1), append(CR,[L1],CR1),
        I1 is I+1, combine_lists(L,I1,LE,CR1,R).

combine_lists([H|T],R) :- length(H,LE), list_symmetric([H|T],LE) -> 
        combine_lists([H|T],0,LE,[],R)
        ; write("Can't combine assymetric lists"), nl,fail.

%% Format list contents as a pretty row
print_list([],_).
print_list([H|T],LN) :-
	LN2 is LN + 1,
	format("~d~t~3||",LN2),
	write(H),nl,
	print_list(T,LN2).

print_list(L) :- print_list(L,0).

print_header([]) :- nl.
print_header([H|T]) :- format("~a|",H), print_header(T).

print_table(C,L) :- format("~t~3||"),
        flatten(C,FC), print_header(FC), print_list(L,0).

%% REPL
db_repl :-
        repeat,
        write('> '),
        read(X),
        call(X),
        fail.

%% Add predicates
add_rule_empty_list(Rule) :- dynamic(Rule/1), G =.. [Rule,[]],call(assertz(G)).

add_rule_two_args(Rule) :- dynamic(Rule/2), G =.. [Rule],call(assertz(G)).

rule_replace_list(Rule,List) :- rm_rule_one_arg(Rule),
        dynamic(Rule/1), G =.. [Rule,List], call(assertz(G)).

%% Remove predicates
rm_rule_no_args(Rule) :- G =.. [Rule],call(retractall(G)).

rm_rule_one_arg(Rule) :- rm_rule_no_args(Rule),
        G =.. [Rule,_],call(retractall(G)).

rm_rule_two_args(Rule) :- rm_rule_no_args(Rule),
        G =.. [Rule,_,_],call(retractall(G)).

%% Prepend a string to each list items
list_prepend_items([],_,IL,R) :- R = IL.
list_prepend_items([H|T],Prefix,IL,R) :- 
        string_concat(Prefix,"_",TmpStr1),
        string_concat(TmpStr1,H,TmpStr2),
        atom_string(Atom,TmpStr2),
        append(IL,[Atom],IL2),
        list_prepend_items(T,Prefix,IL2,R).

list_prepend_items(L,P,R) :- list_prepend_items(L,P,[],R).


%% Get a table's columns and an internal column name, allowing namespaces.
get_table_columns(Table,Columns) :- G =.. [Table, Columns], call(G).

column_get_name(Table,X,Column) :- 
        string_concat(Table,"_",TmpStr1),
        string_concat(TmpStr1,X,TmpStr2),
        atom_string(Column,TmpStr2).

column_set_value(Table,Column,TableID,Value) :-
        column_get_name(Table,Column,X),
        G =.. [X,TableID,Value],call(assertz(G)).

%% Validate columns
%$ * -> all columns
validate_columns(Table,A,_,Columns) :-
	A = '*', get_table_columns(Table,Columns).

validate_columns(_,[],C,Columns) :- Columns = C.
validate_columns(Table,[H|T],C,Columns) :- is_column_in_table(Table,H)
	-> append(C,[H],C1),
	validate_columns(Table,T,C1,Columns)
	; write("No such column "),write(H),
	write(" in table "),write(Table),nl,fail.

%%% Cleanup up the memory space and reload the file
%% XXX literal
cleanup :- table_index(L), drop_tables(L),
        rm_rule_one_arg(table_index), consult("db.pl").
