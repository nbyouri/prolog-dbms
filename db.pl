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
select(_,_).
select(X,C) :-
        table_index(L),member(X,L)
        -> select_p(X,C)
        ; write("No such table exists."),nl.

select_pp(_,[],TL,L) :- L = TL.
select_pp(X,[H|T],TL,L) :- 
	is_column_in_table(X,H)
	-> column_as_list(X,H,CL),
	append(TL,[CL],TL1),
	select_pp(X,T,TL1,L)
        ; write("No such column in table "), write(X).

select_p(X,C) :-
	select_pp(X,C,[],L), combine_lists(L,R), print_list(R).

select_all(TableName) :- get_table_columns(TableName,L),
        select(TableName,L).

is_column_in_table(TableName,ColumnName) :- 
        get_table_columns(TableName,L),member(ColumnName, L), !.

column_as_list(TableName,ColumnName,R) :-
        column_get_name(TableName,ColumnName,X),
        G =.. [X,_,L], findall(L,G,R).

column_as_list(TableName,ColumnName,ID,R) :-
        column_get_name(TableName,ColumnName,X),
        G =.. [X,ID,L], findall(L,G,R).

%% Create columns
create_column(TableName,ColumnName) :-
        get_table_columns(TableName,L),
        append(L,[ColumnName],NL),
        rule_replace_list(TableName,NL).

create_columns(_,[]).
create_columns(TableName,[H|T]) :-
        create_column(TableName,H),
        column_get_name(TableName,H,X),
        add_rule_two_args(X),
        create_columns(TableName, T).

%% Create table
register_table_in_index(TableName) :-
        table_index(L), append(L, [TableName], NL),
        rule_replace_list(table_index, NL).

create_table(TableName, Columns) :- add_rule_empty_list(TableName),
        register_table_in_index(TableName),
        create_columns(TableName,Columns).

%% Drop a column
drop_column(TableName,X) :-
        column_get_name(TableName,X,ColumnName),
	is_column_in_table(TableName, X) ->
        get_table_columns(TableName,L), delete(L,X,NL),
        rule_replace_list(TableName,NL),
	rm_rule_two_args(ColumnName)
        ; write("No such column in table "), write(TableName), nl.

drop_columns(_,[]).
drop_columns(TableName, [H|T]) :- drop_column(TableName, H),
        drop_columns(TableName, T).
        
%% Drop a table
delete_table_from_index(TableName) :-
        table_index(L), delete(L,TableName,NL),
        rule_replace_list(table_index, NL).

drop_table(TableName) :- get_table_columns(TableName, L),
        drop_columns(TableName, L),
        rm_rule_one_arg(TableName),
        delete_table_from_index(TableName).

drop_tables([]).
drop_tables([H|T]) :- drop_table(H), drop_tables(T).

%% Insert values into a table
insert_row(_,_,_,[]).
insert_row(TableName,ID,[H|T],[H2|T2]) :-
        column_set_value(TableName,H,ID,H2),
        insert_row(TableName,ID,T,T2).

insert(TableName,[ID|Values]) :- get_table_columns(TableName,ColumnNames),
        length(ColumnNames,L1), length([ID|Values],L2),
        L1 = L2 -> insert_row(TableName,ID,ColumnNames,[ID|Values])
        ; write("Arguments do not match the table "), write(TableName), nl.

%% Where, list filtering for select,update and delete
%include,exclude,maplist

%% Check if a string contains only a number
is_number_p([]).
is_number_p([H|T]) :- char_type(H,digit), is_number_p(T).

is_number(S) :- atom_chars(S,X), length(X,L), L>0, is_number_p(X).

%% Filter a list based on a predicate
%% XXX return ID
filter(TableName,ColumnName,Op,Val,L) :-
	is_number(Val) ->
	column_as_list(TableName,ColumnName,ICN),
	G =.. [Op,Y,Val],
	findall(Y,(member(X,ICN),atom_number(X,Y), G),L)
	; column_as_list(TableName,ColumnName,ICN),
 	G =.. [Op,X,Val],
	findall(X,(member(X,ICN), G),L).

%% Get column ID matching value
%% XXX add filter capabilities
where_id(TableName,ColumnName,Val,L) :-
	column_get_name(TableName,ColumnName,X),
	G =.. [X,Y,Val],
	findall(Y,G,L).

%% XXX: MOVE
%% Select in table based on ID
select_columns_id(_,[],_,CL,L) :- L = CL.
select_columns_id(TableName,[H|T],ID,CL,L) :-
	column_as_list(TableName,H,ID,CL1),
	append(CL,CL1,CL2),
	select_columns_id(TableName,T,ID,CL2,L).

select_id(_,_,[],CL,L) :- L = CL.
select_id(TableName,ColumnNames,[H|T],CL,L) :-
	select_columns_id(TableName,ColumnNames,H,[],CL1),
	append(CL,[CL1],CL2),
	select_id(TableName,ColumnNames,T,CL2,L).

select_id(T,C,I,L) :- select_id(T,C,I,[],L),!.

select_where(TableName,ColumnNames,
%%% XXX MOVE ^
	
	
%% Verify all lists have the same size
list_symmetric([],_).
list_symmetric([H|T],LE) :- length(H,LE1), LE1=LE -> list_symmetric(T,LE1)
        ; false.

%% Get nth element of each list in list of lists
lists_nth([],_,CL,R) :- R=CL.
lists_nth([H|T],I,CL,R) :- nth0(I,H,X), append(CL,[X],CL1),
	lists_nth(T,I,CL1,R).

%% Combine a list of n lists of n elements
combine_lists_p(L,I,LE,CR,R) :- I=LE -> R = CR
        ; lists_nth(L,I,[],L1), append(CR,[L1],CR1),
        I1 is I+1, combine_lists_p(L,I1,LE,CR1,R).

combine_lists([H|T],R) :- length(H,LE), list_symmetric([H|T],LE) -> 
        combine_lists_p([H|T],0,LE,[],R)
        ; write("Can't combine assymetric lists"), nl.

%% Format list contents as a pretty row
%build_list_format(N,C,T,F) :- N = 0 -> F = T
%	; C1 is C+5, N1 is N-1, 
%	string_concat("~s~t~",C1,Str1),
%	string_concat(Str1,"+",Str2),
%	string_concat(T,Str2,T1), build_list_format(N1,C1,T1,F).

print_list([H|T]) :- %length(H,LE),
	%%% couldn't properly format() %%%
	%build_list_format(LE,0,'',F),
	%string_concat(F,"~n",F1),
	%forall(member(L,[H|T]),format(F1,L)).
	forall(member(L,[H|T]),(write(L),nl)).

%% Pretty print results as a table
p_print_table([],_).
p_print_table([H|T],N) :- write(N), write(" -> "), write(H), nl,
        N1 is N+1, p_print_table(T, N1).
print_table(X) :- length(X,L),
        L > 0 -> p_print_table(X, 0); write("Empty set.").


%% Utils
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
p_list_prepend_items([],_,IL,R) :- R = IL.
p_list_prepend_items([H|T],Prefix,IL,R) :- 
        string_concat(Prefix,"_",TmpStr1),
        string_concat(TmpStr1,H,TmpStr2),
        atom_string(Atom,TmpStr2),
        append(IL,[Atom],IL2),
        p_list_prepend_items(T,Prefix,IL2,R).

list_prepend_items(L,P,R) :- p_list_prepend_items(L,P,[],R).


%% Get a table's columns and an internal column name, allowing namespaces.
get_table_columns(TableName,Columns) :- G =.. [TableName, Columns], call(G).

column_get_name(TableName,X,ColumnName) :- 
        string_concat(TableName,"_",TmpStr1),
        string_concat(TmpStr1,X,TmpStr2),
        atom_string(ColumnName,TmpStr2).

column_set_value(TableName,ColumnName,TableID,Value) :-
        column_get_name(TableName,ColumnName,X),
        G =.. [X,TableID,Value],call(assertz(G)).

%%% Cleanup up the memory space and reload the file
%% XXX literal
cleanup :- table_index(L), drop_tables(L),
        rm_rule_one_arg(table_index), consult("db.pl").
