%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					   %
% Facts: Client, Commande, Detail, Produit %
%					   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Clients table
:- dynamic(client_ncli/2).
:- dynamic(client_nom/2).
:- dynamic(client_adresse/2).
:- dynamic(client_localite/2).
:- dynamic(client_cat/2).
:- dynamic(client_compte/2).

clients_ncli("B062","B062").
clients_ncli("B112","B112").
clients_ncli("B332","B332").
clients_ncli("B512","B512").
clients_ncli("C003","C003").
clients_ncli("C123","C123").
clients_ncli("C400","C400").
clients_ncli("D063","D063").
clients_ncli("F010","F010").
clients_ncli("F400","F400").
clients_ncli("K111","K111").
clients_ncli("L422","L422").
clients_ncli("S127","S127").
clients_ncli("S712","S712").
clients_ncli("F011","F011").
clients_ncli("K729","K729").

clients_nom("B062","Goffin").
clients_nom("B112","Hansenne").
clients_nom("B332","Monti").
clients_nom("B512","Gillet").
clients_nom("C003","Avron").
clients_nom("C123","MERCIER").
clients_nom("C400","Ferard").
clients_nom("D063","Mercier").
clients_nom("F010","Toussaint").
clients_nom("F400","Jacob").
clients_nom("K111","Vanbist").
clients_nom("L422","Franck").
clients_nom("S127","Vanderka").
clients_nom("S712","Guillaume").
clients_nom("F011","PONCELET").
clients_nom("K729","NEUMAN").

clients_adresse("B062", "72, rue de la Gare").
clients_adresse("B112", "23, rue Dumont").
clients_adresse("B332", "112, rue Neuve").
clients_adresse("B512", "14, rue de l'Eté").
clients_adresse("C003", "8, rue de la Cure").
clients_adresse("C123", "25, rue Lemaitre").
clients_adresse("C400", "65, rue du Tertre").
clients_adresse("D063", "201, boulevard du Nord").
clients_adresse("F010", "5, rue Godefroid").
clients_adresse("F400", "78, chemin du Moulin").
clients_adresse("K111", "180, rue Florimont").
clients_adresse("L422", "60, rue de Wépion").
clients_adresse("S127", "3, avenue des Roses").
clients_adresse("S712", "14a, chemin des Roses").
clients_adresse("F011", "17, Clos des Erables").
clients_adresse("K729", "40, rue Bransart").

clients_localite("B062","Namur").
clients_localite("B112","Poitiers").
clients_localite("B332","Genève").
clients_localite("B512","Toulouse").
clients_localite("C003","Toulouse").
clients_localite("C123","Namur").
clients_localite("C400","Poitiers").
clients_localite("D063","Toulouse").
clients_localite("F010","Poitiers").
clients_localite("F400","Bruxelles").
clients_localite("K111","Lille").
clients_localite("L422","Namur").
clients_localite("S127","Namur").
clients_localite("S712","Paris").
clients_localite("F011","Toulouse").
clients_localite("K729","Toulouse").

clients_cat("B062","B2").
clients_cat("B112","C1").
clients_cat("B332","B2").
clients_cat("B512","B1").
clients_cat("C003","B1").
clients_cat("C123","C1").
clients_cat("C400","B2").
clients_cat("D063","B2").
clients_cat("F010","C1").
clients_cat("F400","C2").
clients_cat("K111","B1").
clients_cat("L422","C1").
clients_cat("S127","C1").
clients_cat("S712","B1").
clients_cat("F011","B2").
clients_cat("K729","B2").

clients_compte("B062","-3200").
clients_compte("B112","1250").
clients_compte("B332","0").
clients_compte("B512","-8700").
clients_compte("C003","-1700").
clients_compte("C123","-2300").
clients_compte("C400","350").
clients_compte("D063","-2250").
clients_compte("F010","0").
clients_compte("F400","0").
clients_compte("K111","720").
clients_compte("L422","0").
clients_compte("S127","-4580").
clients_compte("S712","0").
clients_compte("F011","0").
clients_compte("K729","0").

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
select(X, [H|T]) :-
        is_column_in_table(X,H)
        -> select_column(X,H), select(X,T)
        ; write("No such column in table "), write(X).

select_all(TableName) :- get_table_columns(TableName,L),
        select(TableName,L).

is_column_in_table(TableName,ColumnName) :- 
        get_table_columns(TableName,L),member(ColumnName, L), !.

column_as_list(TableName,ColumnName,R) :-
        column_get_name(TableName,ColumnName,X),
        G =.. [X,_,L], findall(L,call(G),R).

select_column(TableName,ColumnName) :-
	column_as_list(TableName,ColumnName,L),
        print_table(L).

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
        write(X),nl,
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

%% Drop a row matching predicate 
% TODO

%% Insert values into a table
insert_row(_,_,_,[]).
insert_row(TableName,ID,[H|T],[H2|T2]) :-
        column_set_value(TableName,H,ID,H2),
        insert_row(TableName,ID,T,T2).

insert(TableName, [ID|Values]) :- get_table_columns(TableName,ColumnNames),
        length(ColumnNames, L1), length([ID|Values], L2),
        L1 = L2 -> insert_row(TableName,ID,ColumnNames,[ID|Values])
        ; write("Arguments do not match the table "), write(TableName), nl.



%% Combine two lists
%%% handle null case XXX 
%combine2([],[],L) :- write(L),nl.
%combine2([H|T],[H1|T1],L) :- append([H],[H1],L1),combine2(T,T1,L1).
%
%combine([],_).
%combine([H|T],R) :- %column_as_list(H,L1), column_as_list(H1,L2),
%        combine2(H,L2,R1), combine([H1|T],R1).


%% Pretty print results as a table
p_print_table([],_).
p_print_table([H|T], N) :- write(N), write(" -> "), write(H), nl,
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
        column_get_name(TableName,TableID,TID),
        G =.. [X,TID,Value],call(assertz(G)).

%%% Cleanup up the memory space and reload the file
%% XXX literal
cleanup :- table_index(L), drop_tables(L),
        rm_rule_one_arg(table_index), consult("db.pl").
