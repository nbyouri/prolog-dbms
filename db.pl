%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					   %
% Facts: Client, Commande, Detail, Produit %
%					   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Clients table
:- dynamic(ncli/2).
:- dynamic(nom/2).
:- dynamic(adresse/2).
:- dynamic(localite/2).
:- dynamic(cat/2).
:- dynamic(compte/2).

ncli("B062","B062").
ncli("B112","B112").
ncli("B332","B332").
ncli("B512","B512").
ncli("C003","C003").
ncli("C123","C123").
ncli("C400","C400").
ncli("D063","D063").
ncli("F010","F010").
ncli("F400","F400").
ncli("K111","K111").
ncli("L422","L422").
ncli("S127","S127").
ncli("S712","S712").
ncli("F011","F011").
ncli("K729","K729").

nom("B062","Goffin").
nom("B112","Hansenne").
nom("B332","Monti").
nom("B512","Gillet").
nom("C003","Avron").
nom("C123","MERCIER").
nom("C400","Ferard").
nom("D063","Mercier").
nom("F010","Toussaint").
nom("F400","Jacob").
nom("K111","Vanbist").
nom("L422","Franck").
nom("S127","Vanderka").
nom("S712","Guillaume").
nom("F011","PONCELET").
nom("K729","NEUMAN").

adresse("B062", "72, rue de la Gare").
adresse("B112", "23, rue Dumont").
adresse("B332", "112, rue Neuve").
adresse("B512", "14, rue de l'Eté").
adresse("C003", "8, rue de la Cure").
adresse("C123", "25, rue Lemaitre").
adresse("C400", "65, rue du Tertre").
adresse("D063", "201, boulevard du Nord").
adresse("F010", "5, rue Godefroid").
adresse("F400", "78, chemin du Moulin").
adresse("K111", "180, rue Florimont").
adresse("L422", "60, rue de Wépion").
adresse("S127", "3, avenue des Roses").
adresse("S712", "14a, chemin des Roses").
adresse("F011", "17, Clos des Erables").
adresse("K729", "40, rue Bransart").

localite("B062","Namur").
localite("B112","Poitiers").
localite("B332","Genève").
localite("B512","Toulouse").
localite("C003","Toulouse").
localite("C123","Namur").
localite("C400","Poitiers").
localite("D063","Toulouse").
localite("F010","Poitiers").
localite("F400","Bruxelles").
localite("K111","Lille").
localite("L422","Namur").
localite("S127","Namur").
localite("S712","Paris").
localite("F011","Toulouse").
localite("K729","Toulouse").

cat("B062","B2").
cat("B112","C1").
cat("B332","B2").
cat("B512","B1").
cat("C003","B1").
cat("C123","C1").
cat("C400","B2").
cat("D063","B2").
cat("F010","C1").
cat("F400","C2").
cat("K111","B1").
cat("L422","C1").
cat("S127","C1").
cat("S712","B1").
cat("F011","B2").
cat("K729","B2").

compte("B062","-3200").
compte("B112","1250").
compte("B332","0").
compte("B512","-8700").
compte("C003","-1700").
compte("C123","-2300").
compte("C400","350").
compte("D063","-2250").
compte("F010","0").
compte("F400","0").
compte("K111","720").
compte("L422","0").
compte("S127","-4580").
compte("S712","0").
compte("F011","0").
compte("K729","0").

% Commande table
:- dynamic(ncom_co/2).
:- dynamic(ncli_co/2).
:- dynamic(date/2).

ncom_co("30178","30178").
ncom_co("30179","30179").
ncom_co("30182","30182").
ncom_co("30184","30184").
ncom_co("30185","30185").
ncom_co("30186","30186").
ncom_co("30188","30188").

ncli_co("30178","K111").
ncli_co("30179","C400").
ncli_co("30182","S127").
ncli_co("30184","C400").
ncli_co("30185","F011").
ncli_co("30186","C400").
ncli_co("30188","B512").

date("30178","2008-12-22").
date("30179","2008-12-22").
date("30182","2008-12-23").
date("30184","2008-12-23").
date("30185","2009-01-02").
date("30186","2009-01-02").
date("30188","2009-01-02").

% Detail table
:- dynamic(ncom_de/2).
:- dynamic(npro_de/2).
:- dynamic(qcom/2).

ncom_de("30178","30178").
ncom_de("30179","30179").
ncom_de("30179","30179").
ncom_de("30182","30182").
ncom_de("30184","30184").
ncom_de("30185","30185").
ncom_de("30185","30185").
ncom_de("30185","30185").
ncom_de("30186","30186").
ncom_de("30188","30188").
ncom_de("30188","30188").
ncom_de("30188","30188").
ncom_de("30184","30184").
ncom_de("30188","30188").

npro_de("30178","CS464").
npro_de("30179","CS262").
npro_de("30179","PA60").
npro_de("30182","PA60").
npro_de("30184","CS464").
npro_de("30185","CS464").
npro_de("30185","PA60").
npro_de("30185","PS222").
npro_de("30186","PA45").
npro_de("30188","CS464").
npro_de("30188","PA60").
npro_de("30188","PH222").
npro_de("30184","PA45").
npro_de("30188","PA45").

qcom("30178","25").
qcom("30179","60").
qcom("30179","20").
qcom("30182","30").
qcom("30184","120").
qcom("30185","260").
qcom("30185","15").
qcom("30185","600").
qcom("30186","3").
qcom("30188","180").
qcom("30188","70").
qcom("30188","92").
qcom("30184","20").
qcom("30188","22").

% Produit table
:- dynamic(npro/2).
:- dynamic(libelle/2).
:- dynamic(prix/2).
:- dynamic(qstock/2).

npro("CS262","CS262").
npro("CS264","CS264").
npro("CS464","CS464").
npro("PA60","PA60").
npro("PS222","PS222").
npro("PA45","PA45").
npro("PH222","PH222").

libelle("CS262","Chev. Sapin 200*6*2").
libelle("CS264","Chev. Sapin 200*6*4").
libelle("CS464","Chev. Sapin 400*6*4").
libelle("PA60","Pointe Acier 60 (10K)").
libelle("PS222","PL. Sapin 200*20*2").
libelle("PA45","POINTE ACIER 45 (20K)").
libelle("PH222","PL. HETRE 200x20x2").

prix("CS262","75").
prix("CS264","120").
prix("CS464","220").
prix("PA60","95").
prix("PS222","185").
prix("PA45","105").
prix("PH222","185").

qstock("CS262","45").
qstock("CS264","2690").
qstock("CS464","450").
qstock("PA60","134").
qstock("PS222","1220").
qstock("PA45","580").
qstock("PH222","1220").

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
        select_column(X, H),
        select(X, T).

select_all(TableName) :- get_table_columns(TableName,L),
        select(TableName,L).

is_column_in_table(TableName, ColumnName) :- 
        get_table_columns(TableName,L),member(ColumnName, L), !.

column_as_list(ColumnName,L) :- G =.. [ColumnName], findall(Y, call(G,_,Y), L).

select_column(TableName, ColumnName) :-
        is_column_in_table(TableName, ColumnName)
	-> column_as_list(ColumnName, L),
        print_table(L)
        ; write("No such column in table "),
        write(TableName), nl.

%% Create columns
register_column_in_table(TableName, ColumnName) :-
        get_table_columns(TableName, L), append(L, [ColumnName], NL),
        rule_replace_list(TableName, NL).

create_columns(_,[]).
create_columns(TableName, [H|T]) :- register_column_in_table(TableName, H),
        add_rule_two_args(H), create_columns(TableName, T).

%% Create table
register_table_in_index(TableName) :-
        table_index(L), append(L, [TableName], NL),
        rule_replace_list(table_index, NL).

create_table(TableName, Columns) :- add_rule_empty_list(TableName),
        register_table_in_index(TableName),
        create_columns(TableName,Columns).

%% Drop a column
delete_column_in_table(TableName, ColumnName) :-
        get_table_columns(TableName, L), delete(L,ColumnName,NL),
        rule_replace_list(TableName, NL).

drop_column(TableName, ColumnName) :-
	is_column_in_table(TableName, ColumnName) ->
        delete_column_in_table(TableName, ColumnName),
	rm_rule_two_args(ColumnName) ;
	write("No such column in table "), write(TableName), nl.

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
insert_row(_, _, []).
insert_row(ID, [H|T], [H2|T2]) :- column_set_value(H, ID, H2),
        insert_row(ID, T, T2).
insert(TableName, [ID|Values]) :- get_table_columns(TableName,ColumnNames),
        length(ColumnNames, L1), length([ID|Values], L2),
        L1 = L2 -> insert_row(ID, ColumnNames, [ID|Values])
        ; write("Arguments do not match the table "), write(TableName), nl.


p_print_table([],_).
p_print_table([H|T], N) :- write(N), write(" -> "), write(H), nl,
        N1 is N+1, p_print_table(T, N1).
print_table(X) :- p_print_table(X, 0).


%% Utils
db_repl :-
        repeat,
        write('> '),
        read(X),
        call(X),
        fail.

add_rule_empty_list(Rule) :- dynamic(Rule/1), G =.. [Rule,[]],call(assertz(G)).

%%% XXX how to avoid _,_ -> true ?
add_rule_two_args(Rule) :- dynamic(Rule/2), G =.. [Rule,_,_],call(assertz(G)).

rule_replace_list(Rule,List) :- rm_rule_one_arg(Rule),
        dynamic(Rule/1), G =.. [Rule,List], call(assertz(G)).

rm_rule_no_args(Rule) :- G =.. [Rule],call(retractall(G)).

rm_rule_one_arg(Rule) :- rm_rule_no_args(Rule),
        G =.. [Rule,_],call(retractall(G)).

rm_rule_two_args(Rule) :- rm_rule_no_args(Rule),
        G =.. [Rule,_,_],call(retractall(G)).

get_table_columns(TableName, List) :- G =.. [TableName, List], call(G).

column_set_value(ColumnName, TableID, Value) :-
        G =.. [ColumnName, TableID, Value],call(assertz(G)).

%%% Cleanup up the memory space and reload the file
%% XXX literal
cleanup :- table_index(L), drop_tables(L),
        rm_rule_one_arg(table_index), consult("db.pl").
