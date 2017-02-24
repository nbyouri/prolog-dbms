%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					   %
% Facts: Client, Commande, Detail, Produit %
%					   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Clients table
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
ncli_co("30178","K111").
ncli_co("30179","C400").
ncli_co("30182","S127").
ncli_co("30184","C400").
ncli_co("30185","F011").
ncli_co("30186","C400").
ncli_co("30188","B512").

co_date("30178","2008-12-22").
co_date("30179","2008-12-22").
co_date("30182","2008-12-23").
co_date("30184","2008-12-23").
co_date("30185","2009-01-02").
co_date("30186","2009-01-02").
co_date("30188","2009-01-02").

% Detail table
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



%%%
% Rules
%%%
get_client(Ncli,Nom,Adresse,Localite,Cat,Compte) :- 
    nom(Ncli,Nom),
    adresse(Ncli,Adresse),
    localite(Ncli,Localite),
    cat(Ncli,Cat),
    compte(Ncli,Compte).




%print_table(A,B) :- write(A), nl, write(B), nl.
