/* Projet de SAS M1ISF: étudier les corrélations entre les salaires des joueurs de premier league avec perf et valeure marchande */ 
/* Importer base de données */ 
/* Code généré (IMPORT) */ 
/* Fichier source : Basefinale.xlsx */ 
/* Chemin source : /home/u64029993/Projet SAS M1ISF */ 
/* Code généré le : 03/10/2024 11:47 */ 
/* Bibliothéque WORK */ 
 
/* Macros */ 
%include "/home/u64029993/Projet SAS M1ISF/Macros.sas"; 
 
 
%web_drop_table(TABLE1); 
 
FILENAME REFFILE '/home/u64029993/Projet SAS M1ISF/Basefinale.xlsx'; 
 
PROC IMPORT DATAFILE=REFFILE 
	DBMS=XLSX 
	OUT=TABLE1; 
	GETNAMES=YES; 
RUN; 
 
%web_open_table(TABLE1); 
 
/* Partie 1 - Traitement spécifique des données */ 
/* conformément à notre accord orale, la partie 1.I et 1.II à été directement récupéré du Projet R */ 
 
												/* I - Import et fusion des bases */ 
/* Sous R */  
/* Juste enlever les lignes doublons (par noms) */ 
PROC SORT DATA=TABLE1 nodupkey; 
BY NAME; 
RUN; 
%web_open_table(TABLE1); 
 
												/* II - Nettoyage de la base de données */ 
/* Sous R */  
/* Spécifique à SAS */ 
/* Nom colonne avec point non accepté */ 
proc datasets library=work nolist; 
    modify table1; 
    rename  
    "Goals.per.match"n = Goals_per_match 
    "Big.Chances.Created"n = Big_Chances_Created 
    "Shots.on.target"n = Shots_on_target 
    "Yellow.cards"n = Yellow_cards 
    "Duels.Won"n = Duels_Won 
    "Red.Cards"n = Red_Cards 
    "Shots.on.Target"n = Shots_on_Target 
    "Aerial.battles.lost"n = Aerial_battles_lost  
    "Aerial.battles.won"n= Aerial_battles_won  
    "Big.chances.missed"n= Big_chances_missed  
    "Blocked.shots"n= Blocked_shots  
    "Clean.sheets"n= Clean_sheets  
    "Duels.lost "n= Duels_lost  
    "Errors.leading.to.goal"n= Errors_leading_to_goal  
    "Freekicks.scored"n= Freekicks_scored 
    "Goals.conceded"n= Goals_conceded 
    "Last.man.tackles"n= Last_man_tackles 
    "Own.goals"n= Own_goals  
    "Penalties.saved"n= Penalties_saved 
    "Penalties.scored"n= Penalties_scored 
    "Hit.woodwork"n = Hit_woodwork; 
quit; 
 
												/* III - Modification et création de variable */ 
/* Détection des valeurs aberrantes */ 
proc sgplot DATA=TABLE1; 
    vbox Yearly_Salary_num / datalabel=Yearly_Salary_num; 
    title "Boîte à moustache des salaires"; 
    yaxis label="Valeurs"; 
run; 
proc sgplot DATA=TABLE1; 
    vbox Market_value / datalabel=Market_value; 
    title "Boîte à moustache des valeurs marchandes"; 
    yaxis label="Valeurs"; 
run; 
proc sgplot DATA=TABLE1; 
    vbox Performances / datalabel=Performances; 
    title "Boîte à moustache des Performances"; 
    yaxis label="Valeurs"; 
run; 
								 
/* Création de la variable Performances */ 
/* Étape 1 : Calcul des min et max pour la normalisation */ 
proc sql; 
    create table stats as 
    select  
        /* Calcul des min et max pour les variables communes */ 
        min(Wins) as Min_Wins, max(Wins) as Max_Wins, 
        min(Tackles) as Min_Tackles, max(Tackles) as Max_Tackles, 
        min(Last_man_tackles) as Min_LastMT, max(Last_man_tackles) as Max_LastMT, 
        min(Blocked_shots) as Min_BlockedShots, max(Blocked_shots) as Max_BlockedShots, 
        min(Duels_won) as Min_DuelsWon, max(Duels_won) as Max_DuelsWon, 
        min(Duels_lost) as Min_Duelslost, max(Duels_lost) as Max_Duelslost, 
        min(Aerial_battles_won) as Min_AerialsWon, max(Aerial_battles_won) as Max_AerialsWon, 
        min(Aerial_battles_lost) as Min_AerialsLost, max(Aerial_battles_lost) as Max_AerialsLost, 
        min(Own_goals) as Min_Owngoals, max(Own_goals) as Max_Owngoals, 
        min(Errors_leading_to_goal) as Min_Errors, max(Errors_leading_to_goal) as Max_Errors, 
        min(Fouls) as Min_Fouls, max(Fouls) as Max_Fouls, 
        min(Yellow_cards) as Min_Yellow, max(Yellow_cards) as Max_Yellow, 
        min(Red_cards) as Min_Red, max(Red_cards) as Max_Red, 
        min(Clean_sheets) as Min_CleanS, max(Clean_sheets) as Max_CleanS, 
        min(Punches) as Min_Punches, max(Punches) as Max_Punches, 
        min(Goals_conceded) as Min_GoalsC, max(Goals_conceded) as Max_GoalsC, 
        min(Penalties_saved) as Min_PenaltiesS, max(Penalties_saved) as Max_PenaltiesS, 
        min(Shots_on_target) as Min_Shots_on_Target, max(Shots_on_target) as Max_Shots_on_Target, 
        min(Goals_per_match) as Min_GoalsPM, max(Goals_per_match) as Max_GoalsPM, 
        min(Goals) as Min_Goals, max(Goals) as Max_Goals, 
        min(Penalties_scored) as Min_PenaltiesS, max(Penalties_scored) as Max_PenaltiesS, 
        min(Freekicks_scored) as Min_FreekicksS, max(Freekicks_scored) as Max_FreekicksS, 
        min(Offsides) as Min_Offsides, max(Offsides) as Max_Offsides, 
        min(Assists) as Min_Assists, max(Assists) as Max_Assists, 
        min(Big_chances_created) as Min_BigChancesC, max(Big_chances_created) as Max_BigChancesC, 
        min(Big_chances_missed) as Min_BigChancesM, max(Big_chances_missed) as Max_BigChancesM, 
        min(Interceptions) as Min_Interceptions, max(Interceptions) as Max_Interceptions, 
        min(Shots) as Min_Shots, max(Shots) as Max_Shots 
    from table1; 
quit; 
 
/* Étape 2 : Calculer les performances */ 
data table1; 
    if _n_ = 1 then set stats; /* Charger les min/max dans la mémoire */ 
    set table1; 
 
    /* Appliquer les calculs uniquement pour les Midfielders */ 
    if Position = "Midfielder" then do; 
        /* Normalisation des statistiques */ 
        Wins_norm = (Wins - Min_Wins) / (Max_Wins - Min_Wins); 
        Shots_norm = (Shots - Min_Shots) / (Max_Shots - Min_Shots); 
        Shots_on_Target_norm = (Shots_on_Target - Min_Shots_on_Target) / (Max_Shots_on_Target - Min_Shots_on_Target); 
        Interceptions_norm = (Interceptions - Min_Interceptions) / (Max_Interceptions - Min_Interceptions); 
        Duels_Won_norm = (Duels_Won - Min_DuelsWon) / (Max_DuelsWon - Min_DuelsWon); 
        Goals_norm = (Goals - Min_Goals) / (Max_Goals - Min_Goals); 
        Reds_Cards_norm = 1 - ((Red_Cards - Min_Red) / (Max_Red - Min_Red)); /* Inversion */ 
        Yellow_Cards_norm = 1 - ((Yellow_Cards - Min_Yellow) / (Max_Yellow - Min_Yellow)); /* Inversion */ 
        Fouls_norm = 1 - ((Fouls - Min_Fouls) / (Max_Fouls - Min_Fouls)); /* Inversion */ 
        Assists_norm = (Assists - Min_Assists) / (Max_Assists - Min_Assists); 
        Big_Chances_Created_norm = (Big_Chances_Created - Min_BigChancesC) / (Max_BigChancesC); 
 
        /* Calcul de la colonne Performances */ 
        Performances = ( 
            0.1 * Wins_norm + 
            0.1 * Shots_norm + 
            0.05 * Shots_on_Target_norm + 
            0.1 * Interceptions_norm + 
            0.1 * Duels_Won_norm + 
            0.2 * Goals_norm + 
            0.05 * Reds_Cards_norm + 
            0.05 * Yellow_Cards_norm + 
            0.05 * Fouls_norm + 
            0.15 * Assists_norm + 
            0.1 * Big_Chances_Created_norm 
        ) * 100; /* Mettre sur 100 */ 
       end; 
        
     if Position = 'Defender' then do; 
        /* Normalisation des variables */ 
        Wins_norm = (Wins - Min_Wins) / (Max_Wins - Min_Wins); 
        Tackles_norm = (Tackles - Min_Tackles) / (Max_Tackles - Min_Tackles); 
        LastMT_norm = (Last_man_tackles - Min_LastMT) / (Max_LastMT - Min_LastMT); 
        BlockedShots_norm = (Blocked_shots - Min_BlockedShots) / (Max_BlockedShots - Min_BlockedShots); 
        DuelsWon_norm = (Duels_won - Min_DuelsWon) / (Max_DuelsWon - Min_DuelsWon); 
        Duelslost_norm = 1 - ((Duels_lost - Min_Duelslost) / (Max_Duelslost - Min_Duelslost)); 
        AerialsWon_norm = (Aerial_battles_won - Min_AerialsWon) / (Max_AerialsWon - Min_AerialsWon); 
        AerialsLost_norm = 1 - ((Aerial_battles_lost - Min_AerialsLost) / (Max_AerialsLost - Min_AerialsLost)); 
        Owngoals_norm = 1 - ((Own_goals - Min_Owngoals) / (Max_Owngoals - Min_Owngoals)); 
        Errors_norm = 1 - ((Errors_leading_to_goal - Min_Errors) / (Max_Errors - Min_Errors)); 
        Fouls_norm = 1 - ((Fouls - Min_Fouls) / (Max_Fouls - Min_Fouls)); 
        Yellow_norm = 1 - ((Yellow_cards - Min_Yellow) / (Max_Yellow - Min_Yellow)); 
        Red_norm = 1 - ((Red_cards - Min_Red) / (Max_Red - Min_Red)); 
 
        /* Calcul de la performance */ 
        Performances = ( 
            0.1 * Wins_norm +  
            0.025 * Tackles_norm +  
            0.1 * LastMT_norm +  
            0.05 * BlockedShots_norm +  
            0.075 * DuelsWon_norm +  
            0.05 * Duelslost_norm +  
            0.025 * AerialsWon_norm +  
            0.025 * AerialsLost_norm +  
            0.15 * Owngoals_norm +  
            0.1 * Errors_norm +  
            0.05 * Fouls_norm +  
            0.1 * Yellow_norm +  
            0.15 * Red_norm 
        ) * 100; /* Note sur 100 */ 
       end; 
     if Position = 'Forward' then do; 
        /* Normalisation des variables */ 
        Wins_norm = (Wins - Min_Wins) / (Max_Wins - Min_Wins); 
        Shots_on_Target_norm = (Shots_on_target - Min_Shots_on_Target) / (Max_Shots_on_Target - Min_Shots_on_Target); 
        GoalsPM_norm = (Goals_per_match - Min_GoalsPM) / (Max_GoalsPM - Min_GoalsPM); 
        Goals_norm = (Goals - Min_Goals) / (Max_Goals - Min_Goals); 
        PenaltiesS_norm = (Penalties_scored - Min_PenaltiesS) / (Max_PenaltiesS - Min_PenaltiesS); 
        FreekicksS_norm = (Freekicks_scored - Min_FreekicksS) / (Max_FreekicksS - Min_FreekicksS); 
        Offsides_norm = 1 - ((Offsides - Min_Offsides) / (Max_Offsides - Min_Offsides)); /* Moins d'offside est mieux */ 
        Assists_norm = (Assists - Min_Assists) / (Max_Assists - Min_Assists); 
        Big_Chances_Created_norm = (Big_chances_created - Min_BigChancesC) / (Max_BigChancesC - Min_BigChancesC); 
        Big_Chances_missed_norm = 1 - ((Big_chances_missed - Min_BigChancesM) / (Max_BigChancesM - Min_BigChancesM)); 
 
        /* Calcul de la performance */ 
        Performances = ( 
            0.05 * Wins_norm +  
            0.05 * Shots_on_Target_norm +  
            0.1 * GoalsPM_norm +  
            0.2 * Goals_norm +  
            0.15 * PenaltiesS_norm +  
            0.2 * FreekicksS_norm +  
            0.025 * Offsides_norm +  
            0.2 * Assists_norm +  
            0.15 * Big_Chances_Created_norm +  
            0.1 * Big_Chances_missed_norm 
        ) * 100;  
       end; 
    if Position = 'Goalkeeper' then do; 
        /* Normalisation des variables */ 
        Wins_norm = (Wins - Min_Wins) / (Max_Wins - Min_Wins); 
        CleanS_norm = (Clean_sheets - Min_CleanS) / (Max_CleanS - Min_CleanS); 
        Punches_norm = (Punches - Min_Punches) / (Max_Punches - Min_Punches); 
        GoalsC_norm = 1 - ((Goals_conceded / Appearances) - Min_GoalsC) / (Max_GoalsC - Min_GoalsC); /* Inverser pour moins de buts concédés */ 
        Errors_norm = 1 - ((Errors_leading_to_goal - Min_Errors) / (Max_Errors - Min_Errors)); /* Inverser pour moins d'erreurs */ 
        PenaltiesS_norm = (Penalties_saved - Min_PenaltiesS) / (Max_PenaltiesS - Min_PenaltiesS); 
 
        /* Calcul de la performance */ 
        Performances = ( 
            0.15 * Wins_norm +  
            0.25 * CleanS_norm +  
            0.05 * Punches_norm +  
            0.2 * GoalsC_norm +  
            0.2 * Errors_norm +  
            0.15 * PenaltiesS_norm 
        ) * 100; 
    end; 
run; 
 
/* Étape 3 : Vérifier les performances */ 
data table1; 
    set table1; 
    if missing(Performances) then Performances = 10; 
    if Performances > 100 then Performances = 84.21255; 
run; 
proc print data=table1; 
    var Name Position Performances; 
run; 
 
/* Supprimer les colonnes de stats inutile dans table1 */ 
proc sql noprint; 
    select name into :col_list separated by ' ' 
    from dictionary.columns 
    where libname = "WORK" and memname = "STATS";  
quit; 
data table1(drop=&col_list); 
    set table1; 
run; 
/* Supprimer les variables normalisé */ 
proc sql noprint; 
    select name into :col_list separated by ' ' 
    from dictionary.columns 
    where libname = "WORK" and memname = "TABLE1" and upcase(name) like '%_NORM'; 
quit; 
data table1(drop=&col_list); 
    set table1; 
run; 
													/* AFFICHAGE PROPRE !*/ 
PROC PRINT DATA = TABLE1; 
RUN; 
viewtable work.table1; 
PROC CONTENTS DATA=TABLE1; 
RUN; 
 
/* Partie 2 - Statistiques descriptives */ 
 
												/* I - résumé stat des variables d'intérêts */ 
/* Statistique descriptive de la base */ 
PROC MEANS DATA=TABLE1; 
RUN; 
 
/* Statistique des variable choisis */ 
PROC MEANS DATA=TABLE1 mean max min range median Q3; 
VAR Yearly_Salary_num Performances Market_value; 
title 'Summary of Salary, Market Value and Performances'; 
RUN; 
PROC MEANS DATA=TABLE1 n std; 
VAR Yearly_Salary_num Performances Market_value; 
title 'Écart types des variables Salary Performances et Market Value'; 
RUN; 
 
													/* II - Analyse graphique élémentaire */ 
/* Graphique entre deux variables */									 
proc sgplot data=table1; 
    scatter x=Yearly_Salary_num y=Market_value / markerattrs=(symbol=circlefilled color=blue);; 
    title "Graphique de dispersion entre Salaires et Valeur Marchande"; 
run; 
proc sgplot data=table1; 
    scatter x=Yearly_Salary_num y=Performances / markerattrs=(symbol=circlefilled color=blue);; 
    title "Graphique de dispersion entre Salaires et Performances"; 
run; 
proc sgplot data=table1; 
    scatter x=Performances y=Market_value / markerattrs=(symbol=circlefilled color=blue);; 
    title "Graphique de dispersion entre Performances et Valeur Marchande"; 
run; 
proc sgplot data=table1; 
    scatter x=Yearly_Salary_num y=Appearances / markerattrs=(symbol=circlefilled color=blue);; 
    title "Graphique de dispersion entre Salaires et Nombre de match"; 
run; 
proc sgplot data=table1; 
    scatter x=Yearly_Salary_num y=Wins / markerattrs=(symbol=circlefilled color=blue);; 
    title "Graphique de dispersion entre Salaires et Nombre de victoires"; 
run; 
proc sgplot data=table1; 
    scatter x=Wins y=Appearances / markerattrs=(symbol=circlefilled color=blue);; 
    title "Graphique de dispersion entre nombre de Victoire et de match"; 
run; 
 
													/* III - Matrice de corrélation */ 
/* 1 - Test de Pearson complet*/ 
proc corr DATA=TABLE1; 
run; 
 
/* 2 - Créer une heatmap des coefs de corrélation des variables d'intérêts */ 
proc corr data=table1 outp=corr_matrix noprint; 
    var Yearly_Salary_num Market_Value Performances;  
run; 
data corr_matrix_clean; 
    set corr_matrix; 
    if _TYPE_ = "CORR"; /* garder que les coefficients de corrélation */ 
run; 
proc sort data=corr_matrix_clean out=corr_matrix_clean_sorted;/* Trier */ 
    by _NAME_; 
run; 
/* Transposer les données */ 
proc transpose data=corr_matrix_clean_sorted out=corr_matrix_long name=Variable_X; 
    by _NAME_; 
run; 
/* Renommez la colonne contenant les coefficients transposés */ 
data corr_matrix_long; 
    set corr_matrix_long; 
    rename COL1=Corr_Value;  
run; 
/* Graphique en heatmap */ 
proc sgplot data=corr_matrix_long; 
    heatmapparm x=_NAME_ y=Variable_X colorresponse=Corr_Value /  
        colormodel=(blue white red); 
    scatter x=_NAME_ y=Variable_X / markerchar=Corr_Value markercharattrs=(size=10 weight=bold color=black); 
    gradlegend / title="Corrélation"; /* Légende du dégradé */ 
    title "Heatmap des coefficients de corrélation des variables d'intérêts"; 
run; 
 
/* 10 plus gros coefficients avec la variable Yearly_Salary_num */ 
/* 1: Calculer les corrélations entre Yearly_Salary et toutes les autres variables */ 
proc corr data=TABLE1 outp=corr_output noprint; 
    var Yearly_Salary_num; /* Variable cible */ 
    with _NUMERIC_; /* Corrélation avec toutes les autres variables */ 
run; 
 
/* 2: Sélectionner les 10 variables les plus corrélées avec Yearly_Salary */ 
proc sql; 
    create table top10_corr as 
    select _NAME_, Yearly_Salary_num 
    from corr_output 
    where _NAME_ ne 'Yearly_Salary_num' /* Exclure Yearly_Salary de la liste */ 
    order by abs(Yearly_Salary_num) desc; /* Trier par la valeur absolue de la corrélation */ 
quit; 
 
/* Sélectionner les 10 variables les plus corrélées */ 
data top10_corr; 
    set top10_corr; 
    if _N_ <= 13;  
run; 
proc sql noprint; 
    select _NAME_ into :top10_vars separated by ' ' 
    from top10_corr; 
quit; 
/* 3: Calculer la matrice de corrélation des 10 variables */ 
proc corr data=TABLE1 outp=corr_matrix2 noprint; 
    var Yearly_Salary_num &top10_vars.; /* Liste des 10 variables */ 
run; 
data corr_matrix2; 
    set corr_matrix2; 
    if _TYPE_ = "CORR"; /* Garder uniquement les coefficients de corrélation */ 
run; 
proc sort data=corr_matrix2; 
    by _NAME_; 
run; 
/* 4: Créer une matrice de corrélation sous forme de heatmap */ 
proc transpose data=corr_matrix2 out=corr_matrix_long2 name=Variable_X; 
    by _NAME_; 
run; 
data corr_matrix_long2; 
    set corr_matrix_long2; 
    if _NAME_ ne 'Yearly_Salary_num'; /* Garder uniquement les corrélations avec les variables */ 
    Corr_Value2 = col1; /* Créer une variable Corr_Value2 contenant les coefficients */ 
run; 
/* 5: Créer la heatmap avec les coefficients de corrélation */ 
proc sgplot data=corr_matrix_long2; 
    heatmapparm x=_NAME_ y=Variable_X colorresponse=Corr_Value2 /  
        colormodel=(blue white red) /* Dégradé de couleur pour les corrélations */ 
        outline; /* Ajouter un contour pour chaque cellule */ 
    text x=_NAME_ y=Variable_X text=Corr_Value2 /  
        position=center colormodel=(blue white red); /* Afficher la valeur de la corrélation dans chaque cellule */ 
    gradlegend / title="Corrélation"; /* Légende pour la couleur */ 
    title "Heatmap de la matrice de corrélation des 10 variables les plus corrélées avec Yearly_Salary"; 
run; 
 
											/* Test de normalité */ 
ods output TestsForNormality=shapiro_results; /* Crée un dataset avec les tests de normalité */ 
proc univariate data=TABLE1 normal; 
    var Yearly_Salary_num Market_value Performances Wins Goals Big_Chances_Created Appearances; /* Remplacez par les variables spécifiques */ 
run; 
data shapiro_results; /* uniquement shapiro-wilk */ 
	set shapiro_results; 
	where Test="Shapiro-Wilk"; 
run; 
proc print data=shapiro_results; 
run;  
ods output close; 
/* Distribution comparaison */ 
PROC UNIVARIATE DATA=TABLE1;  
VAR Yearly_Salary_num;  
HISTOGRAM Yearly_Salary_num/ NORMAL; 
 
										/* Graphique complémentaire */ 
/* Histogramme */ 
proc sgplot data=TABLE1; 
    vbar Club / response=Yearly_Salary_num; 
    title "Histogramme des salaires moyens par club"; 
run; 
proc sgplot data=TABLE1; 
    vbar Position / response=Yearly_Salary_num group=Position groupdisplay=cluster; /* faire varier les couleurs */ 
    title "Histogramme des salaires moyens par poste"; 
run; 
 
/* Partie 3 - Économétrique */ 
									/* I - Estimation économétrique */ 
/* Modèle 1: */ 
/* récupération résultat avec ODS */ 
ods output ParameterEstimates=Param_Estimates_mod1  
           FitStatistics=Fit_Stats_mod1; 
proc reg data=TABLE1; 
    model Yearly_Salary_num = Market_Value Performances; 
    title "Estimation économétrique : Modèle 1"; 
run; 
quit; 
ods output close; 
proc print data=Param_Estimates_mod1;  
    title "Paramètres estimés du modèle 1";  
run; 
proc print data=Fit_Stats_mod1;  
    title "Statistiques d'ajustement du modèle 1";  
run; 
 
/* obtenir la stat de Fisher */ 
ods output ANOVA=Anova_Stats_mod1;  
proc reg data=TABLE1; 
    model Yearly_Salary_num = Market_Value Performances; 
    title "Estimation économétrique : Statistique de Fisher"; 
run; 
quit; 
ods output close; 
proc print data=Anova_Stats_mod1;  
    title "Statistique de Fisher (F Value) et autres statistiques ANOVA"; 
run; 
 
/* Test */ 
/* 1 - Test d'hétéroscédasticité BReush-Pagan*/ 
%bp_test(data=table1, var_expliquee=Yearly_Salary_num, var_exp1=Market_Value, var_exp2=Performances) 
/* Graphique homogénéité de la variance */ 
/* Estimation du modèle et calcul des résidus */ 
proc reg data=TABLE1 outest=reg_results noprint; 
    model Yearly_Salary_num = Market_Value Performances; 
    output out=residuals_data r=residuals p=fitted_values; 
run; 
/* Regroupement des fitted values et calcul de la variance des résidus */ 
proc sql; 
    create table residuals_variance as 
    select  
        round(fitted_values, 1000) as fitted_value_group format=8.2, 
        var(residuals) as variance_residuals 
    from residuals_data 
    group by calculated fitted_value_group; 
quit; 
/*Graphique */ 
proc sgplot data=residuals_variance; 
    scatter x=fitted_value_group y=variance_residuals /  
        markerattrs=(symbol=CircleFilled size=10 color=navy)  
        transparency=0.2 name="points"; 
     
    series x=fitted_value_group y=variance_residuals /  
        lineattrs=(color=red thickness=2 pattern=solid)  
        name="trend"; 
     
    keylegend "points" "trend" / title="Légende" position=topright across=1; 
    xaxis label="Valeurs ajustées (Fitted Values)"  
          grid  
          valueshint display=(nolabel noline noticks); 
    yaxis label="Variance des résidus"  
          grid  
          valueshint display=(nolabel noline noticks); 
    title "Variance des Résidus en Fonction des Valeurs Ajustées"; 
    footnote "Chaque point représente un groupe de fitted values (arrondi à 1000)."; 
run; 
 
/* La variable Performances coupable ? Test de GQ:*/ 
/* Étape 1 : Trier les données en deux groupes de Performances */ 
proc sort data=TABLE1 out=sorted_table; 
    by Performances; 
run; 
data group1 group2; 
    set sorted_table nobs=n; 
    if _N_ <= (n/2 - n/10) then output group1; /* Première moitié sans le centre */ 
    else if _N_ > (n/2 + n/10) then output group2; /* Deuxième moitié sans le centre */ 
run; 
/* régression sur chaque groupes */ 
proc reg data=group1 outest=estimates_group1 noprint; 
    model Yearly_Salary_num = Market_Value Performances; 
    output out=residuals_group1 r=residuals; 
run; 
proc reg data=group2 outest=estimates_group2 noprint; 
    model Yearly_Salary_num = Market_Value Performances; 
    output out=residuals_group2 r=residuals; 
run; 
/* Étape 4 : Calculer les variances et le nombre d'observations */ 
proc sql noprint; 
    select var(residuals), count(*) into :variance1, :n1 from residuals_group1; 
    select var(residuals), count(*) into :variance2, :n2 from residuals_group2; 
quit; 
/*Calculer le ratio des variances et la statistique de Fisher */ 
data gq_test_results; 
    Ratio_F = &variance2 / &variance1; 
    df1 = &n2 - 2; /* Degrés de liberté pour le groupe 2 */ 
    df2 = &n1 - 2; /* Degrés de liberté pour le groupe 1 */ 
    P_value = 1 - probf(Ratio_F, df1, df2); /* P-valeur associée */ 
run; 
/* Visualiser les résultats */ 
proc print data=gq_test_results; 
    title "Test de Goldfeld-Quandt"; 
    var Ratio_F df1 df2 P_value; 
run; /* Coupable */  
 
/* 2 - Test d'autocorrélation */ 
/* Durbin-Watson test */ 
proc autoreg data=table1; 
   model Yearly_Salary_num = Market_Value Performances / dw=4 dwprob DW=1; 
run; 
 
proc reg data=TABLE1 outest=model_estimates noprint; 
    model Yearly_Salary_num = Market_Value Performances; 
    output out=residuals r=residual; 
run; 
proc arima data=residuals; 
    identify var=residual nlag=20; /* 20 lags par défaut */ 
    title "ACF des résidus"; 
run; 
 
/* 3 - Test de colinéarité */ 
/* VIF */ 
proc reg data=table1; 
   model Yearly_Salary_num = Market_Value Performances 
   / tol vif collin; /* Cette ligne calcule le VIF pour chaque variable explicative */ 
run; 
 
/* 4 - Normalité des résidus */ 
/* Test de shapiro wilk */ 
/* Ajuster le modèle et récupérer les résidus */ 
proc reg data=TABLE1 outest=reg_results noprint; 
    model Yearly_Salary_num = Market_Value Performances; 
    output out=residuals_data r=residuals; 
run; 
%shapiro_wilk_resid(data=residuals_data, residuals=residuals); 
/* Créer un graphique Q-Q pour vérifier la normalité des résidus */ 
%check_normality(data=table1, model=Yearly_Salary_num = Market_Value Performances, depvar=Yearly_Salary_num, indepvars=Market_Value Performances); 
 
/* Modèle 2: */ 
data table1; 
    set table1; 
    /* Crée une variable Position_2 avec des valeurs indicatrices */ 
    if Position = 'Forward' then Position_2 = 1; 
    else if Position = 'Defender' then Position_2 = 2; 
    else if Position = 'Goalkeeper' then Position_2 = 3; 
    else if Position = 'Midfielder' then Position_2 = 4; 
     
       /* Appliquer la transformation logarithmique */ 
    log_Yearly_Salary_num = log(Yearly_Salary_num); 
    log_Market_Value = log(Market_Value); 
    log_Performances = log(Performances); 
run; 
 
/*Ajuster le modèle linéaire avec log-transformation et ajouter les variables pertinentes */ 
ods graphics on; 
proc glm data=table1 plots=all; 
    class Position(ref="Defender"); /* Déclare Defender comme référence */ 
    model log_Yearly_Salary_num = log_Market_Value log_Performances Age Position / solution; 
    lsmeans Position / pdiff adjust=tukey plot=meanplot cl; /* Affiche les moyennes ajustées */ 
    title "Analyse des Salaires par Position avec Log-Transformation"; 
run; 
ods graphics off; 
 
/* Tentatives de correction */ 
/* Les moindres carrées généralisés */ 
ods graphics on; 
/* Modèle simplifié */ 
proc model data=table1; 
    parms b0 b1 b2 b3; 
    log_Yearly_Salary_num = b0  
                          + b1*log_Market_Value  
                          + b2*log_Performances  
                          + b3*Age; 
    instruments log_Market_Value log_Performances Age; 
    fit log_Yearly_Salary_num / gmm kernel=(bart, 6,); 
run; 
ods graphics off; 
 