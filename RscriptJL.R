    # Projet R: Le salaire des joueurs de premier league est il corrélé avec sa valeur marchande et ses performances sportives ? 

# Mise en place
getwd()
#Installer package si nécéssaire 
source("Installer_packages.R")
#Activer les packages
source("Activer_packages.R")
                              # Partie 1: Traitement spécifique des données ----
# I - Import et fusion des bases ----

# 1- Import de la base sur les joueurs avec info et stats utile pour performances

base1 <- read.csv("~/Desktop/dataset - 2020-09-24.csv")

# 2- Import de la base avec noms et valeur marchande des joueurs

base2 <- read.csv("~/Desktop/players.csv")

#3- Import de la base avec nom et salaire via un code HTML
base3 <- read_xlsx("/Users/julienlassus/Desktop/Projet R M1ISF/premier_league_salaries_2020_2021_updated.xlsx")


# Assemblage des 3 bases pour une seule
#Suppression de tous les accents pour pouvoir fusionner les bases à l"aide des noms
base1$Name <- stri_trans_general(base1$Name,"Latin-ASCII")
base2$Name <- stri_trans_general(base2$Name,"Latin-ASCII")
base3$Name <- stri_trans_general(base3$Name,"Latin-ASCII")
# 1- Rajout des stats utile de base2  non présentes en base1 (associée au bon joueur)
  # Rajout de la valeure marchande dans base1
base1_etVM <- base1 %>%
  left_join(base2 %>% select(Name,Markey.Value.In.Millions...),by = "Name")
head(base1_etVM) #verifiction ok
summary(base1_etVM)


# 2- Rajout du salaire pour tous les joueurs 
# a - Renommer les variables pou cohérence
colnames(base3)[colnames(base3) == "Player"] <- "Name"
colnames(base3)[colnames(base3) == "Weekly Salary"] <- "Weekly_Salary"
colnames(base3)[colnames(base3) == "Yearly Salary"] <- "Yearly_Salary"
# b - fusion
basefull <- base1_etVM %>%
  left_join(base3 %>% select(Name,Weekly_Salary,Yearly_Salary),by ="Name")
#Vérif si le salaire est bon
basefull[572,]

                  # II - Nettoyage de la base -----
# Valeurs manquantes: 
# Lorsque la valeur marchande n'est pas indiqué cela signifie que la VM <16.8 et n'est donc pas dispo
# Nous allons donc y mettre une valeur moyenne
base1_etVM <- base1_etVM %>%
  mutate(Markey.Value.In.Millions...= ifelse(is.na(Markey.Value.In.Millions...),8.4,Markey.Value.In.Millions...))
base1_etVM$Markey.Value.In.Millions...[1:10] #Fonctionne

#Obtenir le nom de tous les joueurs ou le salaire n'est pas renseigné
joueurs_sans_salaire <- c()
for (i in 1:nrow(basefull)){
  if(is.na(basefull$Weekly_Salary[i])){
    joueurs_sans_salaire <- c(joueurs_sans_salaire,basefull$Name[i])
  }
}
print(joueurs_sans_salaire) #101 joueurs 
# A - Joueur en pret ou transférer tardivement - pas en PL cette année la (dans base1) mais pas dans base3
base1_etVM <- base1_etVM[!base1_etVM$Name %in% joueurs_sans_salaire, ] 
print(joueurs_sans_salaire) #Plus de joueurs sans salaire

# B - Suppression des accents,fusion base3 et base1etVM les joueurs dont le nom comporte des accents dans base3 n'ont pas été fusionné
# C - Les différence majuscule / minuscules empechent fusion 
base3$Name <- replace(base3$Name,base3$Name == "David De Gea","David de Gea") 
base3$Name <- replace(base3$Name,base3$Name == "Bobby De Cordova-Reid","Bobby de Cordova-Reid")
base3$Name <- replace(base3$Name,base3$Name == "Heung-min Son","Son Heung-Min") 
base3$Name <- replace(base3$Name,base3$Name == "Ahmed Elmohamady","Ahmed El Mohamady")
# NA normal ou le joueur en à 0 (exemple: gardien - 0 but) - remplacer par 0
colonnes_anormales <- c("Shooting.accuracy","Cross.accuracy")
basefull[,!(names(basefull) %in% colonnes_anormales)] <- lapply(
  basefull[ , !(names(basefull) %in% colonnes_anormales)], 
  function(x) ifelse(is.na(x), 0, x)
)

# NA anormales à corriger 
# a - Goals per match
for (i in 1:nrow(basefull)){
  if(is.na(basefull$Goals.per.match[i])){
    if(basefull$Appearances[i] != 0) {
    basefull$Goals.per.match[i] <- basefull$Goals[i] / basefull$Appearances[i]
    }
    else {
      basefull$Goals.per.match[i] <- 0
    }
  }
}
basefull$Goals.per.match <- ceiling(basefull$Goals.per.match* 100) / 100# Arrondir au centième supérieur
#Correction d'erreur présentes dans le base initiale (Ex: 0/42 = 42)

#Correction
for (i in 1:nrow(basefull)){
  if(basefull$Goals.per.match[i] > 1){
  basefull$Goals.per.match[i] <- basefull$Goals[i] / basefull$Appearances[i]
}
}
for (i in 1:nrow(basefull)){
  if(basefull$Goals.per.match[i] == 1 & basefull$Goals[i] == 0 ){
    basefull$Goals.per.match[i] <- 0
  }
}

# b - shooting accuracy
for (i in 1:nrow(basefull)){
  if(basefull$Shooting.accuracy..[i] == ""){
    if(basefull$Shots[i] != 0) {
      basefull$Shooting.accuracy..[i] <- basefull$Shots.on.target[i] / basefull$Shots[i]
    }
    else {
      basefull$Shooting.accuracy..[i] <- 0
    }
  }
}



#Suppression des joueurs doublons dans basefull 
basefull <- basefull %>% distinct(Name, .keep_all = TRUE)

                                        # III - Modification et création de variable  ----
#Détection de valeurs abberantes dans base1 
boxplot(base1$Goals.per.match, main="Boite à moustache du goals / match", ylab="Valeurs", col = "lightblue",notch = TRUE)
outliers <- boxplot.stats(base1$Goals.per.match)$out
print("Valeurs aberrantes :")
print(outliers)
numeric_columns <- basefull[, sapply(basefull, is.numeric)]
boxplot(numeric_columns, main = "Détection des valeurs aberantes")
boxplot(basefull$Yearly_Salary_num, main = "Détection des valeurs aberantes")
boxplot(basefull$Goals, main = "Détection des valeurs abberantes")
boxplot(basefull$Markey.Value.In.Millions..., main = "Détection des valeurs abberantes")
boxplot(basefull$Performances, main = "Détection des valeurs abberantes")

# Correction des valeurs prises comme characters alors que numérique
#Pour salaire annuel
# 1. Retirer le symbole de l'euro (€) et remplacer la virgule par un point
basefull$Yearly_Salary <- gsub("€", "", basefull$Yearly_Salary)  # Retirer €
basefull$Yearly_Salary <- gsub(",", ".", basefull$Yearly_Salary)  # Remplacer , par .
basefull$Yearly_Salary_num <- gsub("\\.","",basefull$Yearly_Salary)
# 2. Convertir en numérique
basefull$Yearly_Salary_num <- as.numeric(basefull$Yearly_Salary_num)
summary(basefull$Yearly_Salary_num) #Fonctionne
#Pour salaire hebdomadaire
basefull$Weekly_Salary <- gsub("€", "", basefull$Weekly_Salary)
basefull$Weekly_Salary <- gsub(",", ".", basefull$Weekly_Salary) 
basefull$Weekly_Salary_num <- gsub("\\.","",basefull$Weekly_Salary)
basefull$Weekly_Salary_num <- as.numeric(basefull$Weekly_Salary_num)
summary(basefull$Weekly_Salary_num) #Fonctionne
#Suppression ancienne colonne 
basefull <- basefull %>%
  select(-Yearly_Salary,-Weekly_Salary)


# Création d'indicateurs de performances -> note sur 100 (performannce) pour chaque position
# Fonction de normalisation (convertit les valeurs sur 0 - 1)
normalise <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) #na.rm évité les valeurs manquantes
}
#Midfielder: Wins,  shots, shots on target, Interceptions, Duels Won, goals, reds card, yellow cards, Fouls, Assist, Big chances created
# Filtrer les Midfielders (milieux de terrain)
midfielders <- basefull %>% filter(Position == "Midfielder")
# Normaliser les variables sélectionnées pour les Midfielders
midfielders <- midfielders %>%
  mutate(
    Wins_norm = normalise(Wins),
    Shots_norm = normalise(Shots),
    Shots_on_Target_norm = normalise(Shots.on.target),
    Interceptions_norm = normalise(Interceptions),
    Duels_Won_norm = normalise(Duels.won),
    Goals_norm = normalise(Goals),
    Reds_Cards_norm = 1 - normalise(Red.cards),  # Inverser pour que moins de cartons soit mieux
    Yellow_Cards_norm = 1 - normalise(Yellow.cards),  # Idem
    Fouls_norm = 1 - normalise(Fouls),  # Moins de fautes est mieux
    Assists_norm = normalise(Assists),
    Big_Chances_Created_norm = normalise(Big.chances.created)
  )
#Définir des poids en fonction de l'importance de chaque statistique
# Définir les poids pour chaque statistique
poids <- list(
  Wins = 0.1,
  Shots = 0.1,
  Shots_on_Target = 0.05,
  Interceptions = 0.1,
  Duels_Won = 0.1,
  Goals = 0.2, #Goals plus important 
  Red_Cards = 0.05,
  Yellow_Cards = 0.05,
  Fouls = 0.05,
  Assists = 0.15,
  Big_Chances_Created = 0.1
)
# Créer une colonne performances midlefield dans basefull avec note de perfromance / 100
# Calculer la note de performance pour chaque milieu de terrain
midfielders <- midfielders %>%
  mutate(
    Performances_Midfielder = (
      poids$Wins * Wins_norm +
        poids$Shots * Shots_norm +
        poids$Shots_on_Target * Shots_on_Target_norm +
        poids$Interceptions * Interceptions_norm +
        poids$Duels_Won * Duels_Won_norm +
        poids$Goals * Goals_norm +
        poids$Red_Cards * Reds_Cards_norm +
        poids$Yellow_Cards * Yellow_Cards_norm +
        poids$Fouls * Fouls_norm +
        poids$Assists * Assists_norm +
        poids$Big_Chances_Created * Big_Chances_Created_norm
    ) * 100  # Note sur 100
  )

# Réintégrer cette colonne dans la base initiale
basefull <- basefull %>%
  left_join(midfielders %>% select(Name, Performances_Midfielder), by = "Name") # Fonctionne - NA pour autres postes

# meme méthode pour les autres postes: 
# Gardien: Wins, clean sheets,Punches, goals conceded, error leading to a goals, saves, penalty saved,
goalkeeper <- basefull %>% filter(Position == "Goalkeeper")
goalkeeper <- goalkeeper %>%
  mutate(
    Wins_norm = normalise(Wins),
    CleanS_norm = normalise(Clean.sheets),
    Punches_norm = normalise(Punches),
    GoalsC_norm = 1 - normalise(Goals.conceded/Appearances),  # Inverser pour que moins de but concédée soit mieux
    Errors_norm = 1 - normalise(Errors.leading.to.goal),  # Idem
    PenaltiesS_norm = normalise(Penalties.saved)
  )
poids2 <- list(
  Wins = 0.15,
  CleanS = 0.25,
  Punches = 0.05,
  GoalsC = 0.2,
  Errors = 0.2,
  PenaltiesS = 0.15
)
goalkeeper <- goalkeeper %>%
  mutate(
    Performances_Goalkeeper = (
      poids2$Wins * Wins_norm +
        poids2$CleanS * CleanS_norm +
        poids2$Punches * Punches_norm +
        poids2$GoalsC * GoalsC_norm +
        poids2$Errors * Errors_norm +
        poids2$PenaltiesS * PenaltiesS_norm
    ) * 100  # Note sur 100
  )
basefull <- basefull %>%
  left_join(goalkeeper %>% select(Name, Performances_Goalkeeper), by = "Name")

#Defender: Wins, Tackles, Last man tackles, Blocked shots,Duels won/ lost, Aerials battles won/lost, Own goals, error leading to goals, Fouls, Yellow card, Red cards
defender <- basefull %>% filter(Position == "Defender")
defender <- defender %>%
  mutate(
    Wins_norm = normalise(Wins),
    Tackles_norm = normalise(Tackles),
    LastMT_norm = normalise(Last.man.tackles),
    BlockedShots_norm = normalise(Blocked.shots),
    DuelsWon_norm = normalise(Duels.won),
    Duelslost_norm = 1 - normalise(Duels.lost),
    AerialsWon_norm = normalise(Aerial.battles.won),
    AerialsLost_norm = 1 - normalise(Aerial.battles.lost),
    Owngoals_norm = 1 - normalise(Own.goals),
    Errors_norm = 1 - normalise(Errors.leading.to.goal),
    Fouls_norm = 1 - normalise(Fouls),
    Reds_norm = 1 - normalise(Red.cards),# Inverser pour que moins de cartons soit mieux
    Yellow_norm = 1 - normalise(Yellow.cards)# Inverser pour que moins de cartons soit mieux
  )
poids3 <- list(
  Wins = 0.1,
  Tackles = 0.025,
  LastMT = 0.1, 
  BlockedShots = 0.05,
  Duelswon = 0.075,
  Duelslost = 0.05, 
  Aerialswon = 0.025,
  Aeriallost = 0.025, 
  Owngoals = 0.15,
  Errors = 0.1,
  Fouls = 0.05, 
  Yellow = 0.1, 
  Red = 0.15
)
defender <- defender %>%
  mutate(
    Performances_Defender = ( 
      poids3$Wins * Wins_norm + 
        poids3$Tackles * Tackles_norm +
        poids3$LastMT * LastMT_norm + 
        poids3$BlockedShots * BlockedShots_norm + 
        poids3$Duelswon * DuelsWon_norm + 
        poids3$Duelslost * Duelslost_norm +
        poids3$Aerialswon * AerialsWon_norm +
        poids3$Aeriallost * AerialsLost_norm +
        poids3$Owngoals * Owngoals_norm + 
        poids3$Errors * Errors_norm +
        poids3$Fouls * Fouls_norm +
        poids3$Yellow * Reds_norm +
        poids3$Red * Yellow_norm
    ) * 100
  )
basefull <- basefull %>%
  left_join(defender %>% select(Name, Performances_Defender), by = "Name")

#Forward:Wins, Goals, Goals per match, penalties scored, Freekicks scored , Shots on target, Big chances missed, Assist, Big chances created, Offsides
forward <- basefull %>% filter(Position == "Forward")
forward <- forward %>% 
  mutate(
    Wins_norm = normalise(Wins),
    Shots_on_Target_norm = normalise(Shots.on.target),
    GoalsPM_norm = normalise(Goals.per.match),
    Goals_norm = normalise(Goals),
    PenaltiesS_norm = normalise(Penalties.scored),
    FreekicksS_norm = normalise(Freekicks.scored), 
    Offsides_norm = 1 - normalise(Offsides), #Moins d'offside est mieux 
    Assists_norm = normalise(Assists),
    Big_Chances_Created_norm = normalise(Big.chances.created),
    Big_Chances_missed_norm = 1 - normalise(Big.chances.missed)
  )
poids4 <- list(
  Wins = 0.05,
  Shots_on_Target = 0.05,
  GoalsPM = 0.1,
  Goals = 0.2, #Goals plus important 
  PenaltiesS = 0.15,
  FreekicksS = 0.2,
  Ofssides = 0.025,
  Assists = 0.2,
  BigChancesC = 0.15,
  BigChancesM = 0.1
)
forward <- forward %>%
  mutate(
    Performances_Forward =(
      poids4$Wins * Wins_norm +
        poids4$Shots_on_Target * Shots_on_Target_norm +
        poids4$GoalsPM * GoalsPM_norm +
        poids4$Goals * Goals_norm +
        poids4$PenaltiesS * PenaltiesS_norm +
        poids4$FreekicksS * FreekicksS_norm +
        poids4$Ofssides * Offsides_norm +
        poids4$Assists * Assists_norm +
        poids4$BigChancesC * Big_Chances_Created_norm + 
        poids4$BigChancesM * Big_Chances_missed_norm 
    ) * 100
  )
basefull <- basefull %>%
  left_join(forward %>% select(Name, Performances_Forward),by = "Name")

#Pour terminer fusionner les 4 bases en une colonne performance
basefull <- basefull %>%
  mutate(
    Performances = coalesce(Performances_Midfielder, Performances_Forward, Performances_Goalkeeper, Performances_Defender)
  ) 
basefull <- basefull %>%
  select(-Performances_Defender,-Performances_Goalkeeper,-Performances_Forward,-Performances_Midfielder) #Tjrs dans Defender, Forward...

summary(basefull$Performances)
                                                  # Partie 2 - Statistiques descriptives  ----

# I - Résumé stat des variables les plus importantes ----
summary(basefull)
summary(basefull$Yearly_Salary_num)
summary(basefull$Markey.Value.In.Millions...)
summary(basefull$Goals)
summary(basefull$Performances)
# Affichage graphique
summary_stats <- as.data.frame(t(summary(basefull$Markey.Value.In.Millions...)))
formattable(summary_stats, 
            list(`Min.` = color_tile("white", "lightblue"),
                 `Mean` = color_tile("white", "lightgreen"),
                 `Max.` = color_tile("white", "lightpink")),
            caption = "Résumé des salaires annuels")


# Calcul des écarts types des variables souhaitées
std_devs <- sapply(basefull[, c("Yearly_Salary_num", "Markey.Value.In.Millions...", "Performances")], sd)
#Tableau en lateX contenant les écarts types: 
std_devs <- format(std_devs, scientific = FALSE, big.mark = ",", digits = 2)
std_devs_df <- data.frame(Variables = names(std_devs), `Ecart-type` = std_devs)
# Affichage avec kable pour LaTeX
kable(std_devs_df, format = "latex", booktabs = TRUE, 
      caption = "Écarts-types des variables sélectionnées")

# II - Analyse graphiques élémentaire----
#Relation entre salaire et valeurs marchandes ?
ggplot(basefull, aes(x = Yearly_Salary_num, y = Markey.Value.In.Millions...)) +
  geom_point(color = "blue") +
  labs(title = "Salaire vs Valeur Marchande des Joueurs",
       x = "Salaire Annuel (€)", 
       y = "Valeur Marchande (Millions €)") +
  theme_minimal()
ggplot(basefull) +
  aes(x = Yearly_Salary_num, y = Markey.Value.In.Millions...) +
  geom_point(colour = "blue", alpha = .25) +
  geom_smooth(method = "lm") +
  labs(x = "Salaire en €", y = "VM joueurs") +
  theme_light() #avec droite linéaire
#Relation entre performances et salaire
ggplot(basefull, aes(x = Yearly_Salary_num, y = Performances)) +
  geom_point(color = "blue") +
  labs(title = "Salaire vs Performances",
       x = "Salaire Annuel (€)", 
       y = "Performances sur 100") +
  theme_minimal()
#relation nombre de match et salaire
ggplot(basefull, aes(x = Yearly_Salary_num, y = Appearances)) +
  geom_point(color = "blue") +
  labs(title = "Salaire vs Nombre de match",
       x = "Salaire Annuel (€)", 
       y = "Nombre de match en carriére") +
  theme_minimal()
#relation nombre de victoires en carrière et salaire
ggplot(basefull, aes(x = Yearly_Salary_num, y = Wins)) +
  geom_point(color = "blue") +
  labs(title = "Salaire vs Nombre de victoire en carrière",
       x = "Salaire Annuel (€)", 
       y = "Nombre de victoire en carriére") +
  theme_minimal()
#Graphique nombre de match et nombre de victoire
ggplot(basefull, aes(x = Appearances, y = Wins)) +
  geom_point(color = "blue") +
  labs(title = "Nombre de match vs Nombre de victoire en carrière",
       x = "Nombre de match", 
       y = "Nombre de victoire en carriére") +
  theme_minimal()

# III - Matrice de corrléation ----
num_int_vars <- basefull[, sapply(basefull, function(x) is.numeric(x) || is.integer(x))]
cor_matrice <- cor(num_int_vars, use = "complete.obs")
corrplot(cor_matrice,type="lower")
corrplot(cor_matrice,method="number") #Illisible 

#10 variables les plus corrélé avec Salaire
cor_salary <- cor_matrice["Yearly_Salary_num", ]
# Trier les corrélations par ordre décroissant (proche de 1)
sorted_cor <- sort(cor_salary, decreasing = TRUE)
# Extraire les 10 plus proches de 1
top10 <- head(sorted_cor[sorted_cor < 1], 10)
print(top10)
top10_vars <- names(top10)
cor_matrix_top10 <- cor_matrice[c("Yearly_Salary_num", top10_vars), c("Yearly_Salary_num", top10_vars)]
#graphique
corrplot(cor_matrix_top10, method = "color", type = "upper", 
         tl.col = "black", tl.cex = 0.8, addCoef.col = "black", 
         title = "Matrice de corrélation : Yearly_Salary_num et 10 variables les plus corrélées")

# Nos variables d'intérêts
data_cor <- basefull %>%
  select(Yearly_Salary_num, Markey.Value.In.Millions..., Performances)
cor_matrix <- cor(data_cor, use = "complete.obs")
print(cor_matrix)
ggcorrplot(cor_matrix, method = "circle", lab = TRUE)

# IV - Les variables suivent-elles une loi normales ? ----
shapiro.test(basefull$Yearly_Salary_num) 
shapiro.test(basefull$Markey.Value.In.Millions...) 
shapiro.test(basefull$Wins)
shapiro_test <- shapiro.test(basefull$Performances)

# Sélection des variables spécifiques
selected_vars <- basefull[, c("Yearly_Salary_num", "Markey.Value.In.Millions...", 
                              "Performances", "Wins", "Goals", 
                              "Big.chances.created", "Appearances")]
# Fonction pour effectuer le test de Shapiro-Wilk et collecter les résultats
shapiro_results <- sapply(selected_vars, function(x) {
  if (length(unique(x)) > 3) { # Le test de Shapiro requiert au moins 3 valeurs distinctes
    test <- shapiro.test(x)
    c(statistic = test$statistic, p_value = test$p.value)
  } else {
    c(statistic = NA, p_value = NA)
  }
})
# Transposer et convertir les résultats en data frame
shapiro_results_df <- as.data.frame(t(shapiro_results))
# Ajouter une colonne pour indiquer si la distribution est normale (p-value > 0.05)
shapiro_results_df$Normal <- ifelse(shapiro_results_df$p_value > 0.05, "Yes", "No")
# Renommer les colonnes
colnames(shapiro_results_df) <- c("Shapiro-W Statistic", "p-value", "Normal Distribution")
# Formater les valeurs numériques
shapiro_results_df <- shapiro_results_df %>%
  mutate(across(where(is.numeric), ~ round(., 4))) # Arrondir à 4 décimales
# Créer le tableau avec kableExtra pour un rendu visuel agréable
kable(shapiro_results_df, caption = "Shapiro-Wilk Test for Selected Variables") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:3, width = "5em") %>%
  row_spec(0, bold = TRUE)

# V - Test Anova ----
# 1- catégorisation de "Appearances"
basefull$Appearances_Cat <- cut(basefull$Appearances, breaks = 4, labels = c("Low", "Medium", "High", "Very High"))
# ANOVA pour examiner l'effet des apparitions catégorisées sur le salaire annuel
anova_appearances <- aov(Yearly_Salary_num ~ Appearances_Cat, data = basefull)
summary(anova_appearances)
#2 - # Catégoriser plusieurs variables
basefull$Performances_Cat <- cut(basefull$Performances, breaks = 4, labels = c("Low", "Medium", "High", "Very High"))
basefull$Wins_Cat <- cut(basefull$Wins, breaks = 4, labels = c("Low", "Medium", "High", "Very High"))
# ANOVA factorielle : Performances et Wins catégorisées sur Yearly Salary
anova_factorial <- aov(Yearly_Salary_num ~ Performances_Cat * Wins_Cat, data = basefull)
summary(anova_factorial)
stargazer(anova_factorial, types="text")
# Extraire le tableau ANOVA
anova_results <- summary(anova_factorial)
# Convertir en data.frame pour kable
anova_df <- as.data.frame(anova_results[[1]])
# Utiliser kable pour afficher les résultats
kable(anova_df, format = "latex", caption = "Tableau des résultats ANOVA", digits = 4)

# VI - Graphique complémentaire ----
#Histogramme des salaires par club
ggplot(basefull, aes(x = Club, y = Yearly_Salary_num)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogramme des salaires par club",
       x = "Club",
       y = "Salaire annuel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Histogramme des salaires par position
ggplot(basefull, aes(x = Position, y = Yearly_Salary_num)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Histogramme des salaires par position",
       x = "Club",
       y = "Salaire annuel") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


                                        #Partie 3 - Économétrie ----

# I - Estimation économétrique: modéle 1 et 2 ----
mod1 <- lm(
  Yearly_Salary_num ~ Markey.Value.In.Millions... + Performances,
  data = basefull
) 
#1 - Test et données de base du modèle 
summary(mod1)
stargazer(mod1, types="text",out="modeles.txt")
check_outliers(mod1)
check_model(mod1)

#Affichage graphique
mod1 %>%
  tbl_regression(intercept = TRUE)
ggcoef_model(mod1)

# 2 - Hétéroscédasticité: Test de Breush - Pagan
bptest(mod1) # >0.05 
#par graphique des résidus 
x7 <- check_heteroscedasticity(mod1)
plot(x7)
# 2bis - Via Goldfeld-Quandt: voir si la var explicatives Performances est responsable
# Trier les données en fonction de la variable 'Performances'
base_sorted <- basefull[order(basefull$Performances), ]
# Ajuster le modèle de régression avec les données triées
mod_sorted <- lm(Yearly_Salary_num ~ Markey.Value.In.Millions... + Performances, data = base_sorted)
# Appliquer le test de Goldfeld-Quandt sur le modèle trié par 'Performances'
gqtest(mod_sorted, order.by = base_sorted$Performances)


# 3 - Autocorrélation: Test de Durbin - Watson 
dwtest(mod1) #AC positive (<2 et pvalue très faible) manque de variables explicatives pertinente ? 
check_autocorrelation(mod1)
acf(residuals(mod1))

#4 - Multicolinéarité: Variance Inflation Factor
x4 <- check_collinearity(mod1) 
plot(x4)
vif(mod1) #pas de multicolinéarité (proche de 1) 
check_collinearity(mod1)

#5 - Normalité des résidus
shapiro.test(residuals(mod1)) # pvalue <0.05 pas normalement distribuées ?
x5 <- check_normality(mod1)
x5

#Modéle 2: Linéarisation + rajout d'une variable pertinente et variable position (dummy) 
basefull$Position_2 <- as.factor(basefull$Position) #R transforme variable factor en dummy automatiquement

mod2 <- lm(
  log(Yearly_Salary_num) ~ log(Markey.Value.In.Millions...) + log(Performances) + Age + Position_2,
  data= basefull
  )
summary(mod2) 

#Graphique pvalue, valeur coef et intervalle de confiance
mod2 %>%
  tbl_regression(intercept = TRUE)
ggcoef_model(mod2) 
check_model(mod2)

#Test
check_heteroscedasticity(mod2)
check_autocorrelation(mod2)
check_collinearity(mod2)
check_normality(mod2)
bptest(mod2)
dwtest(mod2)
bgtest(mod2)
vif(mod2)
shapiro.test(residuals(mod2))

#Tentaives de correction: ----

#1 - Erreur robuste de N&W ----
c0 <- coeftest(mod2)
c1 <- coeftest(mod2, vcov = NeweyWest(mod2, lag = 1, prewhite = FALSE))#Correction avec estimateur robuste de Newey et West
# Comparer les résultats dans un tableau
# Extraire les statistiques et p-values
comparaison_table <- data.frame(
  Variable = rownames(c0),
  Estimate = c0[, "Estimate"],
  `Std. Error (Classique)` = c0[, "Std. Error"],
  `t value (Classique)` = c0[, "t value"],
  `p-value (Classique)` = c0[, "Pr(>|t|)"],
  `Std. Error (Newey-West)` = c1[, "Std. Error"],
  `t value (Newey-West)` = c1[, "t value"],
  `p-value (Newey-West)` = c1[, "Pr(>|t|)"]
)

# Créer le tableau graphique avec ggplot2
table_plot <- ggplot(data = comparaison_table, aes(x = Variable)) +
  theme_void() +
  annotation_custom(tableGrob(comparaison_table, rows = NULL), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  labs(title = "Comparaison des Statistiques de Test et p-values (Classique vs. Newey-West)")
print(table_plot)

#2 - Correction via MCG: ----
mod2_gls <- gls(
  log(Yearly_Salary_num) ~ log(Markey.Value.In.Millions...) + log(Performances) + Age + Position_2,
  data = basefull,
  correlation = corAR1(),    # Corrige pour l'autocorrélation de type AR(1)
  weights = varIdent(form = ~ 1 | Position_2)  # Corrige pour l'hétéroscédasticité par position
)
summary(mod2_gls)
stargazer(mod2_gls, types="latex",out="modeles2MCG.txt")








