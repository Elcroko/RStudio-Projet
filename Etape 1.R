
# ANALYSE UNIVARIÉE QUANTITATIVE

# 1. CHARGEMENT DES DONNÉES
# ------------------------------------------------------------------------------
# On charge le fichier généré précédemment
# check.names = FALSE permet de garder les espaces dans les noms de colonnes (ex: "Vols avec armes")
datadep <- read.csv("donnees_finales_projet_2024.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Affichage pour vérification
View(datadep)       # Ouvre le tableau dans RStudio
str(datadep)        # Affiche la structure des variables

# ------------------------------------------------------------------------------
# 2. SÉLECTION DES VARIABLES QUANTITATIVES
# ------------------------------------------------------------------------------
# On isole les colonnes numériques (Population + les 6 crimes) pour faciliter les calculs
# Attention : adapte les indices des colonnes si besoin (ici c'est souvent col 3 à 9)
cols_quanti <- c("Population", 
                 "Homicides", 
                 "Violences physiques hors cadre familial", 
                 "Vols avec armes", 
                 "Vols de véhicule", 
                 "Cambriolages de logement", 
                 "Trafic de stupéfiants")

data_quanti <- datadep[, cols_quanti]

# ------------------------------------------------------------------------------
# 3. CALCULS STATISTIQUES (Moyennes, Médianes, Variances)
# ------------------------------------------------------------------------------

# Option A : Le résumé rapide (Min, Quartiles, Moyenne, Max)
print("--- Résumé Statistique Global ---")
summary(data_quanti)

# Option B : Calculs précis stockés dans un tableau (Idéal pour copier dans ton rapport)
stats_tableau <- data.frame(
  Variable  = names(data_quanti),
  Moyenne   = apply(data_quanti, 2, mean, na.rm = TRUE),
  Mediane   = apply(data_quanti, 2, median, na.rm = TRUE),
  Variance  = apply(data_quanti, 2, var, na.rm = TRUE),
  EcartType = apply(data_quanti, 2, sd, na.rm = TRUE) # L'écart-type est souvent plus parlant
)

print("--- Tableau des Indicateurs de Tendance Centrale et Dispersion ---")
print(stats_tableau)

# Astuce : Pour voir quel département a le max d'Homicides
max_homicides <- max(datadep$Homicides)
dept_max <- datadep$Code_departement[datadep$Homicides == max_homicides]
cat("\nLe record d'Homicides (taux) est détenu par le département :", dept_max, "avec", max_homicides, "pour 1000 hab.\n")

# ------------------------------------------------------------------------------
# 4. HISTOGRAMMES (Distributions)
# ------------------------------------------------------------------------------
# On affiche les 6 crimes sur une même page pour comparer les "allures" (loi normale ou non ?)

par(mfrow = c(2, 3)) # Découpe la fenêtre graphique en 2 lignes, 3 colonnes

# Boucle pour tracer les histogrammes des crimes (on exclut Population ici)
for (col in cols_quanti[-1]) { # [-1] pour ne pas tracer la population
  hist(datadep[[col]], 
       main = paste("Distri.", col),
       xlab = "Taux pour 1000 hab.",
       ylab = "Fréquence",
       col = "skyblue",
       border = "white",
       cex.main = 0.8) # Réduit la taille du titre pour qu'il rentre
}

# ------------------------------------------------------------------------------
# 5. BOXPLOTS (Boîtes à moustaches & Outliers)
# ------------------------------------------------------------------------------
# Utile pour repérer les départements extrêmes

par(mfrow = c(1, 1)) # On remet la fenêtre en pleine page

# A. Boxplot global comparatif (Attention aux échelles très différentes !)
# On exclut "Vols de véhicule" et "Cambriolages" qui sont bcp plus hauts que "Homicides",
# sinon on ne verra rien. On fait des groupes par échelle.

# Groupe 1 : Les crimes "rares" (Homicides, Vols armes)
boxplot(datadep[, c("Homicides", "Vols avec armes")], 
        col = c("red", "orange"),
        main = "Dispersion des Crimes Rares (2024)",
        ylab = "Taux pour 1000 hab.")

# Groupe 2 : Les crimes "fréquents" (Cambriolages, Vols voiture, Violences physiques)
boxplot(datadep[, c("Cambriolages de logement", "Vols de véhicule", "Violences physiques hors cadre familial")], 
        col = c("lightgreen", "gold", "purple"),
        main = "Dispersion des Délits de Masse (2024)",
        names = c("Cambriolages", "Vols Auto", "Violences"), # Raccourcir les noms
        ylab = "Taux pour 1000 hab.")

# B. Identification des valeurs aberrantes pour une les Homicides
boxplot(datadep$Homicides, 
        main = "Zoom sur les Homicides : Qui sont les points noirs ?",
        col = "red",
        horizontal = TRUE) # Horizontal pour mieux lire

# On ajoute le texte des départements extrêmes sur le graphe
outliers <- boxplot.stats(datadep$Homicides)$out
if(length(outliers) > 0){
  text(x = outliers, 
       y = 1.1, 
       labels = datadep$Code_departement[datadep$Homicides %in% outliers], 
       col = "blue", 
       cex = 0.8)
}

# Identification des valeurs aberrantes pour les vols avec armes
boxplot(datadep$`Vols avec armes`, 
        main = "Zoom sur les vols avec armes : Qui sont les points noirs ?",
        col = "purple",
        horizontal = TRUE) # Horizontal pour mieux lire

# On récupère les valeurs statistiques des outliers
outliers <- boxplot.stats(datadep$`Vols avec armes`)$out

# On ajoute le texte SEULEMENT s'il y a des outliers
if(length(outliers) > 0){
  text(x = outliers, 
       y = 1.1, # On place le texte un peu au-dessus de la boîte (y=1)
       # On filtre pour trouver le Code Département correspondant à ces valeurs
       labels = datadep$Code_departement[datadep$`Vols avec armes` %in% outliers], 
       col = "blue", 
       cex = 0.8,    # Taille du texte
       pos = 3)      # pos=3 met le texte au-dessus du point
}
