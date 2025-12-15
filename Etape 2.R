############################################
# Nettoyage de l'environnement de travail
############################################

# Suppression de tous les objets présents dans l'espace de travail
rm(list = ls())

# Libération de la mémoire
gc()


############################################
# Importation des données
############################################

# Lecture du fichier CSV contenant les données du projet
# stringsAsFactors = FALSE permet d'éviter la conversion automatique en facteurs
data_proj <- read.csv("donnees_finales_projet_2024.csv", stringsAsFactors = FALSE)


############################################
# Analyse de la répartition des régions
############################################

# Création d'une table de fréquence des régions
# Code_region correspond au code INSEE des régions
tab <- table(data_proj$Code_region)

# Représentation graphique : diagramme en barres
barplot(
  tab,
  main = "Fréquence des régions",
  xlab = "Code INSEE des régions",
  ylab = "Effectif",
  las = 2            # Orientation verticale des labels pour une meilleure lisibilité
)


############################################
# Réinitialisation de l'environnement
############################################

rm(list = ls())
gc()


############################################
# Importation des données (à nouveau)
############################################

data_proj <- read.csv("donnees_finales_projet_2024.csv", stringsAsFactors = FALSE)


############################################
# Analyse de la taille des départements
############################################

# Table de fréquence des catégories de taille des départements
tab_taille <- table(data_proj$Taille_Departement)

# Affichage de la table de fréquence
print(tab_taille)

# Calcul des pourcentages par catégorie
pourcentages <- prop.table(tab_taille) * 100

# Affichage des pourcentages arrondis à 1 décimale
print(round(pourcentages, 1))


############################################
# Représentation graphique : diagramme en barres
############################################

barplot(
  tab_taille,
  main = "Répartition des Départements par Taille",
  xlab = "Catégorie de taille",
  ylab = "Nombre de départements",
  col = "steelblue",
  border = NA,
  las = 1
)


############################################
# Représentation graphique : diagramme circulaire
############################################

# Création des étiquettes avec les pourcentages
labels_pie <- paste(
  names(tab_taille),
  "\n",
  round(pourcentages, 1),
  "%"
)

# Diagramme circulaire
pie(
  tab_taille,
  labels = labels_pie,
  main = "Répartition par Taille de Département",
  col = c("#66c2a5", "#fc8d62", "#8da0cb") # Couleurs distinctes
)
