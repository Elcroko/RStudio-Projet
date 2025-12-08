library(dplyr)

data <- read.csv("donnees_finales_projet_2024.csv")

# 1. Calcul des moyennes par 'Taille_Departement'
# - group_by() regroupe les données.
# - summarise() applique une fonction de résumé (ici, la moyenne).
# - across() permet d'appliquer la fonction mean() à toutes les colonnes à la fois.
# - Le sélecteur .cols = -c(Code_departement, Code_region) exclut les colonnes d'identification 
#   (Code_departement et Code_region) du calcul de la moyenne.
# - .names = "Moyenne_{.col}" renomme les nouvelles colonnes.

moyennes_par_taille <- data %>%
  group_by(Taille_Departement) %>%
  summarise(
    across(
      # Exclut les colonnes d'identification (et la colonne de regroupement est ignorée par summarise)
      .cols = -c(Code_departement, Code_region), 
      .fns = ~mean(.x, na.rm = TRUE), # Calcul de la moyenne en ignorant les valeurs manquantes (NA)
      .names = "Moyenne_{.col}"      
    ),
    .groups = 'drop' # Supprime le regroupement pour obtenir un dataframe simple
  )

# 2. Affichage du tableau de résultats
print(moyennes_par_taille, n = Inf, width = Inf)
