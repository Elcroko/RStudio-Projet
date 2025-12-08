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








# 1. Liste des colonnes cibles (noms standards R avec points à la place des espaces)
vars_crimes <- c("Homicides", 
                 "Violences.physiques.hors.cadre.familial", 
                 "Vols.avec.armes", 
                 "Vols.de.véhicule", 
                 "Cambriolages.de.logement", 
                 "Trafic.de.stupéfiants")

# 2. Ordonner la catégorie taille (pour que les boxplots soient dans l'ordre logique)
data$Taille_Departement <- factor(data$Taille_Departement, 
                                  levels = c("Petit (<300k)", "Moyen", "Grand (>800k)"))

# 3. Configuration de la fenêtre graphique (2 lignes x 3 colonnes)
par(mfrow = c(2, 3), mar = c(3, 4, 2, 1))

# 4. Boucle : Création Boxplot + Calcul ANOVA pour chaque crime
for (variable in vars_crimes) {
  
  # A. Création du Boxplot
  boxplot(data[[variable]] ~ data$Taille_Departement,
          main = variable,
          col = c("lightblue", "orange", "lightgreen"),
          las = 1,        # Etiquettes verticales à l'endroit
          cex.main = 0.9) # Taille du titre un peu réduite
  
  # B. Calcul et affichage de l'ANOVA dans la console
  cat("\n===========================================\n")
  cat("TEST ANOVA POUR :", variable, "\n")
  res_anova <- aov(data[[variable]] ~ data$Taille_Departement)
  print(summary(res_anova))
}

# Rétablir la configuration graphique par défaut
par(mfrow = c(1, 1))
