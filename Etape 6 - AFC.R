library(readr)
library(dplyr)
library(FactoMineR)
library(factoextra)

# 1. Import des données finales
dat <- read_csv("donnees_finales_projet_2024.csv")

dim(dat)
names(dat)
head(dat)

# 2.1

dat$cat_homicides <- cut(
  dat$Homicides,
  breaks = 3,
  labels = c("Faible_H", "Moyen_H", "Élevé_H")
)
table(dat$cat_homicides)

# 2.2
dat$cat_stups <- cut(
  dat$`Trafic de stupéfiants`,
  breaks = 3,
  labels = c("Faible_S", "Moyen_S", "Élevé_S")
)
table(dat$cat_stups)

# 2.3
dat$delinq_ordinaire <- 
  dat$`Violences physiques hors cadre familial` +
  dat$`Vols avec armes` +
  dat$`Vols de véhicule` +
  dat$`Cambriolages de logement`

summary(dat$delinq_ordinaire)


# 2.4 
dat$cat_ordinaire <- cut(
  dat$delinq_ordinaire,
  breaks = 3,
  labels = c("Faible_O", "Moyen_O", "Élevé_O")
)
table(dat$cat_ordinaire)


#3.1 

tab_stups <- table(dat$Code_region, dat$cat_stups)
tab_stups


#3.2
tab_homicides <- table(dat$Code_region, dat$cat_homicides)
tab_homicides

#4.1

res_stups <- CA(tab_stups, graph = FALSE)

# Inertie (pour le rapport)
res_stups$eig

#4.2
# Biplot (régions + niveaux de stups)
fviz_ca_biplot(res_stups, repel = TRUE,
               title = "AFC - Régions et niveaux de trafic de stupéfiants (2024)")

# Régions seules
fviz_ca_row(res_stups, repel = TRUE,
            title = "AFC - Régions (trafic de stupéfiants)")

# Niveaux de stups seuls
fviz_ca_col(res_stups, repel = TRUE,
            title = "AFC - Niveaux de trafic de stupéfiants")


#5.1 
res_homicides <- CA(tab_homicides, graph = FALSE)

# Inertie (pour le rapport)
res_homicides$eig

#5.2 
fviz_ca_biplot(res_homicides, repel = TRUE,
               title = "AFC - Régions et niveaux d'homicides (2024)")

fviz_ca_row(res_homicides, repel = TRUE,
            title = "AFC - Régions (homicides)")

fviz_ca_col(res_homicides, repel = TRUE,
            title = "AFC - Niveaux d'homicides")


# Pour les stups
res_stups$row$contrib   # régions
res_stups$col$contrib   # niveaux Faible_S / Moyen_S / Élevé_S

# Pour les homicides
res_homicides$row$contrib
res_homicides$col$contrib





