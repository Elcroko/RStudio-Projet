# --- SETUP ---
# install.packages(c("FactoMineR", "factoextra")) # A faire une seule fois
library(FactoMineR)
library(factoextra)

# --- IMPORTATION ---
# On charge le nouveau fichier final
df <- read.csv("donnees_finales_projet_2024.csv", 
               row.names = 1, # La colonne 1 (Code_departement) sert d'identifiant
               check.names = FALSE, 
               encoding = "UTF-8")

# --- SELECTION DES VARIABLES ---
# On garde les 6 variables quantitatives (colonnes 4 à 9)
# On ignore 'Population', 'Code_region', et 'Taille_Departement' pour l'ACP
vars_active <- c("Homicides", 
                 "Violences physiques hors cadre familial", 
                 "Vols avec armes", 
                 "Vols de véhicule", 
                 "Cambriolages de logement", 
                 "Trafic de stupéfiants")

df_acp <- df[, vars_active]

# --- CALCUL ACP ---
# scale.unit = TRUE est crucial pour comparer des homicides (petits chiffres) et des cambriolages (gros chiffres)
res.pca <- PCA(df_acp, scale.unit = TRUE, graph = FALSE)

# --- GRAPHIQUES ---

# 1. Eboulis des valeurs propres
png("eboulis.png", width=800, height=600)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 70), 
         main = "Eboulis des valeurs propres")
dev.off()

# 2. Cercle des corrélations
png("cercle.png", width=800, height=800)
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Cercle des Corrélations (Type de délinquance)")
dev.off()

# 3. Carte des individus
png("carte.png", width=1000, height=1000)
fviz_pca_ind(res.pca, geom = c("point", "text"),
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, # Evite le chevauchement (peut prendre qques secondes)
             labelsize = 3,
             title = "Carte des Départements")
dev.off()
