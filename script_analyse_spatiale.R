# ==================================================================================================
# PROJET D'ÉCONOMÉTRIE SPATIALE - M2 ECAP
# ==================================================================================================
# PROBLÉMATIQUE : Dans quelle mesure la densité de logements sociaux d'un département est-elle influencée 
# par la structure sociale (familles monoparentales, immigration) de ce département et de ses voisins ?
# AUTEUR : Sofia et Florian
# DATE : 2026-01-06
#
# VARIABLES :
# - Y (Dépendante)  : Nb_log_sociaux_10000hab (Nombre de logements sociaux pour 10 000 hab)
# - X (Explicatives): 
#     1. Part_femmes_seuls_enfant (Part des familles monoparentales, %)
#     2. Nb_immigres (Nombre d'immigrés - variable de niveau/stock)
#
# FICHIERS REQUIS :
# - Projet4.xlsx (Données attributaires)
# - DEPARTEMENT.shp (Données géographiques)
# ==================================================================================================

# ==================================================================================================
# ### 1. PRÉPARATION DE L'ENVIRONNEMENT ET DES DONNÉES
# ==================================================================================================

# 1.1 Chargement des bibliothèques nécessaires
# ---------------------------------------------------------
# Si des paquets manquent, décommenter et exécuter : install.packages(c("readxl", "sf", "spdep", "spatialreg", "tmap", "car"))
library(readxl)      # Pour lire Excel
library(sf)          # Pour manipuler le shapefile (cartes)
library(spdep)       # Pour la matrice de poids et tests de Moran
library(spatialreg)  # Pour les modèles économétriques spatiaux (SAR, SEM, SDM...)
library(tmap)        # (Optionnel) Pour de jolies cartes rapides
library(car)         # Pour le test VIF (Multicollinéarité)

# 1.2 Importation des données
# ---------------------------------------------------------
# Lecture du fichier Excel
df_social <- read_excel("Projet4.xlsx")

# Lecture du fond de carte (Shapefile)
carte_dept <- st_read("DEPARTEMENT.shp", quiet = TRUE)

# Vérification du système de projection (CRS) - Doit être Lambert 93 (RGF93)
st_crs(carte_dept)

# 1.3 Nettoyage et Jointure
# ---------------------------------------------------------
# Vérification du nom et de la structure des colonnes pour la jointure
# str(df_social)
# str(carte_dept)

# Jointure : On conserve uniquement les départements présents dans les deux fichiers
# by.x = colonne clé Excel, by.y = colonne clé Shapefile
base_dept <- merge(carte_dept, df_social, by.x = "INSEE_DEP", by.y = "Code_INSEE")

# Vérification qu'on a bien récupéré nos données (94 à 96 départements généralement)
cat("Nombre de départements après jointure : ", nrow(base_dept), "\n")

# Transformation de variable (Optionnel mais recommandé pour les variables de stock comme Nb_immigres)
# Distribution souvent asymétrique -> Logarithme
base_dept$log_Nb_immigres <- log(base_dept$Nb_immigres + 1) # +1 pour éviter log(0)

# 1.4 Distribution des variables (Histogrammes & Boxplots)
# ---------------------------------------------------------
# Visualisation pour détecter les outliers et la normalité
# On compare les distributions. Utilisation de par(mfrow) pour afficher plusieurs graphes.
par(mfrow = c(3, 2)) # 3 lignes, 2 colonnes

# Y : Logements Sociaux
hist(base_dept$Nb_log_sociaux_10000hab, main = "Hist: Logements Sociaux", xlab = "Nb/10k hab", col = "lightblue", border = "white")
boxplot(base_dept$Nb_log_sociaux_10000hab, main = "Box: Logements Sociaux", col = "lightblue", horizontal = TRUE)

# X1 : Familles Monoparentales
hist(base_dept$Part_femmes_seuls_enfant, main = "Hist: Familles Monoparentales", xlab = "%", col = "lightgreen", border = "white")
boxplot(base_dept$Part_femmes_seuls_enfant, main = "Box: Familles Monoparentales", col = "lightgreen", horizontal = TRUE)

# X2 : Immigrés (Logarithme recommandé)
hist(base_dept$log_Nb_immigres, main = "Hist: Immigrés (Log)", xlab = "Log(Nb)", col = "salmon", border = "white")
boxplot(base_dept$log_Nb_immigres, main = "Box: Immigrés (Log)", col = "salmon", horizontal = TRUE)

# Reset de l'affichage graphique
par(mfrow = c(1, 1))

# 1.5 Analyse des corrélations (Question 5 - TD3 Adaptation)
# ---------------------------------------------------------
# On regarde les liens linéaires simples entre variables avant la spatialisation.
# Sélection des variables d'intérêt (sans la géométrie pour cor())
vars_interet <- st_drop_geometry(base_dept)[, c("Nb_log_sociaux_10000hab", 
                                                "Part_femmes_seuls_enfant", 
                                                "Nb_immigres", 
                                                "log_Nb_immigres")]

# Matrice de corrélation
matrice_corr <- cor(vars_interet, use = "complete.obs")
print("Matrice de corrélation :")
matrice_corr


# ==================================================================================================
# ### 2. MATRICE DE PONDÉRATION SPATIALE (W)
# ==================================================================================================
# On utilise ici la contiguïté Reine (Queen) d'ordre 1, standard en analyse départementale.

# 2.1 Création de la liste des voisins
nb_queen <- poly2nb(base_dept, queen = TRUE)

# Résumé de la connectivité (vérification d'îlots éventuels)
print(summary(nb_queen))

# Comparaison avec contiguïté Tour (Rook) d'ordre 1
nb_rook <- poly2nb(base_dept, queen = FALSE)
diff_nb <- diffnb(nb_queen, nb_rook)

print("Différences entre Reine et Tour :")
print(summary(diff_nb))
if(length(diff_nb) == 0) cat("Aucune différence : les définitions de voisins sont identiques.\n")


# 2.2 Création de la matrice de poids row-standardized (W)
listw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

# Visualisation rapide de la matrice
# plot(st_geometry(base_dept), border="grey")
# plot(nb_queen, st_coordinates(st_centroid(base_dept)), add=TRUE, col="red")


# ==================================================================================================
# ### 3. ANALYSE EXPLORATOIRE SPATIALE (ESDA)
# ==================================================================================================

# 3.1 Test de Moran Global sur la variable dépendante (Y)
moran_test <- moran.test(base_dept$Nb_log_sociaux_10000hab, listw_queen, zero.policy = TRUE)
print(moran_test)
# Interprétation : Si p-value < 0.05, il y a autocorrélation spatiale globale significative.

# 3.2 Moran Scatterplot
moran.plot(base_dept$Nb_log_sociaux_10000hab, listw_queen, 
           labels = as.character(base_dept$Libellé), 
           pch = 19, quiet = TRUE,
           main = "Diagramme de Moran - Logements Sociaux")

# NOTE GEODA :
# Pour une exploration locale (LISA) interactive :
# 1. Space > Univariate Local Moran's I.
# 2. Sélectionnez 'Nb_log_sociaux_10000hab'.
# 3. Visualisez la Cluster Map pour identifier les zones High-High (HH) ou Low-Low (LL).
# C'est souvent plus visuel et pertinent pour le rapport écrit.


# ==================================================================================================
# ### 4. MODÉLISATION ÉCONOMÉTRIQUE
# ==================================================================================================

# Définition de l'équation de base
# Y = Nb_log_sociaux
# X = Part_femmes_seuls_enfant + Nb_immigres (ou log_Nb_immigres selon choix)

# 4.1 Modèle OLS (MCO) Classique (Sans effet spatial)
# ---------------------------------------------------------
# Note: On utilise ici Nb_immigres brut comme demandé implicitement, mais le log est souvent mieux.
model_ols <- lm(Nb_log_sociaux_10000hab ~ Part_femmes_seuls_enfant + Nb_immigres, data = base_dept)
summary(model_ols)

# --- Diagnostics OLS ---
# A. Multicollinéarité (VIF)
# VIF < 10 (idéalement < 5) indique absence de multicolinéarité grave.
print(vif(model_ols))

# B. Normalité des résidus (Jarque-Bera)
# H0: Résidus normaux. Si p-value < 0.05, non-normalité.
residus_ols <- residuals(model_ols)
# jarque.bera.test(residus_ols) # Nécessite librarytseries, sinon regarder histogramme
shapiro.test(residus_ols) # Alternative native R

# C. Hétéroscédasticité (Breusch-Pagan)
# H0: Homoscédasticité. Si p-value < 0.05, hétéroscédasticité (problème pour OLS).
bptest(model_ols)

# D. Autocorrélation spatiale des résidus OLS (Moran sur résidus)
lm.morantest(model_ols, listw_queen)
# Si significatif -> Le modèle OLS oublie la structure spatiale -> Modèle spatial requis.


# 4.2 Tests de Spécification (LM Tests) - CHOIX DU MODÈLE
# ---------------------------------------------------------
# Ces tests permettent de choisir entre SAR (Lag) et SEM (Error).
lm_tests <- lm.LMtests(model_ols, listw_queen, test = "all")
print(lm_tests)

# --- RÈGLE DE DÉCISION (Approche Florax et al.) ---
# 1. Regarder LMerr (Error) et LMlag (Lag).
# 2. Si les deux sont non-significatifs -> Garder OLS.
# 3. Si un seul est significatif -> Choisir ce modèle.
# 4. Si les deux sont significatifs -> Regarder les versions ROBUSTES (RLMerr et RLMlag).
#    -> Celui qui a la p-value la plus faible (le plus significatif) l'emporte.

# NOTE : Le modèle SDM (Durbin Spatial) est souvent préféré aujourd'hui car il englobe les deux.
# Il inclut les X retardés spatialement (WX).


# 4.3 Estimation des Modèles Spatiaux
# ---------------------------------------------------------
# Nous estimons les trois principaux pour comparaison complete (Conforme TD3).

# MODÈLE A : SEM (Spatial Error Model) - Erreur spatiale
# Y = XB + u, u = lambda*Wu + e
model_sem <- errorsarlm(Nb_log_sociaux_10000hab ~ Part_femmes_seuls_enfant + Nb_immigres, 
                        data = base_dept, listw = listw_queen)
summary(model_sem)

# MODÈLE B : SAR (Spatial Autoregressive Model) - Lag spatial
# Y = rho*WY + XB + e
model_sar <- lagsarlm(Nb_log_sociaux_10000hab ~ Part_femmes_seuls_enfant + Nb_immigres, 
                      data = base_dept, listw = listw_queen)
summary(model_sar)

# MODÈLE C : SDM (Spatial Durbin Model) - Mixte
# Y = rho*WY + XB + WX*gamma + e
# type="mixed" car équivalent à lagsarlm avec Durbin=TRUE
model_sdm <- lagsarlm(Nb_log_sociaux_10000hab ~ Part_femmes_seuls_enfant + Nb_immigres, 
                      data = base_dept, listw = listw_queen, type = "mixed")
summary(model_sdm)


# 4.4 Comparaison et Choix Final
# ---------------------------------------------------------
# Comparaison par AIC (Le plus faible est le meilleur)
AIC(model_ols, model_sar, model_sem, model_sdm)

# Test LR (Likelihood Ratio) pour voir si SDM simplifie en SAR ou SEM (Nested models)
# H0 : Le modèle restreint (SAR ou SEM) est suffisant par rapport au SDM.
# Si p-value < 0.05, on rejette H0 -> On garde SDM.
lr_sdm_sar <- LR.sarlm(model_sdm, model_sar)
lr_sdm_sem <- LR.sarlm(model_sdm, model_sem)

print(lr_sdm_sar)
print(lr_sdm_sem)


# ==================================================================================================
# ### 5. CALCUL DES IMPACTS (EFFETS DIRECTS, INDIRECTS, TOTAUX)
# ==================================================================================================
# Dans les modèles spatiaux (SAR/SDM), les coefficients béta ne s'interprètent pas directement.
# Il faut calculer les impacts.
# (Pour le SEM et l'OLS, impacts = coefficients).

# Supposons que le modèle SDM ou SAR soit retenu (exemple avec SDM, le plus complet)
# Si vous retenez le SAR, remplacez `model_sdm` par `model_sar`.

W_mat <- as(listw_queen, "CsparseMatrix")
tr_mat <- trW(W_mat, type="mult") # Calcul de la trace pour simulation Monte Carlo

impacts_sdm <- impacts(model_sdm, tr = tr_mat, R = 1000) # R = nombre de simulations
print(summary(impacts_sdm, zstats = TRUE, short = TRUE))

# Interprétation :
# - Direct : Effet d'une variation de X dans le département i sur Y dans le département i (incluant feedback).
# - Indirect (Spillover) : Effet d'une variation de X dans les voisins j sur Y dans i.
# - Total : Somme des deux.

# ==================================================================================================
# FIN DU SCRIPT
# ==================================================================================================
