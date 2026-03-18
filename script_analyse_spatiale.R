# ==============================================================================
# SPATIAL ECONOMETRICS PROJECT - M2 ECAP
# Authors: Sofia, Florian | Date: 2026-01-06
#
# Objective: Spatial modeling of social housing density (dept level)
# Dependent (Y): Nb_log_sociaux_10000hab
# Predictors (X): Part_femmes_seuls_enfant (%), Nb_immigres (stock)
# Inputs: Projet4.xlsx, DEPARTEMENT.shp
# ==============================================================================

# ==============================================================================
# 1. DATA PREPARATION
# ==============================================================================
library(readxl)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(mapview)
library(cartography)
library(spdep)
library(spatialreg)
library(tmap)
library(car)
library(EnvStats)
library(broom)
library(stargazer)
library(readr)
library(outliers)
library(corrplot)


# ------------------------------------------------------------------------------
# 1.1 Import
# ------------------------------------------------------------------------------
carte_dept <- st_read("DEPARTEMENT.shp", quiet = TRUE)
df_social <- read_excel("Projet4.xlsx")
df_extra <- read_delim("base_extra.csv", delim = ";")

# Validate input structures
str(carte_dept)
str(df_social)
str(df_extra)

# CRS check: EPSG:2154 (RGF93 / Lambert-93)
st_crs(carte_dept)

# ------------------------------------------------------------------------------
# 1.2 Data Cleaning & Feature Engineering
# ------------------------------------------------------------------------------
# Filter and standardize auxiliary data (2022)
df_extra_22 <- df_extra |>
    filter(`année_publication` == 2022) |>
    select(
        `année_publication`,
        `code_departement`,
        `Nombre de logements`,
        `Variation de la population sur 10 ans (en %)`,
        `Taux de chômage au T4 (en %)`,
        `Taux de pauvreté* (en %)`,
        `Taux de logements sociaux* (en %)`
    ) |>
    rename(
        publication_year = `année_publication`,
        department_code = code_departement,
        population_change_10yrs_pct = `Variation de la population sur 10 ans (en %)`,
        unemployment_rate_pct = `Taux de chômage au T4 (en %)`,
        poverty_rate_pct = `Taux de pauvreté* (en %)`,
        social_housing_rate_pct = `Taux de logements sociaux* (en %)`
    )

# Merge Datasets
# Keys: INSEE_DEP (sf) <-> Code_INSEE (df_social)
base_dept <- merge(carte_dept, df_social, by.x = "INSEE_DEP", by.y = "Code_INSEE")

# Integrate auxiliary data (df_extra_22)
base_dept <- merge(base_dept, df_extra_22, by.x = "INSEE_DEP", by.y = "department_code")
cat("N =", nrow(base_dept), "departments (Final)\n") # Expected: 94

# Inspect final merged structure
str(base_dept)

# Log-transform highly skewed immigrant stock (+1 to handle zeroes)
base_dept$log_Nb_immigres <- log(base_dept$Nb_immigres + 1)

# Select and rename variables
base_dept <- base_dept |>
    select(
        "INSEE_DEP",
        "INSEE_REG",
        "Libellé",
        "Nb_log_sociaux_10000hab",
        "Part_femmes_seuls_enfant",
        "Nb_immigres",
        "log_Nb_immigres",
        "Nombre de logements",
        "population_change_10yrs_pct",
        "unemployment_rate_pct",
        "poverty_rate_pct",
        "social_housing_rate_pct",
        "geometry"
    ) |>
    rename(
        department_code = INSEE_DEP,
        region_code = INSEE_REG,
        department_name = Libellé,
        social_housing_density = Nb_log_sociaux_10000hab,
        single_parent_ratio = Part_femmes_seuls_enfant,
        immigrant_stock = Nb_immigres,
        log_immigrant_stock = log_Nb_immigres,
        housing_units = `Nombre de logements`
    )

# ------------------------------------------------------------------------------
# 1.3 Summary Statistics
# ------------------------------------------------------------------------------
vars_numeric <- c(
    "social_housing_density",
    "single_parent_ratio",
    "immigrant_stock",
    "log_immigrant_stock",
    "housing_units",
    "population_change_10yrs_pct",
    "unemployment_rate_pct",
    "poverty_rate_pct",
    "social_housing_rate_pct"
)

stats_desc <- st_drop_geometry(base_dept)[, vars_numeric]
summary(stats_desc)

# ------------------------------------------------------------------------------
# 1.4 Distributions (Histograms & Boxplots)
# ------------------------------------------------------------------------------
par(mfrow = c(2, 2))

# Social Housing Density
hist(base_dept$social_housing_density, main = "Distribution: Housing Density", xlab = "Nb/10k hab", ylab = "Frequency", col = "lightblue", border = "white")
grid()
box()
boxplot(base_dept$social_housing_density, main = "Dispersion: Housing Density", col = "lightblue", horizontal = TRUE)

# Single Parent Ratio
hist(base_dept$single_parent_ratio, main = "Distribution: Single-Parent Ratio", xlab = "%", ylab = "Frequency", col = "lightgreen", border = "white")
grid()
box()
boxplot(base_dept$single_parent_ratio, main = "Dispersion: Single-Parent Ratio", col = "lightgreen", horizontal = TRUE)

# Immigrant Stock
hist(base_dept$immigrant_stock, main = "Distribution: Immigrant Stock", xlab = "Levels", ylab = "Frequency", col = "orange", border = "white")
grid()
box()
boxplot(base_dept$immigrant_stock, main = "Dispersion: Immigrant Stock", col = "orange", horizontal = TRUE)

# Log Immigrant Stock
hist(base_dept$log_immigrant_stock, main = "Distribution: Log Immig. Stock", xlab = "Log", ylab = "Frequency", col = "salmon", border = "white")
grid()
box()
boxplot(base_dept$log_immigrant_stock, main = "Dispersion: Log Immig. Stock", col = "salmon", horizontal = TRUE)

# Housing Units
hist(base_dept$housing_units, main = "Distribution: Housing Units", xlab = "Units", ylab = "Frequency", col = "steelblue", border = "white")
grid()
box()
boxplot(base_dept$housing_units, main = "Dispersion: Housing Units", col = "steelblue", horizontal = TRUE)

# Population Change 10Y
hist(base_dept$population_change_10yrs_pct, main = "Distribution: Pop. Change 10Y (%)", xlab = "%", ylab = "Frequency", col = "mediumpurple", border = "white")
grid()
box()
boxplot(base_dept$population_change_10yrs_pct, main = "Dispersion: Pop. Change 10Y (%)", col = "mediumpurple", horizontal = TRUE)

# Unemployment Rate
hist(base_dept$unemployment_rate_pct, main = "Distribution: Unemployment (%)", xlab = "%", ylab = "Frequency", col = "tomato", border = "white")
grid()
box()
boxplot(base_dept$unemployment_rate_pct, main = "Dispersion: Unemployment (%)", col = "tomato", horizontal = TRUE)

# Poverty Rate
hist(base_dept$poverty_rate_pct, main = "Distribution: Poverty Rate (%)", xlab = "%", ylab = "Frequency", col = "goldenrod", border = "white")
grid()
box()
boxplot(base_dept$poverty_rate_pct, main = "Dispersion: Poverty Rate (%)", col = "goldenrod", horizontal = TRUE)

# Social Housing Rate
hist(base_dept$social_housing_rate_pct, main = "Distribution: Social Housing Rate (%)", xlab = "%", ylab = "Frequency", col = "seagreen", border = "white")
grid()
box()
boxplot(base_dept$social_housing_rate_pct, main = "Dispersion: Social Housing Rate (%)", col = "seagreen", horizontal = TRUE)

par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# 1.5 Outlier Assessment (Rosner's Test)
# ------------------------------------------------------------------------------
# Social Housing Density
cat("\n--- Rosner Test: Social Housing Density ---\n")
rosnerTest(base_dept$social_housing_density, k = 10, alpha = 0.05)$all.stats

# Single Parent Ratio
cat("\n--- Rosner Test: Single Parent Ratio ---\n")
rosnerTest(base_dept$single_parent_ratio, k = 10, alpha = 0.05)$all.stats

# Immigrant Stock
cat("\n--- ESD Test: Immigrant Stock (Levels) ---\n")
x2 <- base_dept$immigrant_stock
rval <- function(x2) {
    ares <- abs(x2 - mean(x2)) / sd(x2)
    df <- data.frame(x2, ares)
    r <- max(df$ares)
    list(r, df)
}
n <- length(x2)
alpha <- 0.05
lam <- c(1:20)
R <- c(1:20)
for (i in 1:20) {
    if (i == 1) {
        rt <- rval(x2)
        R[i] <- unlist(rt[1])
        df <- data.frame(rt[2])
        newdf <- df[df$ares != max(df$ares), ]
    } else if (i != 1) {
        rt <- rval(newdf$x2)
        R[i] <- unlist(rt[1])
        df <- data.frame(rt[2])
        newdf <- df[df$ares != max(df$ares), ]
    }
    p <- 1 - alpha / (2 * (n - i + 1))
    t <- qt(p, (n - i - 1))
    lam[i] <- t * (n - i) / sqrt((n - i - 1 + t**2) * (n - i + 1))
}
newdf <- data.frame(c(1:20), R, lam)
names(newdf) <- c("No. Outliers", "Test Stat.", "Critical Val.")
newdf

# Note: Log Immigrant Stock (ln(X2)) is excluded from outlier testing.
# Its boxplot showed no observations beyond the whiskers, confirming absence of outliers.

# Housing Units
cat("\n--- Rosner Test: Housing Units ---\n")
rosnerTest(base_dept$housing_units, k = 10, alpha = 0.05)$all.stats

# Population Change 10Y
cat("\n--- Grubbs Test: Population Change 10Y ---\n")
grubbs.test(base_dept$population_change_10yrs_pct, type = 10, two.sided = TRUE)

# Unemployment Rate
cat("\n--- Rosner Test: Unemployment Rate ---\n")
rosnerTest(base_dept$unemployment_rate_pct, k = 10, alpha = 0.05)$all.stats

# Poverty Rate
cat("\n--- Rosner Test: Poverty Rate ---\n")
rosnerTest(base_dept$poverty_rate_pct, k = 10, alpha = 0.05)$all.stats

# Social Housing Rate
cat("\n--- Rosner Test: Social Housing Rate ---\n")
rosnerTest(base_dept$social_housing_rate_pct, k = 10, alpha = 0.05)$all.stats

# ------------------------------------------------------------------------------
# 1.6 Normality Assessment (Shapiro-Wilk)
# ------------------------------------------------------------------------------
# H0: Normal distribution (Reject if p-value < 0.05)
p_vals <- sapply(st_drop_geometry(base_dept)[vars_numeric], function(x) shapiro.test(x)$p.value)

cat("\n--- Shapiro-Wilk Tests (p-values) ---\n")
print(round(p_vals, 5))
# Conclusion: p < 0.05 for multiple variables.
# Consequence: Non-normal distributions mandate non-parametric Spearman correlation.

# ------------------------------------------------------------------------------
# 1.7 Pairwise Correlations (Spearman)
# ------------------------------------------------------------------------------
vars_interet <- st_drop_geometry(base_dept)[, vars_numeric]
corr_matrix <- cor(vars_interet, use = "complete.obs", method = "spearman")
corrplot(corr_matrix,
    method = "color",
    type = "upper",
    addCoef.col = "black",
    tl.col = "black",
    tl.cex = 0.7,
    number.cex = 0.7,
    col = colorRampPalette(c("blue", "white", "red"))(200),
    title = "Spearman Correlation Matrix",
    mar = c(0, 0, 2, 0)
)



# ==============================================================================
# 2. SPATIAL WEIGHTS MATRIX (W)
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1 Queen Contiguity
# ------------------------------------------------------------------------------
# Neighbors share at least one vertex (edges + corners)
nb_queen <- poly2nb(base_dept, row.names = base_dept$department_name, queen = TRUE)
summary(nb_queen)

# Connectivity visualization
nb_linesQ <- nb2lines(nb_queen, coords = st_centroid(st_geometry(base_dept)))
nb_sfQ <- st_as_sf(nb_linesQ)
ggplot() +
    geom_sf(data = base_dept, fill = "lightgreen", color = "black") +
    geom_sf(data = nb_sfQ, color = "blue") +
    theme_minimal() +
    ggtitle("Contiguity Map: Queen")

# Row-standardized W matrix
WQueen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

# ------------------------------------------------------------------------------
# 2.2 Rook Contiguity (Sensitivity Check)
# ------------------------------------------------------------------------------
# Neighbors share at least one edge (stricter than Queen)
nb_rook <- poly2nb(base_dept, row.names = base_dept$department_name, queen = FALSE)
summary(nb_rook)

# ------------------------------------------------------------------------------
# 2.3 K-Nearest Neighbors
# ------------------------------------------------------------------------------
# Distance-based alternative: each dept connected to its k closest centroids
centroids <- st_centroid(st_geometry(base_dept))
coords <- st_coordinates(centroids)
crs <- st_crs(base_dept)
coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = crs)
coords_sp <- as(coords_sf, "Spatial")

k <- 1
knn_neighbours <- knearneigh(coords, k = k)
neighbors <- knn2nb(knn_neighbours)
summary(neighbors)

# Connectivity visualization
nb_linesKNN <- nb2lines(neighbors, coords = coords_sp)
nb_sfKNN <- st_as_sf(nb_linesKNN)
ggplot() +
    geom_sf(data = base_dept, fill = "lightgreen", color = "black") +
    geom_sf(data = nb_sfKNN, color = "blue") +
    theme_minimal() +
    ggtitle(paste("Contiguity Map:", k, "-Nearest Neighbors"))

# Row-standardized W matrix
KNN <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# ==============================================================================
# 3. EXPLORATORY SPATIAL DATA ANALYSIS (ESDA)
# ==============================================================================

set.seed(1234)

# ------------------------------------------------------------------------------
# 3.1 Spatial Autocorrelation under Queen W
# ------------------------------------------------------------------------------

# Global Moran's I (Asymptotic inference under randomisation)
globalMoranQ <- moran.test(base_dept$social_housing_density, WQueen, zero.policy = TRUE, randomisation = TRUE)
globalMoranQ

# Monte Carlo permutation test (999 simulations under H0: no spatial autocorrelation)
MoranpermQ <- moran.mc(base_dept$social_housing_density, WQueen, nsim = 999, zero.policy = TRUE)
MoranpermQ

# Moran Scatterplot (standardized Y vs spatially lagged Y)
base_dept$social_housing_density_std <- scale(base_dept$social_housing_density)
moran_plot_Q <- moran.plot(
    as.vector(base_dept$social_housing_density_std),
    WQueen,
    xlab = "Social Housing Density (standardized)",
    ylab = "Spatially Lagged Social Housing Density",
    main = "Moran Scatterplot: Queen W",
    labels = as.character(base_dept$department_name)
)

# ------------------------------------------------------------------------------
# 3.2 Spatial Autocorrelation under KNN W
# ------------------------------------------------------------------------------

# Global Moran's I (Asymptotic inference under randomisation)
globalMoranKNN <- moran.test(base_dept$social_housing_density, KNN, zero.policy = TRUE, randomisation = TRUE)
globalMoranKNN

# Monte Carlo permutation test (999 simulations under H0: no spatial autocorrelation)
MoranpermKNN <- moran.mc(base_dept$social_housing_density, KNN, nsim = 999, zero.policy = TRUE)
MoranpermKNN

# Moran Scatterplot (standardized Y vs spatially lagged Y)
moran_plot_KNN <- moran.plot(
    as.vector(base_dept$social_housing_density_std),
    KNN,
    xlab = "Social Housing Density (standardized)",
    ylab = "Spatially Lagged Social Housing Density",
    main = "Moran Scatterplot: KNN W",
    labels = as.character(base_dept$department_name)
)


# ==============================================================================
# 4. ECONOMETRIC MODELING
# ==============================================================================

# ------------------------------------------------------------------------------
# 4.0 OLS Baseline
# ------------------------------------------------------------------------------
# Specification: Y ~ X1 + X2 + X3 + X4
equation <- social_housing_density ~ single_parent_ratio + immigrant_stock + population_change_10yrs_pct + poverty_rate_pct

ols <- lm(equation, data = base_dept)
summary(ols)

# Multicollinearity check (VIF < 5)
vif(ols)

# Spatial distribution of OLS residuals
base_dept$residuals_ols <- residuals(ols)

ggplot(base_dept) +
    geom_sf(aes(fill = residuals_ols)) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    theme_minimal() +
    labs(title = "OLS residuals: spatial distribution")

# ------------------------------------------------------------------------------
# 4.1 Spatial Specification under Queen W
# ------------------------------------------------------------------------------

# Moran test on OLS residuals (H0: no spatial autocorrelation)
moran_lmQ <- lm.morantest(ols, WQueen, alternative = "two.sided")
print(moran_lmQ)

# Lagrange Multiplier Tests ----------------------------------------------------
LM_Q <- lm.LMtests(ols, WQueen, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM_Q)

# Elhorst Procedure (LR Tests) -------------------------------------------------
# Spatial Durbin Model (SDM) vs Spatial Error Model (SEM)
# SEM: Y = XB + u, u = lambda*Wu + e
sdmQ <- lagsarlm(equation, data = base_dept, listw = WQueen, type = "mixed")
summary(sdmQ)

semQ <- errorsarlm(equation, data = base_dept, listw = WQueen, method = "eigen")
summary(semQ)

TestSDM_SEM_Q <- LR.Sarlm(sdmQ, semQ)
print(TestSDM_SEM_Q)

# Spatial Durbin Model (SDM) vs Spatial Autoregressive Model (SAR)
# SAR: Y = rho*WY + XB + e
sarQ <- lagsarlm(equation, data = base_dept, listw = WQueen, method = "eigen")
summary(sarQ)

TestSDM_SAR_Q <- LR.Sarlm(sdmQ, sarQ)
print(TestSDM_SAR_Q)

# ------------------------------------------------------------------------------
# 4.2 Spatial Specification under KNN W
# ------------------------------------------------------------------------------

# Moran test on OLS residuals (H0: no spatial autocorrelation)
moran_lmKNN <- lm.morantest(ols, KNN, alternative = "two.sided")
print(moran_lmKNN)

# Lagrange Multiplier Tests ----------------------------------------------------
LM_KNN <- lm.LMtests(ols, KNN, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM_KNN)

# Elhorst Procedure (LR Tests) -------------------------------------------------
# Spatial Durbin Model (SDM) vs Spatial Error Model (SEM)
# SEM: Y = XB + u, u = lambda*Wu + e
sdmKNN <- lagsarlm(equation, data = base_dept, listw = KNN, type = "mixed")
summary(sdmKNN)

semKNN <- errorsarlm(equation, data = base_dept, listw = KNN, method = "eigen")
summary(semKNN)

TestSDM_SEM_KNN <- LR.Sarlm(sdmKNN, semKNN)
print(TestSDM_SEM_KNN)

# Spatial Durbin Model (SDM) vs Spatial Autoregressive Model (SAR)
# SAR: Y = rho*WY + XB + e
sarKNN <- lagsarlm(equation, data = base_dept, listw = KNN, method = "eigen")
summary(sarKNN)

TestSDM_SAR_KNN <- LR.Sarlm(sdmKNN, sarKNN)
print(TestSDM_SAR_KNN)

##################################################################################
# SOFIA
##################################################################################
# results formatin
# --- helper: extract rho / lambda safely ---
get_rho <- function(m) if (!is.null(m$rho)) round(m$rho, 3) else ""
get_lambda <- function(m) if (!is.null(m$lambda)) round(m$lambda, 3) else ""

# --- (optional) nice labels for regressors ---
cov_labels <- c(
    "Share single mothers",
    "Number of immigrants",
    "Pop. change 10y (%)",
    "Unemployment rate (%)",
    "Poverty rate (%)"
)

# Your SAR residual LM p-values (you already have these)
lm_resid_sar_q <- 0.38173
lm_resid_sar_ppv <- 0.10228


stargazer(
    lag_model_q, sem_model_q, lag_model, sem_model,
    type = "text", # change to "latex" if needed
    title = "Spatial regression models (SAR vs SEM) under alternative weight matrices",
    digits = 3,
    column.labels = c("SAR", "SEM", "SAR", "SEM"),
    column.separate = c(1, 1, 1, 1),
    dep.var.labels = "Social housing per 10,000 (2024)",
    covariate.labels = cov_labels,
    omit.stat = c("rsq", "adj.rsq", "f"),
    add.lines = list(
        c("Weights matrix", "Queen", "Queen", "k-NN (PPV)", "k-NN (PPV)"),
        c(
            "Spatial parameter",
            paste0("rho=", get_rho(lag_model_q)),
            paste0("lambda=", get_lambda(sem_model_q)),
            paste0("rho=", get_rho(lag_model)),
            paste0("lambda=", get_lambda(sem_model))
        ),
        c(
            "LM residual test (p-value)",
            round(lm_resid_sar_q, 3),
            "— (absorbed by <U+03BB>)",
            round(lm_resid_sar_ppv, 3),
            "— (absorbed by <U+03BB>)"
        )
    ),
    out = "sem_sar_model.txt"
)


# Compare models
AIC(mco, sem_model, lag_model, sem_model_q, lag_model_q)
BIC(mco, sem_model, lag_model, sem_model_q, lag_model_q)

par(mfrow = c(1, 2))
hist(residuals(sem_model))
hist(residuals(lag_model))

# Plan Elhorst (2010)
library(spatialreg)

# SDM: Spatial Durbin Model
# Y = rho*WY + XB + WX*gamma + e
# type="mixed" car équivalent à lagsarlm avec Durbin=TRUE

sdm_ppv <- lagsarlm(
    equation,
    data = base_dept,
    listw = PPV,
    type = "mixed",
    method = "eigen",
    zero.policy = TRUE
)

sdm_q <- lagsarlm(
    equation,
    data = base_dept,
    listw = WQueen,
    type = "mixed",
    method = "eigen",
    zero.policy = TRUE
)

summary(sdm_ppv)
coef_names <- names(coef(sdm_ppv))
pretty_names <- coef_names
pretty_names <- gsub("^lag\\.", "W × ", pretty_names)
names(pretty_names) <- coef_names
rho_val <- sdm_ppv$rho
rho_se <- sqrt(diag(sdm_ppv$asyvar))["rho"]
rho_val2 <- sdm_q$rho
rho_se2 <- sqrt(diag(sdm_q$asyvar))["rho"]


stargazer(
    sdm_ppv, sdm_q,
    type = "text",
    title = "Spatial Durbin Model (SDM) – k-nearest neighbours",
    digits = 3,
    # covariate.labels = pretty_names,
    omit.stat = c("rsq", "adj.rsq", "f"),
    add.lines = list(c("Spatial lag parameter (rho)", round(rho_val, 3), round(rho_val2, 3))),
    out = "sdm_model.txt"
)

AIC(sdm_ppv)
impacts(sdm_ppv, listw = PPV, R = 1000)
hist(residuals(sdm_ppv))

# Comparison SDM vs SEM
TestSDM_SEM <- LR.Sarlm(sdm_ppv, sem_model)
TestSDMq_SEM <- LR.Sarlm(sdm_q, sem_model)

print(TestSDM_SEM)
print(TestSDMq_SEM)
