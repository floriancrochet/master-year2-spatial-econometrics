# 1. Data preparation
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


# 1.1 Import
carte_dept <- st_read("DEPARTEMENT.shp", quiet = TRUE)
df_social <- read_excel("Projet4.xlsx")
df_extra <- read_delim("base_extra.csv", delim = ";")

# Validate input structures
str(carte_dept)
str(df_social)
str(df_extra)

# CRS check: EPSG:2154 (RGF93/Lambert-93)
st_crs(carte_dept)

# 1.2 Data cleaning — filter extra data to 2022 and standardize column names
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

# Keys: INSEE_DEP (sf) <-> Code_INSEE (df_social)
base_dept <- merge(carte_dept, df_social, by.x = "INSEE_DEP", by.y = "Code_INSEE")
base_dept <- merge(base_dept, df_extra_22, by.x = "INSEE_DEP", by.y = "department_code")

base_dept$log_Nb_immigres <- log(base_dept$Nb_immigres + 1)
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

# 1.3 Summary statistics
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

# 1.4 Distributions
par(mfrow = c(2, 2))

# Social Housing Density
hist(base_dept$social_housing_density, main = "Social Housing Density", xlab = "Units per 10.000 hab.", ylab = "Frequency", col = "lightblue", border = "white")
grid()
box()
boxplot(base_dept$social_housing_density, main = "Social Housing Density", xlab = "Units per 10.000 hab.", col = "lightblue", horizontal = TRUE)

# Single Parent Ratio
hist(base_dept$single_parent_ratio, main = "Single-Parent Ratio", xlab = "Share (%)", ylab = "Frequency", col = "lightgreen", border = "white")
grid()
box()
boxplot(base_dept$single_parent_ratio, main = "Single-Parent Ratio", xlab = "Share (%)", col = "lightgreen", horizontal = TRUE)

# Immigrant Stock
hist(base_dept$immigrant_stock, main = "Immigrant Stock", xlab = "Stocks (individuals)", ylab = "Frequency", col = "orange", border = "white")
grid()
box()
boxplot(base_dept$immigrant_stock, main = "Immigrant Stock", xlab = "Stocks (individuals)", col = "orange", horizontal = TRUE)

# Log Immigrant Stock
hist(base_dept$log_immigrant_stock, main = "Log Immigrant Stock", xlab = "Log(individuals)", ylab = "Frequency", col = "salmon", border = "white")
grid()
box()
boxplot(base_dept$log_immigrant_stock, main = "Log Immigrant Stock", xlab = "Log(individuals)", col = "salmon", horizontal = TRUE)

# Housing Units
hist(base_dept$housing_units, main = "Housing Units", xlab = "Total units", ylab = "Frequency", col = "steelblue", border = "white")
grid()
box()
boxplot(base_dept$housing_units, main = "Housing Units", xlab = "Total units", col = "steelblue", horizontal = TRUE)

# Population Change 10Y
hist(base_dept$population_change_10yrs_pct, main = "Pop. Change over 10 Years", xlab = "Change (%)", ylab = "Frequency", col = "mediumpurple", border = "white")
grid()
box()
boxplot(base_dept$population_change_10yrs_pct, main = "Pop. Change over 10 Years", xlab = "Change (%)", col = "mediumpurple", horizontal = TRUE)

# Unemployment Rate
hist(base_dept$unemployment_rate_pct, main = "Unemployment", xlab = "Rate (%)", ylab = "Frequency", col = "tomato", border = "white")
grid()
box()
boxplot(base_dept$unemployment_rate_pct, main = "Unemployment", xlab = "Rate (%)", col = "tomato", horizontal = TRUE)

# Poverty Rate
hist(base_dept$poverty_rate_pct, main = "Poverty Rate", xlab = "Rate (%)", ylab = "Frequency", col = "goldenrod", border = "white")
grid()
box()
boxplot(base_dept$poverty_rate_pct, main = "Poverty Rate", xlab = "Rate (%)", col = "goldenrod", horizontal = TRUE)

# Social Housing Rate
hist(base_dept$social_housing_rate_pct, main = "Social Housing Rate", xlab = "Rate (%)", ylab = "Frequency", col = "seagreen", border = "white")
grid()
box()
boxplot(base_dept$social_housing_rate_pct, main = "Social Housing Rate", xlab = "Rate (%)", col = "seagreen", horizontal = TRUE)

par(mfrow = c(1, 1))

# 1.5 Shapiro-Wilk normality test (H0: normal, reject if p < 0.05)
p_vals <- sapply(st_drop_geometry(base_dept)[vars_numeric], function(x) shapiro.test(x)$p.value)
round(p_vals, 5)

# 1.6 Outlier detection
hampel_outliers <- function(data, var_name, col_name) {
    x <- data[[col_name]]
    median_x <- median(x, na.rm = TRUE)
    mad_x <- mad(x, constant = 1.4826, na.rm = TRUE)
    
    # Hampel threshold: 3 MADs
    lower_bound <- median_x - 3 * mad_x
    upper_bound <- median_x + 3 * mad_x
    
    outliers_idx <- which(x < lower_bound | x > upper_bound)
    
    cat(sprintf("\n--- Hampel Test: %s ---\n", var_name))
    cat(sprintf("Median: %.4f | MAD: %.4f | Bounds: [%.4f, %.4f]\n", median_x, mad_x, lower_bound, upper_bound))
    
    if (length(outliers_idx) > 0) {
        cat(sprintf("Number of outliers detected: %d\n", length(outliers_idx)))
        outliers_data <- data.frame(
            Department = data$department_code[outliers_idx],
            Value = x[outliers_idx]
        )
        print(outliers_data)
    } else {
        cat("No outliers detected.\n")
    }
}

# Social Housing Density
hampel_outliers(base_dept, "Social Housing Density", "social_housing_density")

# Single Parent Ratio
hampel_outliers(base_dept, "Single Parent Ratio", "single_parent_ratio")

# Immigrant Stock
hampel_outliers(base_dept, "Immigrant Stock (Levels)", "immigrant_stock")

# Note: log immigrant stock excluded (no boxplot outliers)

# Housing Units
hampel_outliers(base_dept, "Housing Units", "housing_units")

# Population Change 10Y (Grubbs test - normally distributed)
grubbs_res <- grubbs.test(base_dept$population_change_10yrs_pct, type = 10, two.sided = TRUE)
grubbs_res

if (grubbs_res$p.value < 0.05) {
    x_val <- base_dept$population_change_10yrs_pct
    outlier_idx <- which.max(abs(x_val - mean(x_val, na.rm = TRUE)))
    cat(sprintf("-> Outlier detected: Department %s (Value: %.4f)\n", 
                base_dept$department_code[outlier_idx], 
                x_val[outlier_idx]))
}

# Unemployment Rate
hampel_outliers(base_dept, "Unemployment Rate", "unemployment_rate_pct")

# Poverty Rate
hampel_outliers(base_dept, "Poverty Rate", "poverty_rate_pct")

# Social Housing Rate
hampel_outliers(base_dept, "Social Housing Rate", "social_housing_rate_pct")

# 1.7 Spearman pairwise correlations
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


# 2. Spatial weights matrices

# 2.1 Queen contiguity
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

# 2.2 Rook contiguity (sensitivity check)
nb_rook <- poly2nb(base_dept, row.names = base_dept$department_name, queen = FALSE)
summary(nb_rook)

# 2.3 K-nearest neighbours (k=1 reference, k=3 primary, k=6 robustness)

centroids <- st_centroid(st_geometry(base_dept))
coords <- st_coordinates(centroids)
crs <- st_crs(base_dept)
coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = crs)
coords_sp <- as(coords_sf, "Spatial")

# KNN-1 (reference only — likely many subgraphs, not used for estimation)
nb_knn1 <- knn2nb(knearneigh(coords, k = 1))
summary(nb_knn1)
cat("KNN-1 subgraphs:", n.comp.nb(nb_knn1)$nc, "\n")

# KNN-3 (primary specification)
nb_knn3 <- knn2nb(knearneigh(coords, k = 3))
KNN3 <- nb2listw(nb_knn3, style = "W", zero.policy = TRUE)
summary(nb_knn3)

nb_sfKNN3 <- st_as_sf(nb2lines(nb_knn3, coords = coords_sp))
ggplot() +
    geom_sf(data = base_dept, fill = "lightgreen", color = "black") +
    geom_sf(data = nb_sfKNN3, color = "blue") +
    theme_minimal() +
    ggtitle("Contiguity Map: 3-Nearest Neighbors")

# KNN-6 (robustness check)
nb_knn6 <- knn2nb(knearneigh(coords, k = 6))
KNN6 <- nb2listw(nb_knn6, style = "W", zero.policy = TRUE)
summary(nb_knn6)

KNN       <- KNN3
PPV       <- KNN3
neighbors <- nb_knn3

# Connectivity summary table
wmat_stats <- function(nb_obj, label) {
    card_vec <- card(nb_obj)
    data.frame(
        Matrix           = label,
        N                = length(nb_obj),
        "Non-zero links" = sum(card_vec),
        "Avg. neighbors" = round(mean(card_vec), 2),
        Min              = min(card_vec),
        Max              = max(card_vec),
        Subgraphs        = n.comp.nb(nb_obj)$nc,
        check.names      = FALSE
    )
}

conn_df <- rbind(
    wmat_stats(nb_queen, "Queen (contiguity)"),
    wmat_stats(nb_knn1,  "KNN-1 (ref. only)"),
    wmat_stats(nb_knn3,  "KNN-3"),
    wmat_stats(nb_knn6,  "KNN-6")
)
conn_df

# 3. Exploratory spatial analysis

set.seed(1234)

# 3.1 Global Moran's I under Queen W

globalMoranQ <- moran.test(base_dept$social_housing_density, WQueen, zero.policy = TRUE, randomisation = TRUE)
globalMoranQ

MoranpermQ <- moran.mc(base_dept$social_housing_density, WQueen, nsim = 999, zero.policy = TRUE)
MoranpermQ

# Moran scatterplot
base_dept$social_housing_density_std <- scale(base_dept$social_housing_density)
moran_plot_Q <- moran.plot(
    as.vector(base_dept$social_housing_density_std),
    WQueen,
    xlab = "Social Housing Density (standardized)",
    ylab = "Spatially Lagged Social Housing Density",
    main = "Moran Scatterplot: Queen W",
    labels = as.character(base_dept$department_name)
)

# 3.2 Global Moran's I under KNN-3 W

globalMoranKNN <- moran.test(base_dept$social_housing_density, KNN, zero.policy = TRUE, randomisation = TRUE)
globalMoranKNN

MoranpermKNN <- moran.mc(base_dept$social_housing_density, KNN, nsim = 999, zero.policy = TRUE)
MoranpermKNN

# Moran scatterplot
moran_plot_KNN <- moran.plot(
    as.vector(base_dept$social_housing_density_std),
    KNN,
    xlab = "Social Housing Density (standardized)",
    ylab = "Spatially Lagged Social Housing Density",
    main = "Moran Scatterplot: KNN W",
    labels = as.character(base_dept$department_name)
)


# 4. Econometric modeling

# 4.0 OLS baseline
equation <- social_housing_density ~ single_parent_ratio + log_immigrant_stock + population_change_10yrs_pct + poverty_rate_pct

ols <- lm(equation, data = base_dept)
mco <- ols  # alias
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

# 4.1 Model selection under Queen W

moran_lmQ <- lm.morantest(ols, WQueen, alternative = "two.sided")
moran_lmQ

# LM tests
LM_Q <- lm.LMtests(ols, WQueen, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))
LM_Q

# Elhorst LR tests
sdmQ <- lagsarlm(equation, data = base_dept, listw = WQueen, type = "mixed")
summary(sdmQ)

semQ <- errorsarlm(equation, data = base_dept, listw = WQueen, method = "eigen")
sem_model_q <- semQ  # alias
summary(semQ)

TestSDM_SEM_Q <- LR.Sarlm(sdmQ, semQ)
TestSDM_SEM_Q

# SDM vs SAR
sarQ <- lagsarlm(equation, data = base_dept, listw = WQueen, method = "eigen")
lag_model_q <- sarQ  # alias
summary(sarQ)

TestSDM_SAR_Q <- LR.Sarlm(sdmQ, sarQ)
TestSDM_SAR_Q

# 4.2 Model selection under KNN-3 W

moran_lmKNN <- lm.morantest(ols, KNN, alternative = "two.sided")
moran_lmKNN

# LM tests
LM_KNN <- lm.LMtests(ols, KNN, test = c("LMerr", "LMlag", "RLMerr", "RLMlag"))
LM_KNN

# Elhorst LR tests
sdmKNN <- lagsarlm(equation, data = base_dept, listw = KNN, type = "mixed")
summary(sdmKNN)

semKNN <- errorsarlm(equation, data = base_dept, listw = KNN, method = "eigen")
sem_model <- semKNN  # alias
summary(semKNN)

TestSDM_SEM_KNN <- LR.Sarlm(sdmKNN, semKNN)
TestSDM_SEM_KNN

# SDM vs SAR
sarKNN <- lagsarlm(equation, data = base_dept, listw = KNN, method = "eigen")
lag_model <- sarKNN  # alias
summary(sarKNN)

TestSDM_SAR_KNN <- LR.Sarlm(sdmKNN, sarKNN)
TestSDM_SAR_KNN

# helpers
get_rho <- function(m) if (!is.null(m$rho)) round(m$rho, 3) else ""
get_lambda <- function(m) if (!is.null(m$lambda)) round(m$lambda, 3) else ""

cov_labels <- c(
    "Share single mothers",
    "Number of immigrants",
    "Pop. change 10y (%)",
    "Unemployment rate (%)",
    "Poverty rate (%)"
)

lm_resid_sar_q   <- 0.38173
lm_resid_sar_ppv <- 0.10228

stargazer(
    lag_model_q, sem_model_q, lag_model, sem_model,
    type = "text",
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
par(mfrow = c(1, 1))

# SDM (Spatial Durbin Model, type="mixed")

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
rho_val  <- sdm_ppv$rho
rho_val2 <- sdm_q$rho

stargazer(
    sdm_ppv, sdm_q,
    type = "text",
    title = "Spatial Durbin Model (SDM) – k-nearest neighbours",
    digits = 3,
    omit.stat = c("rsq", "adj.rsq", "f"),
    add.lines = list(c("Spatial lag parameter (rho)", round(rho_val, 3), round(rho_val2, 3))),
    out = "sdm_model.txt"
)

AIC(sdm_ppv)
hist(residuals(sdm_ppv))

# SDM vs SEM
TestSDM_SEM  <- LR.Sarlm(sdm_ppv, sem_model)
TestSDMq_SEM <- LR.Sarlm(sdm_q, sem_model)
TestSDM_SEM
TestSDMq_SEM


# 4.3 KNN-6 robustness check

sar_knn6 <- lagsarlm(equation, data = base_dept, listw = KNN6, method = "eigen",
                     zero.policy = TRUE)
summary(sar_knn6)

sem_knn6 <- errorsarlm(equation, data = base_dept, listw = KNN6, method = "eigen",
                       zero.policy = TRUE)
summary(sem_knn6)

sdm_knn6 <- lagsarlm(equation, data = base_dept, listw = KNN6, type = "mixed",
                     method = "eigen", zero.policy = TRUE)
summary(sdm_knn6)

c(SAR = AIC(sar_knn6), SEM = AIC(sem_knn6), SDM = AIC(sdm_knn6))


# 4.4 SDM impact decomposition (preferred spec: Queen W)

set.seed(1234)

sdm_impacts_q <- impacts(sdm_q, listw = WQueen, R = 1000)
summary(sdm_impacts_q, zstats = TRUE, short = TRUE)

sdm_impacts_knn <- impacts(sdm_ppv, listw = PPV, R = 1000)
summary(sdm_impacts_knn, zstats = TRUE, short = TRUE)


# 4.5 SLX model

slx_q   <- lmSLX(equation, data = base_dept, listw = WQueen, zero.policy = TRUE)
slx_knn <- lmSLX(equation, data = base_dept, listw = KNN,    zero.policy = TRUE)

summary(slx_q)
summary(slx_knn)

# Impact decomposition (direct = beta, indirect = theta)
impacts(slx_q)
impacts(slx_knn)

# LR test: SDM vs SLX (H0: rho = 0, df = 1)
lr_sdm_slx_q_stat   <- as.numeric(-2 * (logLik(slx_q)   - logLik(sdm_q)))
lr_sdm_slx_q_pval   <- pchisq(lr_sdm_slx_q_stat,   df = 1, lower.tail = FALSE)
lr_sdm_slx_knn_stat <- as.numeric(-2 * (logLik(slx_knn) - logLik(sdm_ppv)))
lr_sdm_slx_knn_pval <- pchisq(lr_sdm_slx_knn_stat, df = 1, lower.tail = FALSE)

data.frame(
    W    = c("Queen", "KNN-3"),
    chi2 = round(c(lr_sdm_slx_q_stat, lr_sdm_slx_knn_stat), 3),
    p    = round(c(lr_sdm_slx_q_pval, lr_sdm_slx_knn_pval), 4)
)


# 4.6 AIC comparison across all 9 models

aic_df <- data.frame(
    Model = c(
        "OLS",
        "SEM (Queen)", "SAR (Queen)", "SLX (Queen)", "SDM (Queen)",
        "SEM (KNN-3)", "SAR (KNN-3)", "SLX (KNN-3)", "SDM (KNN-3)"
    ),
    AIC = round(c(
        AIC(ols),
        AIC(sem_model_q), AIC(lag_model_q), AIC(slx_q),   AIC(sdm_q),
        AIC(sem_model),   AIC(lag_model),   AIC(slx_knn), AIC(sdm_ppv)
    ), 2)
)

aic_df[order(aic_df$AIC), ]
