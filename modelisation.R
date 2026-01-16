setwd('/Users/ss/Documents/ECAP2/Econ Spatiale/master-year2-spatial-econometrics')
library(sf)
library(ggplot2)
library(readxl)
library(dplyr)
library(spdep)
library(tidyr)
library(spatialreg)
library(car)
library(broom)
library(stargazer)
library(readr)

#data import
df_social <- read_excel("Projet4.xlsx")
df_extra <- read_delim("base_extra.csv", delim=";")
carte_dept <- st_read("DEPARTEMENT.shp", quiet = TRUE)
st_crs(carte_dept)

base_dept <- merge(carte_dept, df_social, by.x = "INSEE_DEP", by.y = "Code_INSEE") #94 deps

#new data import
str(df_extra)
df_extra_22 <- df_extra %>% 
  dplyr::select(`année_publication`, 
                `code_departement`, 
                #`nom_departement`, 
         #`code_region`, `nom_region`,
         `Nombre de logements`,
         `Variation de la population sur 10 ans (en %)`,
         `Taux de chômage au T4 (en %)`,
         `Taux de pauvreté* (en %)`,
         `Taux de logements sociaux* (en %)`) %>% 
  rename(publication_year = `année_publication`,
         department_code = `code_departement`,
         #department_name = `nom_departement`,
         #region_code = `code_region`,
         #region_name = `nom_region`,
         #nb_logement = `Nombre de logements`,
         population_change_10yrs_pct = `Variation de la population sur 10 ans (en %)`,
         unemployment_rate_pct = `Taux de chômage au T4 (en %)`,
         poverty_rate_pct = `Taux de pauvreté* (en %)`,
         social_housing_rate_pct = `Taux de logements sociaux* (en %)`) %>%
  filter(publication_year == 2022)
#merge new data
base_dept <- merge(base_dept, df_extra_22, by.x = "INSEE_DEP", by.y = "department_code") #94 deps
base_dept_simple <- st_drop_geometry(base_dept)
#write.csv2(base_dept_simple, "base_dept_final.csv", row.names = FALSE)
######====prepare the data/weights/matrix=====######

#Queen weights
nb_queen <- poly2nb(base_dept, queen = TRUE)
nb_linesR <- nb2lines(nb_queen, 
                      coords = st_centroid(st_geometry(base_dept)))
nb_sfR <- st_as_sf(nb_linesR)

#Queen carto
ggplot() + 
  geom_sf(data = base_dept, fill = "lightgreen", color = "black") + 
  geom_sf(data = nb_sfR, color = "blue") +
  theme_minimal() +
  ggtitle("Contingency map - Type Queen")

# NN weights
crs <- st_crs(base_dept)
centroids<-st_centroid(st_geometry(base_dept))
coords<-st_coordinates(centroids)
coords_sf<-st_as_sf(as.data.frame(coords), coords= c("X", "Y"), crs = crs)
coords_sp<-as(coords_sf, "Spatial")

k <- 4
knn_neighbours<-knearneigh(coords, k = k)
neighbors<-knn2nb(knn_neighbours)
summary(neighbors)

nb_linesNN<-nb2lines(neighbors, coords=coords_sp)
nb_sfNN<-st_as_sf(nb_linesNN)

#NN carto
ggplot() + 
  geom_sf(data = base_dept, fill = "lightgreen", color = "black") + 
  geom_sf(data = nb_sfNN, color = "blue") +
  theme_minimal() +
  ggtitle(paste("Contingency map - Type", k, "nearest neighbours"))

#matrices
WQueen<-nb2listw(nb_queen,style="W", zero.policy=TRUE)
PPV<-nb2listw(neighbors,style="W", zero.policy=TRUE)


######====moran test global=====######
globalMoranQ<-moran.test(base_dept$Nb_log_sociaux_10000hab, 
                         WQueen, 
                         zero.policy=TRUE,
                         randomisation=TRUE)
globalMoranQ
set.seed(1234)
moran_MC_Q = moran.mc(base_dept$Nb_log_sociaux_10000hab, 
                     WQueen, 
                     nsim = 999, 
                     zero.policy = TRUE)
moran_MC_Q

globalMoranPPV<-moran.test(base_dept$Nb_log_sociaux_10000hab, 
                            PPV, 
                            zero.policy=TRUE,
                            randomisation=TRUE)
globalMoranPPV
set.seed(1234)
moran_MC_PPV = moran.mc(base_dept$Nb_log_sociaux_10000hab, 
                     PPV, 
                     nsim = 999, 
                     zero.policy = TRUE)
moran_MC_PPV

#moran diagram
base_dept$scaled<- scale(base_dept$Nb_log_sociaux_10000hab)
moran_plot<-moran.plot(as.vector(base_dept$scaled),
                       WQueen, 
                       xlab="Number of social housings per 10,000 inhabitants",
                       ylab="Lag Number of social housings per 10,000 inhabitants",
                       main="Matrice type Queen",
                       labels=as.character(base_dept$NOM))

moran_plot_ppv<-moran.plot(as.vector(base_dept$scaled),
                           PPV, 
                           xlab="Number of social housings per 10,000 inhabitants",
                           ylab="Lag Number of social housings per 10,000 inhabitants",
                           main="Matrice type PPV (4 nearest neighbours)",
                           labels=as.character(base_dept$NOM))


######====models=====######

# benchmark OLS model

equation <- Nb_log_sociaux_10000hab~Part_femmes_seuls_enfant+Nb_immigres+population_change_10yrs_pct+poverty_rate_pct
mco<-lm(equation, data=base_dept)
summary(mco)
vif(mco)
base_dept$residuals_ols <- residuals(mco)

library(ggplot2)
ggplot(base_dept) +
  geom_sf(aes(fill = residuals_ols)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  theme_minimal() +
  labs(title = "OLS residuals: spatial distribution")

# Moran test
moran.lm<-lm.morantest(mco, WQueen, alternative="two.sided")
print(moran.lm)
moran.lm.ppv<-lm.morantest(mco, PPV, alternative="two.sided")
print(moran.lm.ppv)

# Lagrange test between SAR and SEM
LMQueen<-lm.LMtests(mco, WQueen, test=c("LMerr", "LMlag", "RLMerr", "RLMlag")) #robust versions included
LM<-lm.LMtests(mco, PPV, test=c("LMerr", "LMlag", "RLMerr", "RLMlag")) #robust versions included
print(LMQueen)
print(LM)
# choose?

# SEM: Spatial Error Model
# Y = XB + u, u = lambda*Wu + e
sem_model_q<-errorsarlm(equation, 
                      data=base_dept, 
                      listw=WQueen, 
                      method="eigen",
                      zero.policy=TRUE)
sem_model<-errorsarlm(equation, 
                        data=base_dept, 
                        listw=PPV, 
                        method="eigen",
                        zero.policy=TRUE)
summary(sem_model_q)

# SAR: Spatial Lag Model
# Y = rho*WY + XB + e
lag_model_q<-lagsarlm(equation, 
                    data=base_dept, 
                    listw=WQueen, 
                    method="eigen",
                    zero.policy=TRUE)
lag_model<-lagsarlm(equation, 
                    data=base_dept, 
                    listw=PPV, 
                    method="eigen",
                    zero.policy=TRUE)
summary(lag_model_q)
summary(lag_model)

#results formatin
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
lm_resid_sar_q   <- 0.38173
lm_resid_sar_ppv <- 0.10228


stargazer(
  lag_model_q, sem_model_q, lag_model, sem_model,
  type  = "text",                 # change to "latex" if needed
  title = "Spatial regression models (SAR vs SEM) under alternative weight matrices",
  digits = 3,
  column.labels = c("SAR", "SEM", "SAR", "SEM"),
  column.separate = c(1,1,1,1),
  dep.var.labels = "Social housing per 10,000 (2024)",
  covariate.labels = cov_labels,
  omit.stat = c("rsq", "adj.rsq", "f"),
  
  add.lines = list(
    c("Weights matrix", "Queen", "Queen", "k-NN (PPV)", "k-NN (PPV)"),
    c("Spatial parameter",
      paste0("rho=", get_rho(lag_model_q)),
      paste0("lambda=", get_lambda(sem_model_q)),
      paste0("rho=", get_rho(lag_model)),
      paste0("lambda=", get_lambda(sem_model))
    ),
    c("LM residual test (p-value)",
      round(lm_resid_sar_q, 3),
      "— (absorbed by λ)",
      round(lm_resid_sar_ppv, 3),
      "— (absorbed by λ)"
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

#Plan Elhorst (2010)
library(spatialreg)

#SDM: Spatial Durbin Model
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
rho_se  <- sqrt(diag(sdm_ppv$asyvar))["rho"]
rho_val2 <- sdm_q$rho
rho_se2  <- sqrt(diag(sdm_q$asyvar))["rho"]


stargazer(
  sdm_ppv,sdm_q,
  type = "text",
  title = "Spatial Durbin Model (SDM) – k-nearest neighbours",
  digits = 3,
  #covariate.labels = pretty_names,
  omit.stat = c("rsq", "adj.rsq", "f"),
  add.lines = list(c("Spatial lag parameter (rho)", round(rho_val, 3), round(rho_val2, 3))),
  out = "sdm_model.txt"
)

AIC(sdm_ppv)
impacts(sdm_ppv, listw=PPV, R=1000)
hist(residuals(sdm_ppv))

# Comparison SDM vs SEM
TestSDM_SEM<-LR.Sarlm(sdm_ppv, sem_model)
TestSDMq_SEM<-LR.Sarlm(sdm_q, sem_model)

print(TestSDM_SEM)
print(TestSDMq_SEM)

