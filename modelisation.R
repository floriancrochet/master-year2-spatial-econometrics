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

k <- 6
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

globalMoranPPV<-moran.test(base_dept$Nb_log_sociaux_10000hab, 
                            PPV, 
                            zero.policy=TRUE,
                            randomisation=TRUE)
globalMoranPPV

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
                           main="Matrice type PPV (6 nearest neighbours)",
                           labels=as.character(base_dept$NOM))


######====models=====######

# benchmark OLS model
equation <- Nb_log_sociaux_10000hab~Part_femmes_seuls_enfant+Nb_immigres+population_change_10yrs_pct+unemployment_rate_pct+poverty_rate_pct
mco<-lm(equation, data=base_dept)
summary(mco)

# Moran test
moran.lm<-lm.morantest(mco, WQueen, alternative="two.sided")
print(moran.lm)
moran.lm.ppv<-lm.morantest(mco, PPV, alternative="two.sided")
print(moran.lm.ppv)

# Lagrange test between SAR and SEM
LMQueen<-lm.LMtests(mco, WQueen, test=c("LMerr", "LMlag", "RLMerr", "RLMlag")) #robust versions included
LM<-lm.LMtests(mco, PPV, test=c("LMerr", "LMlag", "RLMerr", "RLMlag")) #robust versions included
print(LM)
# choose?

# SEM: Spatial Error Model
# Y = XB + u, u = lambda*Wu + e
sem_model<-errorsarlm(equation, 
                      data=base_dept, 
                      listw=PPV, 
                      method="eigen",
                      zero.policy=TRUE)
summary(sem_model)
#stargazer(sem_model, type="text", title="Spatial Error Model Results", digits=3, out="sem_model.txt")

# SAR: Spatial Lag Model
# Y = rho*WY + XB + e
lag_model<-lagsarlm(equation, 
                    data=base_dept, 
                    listw=PPV, 
                    method="eigen",
                    zero.policy=TRUE)
summary(lag_model)
#stargazer(lag_model, type="text", title="Spatial Lag Model Results", digits=3, out="lag_model.txt")  
# Compare models
AIC(mco, sem_model, lag_model)
BIC(mco, sem_model, lag_model) 

hist(residuals(sem_model))
hist(residuals(lag_model))

#Plan Elhorst (2010)
library(spatialreg)
#SLX: Spatial Lag of X Model
slx<-lmSLX(equation, 
           data=base_dept, 
           PPV)
summary(slx)
#stargazer(slx, type="text", title="Spatial Lag of X Model Results", digits=3, out="slx_model.txt")
AIC(slx)
impacts(slx, listw=PPV)

#SDM: Spatial Durbin Model
# Y = rho*WY + XB + WX*gamma + e
# type="mixed" car équivalent à lagsarlm avec Durbin=TRUE

sdm<-lagsarlm(equation, data=base_dept, listw=PPV, type="mixed")
summary(sdm)
#stargazer(sdm, type="text", title="Spatial Durbin Model Results", digits=3, out="sdm_model.txt")
AIC(sdm)
impacts(sdm, listw=PPV, R=1000)
hist(residuals(sdm))

# Comparison SDM vs SEM
TestSDM_SEM<-LR.Sarlm(sdm,sem_model)
print(TestSDM_SEM)

