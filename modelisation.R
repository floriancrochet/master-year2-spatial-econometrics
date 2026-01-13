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

#data import
df_social <- read_excel("Projet4.xlsx")
carte_dept <- st_read("DEPARTEMENT.shp", quiet = TRUE)
st_crs(carte_dept)

base_dept <- merge(carte_dept, df_social, by.x = "INSEE_DEP", by.y = "Code_INSEE") #94 deps

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