################################################################################
#                           D4 -- Script Unificat                              #
################################################################################

#CARREGAR 
library(tm)
library(tidytext)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggplot2)
library(sf)
library(dplyr)
library(maps)
library(leaflet)
library(tmap)
library(tidycensus)
library(tigris)
library(units)
library(SnowballC)
library(slam)
library(Matrix)
library(xlsx)
library(topicmodels)
library(RColorBrewer)
library(wordcloud)
library(remotes)
library(lda)
library(raster)
library(spatstat)
library(geosphere)
library(viridis)
library(gganimate)
library(lubridate)
library(gridExtra)

# LECTURA BASE DE DADES

path <- "C:/Users/sambr/Desktop/FIB/PMAAD/Laboratori/Projecte/text/" # <- Escriure el seu path
base_de_dades <- read.csv(paste0(path,"dataset_imputado_final.csv"), header = TRUE, sep = ",")

                             ###############             
##############################  1 -- MCA   #####################################
                             ###############             

df <- base_de_dades
# 1. Eliminar columnes no rellevants (temps, longitud, latitud)
cols_to_remove <- c("Start_Time", "Start_Lat", "Start_Lng", "Description", "Start_Date", "Longitude")
df <- df[, !(names(df) %in% cols_to_remove)]

# 2. Forçar columnes amb menys de X valors únics com a factors
threshold <- 10
qualitative_cols <- sapply(df, function(col) length(unique(col)) < threshold)
df[qualitative_cols] <- lapply(df[qualitative_cols], factor)

# 3. Filtrar només les columnes que són factors
df.mca <- df[, sapply(df, is.factor)]

cat("Variables qualitatives utilitzades:\n")
print(colnames(df.mca))

# 4. Executar MCA
res.mca <- MCA(df.mca, method = "Indicator", graph = FALSE)

# 5. Gràfiques
fviz_screeplot(res.mca, addlabels = TRUE)

fviz_mca_var(res.mca,
             select.var = list(cos2 = 0.3),
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             ggtheme = theme_minimal())

fviz_mca_biplot(res.mca, repel = TRUE, ggtheme = theme_minimal()) # mostra tots els individus

fviz_mca_biplot(res.mca,
                select.ind = list(contrib = 100),
                repel = TRUE)                           # mostra els individus més rellevants





                     ################             
######################  2 -- FAMD   ############################################
                     ################


# Eliminem variables irrellevants
df_famd <- base_de_dades[, !(names(base_de_dades) %in% c("Source", "Start_Time", "Description", "Street", 
                                   "City", "County", "Start_Lat", "Start_Lng", 
                                   "Start_Date", "idx_original" ))]

# Canviem les etiquetes de les variables booleanes 
vars_booleanas <- c("Amenity", "Crossing", "Junction", "Railway", "Station", "Stop", "Traffic_Signal", "is_day_light", 
                    "hospital_nearby", "traffic_light_nearby", "traffic_sign_nearby", "police_nearby", "commercial_area_nearby", 
                    "school_zone_nearby")
for (bool in vars_booleanas) {
  df_famd[[bool]] <- as.logical(df_famd[[bool]])
  df_famd[[bool]] <- ifelse(df_famd[[bool]], paste0(bool, "_TRUE"), paste0(bool, "_FALSE"))
}

head(df_famd, 4)
str(df_famd)

# FAMD
res.famd <- FAMD(df_famd,ncp=20, graph = FALSE)

# Eigenvalues i variàncies de les dimensions
eig.val <- get_eigenvalue(res.famd)
eig.val

# Plot percentatge inèrcia
fviz_screeplot(res.famd,ncp=20)

res.famd <- FAMD(df_famd,ncp=8, graph = FALSE)

var <- get_famd_var(res.famd)
head(var$coord)v # Coordenades de les variables
head(var$cos2) # Cos2, qualitat de la representació
head(var$contrib) # Contribució a les dimensions

# Variables

# Plot 
fviz_famd_var(res.famd,axes = c(1, 2), repel = TRUE)
fviz_famd_var(res.famd,axes = c(3, 4), repel = TRUE)
fviz_famd_var(res.famd,axes = c(5, 6), repel = TRUE)
fviz_famd_var(res.famd,axes = c(7, 8), repel = TRUE)

#Contribució a les dimensions:
fviz_contrib(res.famd, "var", axes = 1,top = 15) # Dim 1
fviz_contrib(res.famd, "var", axes = 2,top = 15) # Dim 2
fviz_contrib(res.famd, "var", axes = 3,top = 15) # Dim 3
fviz_contrib(res.famd, "var", axes = 4,top = 15) # Dim 4
fviz_contrib(res.famd, "var", axes = 5,top = 15) # Dim 5
fviz_contrib(res.famd, "var", axes = 6,top = 15) # Dim 6
fviz_contrib(res.famd, "var", axes = 7,top = 15) # Dim 7
fviz_contrib(res.famd, "var", axes = 8,top = 15) # Dim 8

# Variables numèriques
# Plot segons la contribució
quanti.var <- get_famd_var(res.famd, "quanti.var")
fviz_famd_var(res.famd, "quanti.var",axes = c(1, 2), col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
fviz_famd_var(res.famd, "quanti.var",axes = c(1, 3), col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
# Plot segons la qualitat de representacio
fviz_famd_var(res.famd, "quanti.var",axes = c(1, 2), col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)
fviz_famd_var(res.famd, "quanti.var",axes = c(1, 3), col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)

# Variables Categòriques
quali.var <- get_famd_var(res.famd, "quali.var")
contribuciones <- res.famd$var$contrib
head(contribuciones)
# Plot segons la contribució
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Plot amb només les més importants
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              select.var = list(contrib = 15)) 
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", axes=c(3,4),
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              select.var = list(contrib = 15)) 

# Individuus
ind <- get_famd_ind(res.famd)
fviz_famd_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE,label = "none")
fviz_famd_ind(res.famd, col.ind = "cos2", axes=c(3,4),
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE,label = "none")
fviz_famd_ind(res.famd, col.ind = "cos2", axes=c(5,6),
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE,label = "none")
fviz_famd_ind(res.famd, col.ind = "cos2", axes=c(7,8),
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE,label = "none")

# Per variables
fviz_mfa_ind(res.famd, 
             habillage = "traffic_light_nearby", 
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE 
             ,label = "none")

fviz_mfa_ind(res.famd, 
             habillage = "Visibility_Category", # color by groups 
             repel = TRUE # Avoid text overlapping
             ,label = "none")

fviz_mfa_ind(res.famd, 
             habillage = "school_zone_nearby", # color by groups 
             repel = TRUE # Avoid text overlapping
             ,label = "none")

fviz_mfa_ind(res.famd, 
             habillage = "Start_Time_Cat", axes=c(1,7),
             palette = c( "#E7B800","#00AFBB","#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE 
             ,label = "none")



# CLUSTERING
#res.hcpc <- HCPC(res.famd, graph = TRUE)
# Millors resultats amb 3 clusters:
res.hcpc <- HCPC(res.famd, nb.clust = 3, graph = TRUE)
res.hcpc$data.clust
table(res.hcpc$data.clust$clust)
# Plot clusters
fviz_cluster(res.hcpc, repel = TRUE, geom = "point")
fviz_cluster(res.hcpc, repel = TRUE,axes=c(3,4), geom = "point")
fviz_cluster(res.hcpc, repel = TRUE,axes=c(5,6), geom = "point")
fviz_cluster(res.hcpc, repel = TRUE,axes=c(7,8), geom = "point")

#CPG
ClassPanelGraph <- function(var, data, cluster_hier) {
  if (is.numeric(data[,var])){
    plot <- ggplot(data = data, aes(x = data[, var])) +
      geom_histogram(fill = "gray", color = "black") +
      facet_grid(get(cluster_hier) ~ .) + ylab("") + xlab(var)
  } else {
    plot <- ggplot(data = data, aes(x = data[, var])) +
      geom_bar(fill = "gray", color = "black") +
      facet_grid(get(cluster_hier) ~ .) + ylab( "") + xlab(var) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  return(plot)
}
datos <- res.hcpc$data.clust
datos$cluster <- as.factor(datos$clust)  # renombramos de "clust" a "cluster"
datos <- subset(datos, select = -clust)
tipos <- sapply(datos, class)
varNum <- names(tipos)[which(tipos %in% c("integer", "numeric"))]
varCat <- names(tipos)[which(tipos %in% c("factor", "character","logical"))]
varCat <- varCat[varCat != "cluster"]

plots <- lapply(c(varNum, varCat), ClassPanelGraph, data = datos, 
                cluster_hier = "cluster")
CPG <- ggarrange(plotlist = plots, ncol = 4, nrow = ceiling(length(plots) / 4))
#ggsave(paste0(path,"Plots/CPG_dataset2.png"), CPG, dpi = 150, width = 14, height = 14, units = "in")
plot(CPG)

# Termòmetre
# Numèriques:
verd2vermell <- varNum # Totes van de verd a vermell
df_clustered <- datos %>% group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>% data.frame()
datos_modelo <- df_clustered %>% 
  pivot_longer(!cluster, names_to = "variable", values_to = "sum") %>% 
  data.frame()
quien <- which(datos_modelo$variable %in% verd2vermell)
datos_modelo[quien, "direccion"] <- 1
datos_modelo[which(is.na(datos_modelo$direccion)), "direccion"] <- -1
listaDatos <- list()

for (var in varNum) {
  min_val <- min(datos[[var]], na.rm = TRUE)
  max_val <- max(datos[[var]], na.rm = TRUE)
  tall1_val <- quantile(datos[[var]], 0.33, na.rm = TRUE)
  tall2_val <- quantile(datos[[var]], 0.66, na.rm = TRUE)
  if (min_val == 0 && tall1_val == 0) {
    tall1_val <- 0.001
  }
  subtabla <- datos_modelo[which(datos_modelo$variable == var), ]
  # Classifiquem
  subtabla$grupo <- cut(subtabla$sum, 
                        breaks = c(min_val, tall1_val, tall2_val, max_val), 
                        labels = c(1, 2, 3), 
                        include.lowest = TRUE)
  # Assignem colors
  subtabla$color <- ifelse(subtabla$direccion == -1, 
                           ifelse(subtabla$grupo == "1", "red",
                                  ifelse(subtabla$grupo == "2", "yellow", "green")),
                           ifelse(subtabla$grupo == "1", "green",
                                  ifelse(subtabla$grupo == "2", "yellow", "red"))) 
  subtabla[, c("sum", "direccion", "grupo")] <- NULL
  listaDatos[[var]] <- subtabla
}
varNumColor <- dplyr::bind_rows(listaDatos)

# Categòriques
# Calculem les modalitats
modalidades <- c(); variables <- c()
for (vC in varCat) {
  mod <- unique(as.character(datos[, vC])); modalidades <- c(modalidades, mod)
  variables <- c(variables, rep(vC, length(mod)))
}
dfCate <- data.frame(variables, modalidades)

# Creem un excel per posar els colors de cada modalitat
# ***(No cal exectutar si la taula ja està feta)***
#xlsx::write.xlsx(dfCate, file = paste0(path, "variables_categoricas2.xlsx"), sheetName = "categorias", col.names = TRUE, row.names = FALSE)

# Llegim la taula completada
df <- xlsx::read.xlsx(file = paste0(path, "variables_categoricas2.xlsx"), 
                      sheetName = "categorias")
# Obtenim els colors
color_mapping_cat <- split(df, df$variables)  
color_mapping_cat <- lapply(color_mapping_cat, function(x) {
  setNames(as.list(x$color), x$modalidades) 
})
df$id <- paste0(df$variables, "_", df$modalidades)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
df_moda <- datos %>%
  select(cluster, all_of(varCat)) %>%
  pivot_longer(cols = -cluster, names_to = "variable", values_to = "valor") %>%
  group_by(cluster, variable) %>%
  summarise(moda = get_mode(valor), .groups = "drop") %>% data.frame()

df_moda$id <- paste0(df_moda$variable, "_", df_moda$moda)

m <- match(df_moda$id, df$id)
df_moda$color <- df[m, "color"]
varCatColor <- df_moda %>% select(-moda, -id) %>% data.frame()

# Unim les dues bases de dades
dfColor <- rbind(varNumColor, varCatColor)

# TLP 
# Complet
ggplot(dfColor, aes(x = variable, y = factor(cluster), fill = color)) +
  geom_tile(color = "black", size = 0.5) +
  scale_fill_manual(values = c("red" = "red", "yellow" = "yellow", "green" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Traffic Light Panel (TLP)", x = "Variables", y = "Clusters")

#ggsave(filename = paste0(path, "Plots/TLPCompleto.png"), width = 6, height = 4, bg = "white")

# Variables discriminatòries
dfColorRed <- dfColor
for (var in unique(dfColorRed$variable)) {
  subset <- dfColorRed[which(dfColorRed$variable == var), ]
  if (length(unique(subset$color)) == 1) {
    dfColorRed <- dfColorRed %>% 
      filter(variable != var)
  }
}
ggplot(dfColorRed, aes(x = variable, y = factor(cluster), fill = color)) +
  geom_tile(color = "black", size = 0.5) +
  scale_fill_manual(values = c("red" = "red", "yellow" = "yellow", "green" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Traffic Light Panel (TLP)", x = "Variables", y = "Clusters")

#ggsave(filename = paste0(path, "Plots/TLP_VariablesDiscriminantes.png"), width = 6, height = 4, bg = "white")

# aTLP
# Calculem el CV de les numèriques
cv <- function(x, na.rm = TRUE) {
  sd(x, na.rm = na.rm) / mean(x, na.rm = na.rm)
}
dfCV <- datos %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), cv, na.rm = TRUE)) %>%
  pivot_longer(!cluster, names_to = "variable", values_to = "cv") %>% 
  data.frame()
m <- match(paste0(dfColor$cluster, dfColor$variable), paste0(dfCV$cluster, dfCV$variable))
dfColor[, "cv"] <- dfCV[m, "cv"]
dfColor <- dfColor %>%
  mutate(R = NA, G = NA, B = NA) %>%
  mutate( 
    R = ifelse(color == "red", 255, R),
    G = ifelse(color == "red", 0, G),
    B = ifelse(color == "red", 0, B)
  ) %>%
  mutate( 
    R = ifelse(color == "green", 0, R),
    G = ifelse(color == "green", 255, G),
    B = ifelse(color == "green", 0, B)
  ) %>% mutate( 
    R = ifelse(color == "yellow", 255, R),
    G = ifelse(color == "yellow", 255, G),
    B = ifelse(color == "yellow", 0, B)
  )

dfColor$cv[is.na(dfColor$cv)] <- 0 
# Modificació del coeficient de variació
sx <- 80 + 125*(1-dfColor$cv) + 50*(1-dfColor$cv)^2
sxa <- 180 + 180*(1-dfColor$cv) - 143*((1-dfColor$cv)^2) + 38*((1-dfColor$cv)^3)

dfColor[which(dfColor$color == "red"), "R"] <- sx[which(dfColor$color == "red")]
dfColor[which(dfColor$color == "green"), "G"] <- sx[which(dfColor$color == "green")]
dfColor[which(dfColor$color == "yellow"), "G"] <- sx[which(dfColor$color == "yellow")]
dfColor[which(dfColor$color == "yellow"), "R"] <- sxa[which(dfColor$color == "yellow")]
dfColor[which(is.na(dfColor$R)), "R"] <- 255
dfColor[which(is.na(dfColor$G)), "G"] <- 255
dfColor$R <- pmin(dfColor$R, 255)
dfColor$G <- pmin(dfColor$G, 255)
dfColor$B <- pmin(dfColor$B, 255)
dfColor[, "color"] <- rgb(dfColor$R, dfColor$G, dfColor$B, maxColorValue = 255)
dfColor[, c("cv", "R", "G", "B")] <- NULL

# aTLP Complet
ggplot(dfColor, aes(x = variable, y = factor(cluster), fill = color)) +
  geom_tile(color = "black", size = 0.5) +
  scale_fill_identity() +  # Usa los colores tal como están en el dataframe
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Annotated Traffic Light Panel (aTLP)", x = "Variables", y = "Clusters")

#ggsave(filename = paste0(path, "Plots/aTLPCompleto.png"), width = 6, height = 4, bg = "white")

# aTLP Variables Discriminatòries
dfTLP <- dfColorRed
dfColorRed <- dfColor[dfColor$variable %in% dfTLP$variable, ]
ggplot(dfColorRed, aes(x = variable, y = factor(cluster), fill = color)) +
  geom_tile(color = "black", size = 0.5) +
  scale_fill_identity() +  # Usa los colores tal como están en el dataframe
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Annotated Traffic Light Panel (aTLP)", x = "Variables", y = "Clusters")

#ggsave(filename = paste0(path, "Plots/aTLP_VariablesDiscriminantes.png"), width = 6, height = 4, bg = "white")


                     #################################             
######################  3 -- GEOSPATIAL DESCRIPTIVE   ##################################
                     #################################

df <- base_de_dades
#primera visualització dels accidents
ggplot(data = df,
       aes(x = Start_Lng , y = Start_Lat)) + 
  geom_point(col="blue", size = 0.1, alpha = 0.3) +
  coord_fixed()

#representació de cadascuna de les variables en format de mapa per cada punt
crs = 4326 
accidents_sf <- st_as_sf(df,
                         coords = c("Start_Lng", "Start_Lat"),
                         crs = 4326)
print(accidents_sf)       
summary(accidents_sf)    

#gràfics per cada variable
plot(accidents_sf, max.plot = 36)

#accidents al mapa general dels estats units
states <- map_data("state")

ggplot() +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = df,
             aes(x = Start_Lng, y = Start_Lat),
             color = "blue", size = 0.1, alpha = 0.3) +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Accidents de tràfic en MA, NJ i NY")


#mapa nomes dels 3 estats de la nostra base de dades
states_filtered <- states %>%
  filter(region %in% c("massachusetts", "new jersey", "new york"))

ggplot() +
  geom_polygon(data = states_filtered, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = df,
             aes(x = Start_Lng, y = Start_Lat),
             color = "blue", size = 0.1, alpha = 0.3) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents de tràfic a MA, NJ i NY")

ggplot() +
  geom_polygon(data = states_filtered, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = df,
             aes(x = Start_Lng, y = Start_Lat, color = State),  
             size = 0.1, alpha = 0.6) +  
  scale_color_manual(
    values = c("MA" = "blue", "NJ" = "green", "NY" = "red"),
    guide = guide_legend(override.aes = list(size = 4))  
  ) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents de tràfic per estats") +
  theme(legend.title = element_blank())


#visualitzaciÓ mapa interectiu amb carreteres
df_sf <- st_as_sf(df, 
                  coords = c("Start_Lng", "Start_Lat"), 
                  crs = 4326)

leaflet(df_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%  # Mapa base
  addCircleMarkers(radius = 3, 
                   fillOpacity = 0.7, 
                   stroke = FALSE, 
                   color = "blue") 

#analisi dels accidents per estats
table(df$State)
df_ma <- df %>% filter(State == "MA")
df_nj <- df %>% filter(State == "NJ")
df_ny <- df %>% filter(State == "NY")
ma_map <- states_filtered %>% filter(region == "massachusetts")
nj_map <- states_filtered %>% filter(region == "new jersey")
ny_map <- states_filtered %>% filter(region == "new york")


# Mapa per Massachusetts
ggplot(df_ma, aes(x = Start_Lng, y = Start_Lat)) +
  geom_point(color = "blue", size = 0.1, alpha = 0.6) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents a Massachusetts")

# Mapa per New Jersey
ggplot(df_nj, aes(x = Start_Lng, y = Start_Lat)) +
  geom_point(color = "green", size = 0.1, alpha = 0.6) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents a New Jersey")

# Mapa per New York
ggplot(df_ny, aes(x = Start_Lng, y = Start_Lat)) +
  geom_point(color = "red", size = 0.1, alpha = 0.6) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents a New York")

# Massachusetts
ggplot() +
  geom_polygon(data = ma_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "black") +
  geom_point(data = df_ma, aes(x = Start_Lng, y = Start_Lat),
             color = "blue", size = 0.1, alpha = 0.6) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents a Massachusetts")

# New Jersey
ggplot() +
  geom_polygon(data = nj_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "black") +
  geom_point(data = df_nj, aes(x = Start_Lng, y = Start_Lat),
             color = "green", size = 0.1, alpha = 0.6) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents a New Jersey")

# New York
ggplot() +
  geom_polygon(data = ny_map, aes(x = long, y = lat, group = group), 
               fill = "lightgrey", color = "black") +
  geom_point(data = df_ny, aes(x = Start_Lng, y = Start_Lat),
             color = "red", size = 0.1, alpha = 0.6) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents a New York")


#MAPES RESPECTE A LA POBLACIÓ DE CADA ESTAT
pop_data <- data.frame(
  State = c("MA", "NJ", "NY"),
  Population = c(6902149, 9218858, 20222664)
)

accident_counts <- df %>%
  filter(State %in% c("MA", "NJ", "NY")) %>%
  group_by(State) %>%
  summarise(Accidents = n())

state_stats <- left_join(accident_counts, pop_data, by = "State") %>%
  mutate(Accidents_per_million = (Accidents / Population) * 1e6)

state_stats$region <- c("massachusetts", "new jersey", "new york")

map_data <- left_join(states_filtered, state_stats, by = "region")
ggplot() +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group,
                                    fill = Accidents_per_million),
               color = "black") +
  scale_fill_gradient(low = "lightyellow", high = "red",
                      name = "Accidents\n(per milió)") +
  coord_fixed(1.3) +
  theme_minimal() +
  labs(title = "Densitat d'accidents de trànsit per milió d'habitants (2018)",
       subtitle = "Estats: MA, NJ, NY")


#població als estats al 2018
census_api_key("PONER_CLAVE_AQUÍ", install = TRUE, overwrite = TRUE)  

poblacion <- get_acs(
  geography = "county",
  variables = "B01003_001",  # Població total (ACS)
  year = 2018,               # Es pot fer sevir 2017 o 2018
  survey = "acs5",           
  state = c("NY", "MA", "NJ"),
  geometry = TRUE
) %>%
  mutate(
    NAME = gsub(" County, .*", "", NAME),
    # Calcular densitat (població/àrea en km²)
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    densidad = estimate / area_km2
  )

ggplot(poblacion) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c(
    option = "inferno",
    trans = "log10",
    name = "Població (2018)",
    labels = scales::comma_format()
  ) +
  theme_void() +
  labs(title = "Població per Condado (ACS 2018)")

ggplot(poblacion) +
  geom_sf(aes(fill = densidad), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = expression(Densidad~(hab/km^2)),
    labels = scales::comma_format()
  ) +
  theme_void()

#població dels estats al 2018 censal

estados <- c("NY", "MA", "NJ")
poblacion_tracts <- get_acs(geography = "tract",
                            variables = "B01003_001",
                            year = 2018,
                            state = estados,
                            geometry = TRUE,
                            output = "wide") %>%
  mutate(area_m2 = st_area(geometry),
         area_millas = as.numeric(set_units(area_m2, "mi^2")),
         densidad = B01003_001E / area_millas)


poblacion_tracts <- poblacion_tracts %>%
  mutate(densidad_cat = cut(densidad,
                            breaks = c(1, 10, 25, 50, 100, 250, 500, 1000, 2500, 5000, Inf),
                            labels = c("1...10", "10...25", "25...50", "50...100", "100...250",
                                       "250...500", "500...1000", "1000...2500", "2500...5000", ">5000"),
                            include.lowest = TRUE))

colores <- c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476",
             "#41ab5d", "#238b45", "#006d2c", "#00441b", "#bd0026")

ggplot(poblacion_tracts) +
  geom_sf(aes(fill = densidad_cat), color = NA) +
  scale_fill_manual(values = colores,
                    name = "Habitantes / milla²",
                    drop = FALSE) +
  labs(title = "Densitat de població per zona censal (2018)",
       subtitle = "Estats de Nueva York, Nueva Jersey i Massachusetts",
       caption = "Fuente: U.S. Census Bureau ACS 2018") +
  theme_minimal() +
  theme(legend.position = "right")


#MAPES PER VARIABLE
names(df)

#severity
unique(df$Severity_Category)

ggplot() +
  
  geom_sf(data = df_sf, aes(color = Severity_Category), size = 0.5, alpha = 0.6) +
  
  scale_color_manual(values = c( "Molt poc impacte"= "green", 
                                 "Poc impacte" = "yellow", 
                                 "Impacte considerable" = "orange", 
                                 "Impacte molt fort/alt" = "red"), 
                     guide = guide_legend(override.aes = list(size = 4))) + 
  
  coord_sf() +  
  theme_minimal() +
  labs(title = "Accidents segons Severity",
       color = "Severity")

ggplot(data = df_sf) +
  
  geom_sf(aes(color = Severity_Category), size = 0.5, alpha = 0.6) +
  
  scale_color_manual(values = c("Molt poc impacte" = "green", 
                                "Poc impacte" = "yellow", 
                                "Impacte considerable" = "orange", 
                                "Impacte molt fort/alt" = "red"),
                     guide = guide_legend(override.aes = list(size = 4))) + 
  
  coord_sf() +
  theme_minimal() +
  facet_wrap(~ Severity_Category) + 
  labs(title = "Accidents segons Severity desglossats",
       color = "Severity")


#Start_Time_Cat
unique(df$Start_Time_Cat)

ggplot() +
  
  geom_sf(data = df_sf, aes(color = Start_Time_Cat), size = 0.5, alpha = 0.6) +
  
  scale_color_manual(values = c( "Tarde"= "green", 
                                 "Mañana" = "lightblue", 
                                 "Noche" = "blue"), 
                     guide = guide_legend(override.aes = list(size = 4))) + 
  
  coord_sf() +  
  theme_minimal() +
  labs(title = "Accidents segons el moment del dia",
       color = "Start_Time_Cat")

ggplot(data = df_sf) +
  
  geom_sf(aes(color = Start_Time_Cat), size = 0.5, alpha = 0.6) +
  
  scale_color_manual(values = c( "Tarde"= "green", 
                                 "Mañana" = "lightblue", 
                                 "Noche" = "blue"), 
                     guide = guide_legend(override.aes = list(size = 4))) + 
  
  coord_sf() +  
  theme_minimal() +
  facet_wrap(~ Start_Time_Cat) + 
  labs(title = "Accidents segons el moment del dia",
       color = "Start_Time_Cat")

#condicions meteorologiques

str(df[, c("Humidity...", "Pressure.in.", "Wind_Speed.mph.", "Precipitation.in.", "Weather_Condition", "Wind_Direction", "Visibility_Category", "is_day_light", "Temperature.C.", "Wind_Chill.C.")])

#humidity

ggplot() +
  geom_sf(data = df_sf, aes(color = Humidity...), size = 0.5, alpha = 0.6) +
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  coord_sf() +
  theme_minimal() +
  labs(title = "Distribució de la humitat en els accidents",
       color = "Humitat (%)")

#temperature
ggplot(df_sf) +
  geom_sf(aes(color = `Temperature.C.`), size = 0.5, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", na.value = "grey80") +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Temperatura (ºC) en els accidents de trànsit",
    color = "Temperatura ºC"
  )

#precipitation
ggplot(df_sf) +
  geom_sf(aes(color = `Precipitation.in.`), size = 0.5, alpha = 0.7) +
  scale_color_viridis_c(option = "turbo", na.value = "grey80") +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Precipitació en els accidents de trànsit",
    color = "Precipitació "
  )

#wind_speed
ggplot(df_sf) +
  geom_sf(aes(color = `Wind_Speed.mph.`), size = 0.5, alpha = 0.7) +
  scale_color_viridis_c(option = "plasma", na.value = "grey80") +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Velocitat del vent (mph) en els accidents de trànsit",
    color = "Wind Speed (mph)"
  )

#Wind Direction
unique(df$Wind_Direction)

ggplot() +
  geom_sf(data = df_sf, aes(color = Wind_Direction), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "North" = "#1f77b4", "South" = "#ff7f0e", "East" = "#2ca02c", "West" = "#d62728",
      "NE" = "#9467bd", "NW" = "#8c564b", "SE" = "#e377c2", "SW" = "#7f7f7f",
      "NNE" = "#bcbd22", "ENE" = "#17becf", "ESE" = "#aec7e8", "SSE" = "#ffbb78",
      "SSW" = "#98df8a", "WSW" = "#ff9896", "WNW" = "#c5b0d5", "NNW" = "#c49c94",
      "Variable" = "black", "Calm" = "grey"
    ),
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Accidents segons la direcció del vent",
    color = "Direcció del vent"
  )

#weather condition
unique(df$Weather_Condition)

ggplot() +
  geom_sf(data = df_sf, aes(color = Weather_Condition), size = 0.5, alpha = 0.6) +
  scale_color_viridis_d(option = "turbo", guide = guide_legend(override.aes = list(size = 4))) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Accidents segons la condició meteorològica",
    color = "Condició meteo"
  )

#visibility
unique(df$Visibility_Category)

ggplot() +
  geom_sf(data = df_sf, aes(color = Visibility_Category), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "Bona_Visibilitat" = "green",
      "Poca_Visibilitat" = "yellow",
      "Molt_Poca_Visibilitat" = "red",
      "Exelent_Visibilitat" = "blue"
    ),
    na.translate = FALSE,  
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Accidents segons la visibilitat",
    color = "Visibilitat"
  )

#MAPES NEARBY

unique(df$hospital_nearby)
ggplot() +
  geom_sf(data = df_sf, aes(color = hospital_nearby), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "TRUE" = "green",
      "FALSE" = "red"
    ),
    na.translate = FALSE,  
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Hospitals aprop",
    color = "Hospitals"
  )


unique(df$traffic_light_nearby)
ggplot() +
  geom_sf(data = df_sf, aes(color = traffic_light_nearby), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "TRUE" = "green",
      "FALSE" = "red"
    ),
    na.translate = FALSE,  
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Semàfors aprop",
    color = "Semàfors"
  )


unique(df$traffic_sign_nearby)
ggplot() +
  geom_sf(data = df_sf, aes(color = traffic_sign_nearby), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "TRUE" = "green",
      "FALSE" = "red"
    ),
    na.translate = FALSE,  
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Señals aprop",
    color = "Señals"
  )


unique(df$police_nearby)
ggplot() +
  geom_sf(data = df_sf, aes(color = police_nearby), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "TRUE" = "green",
      "FALSE" = "red"
    ),
    na.translate = FALSE,  
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "policia aprop",
    color = "Policia"
  )


unique(df$commercial_area_nearby)
ggplot() +
  geom_sf(data = df_sf, aes(color = commercial_area_nearby), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "TRUE" = "green",
      "FALSE" = "red"
    ),
    na.translate = FALSE,  
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Zona comercial aprop",
    color = "Zona comercial"
  )


unique(df$school_zone_nearby)
ggplot() +
  geom_sf(data = df_sf, aes(color = school_zone_nearby), size = 0.5, alpha = 0.6) +
  scale_color_manual(
    values = c(
      "TRUE" = "green",
      "FALSE" = "red"
    ),
    na.translate = FALSE,  
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "Zona escolar aprop",
    color = "Zona escolar"
  )


#mapa animat

df_sf <- st_as_sf(df, coords = c("Start_Lng", "Start_Lat"), crs = 4326)

bbox_3states <- st_bbox(df_sf)

# animació per any
accidents_anim <- tm_shape(df_sf, bbox = bbox_3states) + 
  tm_basemap("CartoDB.Positron") +  
  tm_dots(col = "State",            
          palette = c("MA" = "#4daf4a", "NY" = "#377eb8", "NJ" = "#e41a1c"),
          size = 0.15,             
          alpha = 0.6,              
          border.lwd = 0) +         
  tm_facets(by = "year", 
            nrow = 1, 
            ncol = 1, 
            free.coords = FALSE) +
  tm_layout(main.title = "Accidentes por año",
            legend.outside = TRUE)

tmap_animation(accidents_anim, 
               filename = "accidents_animation.gif", 
               delay = 50, 
               width = 1200, 
               height = 800)


# animació per mes
df$year_month <- format(df$Start_Date, "%Y-%m")

df_sf <- st_as_sf(df, coords = c("Start_Lng", "Start_Lat"), crs = 4326) 


accidents_anim_mes <- tm_shape(df_sf, bbox = bbox_3states) + 
  tm_basemap("CartoDB.Positron") +
  tm_dots(col = "State",
          palette = c("MA" = "#4daf4a", "NY" = "#377eb8", "NJ" = "#e41a1c"),
          size = 0.15,
          alpha = 0.6,
          border.lwd = 0) + 
  tm_facets(by = "year_month",  
            nrow = 1,
            ncol = 1,
            free.coords = FALSE) +
  tm_layout(main.title = "Accidentes por mes",
            legend.outside = TRUE)

tmap_animation(accidents_anim_mes, 
               filename = "accidents_animation_mes.gif", 
               delay = 50, 
               width = 1200, 
               height = 800)

#animació per mes acumulat

df_sf <- df_sf %>% 
  arrange(Start_Date) %>% 
  mutate(year_month = format(Start_Date, "%Y-%m"))

meses_unicos <- sort(unique(df_sf$year_month))

mapas_acumulativos <- lapply(meses_unicos, function(mes_actual) {
  datos_acumulados <- df_sf %>% 
    filter(year_month <= mes_actual)
  
  tm_shape(datos_acumulados, bbox = bbox_3states) +
    tm_basemap("CartoDB.Positron") +
    tm_dots(
      col = "State",
      palette = c("MA" = "#4daf4a", "NY" = "#377eb8", "NJ" = "#e41a1c"),
      size = 0.15,
      alpha = 0.6,
      border.lwd = 0
    ) +
    tm_layout(
      main.title = paste("Accidentes hasta", mes_actual),
      legend.outside = TRUE
    )
})

tmap_animation(
  mapas_acumulativos,
  filename = "animacion_acumulativa.gif",
  delay = 100,  
  width = 1200,
  height = 800
)

                 #####################################
################# 5 -- GEOESTADÍSTICA PROCÉS DE PUNTS ###############
                 #####################################

#procés de punts amb variable severity
View(df)
str(df)

#variable de marca
df$Severity_Category <- as.factor(df$Severity_Category)
levels(df$Severity_Category)  

mi_ppp <- ppp(
  x = df$Start_Lng,         
  y = df$Start_Lat,         
  xrange = range(df$Start_Lng),
  yrange = range(df$Start_Lat),
  marks = df$Severity_Category       
)
#eliminar duplicats
mi_ppp <- unique(mi_ppp)

summary(mi_ppp)


# Gráfics #

plot(mi_ppp, main = "Patró dels punts amb Severity", cols = "red")

states <- map_data("state")
states_filtered <- states %>%
  filter(region %in% c("massachusetts", "new jersey", "new york"))
df_points <- data.frame(
  x = mi_ppp$x,
  y = mi_ppp$y,
  mark = marks(mi_ppp)  )

###variable severity amb només marca
ggplot() +
  geom_polygon(data = states_filtered, aes(x = long, y = lat, group = group),
               fill = "white", color = "blue") +
  geom_point(data = df_points, aes(x = x, y = y, shape = mark), size = 2, color = "black") +
  coord_fixed(1.3) +
  labs(title = "Patró dels punts amb Severity",
       shape = "Severity") +
  theme_minimal()


###variable severity amb marca i color 
ggplot() +
  geom_polygon(data = states_filtered, aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  geom_point(data = df_points, aes(x = x, y = y, shape = mark, color = mark), size = 2) +
  coord_fixed(1.3) +
  labs(title = "Patró dels punts amb Severity",
       shape = "Severity", color = "Severity") +
  theme_minimal()


###Densitat dels punts
plot(density(mi_ppp))
contour(density(mi_ppp))

densidad <- density(mi_ppp)
plot(densidad, main = "Densitat amb contorns")
contour(densidad, add = TRUE)
map("state", regions = c("massachusetts", "new jersey", "new york"),
    add = TRUE, col = "black", lwd = 2)

dens_df <- expand.grid(
  x = densidad$xcol,
  y = densidad$yrow
)
dens_df$z <- as.vector(t(densidad$v)) 
states <- map_data("state") %>%
  filter(region %in% c("massachusetts", "new jersey", "new york"))

ggplot() +
  geom_raster(data = dens_df, aes(x = x, y = y, fill = z), interpolate = TRUE) +
  scale_fill_viridis_c(option = "plasma", name = "Densitat") +
  geom_contour(data = dens_df, aes(x = x, y = y, z = z), color = "black", alpha = 0.4) +
  geom_polygon(data = states, aes(x = long, y = lat, group = group), 
               fill = NA, color = "black", linewidth = 0.6) +
  coord_fixed(1.3) +
  labs(title = "Densitat de punts amb contorns i mapa d'estats") +
  theme_minimal()

### Intensitat per quadrícules
quadrat_count <- quadratcount(mi_ppp, nx = 4, ny = 4)
plot(mi_ppp, main = "Conteig per quadrícula")
plot(quadrat_count, add = TRUE)

quadrat_df <- as.data.frame(as.table(quadrat_count))
nx <- 4
ny <- 4
quadrat_count <- quadratcount(mi_ppp, nx = nx, ny = ny)

win <- as.owin(mi_ppp)
x_breaks <- seq(win$xrange[1], win$xrange[2], length.out = nx + 1)
y_breaks <- seq(win$yrange[1], win$yrange[2], length.out = ny + 1)

quadrat_df <- as.data.frame(as.table(quadrat_count))
names(quadrat_df)

quadrat_df <- quadrat_df %>%
  mutate(
    col = as.numeric(x),
    row = as.numeric(y),
    xmin = x_breaks[col],
    xmax = x_breaks[col + 1],
    ymin = y_breaks[ny - row + 1],
    ymax = y_breaks[ny - row + 2],
    xmid = (xmin + xmax) / 2,
    ymid = (ymin + ymax) / 2
  )
points_df <- data.frame(
  x = mi_ppp$x,
  y = mi_ppp$y,
  marca = marks(mi_ppp)  
)
ggplot() +
  geom_point(data = points_df, aes(x = x, y = y), color = "red", size = 1.5, alpha = 0.6) +
  geom_rect(data = quadrat_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, color = "black", linewidth = 0.5) +
  geom_text(data = quadrat_df, aes(x = xmid, y = ymid, label = Freq),
            color = "black", size = 4, fontface = "bold") +
  coord_fixed() +
  labs(title = "Conteig per quadrícula", x = "X", y = "Y") +
  theme_minimal()



ggplot() +
  geom_rect(data = quadrat_df,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Freq),
            color = "black", alpha = 0.3) +
  scale_fill_viridis_c(name = "Densitat") +
  geom_point(data = points_df,
             aes(x = x, y = y, color = marca, shape = marca),
             size = 2, alpha = 0.8) +
  
  coord_fixed() +
  theme_minimal() +
  labs(title = "Conteig per quadrícula amb la variable severity",
       x = "X", y = "Y") +
  theme(legend.position = "right")

#Dividir el patró per categorías
split_severidad <- split(mi_ppp)
summary(split_severidad)

######### Anàlisis individual per categoria ###############################

### impacte fort/alt ###
severidad_alta <- split_severidad$"Impacte molt fort/alt"
plot(density(severidad_alta), 
     main = "Densitat: Impacte molt fort/alt")
contour(density(severidad_alta), col = "white", add = TRUE)
plot(severidad_alta, add = TRUE, col = "red", pch = 20)

#intensitat per quadrícula
Q_severidad <- quadratcount(severidad_alta, nx = 4, ny = 4)
plot(severidad_alta, main = "Conteig per quadrícula: Severitat Alta")
plot(Q_severidad, add = TRUE, cex = 1.2, col = "darkred")


### impacte considerable ###
severidad_considerable <- split_severidad$"Impacte considerable"
plot(density(severidad_considerable), 
     main = "Densitat: Impacte considerable")
contour(density(severidad_considerable), col = "white", add = TRUE)
plot(severidad_considerable, add = TRUE, col = "orange", pch = 20)

#intensitat per quadrícula
Q_severitat <- quadratcount(severidad_considerable, nx = 4, ny = 4)
plot(severidad_considerable, main = "Conteig per quadrícula: Severitat Considerable")
plot(Q_severitat, add = TRUE, cex = 1.2, col = "darkred")


### impacte poc ###
severidad_poca <- split_severidad$"Poc impacte"
plot(density(severidad_poca), 
     main = "Densitat: Impacte poc")
contour(density(severidad_poca), col = "white", add = TRUE)
plot(severidad_poca, add = TRUE, col = "yellow", pch = 20)

#intensitat per quadrícula
Q_severitat <- quadratcount(severidad_poca, nx = 4, ny = 4)
plot(severidad_poca, main = "Conteig per quadrícula: Severitat Poca")
plot(Q_severitat, add = TRUE, cex = 1.2, col = "darkred")


### impacte molt poc ###
severidad_molt_poca <- split_severidad$"Molt poc impacte"
plot(density(severidad_molt_poca), 
     main = "Densitat: Impacte molt poc")
contour(density(severidad_molt_poca), col = "white", add = TRUE)
plot(severidad_molt_poca, add = TRUE, col = "green", pch = 20)

#intensitat per quadrícula
Q_severitat <- quadratcount(severidad_molt_poca, nx = 4, ny = 4)
plot(severidad_molt_poca, main = "Conteig per quadrícula: Severitat Molt Poca")
plot(Q_severitat, add = TRUE, cex = 1.2, col = "darkred")


#Comparació entre categories
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
for (categoria in levels(marks(mi_ppp))) {
  plot(density(split_severidad[[categoria]]), 
       main = paste("Densitat:", categoria),
       col = viridis::viridis(50))
  contour(density(split_severidad[[categoria]]), 
          add = TRUE, 
          col = "white")
}

par(mfrow = c(1, 1))


#Anàlisis d'intensitat comparativa 
#Calcular intensitats
intensidades <- sapply(split_severidad, intensity)

#tabla comparativa
taula_intensitat <- data.frame(
  Categoría = names(intensidades),
  Intensidad = round(intensidades, 4),
  Unidad = "puntos/km²"
)
print(taula_intensitat)

### Prova per veure si hi ha homogenietat

observed <- sapply(split_severidad, function(x) quadratcount(x, nx = 4, ny = 4))
observed_filtered <- observed[rowSums(observed) > 0, ]

chisq_result <- chisq.test(observed_filtered, simulate.p.value = TRUE, B = 10000)
print(chisq_result)

combinaciones <- combn(levels(marks(mi_ppp)), 2)
par(mfrow = c(2, 2))

for (i in 1:ncol(combinaciones)) {
  k <- Kcross(mi_ppp, 
              i = combinaciones[1,i], 
              j = combinaciones[2,i],
              correction = "Ripley")
  plot(k, main = paste(combinaciones[,i], collapse = " vs. "),
       legend = FALSE)
}
par(mfrow = c(1, 1))

#Hipòtesi nul·la (H₀): Les categories de severitat (ex. "Impacti molt fort/*alt", "Impacti considerable", etc.) estan distribuïdes de manera independent de la ubicació espacial (quadrants).
#Hipòtesi alternativa (H₁): Existeix una associació significativa entre la severitat dels accidents i la seva ubicació espacial.
#El p-valor (0.007299 < 0.05) indica que hi ha evidència suficient per a rebutjar H₀. La distribució de les categories de severitat no és aleatòria en l'espai; estan associades a la ubicació geogràfica.
# X-squared alt indica Suggereix una forta associació entre la variable de severitat i la ubicació en quadrants. Els accidents de diferent gravetat tendeixen a concentrar-se en zones específiques.

#provar amb cuadricula mes fina
nx_fino <- 10  
ny_fino <- 10

observed_fino <- sapply(
  split_severidad, 
  function(x) quadratcount(x, nx = nx_fino, ny = ny_fino)
)

observed_fino_filtrado <- observed_fino[rowSums(observed_fino) > 0, ]

set.seed(123) 
chisq_result_fino <- chisq.test(
  observed_fino_filtrado, 
  simulate.p.value = TRUE, 
  B = 10000
)
print(chisq_result_fino)


Window(mi_ppp) <- owin(
  xrange = range(df$Start_Lng), 
  yrange = range(df$Start_Lat)
)

combinaciones <- combn(levels(marks(mi_ppp)), 2)

par(mfrow = c(2, 2))
for (i in 1:ncol(combinaciones)) {
  k <- Kcross(
    mi_ppp, 
    i = combinaciones[1, i], 
    j = combinaciones[2, i],
    correction = "Ripley"  
  )
  plot(k, 
       main = paste(combinaciones[, i], collapse = " vs.\n"),
       legend = FALSE, 
       cex.main = 0.9  
  )
}
par(mfrow = c(1, 1))

persp(density(mi_ppp), theta = 30, phi = 30, col = "lightblue")
hist(df$Start_Lng, main = "Distribución de Longitudes")

cross_cor <- alltypes(mi_ppp, Kcross)
plot(cross_cor)

                  ############################################### 
################# 6 -- GEOESTADÍSTICA PROCÉS DE PUNTS PER CIUTATS ###############
                  ###############################################

######
#Obtenir reguions del mapa amb més accident --> tractar ciutats més importants
######

#Filtrar en mapa unicament els 3 estats de la nostra base de dades
states <- map_data("state")

states_filtered <- states %>%
  filter(region %in% c("massachusetts", "new jersey", "new york"))

####################################################################################
#DESCRITIVA EN FUNCIÓ DE LES CIUTATS
####################################################################################
#1,TRIAR 1 CIUTAT DE CADA ESTAT --> OBJ: ciutat amb més accidents

#trobar ciutats més accidents
top_cities <- df %>%
  filter(State %in% c("MA", "NJ", "NY")) %>%
  group_by(State, City) %>%
  summarise(n_accidentes = n(), 
            Start_Lat = mean(Start_Lat), 
            Start_Lng = mean(Start_Lng)) %>%
  arrange(State, desc(n_accidentes)) %>%
  slice_head(n = 1) %>%
  ungroup()

##graficar:
ggplot() +
  geom_polygon(data = states_filtered, aes(x = long, y = lat, group = group),
               fill = "lightgrey", color = "black") +
  geom_point(data = df,
             aes(x = Start_Lng, y = Start_Lat, color = State),
             size = 0.1, alpha = 0.6) +
  geom_point(data = top_cities,
             aes(x = Start_Lng, y = Start_Lat, fill = State),
             shape = 21, color = "black", size = 4, stroke = 1.5) +
  geom_text(data = top_cities,
            aes(x = Start_Lng, y = Start_Lat, label = City, color = State),
            fontface = "bold", size = 5, nudge_y = 0.75,nudge_x =-1) + 
  scale_color_manual(
    values = c("MA" = "blue", "NJ" = "darkgreen", "NY" = "red"),
    guide = guide_legend(override.aes = list(size = 4))
  ) +
  scale_fill_manual(
    values = c("MA" = "blue", "NJ" = "darkgreen", "NY" = "red"),
    guide = "none"
  ) +
  coord_fixed(ratio = 1.1) +
  theme_minimal() +
  labs(title = "Accidents de tràfic per estats (ciutat amb més accidents marcada)") +
  theme(legend.title = element_blank())

#2, graficar cada ciutat (en un mapa interactiu)
#   - Un mapa per cada ciutat
#   - Accidents nucli ciutat: blau fort
#   - Accidents periferia de la ciutat (40km): blau fluix


# Rati (km) -> det zona periferia ciutat
radio_km <- 40 #des del centre de la ciutat

# guardar mapes
mapas <- list()
ppp_ciutats_list <- list()

for (i in 1:nrow(top_cities)) {
  city_name <- top_cities$City[i]
  state_code <- top_cities$State[i]
  city_data <- df %>% filter(City == city_name, State == state_code)
  center <- c(mean(city_data$Start_Lat), mean(city_data$Start_Lng))
  
  # Calc distancia dels accidents al centre 
  df$dist_to_city <- distHaversine(
    matrix(c(df$Start_Lng, df$Start_Lat), ncol = 2),
    c(center[2], center[1])
  )
  
  # Radi màx de la ciutat
  radio_ciudad <- max(distHaversine(
    
    matrix(c(city_data$Start_Lng, city_data$Start_Lat), ncol = 2),
    c(center[2], center[1])
  ))
  
  # class zona (ciutat: city o afores: voltants_city)
  df$zona <- ifelse(
    df$City == city_name & df$State == state_code, "City",
    ifelse(df$dist_to_city <= radio_km * 1000, "voltants_city", NA)
  )
  # Filtrar punts 
  df_zona <- df %>% filter(zona %in% c("City", "voltants_city"))
  
  #[CREAR POINT PATTER PER PART 3]
  mi_ppp_ciudad <- ppp(
    x = df_zona$Start_Lng,
    y = df_zona$Start_Lat,
    xrange = range(df_zona$Start_Lng),
    yrange = range(df_zona$Start_Lat),
    marks = as.factor(df_zona$Severity_Category)
  )
  mi_ppp_ciudad<-unique(mi_ppp_ciudad)
  
  ppp_ciutats_list[[paste(city_name, state_code, sep = "_")]] <- mi_ppp_ciudad
  
  
  # Combertir en sf
  df_zona_sf <- st_as_sf(df_zona, coords = c("Start_Lng", "Start_Lat"), crs = 4326)
  
  # Crear mapa (interactiu--> leaflet)
  mapa <- leaflet(df_zona_sf) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(
      radius = 3,
      fillOpacity = 0.7,
      stroke = FALSE,
      color = ~ifelse(zona == "City", "darkblue", "blue")
    ) %>%
    setView(lng = center[2], lat = center[1], zoom = 12)
  
  mapas[[paste(city_name, state_code, sep = "_")]] <- mapa
}

#Mostrar mapes:
#Boston (MA)
mapas[[1]]
#Newark (NJ)
mapas[[2]]
#Rochester(NY)
mapas[[3]]

##################################################################################
########                GEOESTADÍSTICA PROCÉS DE PUNTS             ###############
##################################################################################

#objectiu: aplicar un procés de punts amb la variable severity per cada ciutat
#Analitzat els llocs més perillosos

#A, crear un point pattern per cada ciutat i els seus voltants: 
# [s'ha inclos en el bucle per avançar feina]

####deliminar zones
list_states <- list()
###guardar en una llista per cada ciutat;
for (ciutat in names(ppp_ciutats_list)) {
  # nom ciutat i state
  parts <- strsplit(ciutat, "_")[[1]]
  city_name <- parts[1]
  state_code <- parts[2]
  
  # relacionar amb mapa: map_data
  state_map_names <- c(
    "MA" = "massachusetts",
    "NJ" = "new jersey",
    "NY" = "new york"
  )
  state_region <- state_map_names[state_code]
  
  # Filtrar el estat correspondent
  state_poly <- states %>% filter(region == state_region)
  
  # 'fer zoom' filtrar unicament ciutat i voltants
  mi_ppp_ciutat <- ppp_ciutats_list[[ciutat]]
  xlim <- range(mi_ppp_ciutat$x)
  ylim <- range(mi_ppp_ciutat$y)
  state_poly_zoom <- state_poly %>%
    filter(long >= xlim[1], long <= xlim[2],
           lat >= ylim[1], lat <= ylim[2])
  
  # guardar en llista
  list_states[[ciutat]] <- state_poly_zoom
}

#B, Mapa patró de punts (amb 3 categories [no hi ha de 'molt poc impacte'])
#####crear mapa: PATRÓ DELS PUNTS AMB SEVERITY
state_map_names <- c(
  "MA" = "massachusetts",
  "NJ" = "new jersey",
  "NY" = "new york"
)

for (ciutat in names(ppp_ciutats_list)) {
  # nom ciutat i estat
  parts <- strsplit(ciutat, "_")[[1]]
  state_code <- parts[2]
  state_region <- state_map_names[state_code]
  
  # filtrar unicament el poligon del estat analitzat
  state_poly <- states %>% filter(region == state_region)
  
  mi_ppp_ciutat <- ppp_ciutats_list[[ciutat]]
  df_points_ciutat <- data.frame(
    x = mi_ppp_ciutat$x,
    y = mi_ppp_ciutat$y,
    mark = marks(mi_ppp_ciutat)
  )
  
  xlim <- range(df_points_ciutat$x)
  ylim <- range(df_points_ciutat$y)
  
  p <- ggplot() +
    geom_polygon(data = state_poly, aes(x = long, y = lat, group = group),
                 fill = "white", color = "grey60", linewidth = 0.7) +
    geom_point(data = df_points_ciutat, aes(x = x, y = y, shape = mark, color = mark), size = 2) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    labs(title = paste("Patró dels punts amb Severity -", ciutat),
         shape = "Severity", color = "Severity") +
    theme_minimal()
  
  print(p)
}#nota: si executes després de mapes interactius has de canviar de viewer a plots

#Apunt: el mapa de newark, els accidents que estan per fora del diliniat fornem part del estat de NY

#C, Gràfic densitats

density_ciutats <- list()

for (ciutat in names(ppp_ciutats_list)) {
  mi_ppp_ciutat <- ppp_ciutats_list[[ciutat]]
  densidad <- density(mi_ppp_ciutat)
  
  plot(
    densidad,
    main = paste("Densitat amb contorns -", ciutat),
    col = viridis::viridis(50)
  )
  contour(densidad, add = TRUE, col = "white")
}


#D, Gràfics per quadríla 
nx <- 4  # num col
ny <- 4  # num files

for (ciutat in names(ppp_ciutats_list)) {
  mi_ppp_ciutat <- ppp_ciutats_list[[ciutat]]
  quadrat_count <- quadratcount(mi_ppp_ciutat, nx = nx, ny = ny)
  mi_ppp_ciutat$marks <- NULL #no aplicar categories
  
  
  plot(mi_ppp_ciutat, main = paste("Conteig per quadrícula:", ciutat), cols = "lightblue", pch = 20)
  plot(quadrat_count, add = TRUE, col = "black", cex = 1.2)
}

#E, Anàlisis individual per categoria

for (ciutat in names(ppp_ciutats_list)) {
  cat("\n\n### Comparació entre categories per ciutat:", ciutat, "###\n")
  mi_ppp_ciutat <- ppp_ciutats_list[[ciutat]]
  split_severidad_ciutat <- split(mi_ppp_ciutat)
  categories <- levels(marks(mi_ppp_ciutat))
  
  par(mfrow = c(1, 3), mar = c(2, 1, 2, 2))
  for (categoria in categories) {
    plot(
      density(split_severidad_ciutat[[categoria]]),
      main = paste("Densitat:", categoria, "\n(", ciutat, ")"),
      col = viridis(50)
    )
    contour(
      density(split_severidad_ciutat[[categoria]]),
      add = TRUE,
      col = "white"
    )
  }
  par(mfrow = c(1, 1))
}


#F, Anàlisis homogeneïtat
for (ciutat in names(ppp_ciutats_list)) {
  cat("\n\n### Prova d'homogeneïtat per ciutat:", ciutat, "###\n")
  mi_ppp_ciutat <- ppp_ciutats_list[[ciutat]]
  split_severidad_ciutat <- split(mi_ppp_ciutat)
  
  # Test de Chi-quadrat --> mirar si dades segueixen uns distribució al azar en el espai
  observed <- sapply(split_severidad_ciutat, function(x) quadratcount(x, nx = 4, ny = 4))
  observed_filtered <- observed[rowSums(observed) > 0, , drop = FALSE]
  
  if (nrow(observed_filtered) > 1 && ncol(observed_filtered) > 1) {
    chisq_result <- chisq.test(observed_filtered, simulate.p.value = TRUE, B = 10000)
    print(chisq_result)
  } else {
    cat("No hi ha prou dades per fer el test de chi-quadrat a", ciutat, "\n")
  }
  
  # Comparació Kcross entre categories
  categories <- levels(marks(mi_ppp_ciutat))
  combinaciones <- combn(categories, 2)
  par(mfrow = c(2, 2))
  for (i in 1:ncol(combinaciones)) {
    k <- Kcross(mi_ppp_ciutat, 
                i = combinaciones[1, i], 
                j = combinaciones[2, i],
                correction = "Ripley")
    plot(
      k, 
      main = paste(ciutat, ":", combinaciones[1, i], "vs.", combinaciones[2, i]),
      legend = FALSE
    )
  }
  par(mfrow = c(1, 1))
}


######################################################################################
#Anàlisis temporal --> observar evolució geografica dels accidents en funció del temps
######################################################################################
#EVOLUCIÓ PER TRIMESTRES: 2ANYS*4= 8 TRIMESTRES

#1, PATRONS PUNTS
#A, preparar dades (crear col trimestre)

df$Start_Date <- as.POSIXct(df$Start_Date, format = "%Y-%m-%d") # asegurar format temps correcte
df$trimestre <- paste(year(df$Start_Date), "Q", quarter(df$Start_Date), sep = "") # crear col trimestre

ciutats <- c("Boston", "Newark", "Rochester")
estats <- c("MA", "NJ", "NY")

#B, Grafic interactiu --> accidents per trimetres 
for (i in seq_along(ciutats)) {
  ciutat <- ciutats[i]
  estat <- estats[i]
  
  df_ciutat <- df %>% filter(City == ciutat, State == estat)
  
  p <- ggplot(df_ciutat, aes(x = Start_Lng, y = Start_Lat, color = Severity_Category)) +
    geom_point(alpha = 0.7, size = 3) +
    labs(
      title = paste0(ciutat, " - Accidents per trimestre: {closest_state}"),
      x = "Longitud", y = "Latitud"
    ) +
    theme_minimal() +
    transition_states(trimestre, transition_length = 2, state_length = 1)
  
  nom_fitxer <- paste0("animacio_geo_temporal_", ciutat, ".gif")
  anim_save(nom_fitxer, animation = p, nframes = 40, fps = 5, width = 800, height = 600)
} 

#C, Gràfics estatics

graf_temp_geo_ciutat <- list()

for (i in seq_along(ciutats)) {
  ciutat <- ciutats[i]
  estat <- estats[i]
  
  df_ciutat <- df %>% filter(City == ciutat, State == estat)
  
  p <- ggplot(df_ciutat, aes(x = Start_Lng, y = Start_Lat, color = Severity_Category)) +
    geom_point(alpha = 0.7, size = 2) +
    facet_wrap(~ trimestre, ncol = 4) +
    labs(
      title = paste0("Accidents per trimestre - ", ciutat),
      x = "Longitud", y = "Latitud", color = "Severitat"
    ) +
    theme_minimal()
  
  graf_temp_geo_ciutat[[ciutat]] <- p
}

# Mostrar (un al costat del altre)
for (ciutat in names(graf_temp_geo_ciutat)) {
  grid.arrange(
    grobs = list(graf_temp_geo_ciutat[[ciutat]]),
    ncol = 1,
    top = paste("Evolució d'accidents per trimestre -", ciutat)
  )
}

#2, DENSITATS

#Gràfics ESTATICS:EN FUNCIÓ DEL TEMPS
density_temporal_ciutats <- list()

for (ciutat in names(ppp_ciutats_list)) {
  mi_ppp_ciutat <- ppp_ciutats_list[[ciutat]]
  
  # assignar marques temporals a patro punts 
  df_ciutat <- df %>% filter(City == strsplit(ciutat, "_")[[1]][1])
  df_ciutat$trimestre <- paste(year(df_ciutat$Start_Date), "Q", quarter(df_ciutat$Start_Date), sep = "")
  mi_ppp_ciutat$marks <- as.factor(df_ciutat$trimestre)
  
  # Calcular densitat per cada trimestre
  trimestres <- levels(mi_ppp_ciutat$marks)
  
  for (trimestre in trimestres) {
    subset_ppp <- mi_ppp_ciutat[mi_ppp_ciutat$marks == trimestre]
    densidad <- density(subset_ppp)
    
    # save densitat (llista)
    density_temporal_ciutats[[paste(ciutat, trimestre, sep = "_")]] <- densidad
    
    # Graficar desitat
    plot(
      densidad,
      main = paste("Densitat amb contorns -", ciutat, "-", trimestre),
      col = viridis::viridis(50)
    )
    contour(densidad, add = TRUE, col = "white")
  }
}

#Gràfics un al costat del altre en informe i presentació 

### Boston: no informació suficient en primer trimestre i molt poca en el segon del 2017


#3, Validació: Anàlisis homogeneïtat i KCross

#D.1. Anàlisis homogeneïtat per ciutat (en funció temps)
for (ciutat in ciutats) {
  cat("\n\n### Anàlisis homogeneïtat temporal per la ciutat:", ciutat, "###\n")
  
  df_ciutat <- df %>% filter(City == ciutat)
  
  # accidents per trimestre --> veure gràfics tendencia
  accidentes_por_trimestre <- df_ciutat %>%
    group_by(trimestre) %>%
    summarise(n_accidentes = n()) %>%
    ungroup()
  
  # Prova Chi-quadrat
  if (nrow(accidentes_por_trimestre) > 1) {
    chisq_result <- chisq.test(accidentes_por_trimestre$n_accidentes)
    print(chisq_result)
  } else {
    cat("No hi ha suficients dades per realitzar la prova del chi-quadrat en", ciutat, "\n")
  }
  
  # Visualitzar tendencia temporal (num accidents per trimestre)
  p <- ggplot(accidentes_por_trimestre, aes(x = trimestre, y = n_accidentes, group = 1)) +
    geom_line(color = "blue", linewidth = 1) +  
    geom_point(color = "red", size = 2) +
    labs(
      title = paste("Evolució temporal dels accidents -", ciutat),
      x = "Trimestre",
      y = "Nombre d'accidents"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
}

#D.2.Analitzar Kcross

for (ciutat in names(ppp_ciutats_list)) {
  cat("\n\n### Análisis de Kcross temporal para la ciudad:", ciutat, "###\n")
  
  # Obtener el patrón de puntos de la ciudad
  mi_ppp_ciutat <- ppp_ciutats_list[[ciutat]]
  
  # Filtrar datos de la ciudad y asignar marcas temporales (trimestres)
  df_ciutat <- df %>% filter(City == strsplit(ciutat, "_")[[1]][1])
  df_ciutat$trimestre <- paste(year(df_ciutat$Start_Date), "Q", quarter(df_ciutat$Start_Date), sep = "")
  
  # Agregar marcas temporales al patrón de puntos
  mi_ppp_ciutat$marks <- as.factor(df_ciutat$trimestre)
  
  # Obtener combinaciones de trimestres
  trimestres <- levels(mi_ppp_ciutat$marks)
  combinaciones <- combn(trimestres, 2)
  
  # Calcular y graficar Kcross para cada combinación de trimestres
  par(mfrow = c(2, 2))
  for (i in 1:ncol(combinaciones)) {
    k <- Kcross(mi_ppp_ciutat, 
                i = combinaciones[1, i], 
                j = combinaciones[2, i],
                correction = "Ripley")
    plot(
      k, 
      main = paste(ciutat, ":", combinaciones[1, i], "vs.", combinaciones[2, i]),
      legend = FALSE
    )
  }
  par(mfrow = c(1, 1))
}


                 #################################             
######################  7 -- TEXT ANALISYS   ##################################
                 #################################
df <- base_de_dades
# Eliminem les files amb valors buits a la columna Description
df <- subset(df, Description != "" & !is.na(Description))

# Creem el corpus com a VCorpus per evitar problemes
micorpus <- VCorpus(VectorSource(df$Description))

# Preprocessem els textos
micorpus <- tm_map(micorpus, removeNumbers)                           # Eliminem números
micorpus <- tm_map(micorpus, removePunctuation)                      # Eliminem puntuació
micorpus <- tm_map(micorpus, content_transformer(tolower))           # Convertim a minúscules
micorpus <- tm_map(micorpus, removeWords, stopwords("english"))      # Eliminem paraules buides
micorpus <- tm_map(micorpus, stemDocument, language = "english")     # Apliquem stemming
micorpus <- tm_map(micorpus, stripWhitespace)                        # Netegem espais en blanc

# Eliminem documents buits després del preprocés
micorpus <- micorpus[!sapply(micorpus, function(x) nchar(x$content) == 0)]

# Creem la matriu document-terme
dtm <- DocumentTermMatrix(micorpus)

# Ens quedem només amb paraules que apareixen almenys 10 vegades
freq <- col_sums(dtm)                    # Comptem freqüència total de cada terme
dtm <- dtm[, freq >= 10]                 # Seleccionem només els que apareixen ≥10 vegades

# Convertim a matriu
dtm_matrix <- as.matrix(dtm)
dtm_matrix

# ANÀLISI DE CORRESPONDÈNCIES (CA)
res.ca <- CA(dtm_matrix, graph = FALSE)
res.ca

# AUTOVALORS
res.ca$eig

# INFORMACIÓ DELS DOCUMENTS (FILES)
names(res.ca$row)
res.ca$row$coord        # Coordenades
res.ca$row$cos2         # Qualitat de la representació
res.ca$row$contr        # Contribució als eixos
res.ca$row$inertia      # Inèrcia (variabilitat explicada)

# INFORMACIÓ DELS TERMES (COLUMNES)
names(res.ca$col)
res.ca$col$coord
res.ca$col$cos2
res.ca$col$contr
res.ca$col$inertia

# RESUM GENERAL
summary(res.ca)

# CONTRIBUCIÓ MÉS ALTA DELS TERMES ALS EIXOS PRINCIPALS
res.ca$col$contr[order(apply(res.ca$col$contr[,1:2],1,sum), decreasing=TRUE)[1:10], 1:2]
res.ca$col$contr[order(apply(res.ca$col$contr[,3:4],1,sum), decreasing=TRUE)[1:10], 3:4]

# GRÀFICS DE L'ANÀLISI DE CORRESPONDÈNCIES
plot.CA(res.ca, invisible="row")                         # Mostra només les paraules
plot.CA(res.ca, invisible="row", autoLab="yes")          # Amb etiquetes

plot.CA(res.ca, invisible="col")                         # Mostra només els documents
plot.CA(res.ca, invisible="col", autoLab="yes")          # Amb etiquetes

plot.CA(res.ca, invisible="row", axes=c(3,4), autoLab="yes")
plot.CA(res.ca, invisible="col", axes=c(3,4))

plot.CA(res.ca)                                          # Tot el conjunt



#############################
#LATENT DIRICHLET ALLOCATION#
#############################
df <- base_de_dades
# Eliminem les files amb valors buits a la columna Description
#df <- subset(df, Description != "" & !is.na(Description))
#dd <- df$Description










# Preprocessament simple del text: eliminem stopwords i signes de puntuació.
data("stop_words") 

dd <- df %>%
  unnest_tokens(word, Description) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(across(-word)) %>%
  summarize(Description = paste(word, collapse = " ")) %>%
  ungroup()


dd <- dd$Description

dd <- gsub("[[:punct:]]", "", dd)
dd <- gsub("[^[:alnum:][:space:]']", "", dd)
dd <- gsub("[^[:alnum:][:space:]'-]", "", dd)

#Iniciem el DTM
minimumFrequency <- 5
DTM <- DocumentTermMatrix(dd, control = list(bounds = list(global = c(minimumFrequency, Inf))))
dim(DTM)
sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]

# És difícil intuïr una K en aquest cas, però es podien veure més o menys tres clusters en el CA de text
K <- 3 # Així que he utilitzat K=3

# Posem un seed per treure aleatorietat
set.seed(9161)

# Generem el model LDA
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25,alpha=0.2))
topicModel
attr(topicModel,"alpha") 
#Intentant amb tot tipus de combinacions de K i alpha, no hem obtingut cap conjunt de tòpics clarament diferenciables.


tmResult <- posterior(topicModel)


attributes(tmResult)


beta <- tmResult$terms  
dim(beta)                


theta <- tmResult$topics 
dim(theta)
#Mirem els 10 resultats més freqüents
terms(topicModel, 10)

topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
summary(theta)

# Fem núvol de paraules de cada tòpic
#Tòpic 1
topicToViz<-1 
top50terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:50]
words <- names(top50terms)
probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:50]
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

#Tòpic 2
topicToViz<-2 
top50terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:50]
words <- names(top50terms)
probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:50]
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

#Tòpic 3
topicToViz<-3 
top50terms <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:50]
words <- names(top50terms)
probabilities <- sort(tmResult$terms[topicToViz, ], decreasing = TRUE)[1:50]
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

#Observem les freqüències de les top 10 paraules de cada tòpic
topicToFilter <- 1
topicThreshold <- 0.6
selectedDocumentIndexes <- which(theta[, topicToFilter] >= topicThreshold)
selectedDocumentIndexes
(ap_topics <- tidy(topicModel, matrix = "beta"))
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

##Observem la freqüència de paraules de certs exemples
exampleIds <- c(5, 100, 869)
N <- length(exampleIds)
topicProportionExamples <- theta[exampleIds, ]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- reshape2::melt(
  cbind(data.frame(topicProportionExamples),
        document = factor(1:N)
  ),
  variable.name = "topic",
  id.vars = "document")
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip() +
  facet_wrap(~document, ncol = N)



# Funció de càlcul de K òptima, però fins i tot utilitzant la K recomenada,
# No obtenim tòpics clarament diferenciables
result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 20, by = 4),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
result

