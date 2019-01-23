`New_Data_Clean_65%` <- read.csv("~/Downloads/New_Data_Clean_65%.csv", sep=";")
attach(`New_Data_Clean_65%`)



#on enlève l'heure, la latitude, longitude

data = `New_Data_Clean_65%`
data_1 = data[ , -c(1:5)]

#on enlève les variables quantitatives pour les corrélations (ici Mainsaill Full)

data_2 = data_1[ , -c(116)]

#on enlève les lignes où Mainsaill Full = 0  (ici 300)

data_3 = data_2[-c(1:300), ]

#on enlève les variables étant des constantes dans nos données

data_4 = data_3[ , -c(67:72)]
data_4 = data_4[ , -c(89:94)]

#définition des jeux de données Tribord / Babord pour distinguer les amures de navigation

data_tribord = subset(data_4, data$Wind_Angle > 0)
data_babord = subset(data_4, data$Wind_Angle < 0)


#suppression de variables dans le jeu tribord pour avoir des matrices de corrélations simplifiées

#suppression des données du foil au vent

data_tribord = data_tribord[ , -c(65:70)]
data_tribord = data_tribord[ , -c(81:86)]

#supression de Poutres avant B et C et on ne garde que Max de A et D
#suppression des Foil_Port_IN autres que 4 et suppresion Foil_port_in_MIN

data_tribord[, c("Port_Foil_Max_Deformation_in_1", "Port_Foil_Max_Deformation_in_2", "Port_Foil_Max_Deformation_in_3", "Port_Foil_Max_Deformation_in_5",
                 "Port_Foil_Max_Deformation_in_6", "Port_Foil_Max_Deformation_in_7", "Port_Foil_Max_Deformation_in_8",
                 "Port_Foil_Min_Deformation_in_1", "Port_Foil_Min_Deformation_in_2", "Port_Foil_Min_Deformation_in_3", "Port_Foil_Min_Deformation_in_5",
                 "Port_Foil_Min_Deformation_in_6", "Port_Foil_Min_Deformation_in_7", "Port_Foil_Min_Deformation_in_8")] <- list(NULL)

#même chose mais pour Foil_Port_OUT

data_tribord[, c("Port_Foil_Max_Deformation_out_1", "Port_Foil_Max_Deformation_out_2", "Port_Foil_Max_Deformation_out_3", "Port_Foil_Max_Deformation_out_5",
                 "Port_Foil_Max_Deformation_out_6", "Port_Foil_Max_Deformation_out_7", "Port_Foil_Max_Deformation_out_8",
                 "Port_Foil_Min_Deformation_out_1", "Port_Foil_Min_Deformation_out_2", "Port_Foil_Min_Deformation_out_3", "Port_Foil_Min_Deformation_out_5",
                 "Port_Foil_Min_Deformation_out_6", "Port_Foil_Min_Deformation_out_7", "Port_Foil_Min_Deformation_out_8")] <- list(NULL)


#on ne garde que Port_Foil_Max_4

data_tribord[, c("Port_Foil_Min_Deformation_in_4", "Port_Foil_Min_Deformation_out_4")] <- list(NULL)

#suppression des Minimum de Center Hull

data_tribord[, c("Min_Center_Hull_11", "Min_Center_Hull_14","Min_Center_Hull_21", "Min_Center_Hull_24", "Min_Center_Hull_12", "Min_Center_Hull_22", "Min_Center_Hull_13", "Min_Center_Hull_23")] <- list(NULL)

#suppression de Front_Beam (B et C) (MAX)

data_tribord[, c("Max_Starboard_Front_Beam_B1", "Max_Starboard_Front_Beam_C1", "Max_Starboard_Front_Beam_B2", "Max_Starboard_Front_Beam_C2", "Max_Starboard_Front_Beam_B3", "Max_Starboard_Front_Beam_C3",
                 "Max_Starboard_Front_Beam_B4", "Max_Starboard_Front_Beam_C4")] <- list(NULL)


#suppression de Front_Beam (MIN)

data_tribord[, c("Min_Starboard_Front_Beam_A1", "Min_Starboard_Front_Beam_B1", "Min_Starboard_Front_Beam_C1","Min_Starboard_Front_Beam_D1",
                  "Min_Starboard_Front_Beam_A4", "Min_Starboard_Front_Beam_B4", "Min_Starboard_Front_Beam_C4", "Min_Starboard_Front_Beam_D4",
                  "Min_Starboard_Front_Beam_A2", "Min_Starboard_Front_Beam_B2", "Min_Starboard_Front_Beam_C2", "Min_Starboard_Front_Beam_D2",
                  "Min_Starboard_Front_Beam_A3", "Min_Starboard_Front_Beam_B3", "Min_Starboard_Front_Beam_C3", "Min_Starboard_Front_Beam_D3")] <- list(NULL)

#importation FactoMineR

library("FactoMineR")
library("factoextra")

#matrice de corrélation

M_cor = cor(data_tribord)

#visualisation avec des cercles de la matrice de correlation
library(corrplot)
corrplot(M_cor, method="circle")

#heatmat de la matrice de correlation
cormat <- round(cor(data_tribord),2)

#reordonner la matrice de correlation
reorder_cormat <- function(cormat){
  # Utiliser la corrélation entre les variables
  # comme mésure de distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}   

cormat <- reorder_cormat(cormat)

#obtenir que le triangle superieur
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

# Fondre la matrice de corrélation
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)


#suppression valeurs de corrélation inférieures à un certain seuil

melted_cormat_2 = melted_cormat[!(abs(melted_cormat$value) < 0.8), ]

melted_cormat_2 = melted_cormat_2[!(melted_cormat_2$value == 1.00), ]


#Visualiser une matrice de corrélation avec un corrélogramme

library(corrplot)
corrplot(M_cor, type="upper", order="hclust", tl.col="black", tl.srt=45)

##

library(ggplot2)
myplot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       
                       name="Pearson\nCorrelation") + 
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   
                                   size = 12, hjust = 1))+ 
  coord_fixed()

print(myplot)

####################### ACP #######################
etude.pca <- PCA(data_tribord, graph = FALSE, ncp = 3)
print(etude.pca)

##### Analyse des valeurs propres #####

install.packages("factoextra")
library("factoextra")
eig.val <- get_eigenvalue(etude.pca)
eig.val

#graphique du pourcentage de variance expliquee en fonction des dimensions
myplot <- fviz_eig(etude.pca, addlabels = TRUE, ylim = c(0, 50))

#export du plot
# Enregistré au format pdf
pdf("graph_pourcentage_variance_expliquee_dimension.pdf")
print (myplot)
dev.off ()

##### Analyse des variables #####
var <- get_pca_var(etude.pca)
var

###Cercle de correlation
myplot <- fviz_pca_var(etude.pca, col.var = "black", repel = TRUE)
#export du plot
# Enregistré au format pdf
pdf ("cercle_de_correlation.pdf")
print (myplot)
dev.off ()

###graphique des valeurs propres

fviz_eig(etude.pca, addlabels = TRUE, ylim = c(0, 50))

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(etude.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)


#graphique des variables

var <- get_pca_var(etude.pca)
var

# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

#Qualité de la représentation

fviz_cos2(etude.pca, choice = "var", axes = 1:3)
fviz_cos2(etude.pca, choice = "var", axes = 1)
fviz_cos2(etude.pca, choice = "var", axes = 2)
fviz_cos2(etude.pca, choice = "var", axes = 3)

#Contribution des variables aux axes principaux

library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

# Contributions des variables à PC1

fviz_contrib(etude.pca, choice = "var", axes = 1)

# Contributions des variables à PC2

fviz_contrib(etude.pca, choice = "var", axes = 2)

# Contributions des variables à PC3

fviz_contrib(etude.pca, choice = "var", axes = 3)

# Contribution totale à PC1,PC2,PC3

fviz_contrib(etude.pca, choice = "var", axes = 1:3)

fviz_pca_var(etude.pca, alpha.var = "contrib")

#DESCRIPTION DES DIMENSIONS

res.desc <- dimdesc(etude.pca, axes = c(1,3), proba = 0.05)
# Description de la dimension 1
descri_dim1 = res.desc$Dim.1

#Top 20 des individus/variables les plus contibutifs

fviz_pca_biplot (etude.pca, select.ind = list (contrib = 30),
                 select.var = list (contrib = 30),
                 ggtheme = theme_minimal())

# Top 5 variables actives avec le cos2 le plus elevé
fviz_pca_var (etude.pca, select.var = list(cos2 = 30))