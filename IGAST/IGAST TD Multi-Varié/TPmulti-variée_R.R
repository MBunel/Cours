
library(FactoMineR)
library(Rcmdr)
library(ClustGeo)

library(ggplot2)
library(extrafont)
library(rgdal)
library(maptools)
library(broom)
require(gridExtra)

library(reticulate)


#suppréssion des variables globales
#toutes les variables de l'environnement global
rm(list=ls())
#suppréssion d'une variable var
#rm(var)
#rm doit être suivi de gc () pour vider la mémoire vive de R
gc()

#on choisit le dossier dans lequel on travaille; pour lire un fichier ou bien pour Ã©crire
setwd("D:/CR1/Cours/M2IGAST/ModuleStat/TP/TP3/")

#lecture du fichier csv
indic<-read.csv2("indicateurs_pourR.csv");

#sélection des variables quantitatives
matrice_indic<-indic[,3:15];

############################################################
###### 1.ACP + CAH 
############################################################

###### 1.2. Calcul de l'ACP 
#par défaut la fonction effectue la transformation centré-réduit; 
acp<-PCA(matrice_indic) 

#représentation des variables selon les nouvelles dimensions
plot.PCA(acp,choix="var")

#représentation des individus selon les nouvelles dimensions
plot.PCA(acp,choix="ind")

#afficher les valeurs propre; arrondir à deux décimales.  
round(acp$eig,2); 

# les 4 premiers axes contiennent plus de 70% de l'inertie 
#afficher les valeurs propres à l'aide du diagramme 
barplot(acp$eig[,1], main="Valeurs propres", names.arg = paste("dim",1:nrow(acp$eig)))

## enregistrement de tous les résultats ACP (construction des axes pour variables et individus)
print(acp, file="resultats_ACP_tout.csv", sep=";")

## transformation des coordonnéess de l'ACP en un data frame (pour CAH)
# $ permet de recupérer le nom de la variable, de visualiser les valeurs
coord <- data.frame(acp$ind$coord)


######1.2. CAH 
#### Enregistrement des résultats des ACP uniquement pour les axes choisis

## mise en mémoire directe des résultats des ACP (avec  le nombre d'axes à conserver)
res.acp <- PCA(matrice_indic, ncp=4)

##calcul de CAH sans contrainte spatiale
cah1 <- HCPC(res.acp)

##on met les résultats dans un data frame pour pouvoir les exporter plus facilement
resultCAH1<-data.frame(cah1$data.clust)
#export des résultats
exportCAH1<-cbind(indic$code_insee,resultCAH1$clust)
#### Enregistrement des classes par individus 
write.table(exportCAH1, file="result_cah.csv", col.names=TRUE, row.name=TRUE, sep=";")

######1.3. Cartographie des résultats 

# import du fond de carte
GEOFLA <- readOGR("./COMMUNE/COMMUNE.shp")

# on change le format des données pour faciliter la jointure avec le fond de carte
dataCarto <- as.data.frame(exportCAH1)
# on mondifie le nom des colonnes (pour éviter les "V1", "V2")
names(dataCarto) <- c("INSEE_COM", "CLASSEcah")

# On fait une jointure, la claus all.x = FALSE permet de 
# réaliser une jointure droite (on ne conserve pas les communes
# qui ne sont pas présentes dans dataCarto)
GEOFLAsub <- merge(GEOFLA, dataCarto, 
                   by = "INSEE_COM",
                   all.x = FALSE)

# On crée dans la table attributaire des communes une nouvelle colonne
# correspondant au numéro interne de la géométrie (pour faire un 
# jointure untérieurement)
GEOFLAsub@data$id <- getSpPPolygonsIDSlots(GEOFLAsub)
# On décompose les communes en un ensemble de polygones dessinables par ggplot
GEOFLApoly <- tidy(GEOFLAsub)
# On joint ces nouveaux polygones avec la table attributaire 
# (à l'aide de la colonne id) que l'on vient de créer
GEOFLAggplot <- merge(GEOFLApoly, GEOFLAsub@data, by="id")
# NB: il existe une façon plus simple de procéder mais elle nécessite
# une dépendance qu'il n'était pas possible d'installer sur vos machines

# On peut enfin cartographier les résultats
# d'abord dans leur plus simple apparat
ggplot(GEOFLAggplot) +
  aes(x=long, y=lat, group=id, fill=CLASSEcah) +
  geom_polygon()


# on va ensuite corriger les différents problèmes de cette 
# première représentation

# on commence par importer les polices disponibles sur
# votre machine à l'aide du package extrafonts
# Cela peut-être long (plusieurs minutes)
font_import(prompt=FALSE)
# Puis on les charge dans R
loadfonts(device = "win")

carteCAH <- ggplot(GEOFLAggplot) +
  # La colonne CLASSE est de type int, la bibliothèque
  # la considère donc comme une variable continue et la
  # cartographie en conséquence. Notez l'ajout du factor
  # pour caster la variable en amont
  aes(x=long, y=lat, group=id, fill=factor(CLASSEcah)) +
  geom_polygon(color="white") +
  # on ajoute l'appel à cette fonction pour forcer la 
  # représentation avec un ratio 1:1
  coord_equal()

# affichage de la carte
# print(carteCAH)

## pour finir on va embelir cette carte
# modification de la légende et de la gamme
carteCAH <- carteCAH + scale_fill_manual(
  # Titre de la légende
  name = "Classe",
  # Couleurs des classes
  values = c("#ffc107", "#009688", "#2196f3", "#8bc34a", "#607d8b"),
  # Changement style légende
  guide = guide_legend(
    # Légende horizontale et non plus verticale
    direction = "horizontal",
    # hauteur des caissons
    keyheight = unit(2, units = "mm"),
    # largeur des caissons
    keywidth = unit(10, units = "mm"),
    # Le titre de la légende est au-dessus des
    # caissons
    title.position = 'top',
    # et les labels en dessous
    label.position = "bottom",
    # Titre centré par rapport à la légende
    title.hjust = 0.5,
    # Numéros centrés par rapport aux caissons
    label.hjust = 0.5,
    # la légende est sur une ligne
    nrow = 1
  )
) + theme(
      # légende sous le graphiques
      legend.position = "bottom"
      )

# Modification du thème, i.e. de tout ce
# qui touche à l'esthétique
carteCAH <- carteCAH + theme(
  # pas d'axes X et Y
  axis.line = element_blank(),
  # Valeurs des axes
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  # Nom des axes
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  # Grille
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Fond grisé
  plot.background = element_rect(fill="#f5f5f2", color=NA),
  panel.background = element_rect(fill="#f5f5f2", color=NA),
  legend.background = element_rect(fill="#f5f5f2", color=NA),
  # pas de cadre
  panel.border = element_blank(),
  # Marges du graphique
  plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm")
) 

## Pour finir on va s'occuper des labels
# On peut désormais effectuer les dernières modifications
carteCAH <- carteCAH + labs(
  # Ajout d'un titre
  title = "Typologie des communes Iséroises",
  # Ajout d'un titre de légende
  caption = "Sources: Ign 2016, ANR GeoPeuple 2017"
) + theme(
    # Modification du style du tire
    plot.title = element_text(
      # police
      family = "Arial",
      # taille
      size=15,
      # type
      face = "bold"
      ),
    # Modification du style du tire de la légende
    plot.caption = element_text(
      family = "Arial"
      ),
    legend.title = element_text(
      family = "Arial", 
    ),
    # Modification du style des noms de classe
    legend.text = element_text(
      family = "Arial"
      )
  )

# affichage de la carte
print(carteCAH)

############################################################
###### 2.ACP + CAH + K-Means
############################################################

# "kmeansForR" est le nom du fichier python (sans l'extension)
# getwd() renvoie le chemin du répertoire de travail
m_kmeans <- import_from_path("kmeansForR", getwd())

# appel d'une fonction python 
# m_kmeans$maFonction()

# on importe le contenu du fichier f_kmeans.r
source("f_kmeans.r")

# utilisation de la fonction:

# km <- f_kmeans(data, classes)

# utilisation de la fonction, découpage en 5 classes à
# partir des résultats de l'ACP
km <- f_kmeans(res.acp$ind$coord, 5)
# jointure avec les codes insee
exportKM1 <- cbind(indic$code_insee, km$AFFECTATION)

# Ajout des résultats à au fond de carte
# le code est identique à celui de la cah
exportKM1 <- as.data.frame(exportKM1)
names(exportKM1) <- c("INSEE_COM", "CLASSEkm")
GEOFLAsub <- merge(GEOFLAsub, exportKM1, 
                   by = "INSEE_COM",
                   all.x = FALSE)
GEOFLAsub@data$id <- getSpPPolygonsIDSlots(GEOFLAsub)
GEOFLApoly <- tidy(GEOFLAsub)
GEOFLAggplot <- merge(GEOFLApoly, GEOFLAsub@data, by="id")

# Cartographie des kmeans
# le code est identique à celui de la cah
carteKM <- ggplot(GEOFLAggplot) +
  aes(x=long, y=lat, group=id, fill=factor(CLASSEkm)) +
  geom_polygon(color="white") +
  coord_equal() + 
  scale_fill_manual(
    name = "Classe",
    values = c("#ffc107", "#009688", "#2196f3", "#8bc34a", "#607d8b"),
    guide = guide_legend(
      direction = "horizontal",
      keyheight = unit(2, units = "mm"),
      keywidth = unit(10, units = "mm"),
      title.position = 'top',
      label.position = "bottom",
      title.hjust = 0.5,
      label.hjust = 0.5,
      nrow = 1
    )
  ) + theme(
    legend.position = "bottom",
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill="#f5f5f2", color=NA),
    panel.background = element_rect(fill="#f5f5f2", color=NA),
    legend.background = element_rect(fill="#f5f5f2", color=NA),
    panel.border = element_blank(),
    plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm")
  ) + labs(
    title = "Typologie des communes Iséroises",
    caption = "Sources: Ign 2016, ANR GeoPeuple 2017"
  ) + theme(
    plot.title = element_text(
      family = "Arial",
      size=15,
      face = "bold"
    ),
    plot.caption = element_text(
      family = "Arial"
    ),
    legend.title = element_text(
      family = "Arial", 
    ),
    legend.text = element_text(
      family = "Arial"
    )
  )

############################################################
###### 3. Comparer les résultats 
############################################################

# Représentation des deux cartes côte à côte
grid.arrange(carteCAH, carteKM, ncol=1)

############################################################
###### 4.ACP + CAH avec contraintes spatiales
############################################################

#on définit la variable qui porte la géométrie
Vgeom=indic$geom;
#on applique la CAH avec un alpha=1; 
CAH_geo.alpha1 <- hclustgeo(data=res.acp, D.geo=Vgeom, alpha=1)
# à vous: tester plusieurs alpha; choisisez celui qui est optimal pour votre application

###plot of the dendrogram
plot(CAH_geo.alpha1, choice="dendro") #on retient le nb de classes souhaité

##export des résultats
#astuce : très utile pour récuperer et afficher une variable donnée
indic$nom_comm