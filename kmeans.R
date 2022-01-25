# importation des données
cars <- read.table("cars_dataset.txt", header=T, dec=".")
summary(cars)

# centrage et reduction
centrage_reduction <- function(x) {
  return((x-mean(x))/sqrt(var(x)))
}

cars.cr <- apply(cars[,1:5], 2, centrage_reduction)
head(cars.cr)

# la détection d'un nombre adéquat de groupes
nb.essais <- 5
inertie.expl <- rep(0, 10)
for(k in 2:10){
  cars_km <- kmeans(cars.cr, centers = k, nstart = nb.essais)
  inertie.intra[k] <- cars_km$tot.withinss
}

plot(2:10, inertie.expl[-1],type="b",xlab="nombre de groupes",ylab="inertie inta-classes")
abline(v = 4, col = "blue", lwd = 2, lty = 3) 

# appliquer le kmeans sur les variables centrées réduites avec 4 classes 
# et 5 nombre d'essais avec différents individus de départ
nb.classes <- 4
nb.essais <- 5
km <- kmeans(cars.cr, centers = nb.classes, nstart = nb.essais)
print(km)

# récupération des groupes d'apparetenance
groupe <- km$cluster
#calculer les barycentres des classes dans l'espace des variables actives initiales
centres <- NULL
for (k in 1:nb.classes){
  ligne <- colMeans(cars[groupe==k,1:5])
  centres <- rbind(centres,ligne)
}
numero <- seq(from=1,to=nb.classes)
rownames(centres) <- paste("classe_",numero,sep="")
print(centres)
# croiser les classes avec la variable qualitative en produisant un tableau de contingence
print(table(cars$origin, groupe))
# graphique des variables 2 à 2 avec groupe d'appaternance
pairs(cars[,1:5],pch=21,bg=c("red","blue","yellow","green")[groupe])

#ACP sur les données centrées réduites
pca <- princomp(cars.cr,scores=T)
print(pca)
#pour obtenir les valeurs propres et inertie expliquées par chaque composante principale
pca.var <- pca$sdev^2
print(pca.var)
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
print(pca.var.per)
barplot(pca.var.per, xlab = "composante principale", ylab = "pourcentage de l'inertie")
# graphique dans le premier plan factoriel, avec mise en évidence des groupes
plot(pca$scores[,1],pca$scores[,2],type="p",pch=20,col=c("red","blue","yellow","green")[groupe],
     xlab = paste("PC1 - ", pca.var.per[1], "%", sep = ""), ylab = paste("PC2 - ", pca.var.per[2], 
                                                                         "%", sep = ""))

#exportation des données avec le cluster d'appartenance
cars.output <- cbind(cars,groupe)
write.table(cars.output,file="cars_output.txt",sep="\t",dec=".",row.names=F)
head(cars.output)




