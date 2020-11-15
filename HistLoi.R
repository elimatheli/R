#Author Eliot Viseux

#Repartition observée et répartition théorique 

#1.
#dbinom sert à calculer la loi binomiale,c'est à dire le nombre
#de succès dans une taille n avec une probabilité P
dbinom(5, 10,0.5) #ici le resultat est 0.24, c'est  à dire que la chance d'obtenir 5 succès dans 10 experiences
#avec une probabilité de 0.5 est 0.24.

dbinom(5, 4,0.8) #ici le résultat est 0 car on ne peut pas obtenir 5 succès dans 4 expériences.

dbinom(5, 5,1) # ici 1 car la probabilité est de 1 donc nous avons toutes les chances d'obtenir 5 succès.

rbinom(10, 5, 0.8) # rbinom nous permet de tester, ici 10 fois, un certain nombre de fois 5 experiences et nous donne
#les nombres de succès de chaque fois ou les 5 expériences ont été faite

#2.
sample(0:1,10,replace=TRUE)
#sample nous permet de simuler aléatoirement entre 0:1 10 lancer de pièces

#3.
#dunif nous permet de calculer la loi uniforme, la densité doit toujours faire 1,
#on calcul la probabilité du nombre entré avec la formule 1/b-a.

dunif(5, 0,10)#ici le résultat est 0.1 en effet 1/10-0 = 0.1.

dunif(5, 0,4) #ici le résultat est 0 car la chance d'obtenir 5 dans l'intervalle [0,4] est nulle
dunif(5, 0,20)

runif(10, -2,3) # runif va prendre des nombre dans l'intervalle rentré ici [-2,3] et va faire
#10 réalisations.

#4.
xuni <- runif(100,-2,3)
hist(xuni,main="Histogramme Loi Uniforme",xlab="x",xlim = c(-2,3),ylim=c(0,19),col="green")
xuni2 <- runif(900,-2,3)
hist(xuni2,main="Histogramme Loi Uniforme",xlab="x",xlim = c(-2,3),ylim=c(0,100),col="yellow")
#nous pouvons voir que lorsqu'on augmente N, l'histogramme devient plus uniforme.

#5.

de <- round(runif(10,1,6)) # 10 jetés de dés dans l'intervalle [1,6] 
hist(de,main="Histogramme lancé de dés",xlab="valeurs du dé",xlim = c(1,6),ylim=c(0,8),col="blue")

de2 <- round(runif(100,1,6)) # 10 jetés de dés dans l'intervalle [1,6] 
hist(de2,main="Histogramme lancé de dés",xlab="valeurs du dé",xlim = c(1,6),ylim=c(0,60),col="red")
#lorsqu'on augmente N toutes les valeurs du dé deviennent uniformes.
#6.

dens <- function(a,b) 1/(b-a)
curve(dens,col="black",add=TRUE)

#7.

#loi Binomiale pour les 10 lancés de dés
debinom <- rbinom(10, 1,1/6)
hist(debinom,main="Histogramme lancé de dés",xlab="nombre de succès",xlim =c(1,2),ylim=c(0,8),col="red")
#on augmente n.
debinom2 <- rbinom(100, 1,1/6)
hist(debinom2,main="Histogramme lancé de dés",xlab="nombre de succès",xlim =c(1,2),ylim=c(0,25),col="red")

#loi exponentielle
alpha <- 0.5
densexpo <- function(x) alpha*exp(-alpha*x)
curve(densexpo,col="black",add=FALSE)

#loi normale
densnorm <- function(x) (1/(alphan*sqrt(2*pi)))*exp(-((x-u)^2)/(2*alphan^2))
alphan <- 2
u <- -1
curve(densnorm,col="black",add=FALSE)

#Loi forte des grands nombres

#1. loi uniforme

N = 100 #nombre de réalisations
a_born = 0 #borne inférieure de la loi uniforme.
b_born = 1 #borne supérieure de la loi uniforme.
tabunif = data.frame(runif(N, a,b))
Sn = colSums(tabunif) #somme des Xn
Espe = (a_born + b_born)/2
resultunif = Sn/N # On voit que le resultat tend vers 0.5 qui est l'esperance.

#loi exponentielle

lambda = 2
Nexp = 100
tabexpo = data.frame(rexp(Nexp,rate=lambda))
Snexp = colSums(tabexpo) #somme des Xn
Espeexp = 1/lambda #l'esperance de la loi exponentielle.
resultexpo = Snexp/Nexp #on voit un resultat très proche de 0.5 (quand lambda=2 car E(X) = 1/lambda = 1/2)


#2 LOI FORTE DES GRANDS NOMBRES

Mvect = runif(200,0,1)
EspXunif = (1+0)/2
Msum = cumsum(Mvect) #2. On fait la somme cumulative du vecteur
plot((Msum/c(1:200)),xlim=c(0,200),ylim=c(0,1)) #3. On affiche Le Graphe
abline(h=EspXunif,col='red') #4. On Ajoute L'esperence sur le Graphe

#5 On peut voir Que Quand N est petit, il y a plus d'oscillation car la loi uniforme devient uniforme avec un grand nombre d'expériences réalisées
#donc plus N est élévée, plus cela nous donne un graphe uniformisé.

#6 Le Graphe change car R va chercher des valeurs aléatoires pour créer le vecteur M avec la commande runif, ce ne sont donc jamais les mêmes valeurs.


#Illustrations pour d'autres Lois

#7.
Nvectunif <- function(a,b,N) plot((cumsum(runif(N,a,b))/c(1:N)),xlim=c(0,N),ylim=c(a,b))
#Fonction qui sert à faire varier a,b et N avec la loi uniforme.

#8.
Nvectexpo <- function(N,alpha) plot((cumsum(rexp(N,rate=alpha))/c(1:N)),xlim=c(0,N),col='chocolate')
#Fonction qui sert à faire varier alpha et N avec la loi exponentielle.

#9.
Nvectcauch <- function(N) plot((cumsum(rcauchy(N))/c(1:N)),xlim=c(0,N),col='orange')
#Fonction qui sert à faire varier N avec la loi de Cauchy.

#Nous pouvons voir que Quand N augmente, les valeurs ne convergent pas vers une espérance, elles restent plus ou moins aléatoires.

#3. Théorème de la limite centrale.

N_TLC = 100
M_TLC = 10000
mat_TLC = matrix(data = runif(N_TLC*M_TLC,0,1),nrow=N_TLC,ncol=M_TLC)
#on créé la matrice Avec N lignes Et M Colonnes avec les données de runif.

#2.
vect_moyenne <- colSums(mat_TLC)/N_TLC
#on additionne les Colonnes comme indiqué dans l'énoncé et on fait la moyenne.
#3.
hist(vect_moyenne,main="Histogramme des moyennes",xlab="",xlim =c(0.35,0.65),ylim=c(0,2800),col="red")
#Histogramme du vecteur des moyennes des colonnes.
#4.
curve(dnorm(x,mean(vect_moyenne),sd(vect_moyenne)/sqrt(N_TLC)),add=TRUE)


#Illustrations pour d'autres Lois

#5.
Nvectunif_TLC <- function(a,b,N,M){ 
hist(colSums(matrix(data = runif(N*M,a,b),nrow=N,ncol=M))/N,main="Histogramme des moyennes",xlab="",xlim =c(a+b/3,b-b/3),ylim=c(0,M/2),col="red")
vect_moyenne <- (colSums(matrix(data = runif(N*M,a,b),nrow=N,ncol=M))/N)
curve(dnorm(x,mean(vect_moyenne),sd(vect_moyenne)/sqrt(N_TLC)),add=TRUE)
}
#fonction qui fait varier le graphe de la loi uniforme et la courbe de la loi normale.

#6
Nvectexpo_TLC <- function(alpha,N,M){ 
  hist(colSums(matrix(data = rexp(N*M,rate=alpha),nrow=N,ncol=M))/N,main="Histogramme des moyennes",xlab="",xlim =c(0,1),ylim=c(0,M/2),col="red")
  vect_moyenne <- (colSums(matrix(data = rexp(N*M,rate=alpha),nrow=N,ncol=M))/N)
  curve(dnorm(x,mean(vect_moyenne),sd(vect_moyenne)/sqrt(N_TLC)),add=TRUE)
}
#fonction qui fait varier le graphe de la loi exponentielle et la courbe de la loi normale.

#7.
Nvectcauchy_TLC <- function(N,M){ 
  hist(colSums(matrix(data = rcauchy(N),nrow=N,ncol=M))/N,main="Histogramme des moyennes",xlab="",col="red")
  vect_moyenne <- (colSums(matrix(data = rcauchy(N),nrow=N,ncol=M))/N)
  curve(dnorm(x,mean(vect_moyenne),sd(vect_moyenne)/sqrt(N_TLC)),add=TRUE)
}
#fonction qui fait varier le graphe de la loi de Cauchy et la courbe de la loi normale.


#4. Eudier La Formule Du Transfert

#1.
N=100
a=6
X<-runif(N,-a,a)
#X<-runif(N,0,a) Cela produit une erreur NaN car quand a = 0 et x = 0 (2*a)^(-1) tend vers Inf et sqrt(0) = 0 donc Inf*0 = NaN
Y<-X^3
hist(Y,freq=FALSE)
curve((2*a)^(-1)*(sqrt(x))^(-1),add=TRUE)

#2.
#nous Pouvons voir que les valeurs convergent vers 0,car baucoup de valeurs ne se trouvent pas dans l'intervalle 0<y<a^2

#3.
#On peut supposer que A=1/2 et B = -1

#4.

N=100
alpha=2
X<-rexp(N,rate=alpha)
Y<-X^2
hist(Y,freq=FALSE,ylim=c(0,0.5),breaks=15)
curve((alpha/sqrt(x))*exp(-alpha*sqrt(x)),add=TRUE)


#5. Un Autre exemple de convergence

#1.
#Oui les valeurs Mn et mn vont converger vers a et b les bornes quand N va tendre vers l'infini.

#2.
converge_unif <- function(a,b,N,M){
xvect <- runif(N*M,a,b)
mat_converge = matrix(data = runif(xvect),nrow=N,ncol=M)
print(cummax(mat_converge))
#on affiche le maximum cumulatif
print(cummin(mat_converge))
#on affiche le minimum cumulatif
}

#6 Autre exemple de loi limite

#1.
max_unif <- function(a,b,N,M){
  xvect <- runif(N*M,a,b)
  mat_converge = matrix(data = runif(xvect),nrow=N,ncol=M)
  vect_maxi <-cummax(mat_converge)
  #on créé le vecteur du maximum cumulatif
  hist(vect_maxi,breaks=10,ylim=c(0,2),xlim=c(0,1))
  curve(dexp(x,rate=2),add=TRUE,col="red")
}
