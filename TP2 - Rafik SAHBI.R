Data=cars # Les données sont les suivantes
Data
X=Data$dist # Les données des différentes distances sont les suivantes
X
Y=Data$speed # Les données des différentes vitesses sont les suivantes
Y
MoyDist=mean(X) # La moyenne des distances est égale à 
MoyDist
MoyVit=mean(Y) # La moyenne des vitesses est égale à
MoyVit
VarDist=var(X) # La variance des distances est égale à
VarDist
VarVit=var(Y) # La variance des vitesses est égale à
VarVit
EcartTypeDist=sd(X) # L'écart-type des distances est égal à
EcartTypeDist
EcartTypeVit=sd(Y) # L'écart-type des vitesses est égal à
EcartTypeVit
Reg1<-lm(Y~X)
Reg1
Coeff1=coefficients(Reg1)
Coeff1
Reg0<-lm(X~Y)
Reg0
Coeff0=coefficients(Reg0)
Coeff0
c=round(Coeff0[2],6)
c
a=1/c
a
d=round(Coeff0[1],6)
d
b=-d/c
b
X_lab=expression(Distance~~en~~Kilomètres)
X_lab
Y_lab = expression(Vitesse~~en~~Kilomètres~~par~~Heure)
Y_lab
Titre=paste("Droites de régression de la Vitesse par rapport à la Distance et de 
           la Distance par rapport à la vitesse")
Titre
plot(Y~X, xlab=X_lab, ylab=Y_lab, col="blue", main=Titre)
abline(Reg1, col="blue", lwd=2)
abline(b, a, col="red", lwd=2)
MoyG=points(MoyDist,MoyVit,pch=4,lwd=2,col="orange")
MoyG
text(65,14,paste("Barycente=(MoyDist,MoyVit)"),col="orange")
text(100,20,paste("Y = ", round(Coeff1[2],6), "X + ", round(Coeff1[1],6)),
     col="blue")
text(35,5,paste("X = ", round(Coeff0[2],6), "Y - ", abs(round(Coeff0[1],6))),
     col="red")
alpha=atan(abs((a-round(Coeff1[2],6))/(1+a*round(Coeff1[2],6))))
alpha # L'angle aigü en radian vaut
pi
beta=pi-alpha
beta # L'angle obtu en radian vaut
CoeffCorrelation=cor(X,Y)
CoeffCorrelation # Le coefficient de corrélation vaut