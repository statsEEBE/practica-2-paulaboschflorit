#Codigo para problema 2
mis_dades <- iris
mis_dades
dim (mis_dades)
names(mis_dades)
mis_dades$Petal.Length
#mitjana
mean(mis_dades$Petal.Length)
#s (desviació)
sd(mis_dades$Petal.Length)
hist(mis_dades$Petal.Length)
x<- mis_dades$Petal.Length
y <- mis_dades$Sepal.Length
y
plot(x,y,col="red", pch=16)
#donat un valor de x quina és la mitjana de y, regressió condicionada
#trobar m (pendent) i b(intersecció eix y); reacta de regressió
m <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
m
b <- mean(y)-m*mean(x)
b
#predicció x=1.5
m*1.5+b
#per trobar m i b, directament
# altrgr + 4 , espai = ~
mod<- lm(y~x)
summary (mod)
#punts on he fet medicions data.frame
data.frame(x=x)
predict(mod, data.frame(x=1.5))
ypredict<-predict(mod, data.frame(x=x))
#per dibuixar la recta en l'histograma
lines(x,ypredict)
#coeficient de determinació (R^2)
Rsq<-sum ((ypredict-mean(y))^2)/sum((y-mean(y))^2)
Rsq
