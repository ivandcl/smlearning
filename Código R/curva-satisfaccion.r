FACTOR = 1; #Factor de conversi�n del beneficio
PERCENT = seq(0,1,0.01); #Rango de valores para la variaci�n del porcentaje esfuerzo/desconocido
NROWS = length(PERCENT);
dat = CEd = matrix(0,2,NROWS)
for(i in 1:NROWS){
  p<-PERCENT[i];
  acc = c();
  for (t in 1:100){
    e = runif(1000,5,110); #variable aleatoria del esfuerzo [5 a 110]
    b=(e*p/max(e)+(1-p)*(runif(1000,0,1)))*FACTOR; #c�lculo del beneficio - Normalizado
    s = na.omit(b/e);
    acc<-c(acc,cor(b,e)*mean(s)) #c�lculo de Satisfacci�n = Correlaci�n (Esperzo, Beneficio) * Promedio (Beneficio/Esfuerzo)
  }  
  dat[1,i]=mean(acc); #Valor promedio del conjunto de iteraciones
  dat[2,i]=sd(acc); #Desviaci�n t�pica
  
}
#par(oma=c(2,2,0.1,0.1))
par(oma=c(0,0,0,0))
par(omi=c(0,0,0,0))
par(mar=c(3,3,0.1,0.1))
plot(PERCENT*100,dat[1,], xlab="", ylab="", type = "l")
cat("RANGO Satisfacci�n > Factor: ", PERCENT[which(dat[1,]>=FACTOR)])
mtext("Porcentaje de esfuerzo que se traduce en beneficio", side=1, line=2, cex=1, outer=FALSE)
mtext("Satisfacci�n", side=2, line=2, cex=1)
