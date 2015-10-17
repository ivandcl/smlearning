FACTOR = 1; #Factor de conversión del beneficio
PERCENT = seq(0,1,0.01); #Rango de valores para la variación del porcentaje esfuerzo/desconocido
NROWS = length(PERCENT);
dat = CEd = matrix(0,2,NROWS)
for(i in 1:NROWS){
  p<-PERCENT[i];
  acc = c();
  for (t in 1:100){
    e = runif(1000,5,110); #variable aleatoria del esfuerzo [5 a 110]
    b=(e*p/max(e)+(1-p)*(runif(1000,0,1)))*FACTOR; #cálculo del beneficio - Normalizado
    s = na.omit(b/e);
    acc<-c(acc,cor(b,e)*mean(s)) #cálculo de Satisfacción = Correlación (Esperzo, Beneficio) * Promedio (Beneficio/Esfuerzo)
  }  
  dat[1,i]=mean(acc); #Valor promedio del conjunto de iteraciones
  dat[2,i]=sd(acc); #Desviación típica
  
}
#par(oma=c(2,2,0.1,0.1))
par(oma=c(0,0,0,0))
par(omi=c(0,0,0,0))
par(mar=c(3,3,0.1,0.1))
plot(PERCENT*100,dat[1,], xlab="", ylab="", type = "l")
cat("RANGO Satisfacción > Factor: ", PERCENT[which(dat[1,]>=FACTOR)])
mtext("Porcentaje de esfuerzo que se traduce en beneficio", side=1, line=2, cex=1, outer=FALSE)
mtext("Satisfacción", side=2, line=2, cex=1)
