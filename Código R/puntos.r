######################
#
# Analysis of Experiences Supported by SMLearning System - POINTS
# Data source: https://dimo1.ii.uam.es/v01/analytics/points/[idg=136,178,186] 
# 
# Iván D. Claros - ivan.claros@uam.es
# Universidad Autónoma de Madrid - 2014
#
######################
#setwd("~/Dropbox/DATASET")
setwd("C:\\Users\\ic\\Dropbox\\DATASET")
library(ggplot2)
source("header.r")

CoefVar<-function(v){
  return (sd(v)/mean(v))
}

Experiences=c("EPS2012","EPS2013","EPS2014");
cat("\014"); #Clear screen

for(Experience in Experiences){
  
  #Load Authors of Scenario
  authors = read.table(paste(Experience,"\\authors.csv",sep=""),FALSE,"\t")
  authors$X<-paste("X",authors$V1,sep="")
  
  #Cargar los datos de la matriz
  milestones = read.table(paste(Experience,"\\milestones.csv",sep=""),FALSE,"\t")
  milestones$V1 <- strptime(milestones$V1,"%Y-%m-%d %H:%M:%S")
  
  #Points
  points = read.table(paste(Experience,"\\points.csv",sep=""),TRUE,"\t")
  points$ID <- strptime(points$ID,"%Y-%m-%d");
  points = points[points$ID<max(milestones$V1),]
  points = points[points$ID>min(milestones$V1),]
  ns<-names(points)[2:ncol(points)]
  points$All<-rowSums(points[,ns]);
  
  mx<-matrix(unlist(points[,ns]),ncol=length(ns))
  colnames(mx)<-ns
  
  v<-apply(mx, 1, var);
  s<-apply(mx, 1, sd);
  m<-apply(mx, 1, mean);
  mxd<-data.frame(mean=m,ds=s)
  
  #############################################
  #6.8 Valor Medio de la Variable Puntos
  #############################################
  png(filename=paste("Fig7.9-",Experience,"-Puntos.png",sep=""),  width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "transparent", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
  par(mar=c(4,4,1,1))
  matplot(as.POSIXct(points$ID),mxd, bty="L", type="l", xaxt = "n", ylab="Puntos", xlab="Tiempo", lty = c(1,2), col=1)
  axis.POSIXct(1, at = seq(points$ID[1], max(points$ID)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(points$ID[1], max(points$ID)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(milestones$V1)){  
  t<-milestones$V1[d]  
  text(as.POSIXct(t),min(mx),milestones$V2[d],pos=2,col="black",cex = .9, offset = 0)
  abline(v=as.POSIXct(t),col="black",lty=3)
  }
  legend(as.POSIXct(min(milestones$V1)),max(mxd),horiz=FALSE,legend=c("Promedio","Desviación"), lty = c(1,2),merge = FALSE)
  (l<-lm(mean~as.POSIXct(points$ID),data=mxd))
  r.sq<-summary(l)$r.squared
  abline(l,lty=4)
  text(as.POSIXct(mean(l$model[,2])),mean(l$model$mean),paste("R2 =",format(round(r.sq, 2), nsmall = 2)),pos=2,col="black",cex = .9)
  dev.off();
  #############################################
  datos=data.frame(
    x=as.POSIXct(points$ID),
    y=m
  )
  ggplot(datos, aes(x=x, y=y)) +
    geom_line(size = 1) +   
    geom_smooth(method=lm,col=1) +
    ylab("Divergencia") +
    xlab("Tiempo")+
    theme_bw();
  
  
  q <- data.frame(points)
  
  q$mean <- m
  q$sd   <- s
  png(filename=paste(path.img, "Fig7.9B-",Experience,"-Puntos.png",sep=""),  width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "transparent", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
  par(mar=c(2,2,1,1))
  eb <- aes(ymax = mean + sd, ymin = mean - sd)
  ggplot(data = q, aes(x = q$ID, y = mean)) + 
  geom_line(size = 1) + 
  geom_ribbon(eb, alpha = 0.2)+ theme(panel.background = element_rect(fill = 'white', colour = 'gray'))
  dev.off();
  #############################################
  #Fig. 6.9. Indicador de Beneficio de la Interacción para CP1
  #############################################
  png(filename=paste(path.img, "Fig7.9-",Experience,"-Beneficio.png",sep=""),
    width = 480, height = 280, units = "px", pointsize = PTSIZE,
    bg = "transparent", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
  par(mar=c(2,2,1,1))
  delta.time<-as.numeric(diff(points$ID))
  Benefit=list();
  Benefit$Norm<-apply(t(mxd$mean), 1, function(x)(x-min(x))/(max(x)-min(x)))
  Benefit$Value<-diff(Benefit$Norm)/delta.time
  Benefit$Day<- points$ID[(1:length(Benefit$Value))+1]
  sum(Benefit$Value)
  
  plot(as.POSIXct(Benefit$Day),Benefit$Value, type="l", xaxt = "n", ylab="Puntos", xlab="Tiempo", lty = 1, col=1)
  axis.POSIXct(1, at = seq(Benefit$Day[1], max(Benefit$Day)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(Benefit$Day[1], max(Benefit$Day)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(milestones$V1)){  
  t<-milestones$V1[d]  
  text(as.POSIXct(t),min(Benefit$Value),milestones$V2[d],pos=2,col="black",cex = .9, offset = 0)
  abline(v=as.POSIXct(t),col="black",lty=3)  
  }
  text(as.POSIXct(mean(Benefit$Day)),max(Benefit$Value),paste("Coef. Var =",format(round(CoefVar(Benefit$Value),3),nsmall = 3,scientific = FALSE)),pos=1,col="black",cex = 0.9)
  dev.off();
  #############################################
  
  
  #6.9.1 Variable Puntos por Estudiantes
  png(filename=paste(path.img, "Fig7.9.1-",Experience,"-Puntos_Estudiantes.png",sep=""),
    width = 480, height = 280, units = "px", pointsize = PTSIZE,
    bg = "transparent", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"))
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(points$ID),mx, type="l", xaxt = "n", ylab="Puntos", xlab="Tiempo", pch=1:length(ns), lty = 1:length(ns), col=1)
  axis.POSIXct(1, at = seq(points$ID[1], max(points$ID)+6, "weeks"),format = "%b %d")
  axis.POSIXct(1, at = seq(points$ID[1], max(points$ID)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(milestones$V1)){  
  t<-milestones$V1[d]  
  text(as.POSIXct(t),min(mx),milestones$V2[d],pos=2,col="black",cex = .8, offset = 0)
  abline(v=as.POSIXct(t),col="black",lty=3)
  }
  legend(as.POSIXct(min(milestones$V1)),max(mx),horiz=FALSE,legend=authors[authors$X %in% ns,"V2"], col="black", lty = 1:length(ns),merge = FALSE)
  dev.off();
  
  plot(as.POSIXct(points$ID),s, type="l", xaxt = "n", ylab="Puntos", xlab="Tiempo", main="Desviación de la Variable Puntos", lty = c(2,1,3), col=1)
  axis.POSIXct(1, at = seq(points$ID[1], max(points$ID)+6, "weeks"),format = "%b %d")
  axis.POSIXct(1, at = seq(points$ID[1], max(points$ID)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(milestones$V1)){  
  t<-milestones$V1[d]  
  text(as.POSIXct(t),min(mx),milestones$V2[d],pos=2,col="black",cex = .8, offset = 0)
  abline(v=as.POSIXct(t),col="black",lty=3)
  }
  
  
  plot(as.POSIXct(Benefit$Day),Benefit$Norm[(1:length(Benefit$Value))+1], type="l", xaxt = "n", ylab="Puntos", xlab="Tiempo", main="Indicador de Beneficio de la Interacción Acumulado", lty = 1, col=1)
  axis.POSIXct(1, at = seq(Benefit$Day[1], max(Benefit$Day)+6, "weeks"),format = "%b %d")
  axis.POSIXct(1, at = seq(Benefit$Day[1], max(Benefit$Day)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(milestones$V1)){  
  t<-milestones$V1[d]  
  text(as.POSIXct(t),min(Benefit$Value),milestones$V2[d],pos=2,col="black",cex = .8, offset = 0)
  abline(v=as.POSIXct(t),col="black",lty=3)
  }
  
}