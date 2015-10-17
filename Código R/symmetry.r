######################
#
# Analysis of Experiences Supported by SMLearning System 
# 
# Iván D. Claros - ivan.claros@uam.es
# Universidad Autónoma de Madrid - 2014
#
######################

PTSIZE=11
library(igraph)
library(corrgram)
palette("default")

#Configuration of margis
par(mar=c(2,2,1,1))
for(course_id in c("136","178","186")){
  
  #Cargar los datos de la matriz
  social = read.table(paste("../Datasets/social-",course_id,".txt",sep=""),FALSE,"\t")
  authors = read.table(paste("../Datasets/authors-",course_id,".txt",sep=""),FALSE,"\t")
  deadlines = read.table(paste("../Datasets/hitos-",course_id,".txt",sep=""),FALSE,"\t")
  
  labels <- strptime(social$V1,"%Y-%m-%d %H:%M:%S")
  deads <- strptime(deadlines$V1,"%Y-%m-%d")
  
  NAU = nrow(authors) #Numero de autores
  NAR = nrow(social)
  #Integración de los participantes a la experiencia el tiempo
  
  D = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  Dout = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  Din = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  B = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  C = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  Cout = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  Cin = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  AC = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  P = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  S = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  Sin = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  Sout = matrix(0,NAU,NAR,dimnames=list(authors$V2))
  CEd = matrix(0,3,NAR)
  CEc = matrix(0,3,NAR)
  CE = matrix(0,10,NAR)
  nClus = matrix(0,1,NAR)
  
  for(ic in 1:NAR){    
    adj <- t(social[ic,][2:(1+NAU*NAU)])
    mx<-matrix(adj,NAU)
    social$Count[ic]<-sum(mx)
    g<-graph.adjacency(t(mx),mode="directed",weighted=TRUE)
    
    #Calculo de Métricas Individuales
    P[,ic] = page.rank(g,directed=TRUE)$vector
    D[,ic] = degree(g)
    Dout[,ic] = degree(g,mode="out")
    Din[,ic] = degree(g,mode="in")
    B[,ic] = betweenness(g)
    C[,ic] = closeness(g)
    Cout[,ic] = closeness(g,mode="out")
    Cin[,ic] = closeness(g,mode="in")
    S[,ic]=graph.strength(g)
    Sin[,ic]=graph.strength(g,mode="in")
    Sout[,ic]=graph.strength(g,mode="out")
    
    #Calculo de Métricas Grupales    
    CEd[1,ic]=centralization.degree(g,mode="out")$centralization
    CEd[2,ic]=centralization.degree(g,mode="in")$centralization
    CEd[3,ic]=centralization.degree(g)$centralization
    
    CEc[1,ic]=centralization.closeness(g,mode="out")$centralization
    CEc[2,ic]=centralization.closeness(g,mode="in")$centralization
    CEc[3,ic]=centralization.closeness(g)$centralization
    
    CE[1,ic]=centralization.degree(g)$centralization
    CE[2,ic]=centralization.betweenness(g)$centralization
    CE[3,ic]=centralization.closeness(g)$centralization
    CE[5,ic]=graph.density(g, loops=TRUE)
    CE[6,ic]=transitivity(g)
    CE[7,ic]=centralization.degree(g,mode="in")$centralization
    CE[8,ic]=centralization.degree(g,mode="out")$centralization
    CE[9,ic]=reciprocity(g)
    CE[10,ic]=centralization.evcent(g)$centralization
    
    #Calculo de Métricas Clusters
    nClus[1,ic]=(clusters(g, mode="weak"))$no
  }
 
  png(filename=paste("Fig7.14-",course_id,"-Clusters.png",sep=""), width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L', t(nClus),col=c(1), lty=1:3,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Número de clusters"), col=c(1), lty=1:4)
  dev.off();
  
  #Global Metrics Centralization
 
  png(filename=paste("Fig7.x-",course_id,"-Centralization-InOut.png",sep=""), width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L', t(CEd[c(1:2),]),col=c(1), lty=1:3,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Grado de entrada","Grado de salida"), col=c(1), lty=1:4)
  dev.off();
  
    
  #Global Metrics Centralization  
  png(filename=paste("Fig7.11-",course_id,"-Centralization.png",sep=""), width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L', t(CE[c(1:3),]),col=c(1), lty=1:3,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  bty = "o",  horiz=FALSE, legend = cbind("Grado","Intermediación","Cercanía"), col=c(1), lty=1:4)
  dev.off();
 
  #Reciprocidad
  
  png(filename=paste("Fig7.12-",course_id,"-Reciprocity.png",sep=""), width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L', CE[9,],col=c(1), lty=1,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Reciprocidad"), col=c(1), lty=1:4)
  dev.off();  
  
  print(paste(course_id, "Centralización",boxplot.stats(CE[1,])$conf ,collapse = " - "))  
  print(paste(course_id, "Reciprocidad",boxplot.stats(CE[9,])$conf ,collapse = " - "))  
  print(paste(course_id, "Total de comentarios",max(social$Count)))
  ctIO<-cor.test(CEd[1,],CEd[2,]);
  print(paste(course_id,"Correlación ",course_id,"p-value",ctIO$p.value,"Estimado:",ctIO$estimate))
  
  png(filename=paste("Fig7.12-",course_id,"-Comments.png",sep=""), width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L', social$Count,col=c(1), lty=1,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Comentarios"), col=c(1), lty=1:4)
  dev.off();

  #Analisis individual
  IOC = c();
  for(a in 1:length(authors$V1)){
    Metrics<-matrix(0,8,ncol(D))
    Metrics[1,] = S[a,]  
    Metrics[2,] = B[a,]
    Metrics[3,] = C[a,]
    Metrics[4,] = Sin[a,]
    Metrics[5,] = Sout[a,]
    Metrics[6,] = Cin[a,]
    Metrics[7,] = Cout[a,]
    Metrics[8,] = Cin[a,]+Cout[a,]
    
    #muestra la tendencia del estudiante hacia dar o recibir aportes (IC 95%)
    st<-boxplot.stats(na.omit(Metrics[6,]-Metrics[7,])/(Metrics[6,]+Metrics[7,]))
    m<-mean(st$conf)
    IOC<-c(IOC,m)
    
    #Generar imagen para cada estudiantes de sus métrica de entrada y salida
    if(0){
      matplot(as.POSIXct(labels), t(Metrics[c(6,7),]), col=c(1), lty=c(1,2), type = "l",xaxt="n",xlab="Time",ylab="Value",main= paste("Normalized Metrics","Author =",authors$V2[a]))
      axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
      axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
      for(d in 1:length(deads)){  
        t<-deads[d]
        abline(v=as.POSIXct(t),col="#CCCCCC",lty=4)
      }
      legend("bottomright", inset=c(0.01,0.01),horiz=FALSE, legend = cbind("Grado de entrada","Grado de salida"), col=c(1), lty=c(1,2))  
    }    
  }
   
  V(g)$degree.in<-degree(g,mode = "in")
  V(g)$degree.out<-degree(g,mode = "out")
  V(g)$r=IOC #Medida de tendencia In-Out. 
  V(g)$r=round(1+10*(V(g)$r-min(V(g)$r)))
  myPalette<-colorRampPalette(c("gold","white", "deepskyblue"))(n = 10);
  V(g)$color<-myPalette[V(g)$r]
  V(g)$label<-paste("E", authors$V1,sep="")
  V(g)$size=3+ degree(g)    
  png(filename=paste("Fig7.13-",course_id,"-NET.png",sep=""), width = 500, height = 400, units = "px", pointsize = PTSIZE,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  
  par(mar=c(0,0,0,0))
  plot(g)
  dev.off();
}
