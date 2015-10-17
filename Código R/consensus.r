######################
#
# Analysis of Experiences Supported by SMLearning System 
# 
# Iván D. Claros - ivan.claros@uam.es
# Universidad Autónoma de Madrid - 2015
#
######################
require(ggplot2)
require(igraph)

disp<-function(x){
  return (mean(boxplot.stats(x)$conf))
}

calc.Rates<-function(it){
  v<-aggregate(it$rate,FUN=length,by=list(author=it$author)) #Numero de votos por Autores
  v$label<-paste("E",v$author,sep="");
  v$media<-aggregate(it$rate,FUN=mean,by=list(it$author))$x
  v$aportes<-aggregate(it$rate,FUN=length,by=list(it$author))$x
  v$dispercion<-(aggregate((it$rate-it$mean)^2,FUN=disp,by=list(it$author))$x/(v$x))
  return (v);
}

c.points<-list();
samples<-list();
course_id=136
for(course_id in c("136","178","186")){  
  #1. Cargar datos
  rates = read.table(paste("../Datasets/rates-",course_id,".txt",sep=""),header=TRUE,"\t")
  rates$created <- strptime(rates$created,"%Y-%m-%d %H:%M:%S"); #Parse datetime
  rates$days<- as.character(strptime(rates$created,"%Y-%m-%d")); #Parse datetime
  rates$relative.day<- round((rates$created-min(rates$created))/(7*24*3600)) #Tiempo relativo en dias
  
  rates$Err<-(rates$mean-rates$rate)^2;
  plot(aggregate(Err~relative.day,rates,FUN=mean)$Err)
  authors = read.table(paste("../Datasets/authors-",course_id,".txt",sep=""),FALSE,"\t")
  deadlines = read.table(paste("../Datasets/hitos-",course_id,".txt",sep=""),FALSE,"\t")
  
  labels <- strptime(rates$created,"%Y-%m-%d %H:%M:%S")
  deads <- strptime(deadlines$V1,"%Y-%m-%d")
    
  consensus<-rates[rates$votes>2,]
  vt<-aggregate((consensus$rate[consensus$votes>1]-consensus$mean[consensus$votes>1])^2,FUN=mean,by=list(consensus$days[consensus$votes>1]))  
  c.points$y[[paste(course_id)]]<-vt$x
  c.points$x[[paste(course_id)]]<-1:length(vt$x)  
  
  v1<-calc.Rates(consensus[consensus$created<mean(labels),]);
  v1$col="it1"
  
  v2<-calc.Rates(consensus[consensus$created>=mean(labels),]);
  v2$col="it2"
  
  v<-data.frame(mapply(c, v1, v2, SIMPLIFY=FALSE))
  v$Media<-v$media;
  v$Divergencia<-log(100*v$dispercion);
  v$aportes<-log(v$aportes)
  
  
  
  ggplot(v, aes(x = Media, y = Divergencia, label = label)) +
    geom_point(aes(size = Divergencia, colour = factor(col),  alpha=.002)) + 
    geom_text(hjust = 1) +
    scale_size(range = c(10,30)) +
    theme_bw()+ 
    theme(legend.position="none"); 
  ggsave(filename =  paste("Fig7.15-",course_id,"-Rates.png",sep=""))
  
  
  
  #Distribución de votos entre individuos
  consenso.dist<-data.matrix(table(consensus$owner, consensus$author))
  nr<-intersect(row.names(consenso.dist),colnames(consenso.dist))
  g<-graph.adjacency(consenso.dist[nr,nr],weighted = T,mode = "d")
  V(g)$degree.in<-degree(g,mode = "in")
  V(g)$degree.out<-degree(g,mode = "out")
  V(g)$r=r<-(V(g)$degree.in-V(g)$degree.out)/(V(g)$degree.in+V(g)$degree.out)
  V(g)$r=round(1+10*(V(g)$r-min(V(g)$r)))
  myPalette<-colorRampPalette(c("gold","white", "deepskyblue"))(n = 10);
  V(g)$color<-myPalette[V(g)$r]
  V(g)$label<-paste("E", nr,sep="")
  V(g)$size=3+ degree(g)   
  png(filename=paste("Fig7.1x-",course_id,"-Rate-net.png",sep=""), width = 500, height = 400, units = "px", pointsize = 11,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  
  par(mar=c(0,0,0,0))  
  plot(g) 
  dev.off();
  
  png(filename=paste("Fig7.17-",course_id,"-Rate-time.png",sep=""), width = 500, height = 400, units = "px", pointsize = 11,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),jitter(rates$rate),col=c(1), lty=1:3,pch="o", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  abline(v=as.POSIXct(mean(labels)),col=1,lty=4)
  dev.off();
  
  png(filename=paste("Fig7.20-",course_id,"-Calidad-Interaccion.png",sep=""), width = 500, height = 400, units = "px", pointsize = 11,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  
  par(mar=c(4,4,0,0))
  rates$mean.n<- round(rates$mean)
  mv<-table(rates$votes,rates$mean.n)
  barplot(rowMeans(t(mv)),ylab = "Número de votos",xlab="Calidad del recurso", names.arg = c("Muy baja","Baja","Media","Alta","Muy alta"))
  chi<- chisq.test(mv) #Demostración del mas calidad + interacción
  print(paste(course_id,"Demostración del mas calidad + interacción",chi$p.value))
  dev.off();
  
  consenso<-data.frame(
    tiempo=1:length(vt$x),
    divergencia=vt$x
  );
  ggplot(consenso, aes(x=tiempo, y=divergencia)) +
    geom_point(size = 2) +   
    geom_smooth(method=lm,col=1) +
    ylab("Divergencia") +
    xlab("Tiempo")+
    theme_bw();
  ggsave(filename =  paste("Fig7.16",course_id,"Consensus.png",sep=""),scale = 2,units = "cm", width = 10,height = 7)
 
  l<-lm(divergencia~tiempo,data = consenso)
  lsum<-summary(l)
  cc<-cor.test(consenso$tiempo,consenso$divergencia)
  print(paste(course_id,"Consenso","p-value",(cc$p.value),"R2",lsum$r.squared,"M",l$coefficients[2]));
  
  
  #Dividiendo la experiencia en dos partes
  consensus<-rates[rates$votes>2,]
  consensus<-consensus[consensus$created<mean(rates$created),]
  a.dispertion<-na.omit(aggregate((consensus$rate-consensus$mean)^2,FUN=mean,by=list(uid=consensus$author)))
  
  consensus<-rates[rates$votes>2,]
  consensus<-consensus[consensus$created>mean(rates$created),]
  b.dispertion<-na.omit(aggregate((consensus$rate-consensus$mean)^2,FUN=mean,by=list(uid=consensus$author)))
  
    
  c.dispertion<-merge(a.dispertion,b.dispertion,by = "uid")
  
  plot(c.dispertion$x.x,c.dispertion$x.y,main = course_id)
  lines(0:2,0:2,col="red")
  
  
  samples$it1[[paste(course_id)]]<-c.dispertion$x.x
  samples$it2[[paste(course_id)]]<-c.dispertion$x.y 
  samples$names[[paste(course_id)]]<-c.dispertion$uid
  t0<-c.dispertion$x.x;
  t1<-c.dispertion$x.y;
  wt<-wilcox.test(t0,t1,"g",paired = T)   
  print(paste(course_id,"Wlicox.test p-value",wt$p.value,"T0-T1",mean(boxplot.stats(t0)$conf)-mean(boxplot.stats(t1)$conf)))
  
  #Tiende el autor a votarse de la misma forma?
  h0<-table(rates$author[rates$rate>3],rates$rate[rates$rate>3]);
  print(paste("Dependen los votos de dueño del recurso?",0.05>chisq.test(h0)$p.value))
  
  #Dependen los votos de los individuos
  h0<-table(rates$owner,rates$author);
  print(paste("Dependen los votos de dueño del recurso?",0.05>chisq.test(h0)$p.value))
  
  #Dependen los votos de la calidad
  h0<-table(rates$owner,rates$mean.n);
  print(paste("Dependen los votos de la calidad?",0.05>chisq.test(h0)$p.value))
  
  #Depenten el voto del número de votos que han dado otros?
  #h0<-table(rates$rate,rates$votes);
  #print(paste("Depenten el voto del número de votos que han dado otros?",0.05>chisq.test(h0)$p.value))
  
}

#Demostración del consenso
consenso<-data.frame(
  tiempo=unlist(c.points$x),
  divergencia=unlist(c.points$y)
);

boxplot(consenso$divergencia~consenso$tiempo)
plot(aggregate(divergencia~tiempo,consenso,FUN=mean))

cor.test(consenso$tiempo,consenso$divergencia)

l<-lm(divergencia~tiempo,data = consenso)
lsum<-summary(l)

ggplot(consenso, aes(x=tiempo, y=divergencia)) +
  geom_point(size = 2) +   
  geom_smooth(method=lm,col=1) +
  ylab("Divergencia") +
  xlab("Tiempo")+
  theme_bw();
ggsave(filename =  paste("Fig7.16 Consensus total.png",sep=""),scale = 2,units = "cm", width = 10,height = 7)

#Test Kolmogorov-Smirnov 
samples.it1<-unlist(samples$it1)
samples.it2<-unlist(samples$it2)
samples.names<-unlist(samples$names)
plot(samples.it2,samples.it1,ylim=c(0,2),main="Comparación del Error por persona en T0 vs T1")
lines(0:2,0:2)
text(samples.it2,samples.it1,samples.names)

samples.colors<-c(rep(1,length(samples$names[[1]])),rep(2,length(samples$names[[2]])),rep(4,length(samples$names[[3]])))

png(filename=paste("Fig7.18-Consenso-individual.png",sep=""), width = 500, height = 400, units = "px", pointsize = 11,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  
par(mar=c(4,4,0.1,0.1))  
plot(samples.it1,samples.it2,col=samples.colors ,ylim=c(0,2.5),xlim=c(0,2),xlab = "T0",ylab = "T1", bty="L")
abline(a=0,b=1)
legend(0,2.5,c("P1","P2","P3"),col=c(1,2,4),pch = 1)
dev.off();

l=10
barplot(rep(1,l),col=0:l)

wt<-wilcox.test(samples.it1,samples.it2,"g",paired = T) 
print(paste("Wlicox.test p-value",wt$p.value, "IT1",mean(boxplot.stats(it1)$conf),"IT2",mean(boxplot.stats(it2)$conf)))
print(wt)
mean(boxplot.stats(it1)$conf)-mean(boxplot.stats(it2)$conf)
png(filename=paste("Fig7.15-Error-T0-T1.png",sep=""), width = 500, height = 400, units = "px", pointsize = 11,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  
par(mar=c(2,2,1,1))
boxplot(cbind(T0=it1,T1=it2));
dev.off();



