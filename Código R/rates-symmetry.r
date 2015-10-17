######################
#
# Analysis of Experiences Supported by SMLearning System - RATES
# Data source: https://dimo1.ii.uam.es/v01/analytics/social/[idg=186] 
# 
# Iván D. Claros - ivan.claros@uam.es
# Universidad Autónoma de Madrid - 2014
#
######################



for(course_id in c("136","178","186")){  
  SNA<-list();
  
  social = read.table(paste("../Datasets/social-",course_id,".txt",sep=""),FALSE,"\t")
  rates = read.table(paste("../Datasets/rates-social-",course_id,".txt",sep=""),FALSE,"\t")
  authors = read.table(paste("../Datasets/authors-",course_id,".txt",sep=""),FALSE,"\t")
  
  deadlines = read.table(paste("../Datasets/hitos-",course_id,".txt",sep=""),FALSE,"\t")
  deads <- strptime(deadlines$V1,"%Y-%m-%d")
  
  
  
  NAU = nrow(authors) #Numero de autores
  NAR = nrow(rates) 
  
  Metrics<-list();
  for(idc in 1:NAR){    
    adj <- t(rates[idc,][2:(1+NAU*NAU)]);    
    mx<-matrix(adj,NAU)
    g<-graph.adjacency(t(mx),mode="directed",weighted=TRUE);
    SNA$rate[[paste(course_id,idc,sep=".")]]<-betweenness(g);  
    Metrics$degree[idc]<-centralization.degree(g)$centralization
    Metrics$degree.in[idc]<-centralization.degree(g,mode = "in")$centralization
    Metrics$degree.out[idc]<-centralization.degree(g,mode="out")$centralization
    Metrics$reciprocity[idc]<-reciprocity(g)
    Metrics$betweenness[idc]<-centralization.betweenness(g)$centralization
    Metrics$closeness[idc]<-centralization.closeness(g)$centralization
    Metrics$clusters[idc]<-clusters(g)$no
  }
  
  NAR = nrow(social)
  
  MetricsSocial<-list();
  for(idc in 1:NAR){ 
    adj <- t(social[idc,][2:(1+NAU*NAU)]);
    length(adj)
    mx<-matrix(adj,NAU)
    g<-graph.adjacency(t(mx),mode="directed",weighted=TRUE);
    SNA$social[[paste(course_id,idc,sep=".")]]<-betweenness(g);   
    MetricsSocial$degree[idc]<-centralization.degree(g)$centralization
    MetricsSocial$degree.in[idc]<-centralization.degree(g,mode = "in")$centralization
    MetricsSocial$degree.out[idc]<-centralization.degree(g,mode="out")$centralization
    MetricsSocial$reciprocity[idc]<-reciprocity(g)
    MetricsSocial$betweenness[idc]<-centralization.betweenness(g)$centralization
    MetricsSocial$closeness[idc]<-centralization.closeness(g)$centralization
    MetricsSocial$clusters[idc]<-clusters(g)$no
  }
  
  data<-data.frame(MetricsSocial);
  data$days<- as.character(strptime(social$V1,"%Y-%m-%d"))
  
  deg<-aggregate(data$degree,FUN=mean,by=list(days=data$days))
  bet<-aggregate(data$betweenness,FUN=mean,by=list(days=data$days))
  rec<-aggregate(data$reciprocity,FUN=mean,by=list(days=data$days))
  clu<-aggregate(data$clusters,FUN=mean,by=list(days=data$days))
  
  data<-data.frame(Metrics);
  data$days<- as.character(strptime(rates$V1,"%Y-%m-%d"))
  deg.r<-aggregate(data$degree,FUN=mean,by=list(days=data$days))
  bet.r<-aggregate(data$betweenness,FUN=mean,by=list(days=data$days))
  rec.r<-aggregate(data$reciprocity,FUN=mean,by=list(days=data$days))
  clu.r<-aggregate(data$clusters,FUN=mean,by=list(days=data$days))
  
  compare.deg<-merge(deg,deg.r,by = "days")
  
  png(filename=paste("Fig7.18-",course_id,"-Public-Anonimous-Degree.png",sep=""), width = 480, height = 280, units = "px", pointsize = 11,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  
  par(mar=c(4,4,0,0))
  labels<-strptime(compare.deg$days,"%Y-%m-%d")
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L', data.frame(compare.deg[c("x.x", "x.y")]),col=c(1), lty=1:3,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Público","Anónimo"), col=c(1), lty=1:4)
  dev.off();
  
  compare.bet<-merge(bet,bet.r,by = "days")
  
  labels<-strptime(compare.bet$days,"%Y-%m-%d")
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L',main="Intermediación",  data.frame(compare.bet[c("x.x", "x.y")]),col=c(1), lty=1:3,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Público","Anónimo"), col=c(1), lty=1:4)
  
  
  compare.rec<-merge(rec,rec.r,by = "days")
  
  labels<-strptime(compare.rec$days,"%Y-%m-%d")
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L',main="Reciprocidad", data.frame(compare.rec[c("x.x", "x.y")]),col=c(1), lty=1:3,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Público","Anónimo"), col=c(1), lty=1:4)
  
  compare.clu<-merge(clu,clu.r,by = "days")
  
  labels<-strptime(compare.clu$days,"%Y-%m-%d")
  par(mar=c(2,2,1,1))
  matplot(as.POSIXct(labels),bty='L',main="Clusters", data.frame(compare.clu[c("x.x", "x.y")]),col=c(1), lty=1:3,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(deads)){  
    t<-deads[d]
    abline(v=as.POSIXct(t),col=1,lty=4)
  }
  legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Público","Anónimo"), col=c(1), lty=1:4)
  
  compareNetworks<-function(rates,social,SNA,NAU,X){
    labels <- as.character(strptime(rates$V1,"%Y-%m-%d"))
    r<- data.frame(t(matrix(t(unlist(SNA$rate)),NAU)))
    r$days<-labels;
    ag<-aggregate(r[[X]],FUN=mean,by=list(days=r$days))
    
    labelss <- as.character(strptime(social$V1,"%Y-%m-%d"))
    rs<- data.frame(t(matrix(t(unlist(SNA$social)),NAU)))
    rs$days<-labelss;
    ags<-aggregate(rs[[X]],FUN=mean,by=list(days=rs$days))
    
    mixed<- merge(ag,ags,by="days")
    #print(var.test(mixed$x.x,mixed$x.y))
    #print(ks.test(mixed$x.x,mixed$x.y))
    
    labels<-strptime(mixed$days,"%Y-%m-%d")
    par(mar=c(2,2,1,1))
    matplot(as.POSIXct(labels),bty='L',main="Grado", data.frame(mixed[c("x.x", "x.y")]),col=c(1), lty=1:3,pch="x", type = "l", xaxt="n",xlab="Time",ylab="Value")
    axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "weeks"),format = "%b%d")
    axis.POSIXct(1, at = seq(labels[1], max(labels)+6, "days"), labels= FALSE, tcl = -0.2)
    for(d in 1:length(deads)){  
      t<-deads[d]
      abline(v=as.POSIXct(t),col=1,lty=4)
    }
    legend("topright",inset=c(0.01,0.01),  horiz=FALSE, legend = cbind("Público","Anónimo"), col=c(1), lty=1:4)
    return (paste(cor.test(mixed$x.x,mixed$x.y)$estimate,cor.test(mixed$x.x,mixed$x.y)$p.value))
  }
  
 for(i in 1:NAU){
  print(paste(course_id,authors$V2[i],compareNetworks(rates,social,SNA,NAU,paste("X",i,sep="")))) 
 }
  
}


