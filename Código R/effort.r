######################
# 
# Analysis of Experiences Supported by SMLearning System 
# Data source: https://dimo1.ii.uam.es/v01/analytics/logs/[idg=136,178,186] 
# 
# Iván D. Claros - ivan.claros@uam.es
# Universidad Autónoma de Madrid - 2014
#
######################
library(corrgram)
#library(sjPlot)
library(igraph)
#library(plyr)
PTSIZE=11
setwd("~/Dropbox/Thesis/Anexos/Datasets")
cat("\014"); #Clear screen

load.log<-function(filename){
  logs = read.table(filename,TRUE,"\t");
  logs$Date<-strptime(logs$Date,"%Y-%m-%d %H:%M:%S");
  logs$Day<-format(logs$Date,"%Y-%m-%d");  
  return (logs);
};

logs.136 = load.log("logs-136.txt");
hitos.136 = read.table("hitos-136.txt");
hitos.136$Day<-strptime(hitos.136$V1,"%Y-%m-%d");

logs.178 = load.log("logs-178.txt");
hitos.178 = read.table("hitos-178.txt");
hitos.178$Day<-strptime(hitos.178$V1,"%Y-%m-%d");

logs.186 = load.log("logs-186.txt");
hitos.186 = read.table("hitos-186.txt");
hitos.186$Day<-strptime(hitos.186$V1,"%Y-%m-%d");

#Grupo de acciones a analizar

actions.types<-intersect(unique(logs.136$Event),intersect(unique(logs.178$Event),unique(logs.186$Event)))





plot.effort<-function(logs,hitos,actions.types,Experience){
  logs.filtered<-logs[logs$Event %in% actions.types,]
  
  #grafico de esferuzo tiempo/acciones
  time.days<-aggregate(Time~Day,data = logs.filtered, FUN=sum);
  action.days<-aggregate(Time~Day,data = logs.filtered, FUN=length)
  mix<-merge(time.days,action.days,by = "Day")
  mixr<-mix[mix$Time.y>0,]
  mixr$Time.x<-mixr$Time.x/max(mixr$Time.x)
  mixr$Time.y<-mixr$Time.y/max(mixr$Time.y)
  
  png(filename=paste("Fig7.8-",Experience,"-Esfuerzo-TiempoAccion.png",sep=""), width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
  
  par(mar=c(2,2,1,1))
  mixr$Day<-strptime(mixr$Day,"%Y-%m-%d");
  matplot(as.POSIXct (mixr$Day),mixr[c(2,3)],  xaxt = "n", type="l", ylab="Tiempo", xlab="Tiempo", lty = c(1,2), col=1)  
  axis.POSIXct(1, at = seq(mixr$Day[1], max(mixr$Day)+6, "weeks"),format = "%b%d")
  axis.POSIXct(1, at = seq(mixr$Day[1], max(mixr$Day)+6, "days"), labels= FALSE, tcl = -0.2)
  for(d in 1:length(hitos$Day)){  
    t<-hitos$Day[d]  
    text(as.POSIXct(t),0,hitos$V2[d],pos=2,col="black",cex = .8, offset = 0)
    abline(v=as.POSIXct(t),col="black",lty=3)
  }
  ct<-cor.test(mixr$Time.x,mixr$Time.y)  
  leg.time<-paste("Tiempo Sesión, SD: ",format(round(sd(mixr$Time.x),3),nsmall = 3,scientific = FALSE))
  leg.actions<-paste("Acciones SD: ",format(round(sd(mixr$Time.y),3),nsmall = 3,scientific = FALSE))
  leg.corr<-paste("Correlación: ",format(round(ct$estimate,3),nsmall = 3,scientific = FALSE))
  legend(as.POSIXct(min(mixr$Day)),1,horiz=FALSE,legend=c(leg.time,leg.actions), lty = c(1,2,0),merge = FALSE)
  text(x=as.POSIXct(min(mixr$Day)),y=0.7,labels = leg.corr,pos = 4)
  dev.off();
}

plot.effort(logs.136,hitos.136, actions.types,"P1")
plot.effort(logs.178,hitos.178,actions.types,"P2")
plot.effort(logs.186,hitos.186,actions.types,"P3")

effort.calc<-function(students,logs,e,actions.types){
  for(student in students){
    logs.student = logs[logs$Author %in% c(student),]
    
    #Guarda el id el estudiante
    e$student<-c(e$student,student);
    
    #Tiempo por sesion    
    time.sessions = aggregate(Time~Session,data = logs.student, FUN=sum);  
    #e$time.sessions.mean=c(e$time.sessions.mean,mean(time.sessions$Time));
    #e$time.sessions.cv=c(e$time.sessions.cv,sd(time.sessions$Time)/mean(time.sessions$Time));    
    #e$time.sessions.var=c(e$time.sessions.var,var(time.sessions$Time));    
    
    #tiempo por dia
    time.days<-aggregate(Time~Day,data = logs.student, FUN=sum);
    e$s.time=c(e$s.time,sum(time.days$Time));
    #e$time.days.cv=c(e$time.days.cv,sd(time.days$Time)/mean(time.days$Time));    
    e$v.time=c(e$v.time,var(time.days$Time));    
    
    #acciones por sesion
    #action.sessions = aggregate(Time~Session,data = logs.student, FUN=length);
    #e$action.sessions.mean=c(e$action.sessions.mean,mean(action.sessions$Time));    
    #e$action.sessions.cv=c(e$action.sessions.cv,sd(action.sessions$Time)/mean(action.sessions$Time));    
    #e$action.sessions.var=c(e$action.sessions.var,var(action.sessions$Time));    
        
    #acciones por dia
    action.days<-aggregate(Time~Day,data = logs.student, FUN=length)
    #e$action.days.mean=c(e$action.days.mean,mean(action.days$Time));    
    #e$action.days.cv=c(e$action.days.cv,sd(action.days$Time)/mean(action.days$Time));    
    e$v.action=c(e$v.action,var(action.days$Time));    
    
    #Eficiencia
    #e$e.sessions = c(e$efficient.sessions,cor(time.sessions$Time,action.sessions$Time));
    e$e.days = c(e$e.days,cor(time.days$Time,action.days$Time));
    
    #Sesiones por día
    sessions<-table(logs.student$Day)    
    e$l.sessions<-c(e$l.sessions, length(sessions));
    e$s.sessions<-c(e$s.sessions, sum(sessions));
    #e$sessions.mean<-c(e$sessions.mean, mean(sessions));
    #e$sessions.cv<-c(e$sessions.cv, sd(sessions)/mean(sessions));  
    e$v.sessions<-c(e$v.sessions, var(sessions));  
    
    #Por tipos de acciones (Distribucion temporal por dias)   
    for(n in actions.types){
      key.length<-paste("l",n,sep="."); #Total de acciones
      #key.mean<-paste("m.",n,"action.mean",sep=".");     #Promedio
      #key.cv<-paste("",n,"action.cv",sep=".");         #Distribucion temporal
      #key.sd<-paste(n,"action.sd",sep=".");         #Distribucion temporal
      key.var<-paste("v",n,sep=".");         #Distribucion temporal
      
      logs.student.action<-logs.student[logs.student$Event %in% c(n),]
      if(nrow(logs.student.action)==0){
        e[[key.length]]<-c(unlist(e[[key.length]]),NA);          
        #e[[key.mean]]<-c(unlist(e[[key.mean]]),NA);          
        #e[[key.cv]]<-c(unlist(e[[key.cv]]),NA);          
        #e[[key.sd]]<-c(unlist(e[[key.cv]]),NA);          
        e[[key.var]]<-c(unlist(e[[key.cv]]),NA);          
      }else{
        #acciones por sesion
        action.sessions = aggregate(Time~Day,data = logs.student.action, FUN=length);
        e[[key.length]]<-c(unlist(e[[key.length]]),length(action.sessions$Time));
        #e[[key.mean]]<-c(unlist(e[[key.mean]]),mean(action.sessions$Time));    
        #e[[key.cv]]<-c(unlist(e[[key.cv]]),sd(action.sessions$Time)/mean(action.sessions$Time));    
        #e[[key.sd]]<-c(unlist(e[[key.sd]]),sd(action.sessions$Time));    
        e[[key.var]]<-c(unlist(e[[key.var]]),var(action.sessions$Time));    
      }      
    }
  }
  return (e);
}
#correlaciones entre tiempo y acciones
actions.types=c("vResource","aResource","rResource","aAnnotation","eComposition");

e = rbind();

#Filtrar solo estudiantes 136
students = c(30,31,32,33,34,35,1);
e<-effort.calc(students,logs.136,e,actions.types)

#Filtrar solo estudiantes 178
students = c(234,238,241,235,231,233,232,1);
e<-effort.calc(students,logs.178,e,actions.types)

#Filtrar solo estudiantes 186
students = c(266,264,265,261,269,271,268,267,262,1);
e<-effort.calc(students,logs.186,e,actions.types)

e.data<-data.frame(e)
#corrgram(e.data)
# Estudiantes que respondieron la encuesta de satisfacción


survey.raw = read.table("~/Dropbox/Thesis/Anexos/Datasets/survey-producer.csv",TRUE,";")
scores = read.table("~/Dropbox/Thesis/Anexos/Datasets/scores.csv",TRUE,";")
survey.raw<-merge(survey.raw,scores,by = "UID")

GLOBAL = "Q1.GLOBAL";
METHOD=list();
METHOD$GLOBAL<-"Q2.METHOD.GLOBAL";
METHOD$PLANIFICATION<-c(
  "Q3.METHOD.PLANIFICATION.COHERENCE",
  "Q4.METHOD.PLANIFICATION.TEACHER.PRESENCE",
  "Q5.METHOD.PLANIFICATION.DEADLINES"
);
METHOD$FUNDAMENTS<-c(
  "Q6.METHOD.FUNDAMENTS.VIDEO",
  "Q7.METHOD.FUNDAMENTS.DESIGN.GMA",
  "Q8.METHOD.FUNDAMENTS.SKILLS",
  "Q9.METHOD.FUNDAMENTS.GMA.AS.LEARNING.OBJECT"
);
METHOD$EFFORT<-c(
  "Q10.METHOD.EFFORT.VS.LEARNING",
  "Q11.METHOD.EFFORT.INDEXATION",
  "Q12.METHOD.EFFORT.SOCIAL.COMMENT",
  "Q13.METHOD.EFFORT.SOCIAL.VOTE",
  "Q14.METHOD.EFFORT.AUTHORING"
);
METHOD$ITERATION<-"Q15.METHOD.ITERATION";
METHOD$MOTIVATION<-"Q16.METHOD.MOTIVATION";

ENVIRONMENT=list();
ENVIRONMENT$GLOBAL = "Q20.ENVIRONMENT.GLOBAL";
ENVIRONMENT$COHERENCE.METHOD = "Q21.ENVIRONMENT.SUPPORT.METHOD";
ENVIRONMENT$USABILITY = c(
  "Q26.ENVIRONMENT.USABILITY.COHERENCE",  
  "Q28.ENVIRONMENT.USABILITY.PRESENTATION",
  "Q28.ENVIRONMENT.USABILITY.PRESENTATION",
  "Q29.ENVIRONMENT.USABILITY.FRIENDLY",
  "Q30.ENVIRONMENT.USABILITY.NAVEGATION",
  "Q31.ENVIRONMENT.USABILITY.OVERLOAD",
  "Q32.ENVIRONMENT.USABILITY.SEARCH",
  #"Q33.ENVIRONMENT.USABILITY.ACCESIBILIY", (Respuesta Abierta)
  "Q34.ENVIRONMENT.USABILITY.PERFORMANCE"
); 
ENVIRONMENT$UTILITY = c(
  "Q27.ENVIRONMENT.USABILITY.AWARENESS",
  "Q36.ENVIRONMENT.UTILITY.COMMENTS", 
  "Q37.ENVIRONMENT.UTILITY.MULTIMEDIA",
  "Q38.ENVIRONMENT.UTILITY.VOTE.TAGS",
  "Q39.ENVIRONMENT.UTILITY.AUTHORING",
  "Q40.ENVIRONMENT.UTILITY.INTERACTIVITY" 
);

RESULTS=list();
RESULTS$GLOBAL = "Q41.RESULTS.LEARNING.GLOBAL";
RESULTS$LEARNING=c(
  "Q42.RESULTS.LEARNING.ANALYSIS",
  "Q43.RESULTS.LEARNING.SYNTESIS",
  "Q44.RESULTS.LEARNING.COMPOSITION",
  "Q45.RESULTS.LEARNING.CONSUME"
);
RESULTS$OUTCOME="Q44.RESULTS.OUTCOME";

ALL.FACTORS = c("Q1.GLOBAL","Interation","Final", unlist(METHOD), unlist(ENVIRONMENT), unlist(RESULTS));
#Normalizacion de los datos
survey<-survey.raw[ALL.FACTORS]/10
survey$student<-survey.raw$UID

metrics.names<-names(e.data)
data.joined<-merge(survey,e.data,by = "student")

names(survey)
results = rbind();
ns<-names(data.joined);
for(item in ALL.FACTORS){
  for(r in metrics.names[-1]){    
    c<-cor.test(data.joined[,item],data.joined[,r]);
    if(c$p.value<0.05 && abs(c$estimate)>0.5){  
      results$Factor<-c(results$Factor,item)
      results$Metric<-c(results$Metric,r)      
      results$Correlation<-c(results$Correlation,c$estimate)      
    }    
  }   
}

results<-data.frame(results)

fs<-unique(results$Factor)
n.max<-length(fs)

ms<-unique(results$Metric)
mns<-list();
id=0;
for(r in ms){ 
  id=1+id;
  mns[r]<-paste("",n.max+id,sep="")
}

fns<-list();
id=0;
for(fn in fs){
  id=1+id;
  fns[fn]<-paste("",id,sep="")
}

n=1:length(fs)
adjlist=list();
for(fn in fs){   
  els<-results[which(results$Factor==fn),]
  chid<-c();
  for(el in els$Metric){
    chid<-c(chid,unlist(mns[el]))
  }
  adjlist<-append(adjlist, list(chid))
}

g<-graph.adjlist(adjlist,mode="in")

labels <- c();
for(label in c(as.character(fs),as.character(ms))){
  labels<-c(labels,substring(label,1,5))
}
V(g)$label<-labels
#comps <- clusters(g)$membership
#colbar <- rainbow(max(comps)+1)
V(g)[n]$color <-3
V(g)[n.max+(1:length(fs))]$color <-5
V(g)[n]$shape = "rectangle";
V(g)[n.max+(1:length(fs))]$shape <-"circle"
#V(g)$shape
E(g)$weight<-results$Correlation
V(g)$degree=10+degree(g,mode = "out");

#plot(1:10,col=1:10)
#E(g)$label=format(round(results$Correlation,2),nsmall = 2,scientific = FALSE)

V(g)[order(degree(g),decreasing=T)]$label


E(g)[results$Correlation>0.5]$color<-3
E(g)[results$Correlation<0.5*-1]$color<-2
tkplot(g,vertex.size=V(g)$degree, vertex.shape=V(g)$shape)
write.graph(g,file = "actions-satisfaction.gml",format = "gml")
#plot.igraph(g,layout=layout.circle, vertex.size=V(g)$degree,vertex.label.dist=0.1,edge.width=E(g)$weight)
#plot.igraph(g,vertex.size=V(g)$degree,edge.width=E(g)$weight)
#tkplot(g,vertex.size=V(g)$degree, vertex.shape=V(g)$shape)
#plot(g,vertex.size=V(g)$degree)


all.d<-count(results,c("Metric"))
all.s<-all.d[order(all.d$freq,decreasing = T),]
write.csv2(all.s,"interaction-satisfaction-all.csv");

pos<-count(results[results["Correlation"]>0.5,],c("Metric"))
write.csv2(pos,"interaction-satisfaction-pos.csv");

neg<-count(results[results["Correlation"]<0.5*-1,],c("Metric"))
neg.s<-neg[order(neg$Metric,decreasing = T),]
write.csv2(neg.s,"interaction-satisfaction-neg.csv");

write.csv2(results,"interaction-satisfaction-list.csv");



aMetrics<-function(item,data.joined,factors){
  corInt<-rbind();
  
  for(r in factors){   
    c<-cor.test(data.joined[,item],data.joined[,r]);
    if(c$p.value<0.05 && abs(c$estimate)>0){  
      corInt$Factor<-c(corInt$Factor,item)
      corInt$Metric<-c(corInt$Metric,r)      
      corInt$Correlation<-c(corInt$Correlation,c$estimate)      
    }    
  }   
  
  fs<-unique(corInt$Factor)
  n.max<-length(fs)
  
  ms<-unique(corInt$Metric)
  mns<-list();
  id=0;
  for(r in ms){ 
    id=1+id;
    mns[r]<-paste("",n.max+id,sep="")
  }
  
  fns<-list();
  id=0;
  for(fn in fs){
    id=1+id;
    fns[fn]<-paste("",id,sep="")
  }
  
  n=1:length(fs)
  adjlist=list();
  for(fn in fs){   
    els<-corInt[which(corInt$Factor==fn)]
    chid<-c();
    for(el in els$Metric){
      chid<-c(chid,unlist(mns[el]))
    }
    adjlist<-append(adjlist, list(chid))
  }
  
  g<-graph.adjlist(adjlist,mode = "in")
  labels <- c();
  for(label in c(as.character(fs),as.character(ms))){
    labels<-c(labels,substring(label,1,5))
  }
  V(g)$label<-labels
  E(g)[corInt$Correlation>0.5]$color<-3
  E(g)[corInt$Correlation<0.5*-1]$color<-2
  tkplot(g)
  return (corInt);
}

print("Score interaciton vs Global");
print(cor.test(survey$Interation,survey$Q1.GLOBAL))
item<-"Interation"
factors<-c(metrics.names[-1],ALL.FACTORS);
aMetrics("Interation",data.joined,factors)

print("Score Final vs Global");
print(cor.test(survey$Final,survey$Q1.GLOBAL))
aMetrics("Final",data.joined,factors)
