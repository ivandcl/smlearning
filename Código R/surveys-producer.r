######################
#
# Anñalisis de encuesta productor
# Data source: Encuesta de Productor, Apéndice A.
# 
# Iván D. Claros - ivan.claros@uam.es
# Universidad Autónoma de Madrid - 2015
#
######################

library(psych)
library(igraph)
library(corrgram)
library(lattice)

PATH = "../figures/";
PTSIZE = 11;
survey.raw = read.table("../datasets/survey-producer.csv",TRUE,";")
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

ALL.FACTORS = c("Q1.GLOBAL", unlist(METHOD), unlist(ENVIRONMENT), unlist(RESULTS));

#Normalizacion de los datos
survey<-survey.raw[ALL.FACTORS]/10

# 0. Global
boxplot.stats(na.omit(survey$Q1.GLOBAL))

# 1. Método

# 1.1 Global
boxplot.stats(na.omit(survey$Q2.METHOD.GLOBAL))

# 1.2 Planificación
alpha(na.omit(data.frame(survey[METHOD$PLANIFICATION])))
boxplot(na.omit(data.frame(survey[METHOD$PLANIFICATION])),names = c("Coherencia con el objetivo","Presencia docente","Plan de entregas"))
boxplot.stats(rowMeans(na.omit(data.frame(survey[METHOD$PLANIFICATION]))))

ss<-cbind();
for(v in ALL.FACTORS){
  s<-boxplot.stats(na.omit(survey[,v]))  #Resumen de la variable
  ss$Variable<-c(ss$Variable,v)
  ss$Media<-c(ss$Media,s$stats[3])
  ss$IC.LOW<-c(ss$IC.LOW,s$conf[1]) #Invervalo de confianza al 95%
  ss$IC.HIGHT<-c(ss$IC.HIGHT,s$conf[2])    
}
write.csv(data.frame(ss),"../datasets/survey-producer-summary.csv");

Fig7.3=list(
  Método=rowMeans(na.omit(survey[unlist(METHOD)])),
  Entorno=rowMeans(na.omit(survey[unlist(ENVIRONMENT)])),
  Resultados=rowMeans(na.omit(survey[unlist(RESULTS)]))
  );

png(filename=paste(PATH, "Fig7.3-Satisfacción-MER.png",sep=""), width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "transparent", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
par(mar=c(2,2,1,1))
boxplot(Fig7.3)
dev.off();

Fig7.4=list(
  Planificación=rowMeans(na.omit(survey[unlist(METHOD$PLANIFICATION)])),
  Esfuerzo=rowMeans(na.omit(survey[unlist(METHOD$EFFORT)])),
  Fundamentos=rowMeans(na.omit(survey[unlist(METHOD$FUNDAMENTS)])),
  Iteración=rowMeans(na.omit(survey[unlist(METHOD$ITERATION)])),
  Motivación=rowMeans(na.omit(survey[unlist(METHOD$MOTIVATION)]))
);
png(filename=paste(PATH, "Fig7.4-Factores-Método.png",sep=""), width = 480, height = 280, units = "px", pointsize = PTSIZE,    bg = "transparent", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))
par(mar=c(2,2,1,1))
boxplot(Fig7.4)
dev.off();


alpha(na.omit(data.frame(survey[METHOD$FUNDAMENTS])))
#Boxplot
boxplot(na.omit(data.frame(survey[METHOD$FUNDAMENTS])))
#Agregado
boxplot.stats(rowMeans(na.omit(data.frame(survey[METHOD$FUNDAMENTS]))))

alpha(na.omit(data.frame(survey[METHOD$EFFORT])))
#Boxplot
boxplot(na.omit(data.frame(survey[METHOD$EFFORT])))
#Agregado
boxplot.stats(rowMeans(na.omit(data.frame(survey[METHOD$EFFORT]))))

boxplot.stats(na.omit(survey$Q15.METHOD.ITERATION))
boxplot.stats(na.omit(survey$Q16.METHOD.MOTIVATION))

# Visión General
boxplot.stats(rowMeans(na.omit(survey[unlist(METHOD)])))
alpha(na.omit(survey[unlist(METHOD)]))
boxplot(na.omit(survey[unlist(METHOD)]))

boxplot.stats(na.omit(survey$Q20.ENVIRONMENT.GLOBAL))
boxplot.stats(na.omit(survey$Q21.ENVIRONMENT.SUPPORT.METHOD))
alpha(na.omit(data.frame(survey[ENVIRONMENT$USABILITY])))

#Boxplot
boxplot(na.omit(data.frame(survey[ENVIRONMENT$USABILITY])))
#Agregado
boxplot.stats(rowMeans(na.omit(data.frame(survey[ENVIRONMENT$USABILITY]))))

alpha(na.omit(data.frame(survey[ENVIRONMENT$UTILITY])))

#Boxplot
boxplot(na.omit(data.frame(survey[ENVIRONMENT$UTILITY])))
#Agregado
boxplot.stats(rowMeans(na.omit(data.frame(survey[ENVIRONMENT$UTILITY]))))
boxplot(rowMeans(na.omit(data.frame(survey[ENVIRONMENT$UTILITY]))))

boxplot(rowMeans(na.omit(data.frame(survey[c("Q36.ENVIRONMENT.UTILITY.COMMENTS","Q38.ENVIRONMENT.UTILITY.VOTE.TAGS")]))))
boxplot.stats(rowMeans(na.omit(data.frame(survey[c("Q36.ENVIRONMENT.UTILITY.COMMENTS","Q38.ENVIRONMENT.UTILITY.VOTE.TAGS")]))))
(splitHalf(na.omit(data.frame(survey[c("Q36.ENVIRONMENT.UTILITY.COMMENTS","Q38.ENVIRONMENT.UTILITY.VOTE.TAGS")]))))

# Visión General
boxplot.stats(rowMeans(na.omit(survey[unlist(ENVIRONMENT)])))
alpha(na.omit(survey[unlist(ENVIRONMENT)]))
boxplot(na.omit(survey[unlist(ENVIRONMENT)]))

boxplot.stats(na.omit(survey$Q41.RESULTS.LEARNING.GLOBAL))
alpha(na.omit(data.frame(survey[RESULTS$LEARNING])))

#Boxplot
boxplot(na.omit(data.frame(survey[RESULTS$LEARNING])))
#Agregado
boxplot.stats(rowMeans(na.omit(data.frame(survey[RESULTS$LEARNING]))))

boxplot.stats(na.omit(survey$Q44.RESULTS.OUTCOME))

# Visión General
boxplot.stats(rowMeans(na.omit(survey[unlist(RESULTS)])))
alpha(na.omit(survey[unlist(RESULTS)]))
boxplot(na.omit(survey[unlist(RESULTS)]))



