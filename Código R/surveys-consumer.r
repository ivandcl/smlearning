######################
#
# Analysis of Experiences Supported by SMLearning System - 
# Data source: Encuesta de Consumidor, Apendice B. 
# 
# Iván D. Claros - ivan.claros@uam.es
# Universidad Autónoma de Madrid - 2014
#
######################
survey.raw = read.table("../datasets/survey-consumer.csv",TRUE,";")

#1. Motivación
#1.1 Atención
#1.1.1 Multimedia
MOTIVATION=list();
MOTIVATION$ATTENTION=c("Q1.MOTIVATION.ATTENTION.MULTIMEDIA","Q2.MOTIVATION.ATTENTION.INTERACTIVITY");
MOTIVATION$CONFIDENCE=c("Q3.MOTIVATION.CONFIDENCE.MULTIMEDIA","Q4.MOTIVATION.CONFIDENCE.INTERACTIVITY");
MOTIVATION$RELEVANCE=c("Q5.MOTIVATION.RELEVANCE","Q6.MOTIVATION.RELEVANCE.CURIOSITY","Q7.MOTIVATION.RELEVANCE.REFLEXION")
MOTIVATION$SATISFACTION=c("Q8.MOTIVATION.SATISFACTION.MULTIMEDIA","Q9.MOTIVATION.SATISFACTION.INTERACTIVITY")

FORMAT=list();
FORMAT$COMPLETE="Q10.FORMAT.COMPLETE";
FORMAT$DIDACTIC="Q11.FORMAT.DIDACTIC";
FORMAT$EFFECTIVE="Q12.FORMAT.EFFECTIVE";

INTERACTIVITY=list();
INTERACTIVITY$OVERLOAD=c("Q13.INTERACTIVITY.OVERLOAD.LOW","Q14.INTERACTIVITY.OVERLOAD.HIGH");
INTERACTIVITY$SOCIAL=c("Q15.INTERACTIVITY.SOCIAL.GLOBAL","Q16.INTERACTIVITY.SOCIAL.PUBLIC","Q17.INTERACTIVITY.SOCIAL.ANONIMOUS");

SOCIALMEDIA=list();
SOCIALMEDIA$PRIVACITY=c("Q18.SOCIALMEDIA.PRIVACITY");
SOCIALMEDIA$ENTRECATION=c("Q19.SOCIALMEDIA.FACEBOOK");
SOCIALMEDIA$EDUTAINMENT=c("Q20.SOCIALMEDIA.LMS");

ALL.FACTORS = c(unlist(MOTIVATION),unlist(FORMAT),unlist(INTERACTIVITY),unlist(SOCIALMEDIA))

survey<-(survey.raw[ALL.FACTORS])/4

alpha(na.omit(data.frame(survey[ALL.FACTORS])))
boxplot.stats(rowMeans(na.omit(data.frame(survey[ALL.FACTORS]))))

alpha(na.omit(data.frame(survey[unlist(MOTIVATION)])))
alpha(na.omit(data.frame(survey[unlist(MOTIVATION$ATTENTION)])))
alpha(na.omit(data.frame(survey[unlist(MOTIVATION$CONFIDENCE)])))
alpha(na.omit(data.frame(survey[unlist(MOTIVATION$RELEVANCE)])))
alpha(na.omit(data.frame(survey[unlist(MOTIVATION$SATISFACTION)])))

alpha(na.omit(data.frame(survey[unlist(INTERACTIVITY$OVERLOAD)])))
boxplot(na.omit(data.frame(survey[unlist(INTERACTIVITY$OVERLOAD)])))
cor.test(survey$Q13.INTERACTIVITY.OVERLOAD.LOW,survey$Q14.INTERACTIVITY.OVERLOAD.HIGH)
boxplot.stats(survey$Q13.INTERACTIVITY.OVERLOAD.LOW)
boxplot.stats(survey$Q14.INTERACTIVITY.OVERLOAD.HIGH)
plot(jitter(survey$Q13.INTERACTIVITY.OVERLOAD.LOW),jitter(survey$Q14.INTERACTIVITY.OVERLOAD.HIGH))
alpha(na.omit(data.frame(survey[unlist(INTERACTIVITY$SOCIAL)])))

cor.test(survey$Q17.INTERACTIVITY.SOCIAL.ANONIMOUS,survey$Q16.INTERACTIVITY.SOCIAL.PUBLIC)
t.test(survey$Q17.INTERACTIVITY.SOCIAL.ANONIMOUS,survey$Q16.INTERACTIVITY.SOCIAL.PUBLIC)
boxplot(cbind(mean(survey$Q17.INTERACTIVITY.SOCIAL.ANONIMOUS),mean(survey$Q16.INTERACTIVITY.SOCIAL.PUBLIC)))
plot(jitter(survey$Q17.INTERACTIVITY.SOCIAL.ANONIMOUS),jitter(survey$Q16.INTERACTIVITY.SOCIAL.PUBLIC))

alpha(na.omit(data.frame(survey[unlist(SOCIALMEDIA)])))
alpha(na.omit(data.frame(survey[unlist(FORMAT)])))
alpha(na.omit(data.frame(survey[unlist(INTERACTIVITY)])))
boxplot.stats(rowMeans(na.omit(data.frame(survey[unlist(INTERACTIVITY)]))))

ss<-cbind();
for(v in ALL.FACTORS){
  s<-boxplot.stats(na.omit(survey[,v]))  #Resumen de la variable
  ss$Variable<-c(ss$Variable,v)
  ss$Media<-c(ss$Media,s$stats[3])
  ss$IC.LOW<-c(ss$IC.LOW,s$conf[1])
  ss$IC.HIGHT<-c(ss$IC.HIGHT,s$conf[2])    
}
write.csv2(data.frame(ss),"../datasets/survey-consumer-summary.csv");

