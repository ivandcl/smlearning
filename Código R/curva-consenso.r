######################
#
# Curva de teórica para el indicador de consenso
#
# Iván D. Claros - ivan.claros@uam.es
# Universidad Autónoma de Madrid - 2014
#
######################
png(filename=paste("Fig6.1-Curva teorica del consenso.png",sep=""), width = 480, height = 280, units = "px", pointsize = 11,    bg = "white", res = NA, family = "", restoreConsole = TRUE,    type = c("windows", "cairo", "cairo-png"))  

t = 1+seq(0.1,6,length=70)
f = 5+100*exp(-t)
par(oma=c(0,0,0,0))
par(omi=c(0,0,0,0))
par(mar=c(2,2,0.1,0.1))
plot(t,f,lty=1, type = 'l', xlim =c(0,8),ylim = c(0,50), xaxt="n",yaxt="n")
mtext("Tiempo", side=1, line=1, cex=1, outer=FALSE)
mtext("Divergencia de opinión", side=2, line=1, cex=1)

text(7,6.8,"Diversidad")
abline(v = 2,lty=3)
text(2.3,40,"t0")
abline(v = 5.42,lty=3)
text(5.7,40,"t1")
text(4,16,"m = - Consenso")
r<- lm(f~t)
abline(r, lty=2)
text(1,40,"Curva Teórica")
dev.off();
