#1.(c)

casovna_vrsta1 <- ts(euribor[,6], start=c(2011,1), frequency=12)
casovna_vrsta2 <- ts(euribor[,7], start=c(2011,1), frequency=12)

ts.plot(casovna_vrsta1, casovna_vrsta2, xlab='leto', ylab ='obrestna mera', main = '6-mesečna in 9-mesečna obrestna', col=c('blue','green'), lwd = 2)
legend("topright", legend=c('6m euribor', '9m euribor'), col=c('blue','green'),lty=1:1, cex=0.8)

#2.

#3.
#(a)

term_obr_mere <- subset(euribor, select=c('6m','9m'))
class(term_obr_mere) <- 'numeric'
class(term_obr_mere) <- 'data.frame'
term_obr_mere$terminska <- NULL 