casovna_vrsta1 <- ts(euribor[,6], start=c(2011,1), frequency=12)
casovna_vrsta2 <- ts(euribor[,7], start=c(2011,1), frequency=12)

ts.plot(casovna_vrsta1, casovna_vrsta2, xlab='leto', ylab ='obrestna mera', main = '6-mesečna in 9-mesečna obrestna', col=c('blue','green'), lwd = 3)
legend()