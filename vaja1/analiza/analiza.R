#1.(c)

casovna_vrsta1 <- ts(euribor$'6m', start=c(2011,1), frequency=12)
casovna_vrsta2 <- ts(euribor$'9m', start=c(2011,1), frequency=12)

ts.plot(casovna_vrsta1, casovna_vrsta2, xlab='leto', ylab ='obrestna mera v %', main = 'Euribor', col=c('blue','green'), lwd = 2)
legend("topright", legend=c('6m euribor', '9m euribor'), col=c('blue','green'),lty=1:1, cex=0.8)
#2.

#(a)
#Izbrani datumi: 01/08/2011, 02/07/2012, 02/01/2013

#(b)
obresti <- as.data.frame(t(euribor)) %>%
  subset(select=c('02/05/2011', '02/07/2012', '02/01/2013'))
obresti$dospetje <- c(0.25,0.5,1,2,3,6,9,12)
obresti <- melt(obresti, id.vars='dospetje', variable.name = 'datum', value.name = 'obresti')

g2 <- ggplot(obresti, aes(x=dospetje, y=obresti, group=datum, color=datum)) +
  geom_line() + geom_point() +
  labs(title ='Časovna struktura Euribor', y='Obrestna mera [%]', x='Dospetje[mesec]')

#Ob vseh izbranih datumih je oblika grafa normalno, 
#torej, da je ob daljšem dospetju obrestna mera večja.
#Prva in druga krivulja sta pri krajših dospetjih konveksni,
#za srednje ročnosti pa je druga krivulja konkavna. 
#Tretja krivulja je ravna.

#3.
#(a)

term_obr_mere <- subset(euribor, select=c('6m','9m'))
term_obr_mere$'Napoved' <- 1/(9-6)*((1+9*term_obr_mere$`9m`)/(1+6*term_obr_mere$`6m`)-1)
term_obr_mere$'Napoved' <- c(c(NA, NA, NA, NA, NA, NA), term_obr_mere$'Napoved'[-c(31:36)])

#(b)

term_obr_mere$'3m' <- euribor$`3m`

#(c)

term_obr_mere$leto <- as.vector(cbind(seq(2011, 2011, length.out = 12), seq(2012, 2012, length.out = 12), seq(2013, 2013, length.out = 12))) %>%
  as.factor()

term_obr_mere1 <- term_obr_mere[-c(1:5),]
g3 <- ggplot(term_obr_mere1, aes(x=Napoved, y=term_obr_mere1$'3m')) + 
  geom_point(aes(colour = leto)) +
  geom_abline() + 
  geom_smooth(method ="lm") +
  coord_cartesian(xlim=c(0.1,1.7),ylim=c(0.1,1.7)) + 
  labs(title ='3m Euribor 2011-2013', y='Opazovano')

#(d)

term_obr_mere2 <- term_obr_mere1[c(1:7),]
term_obr_mere3 <- term_obr_mere1[c(8:19),]
term_obr_mere4 <- term_obr_mere1[c(20:31),]

g4 <- ggplot(term_obr_mere2, aes(x=Napoved, y=term_obr_mere2$'3m')) + 
  geom_point(aes(colour = leto)) +
  geom_abline() + 
  geom_smooth(method ="lm") +
  coord_cartesian(xlim=c(0.1,1.7),ylim=c(0.1,1.7)) + 
  labs(title ='3m Euribor 2011', y='Opazovano')

g5 <- ggplot(term_obr_mere3, aes(x=Napoved, y=term_obr_mere3$'3m')) + 
  geom_point(aes(colour = leto)) +
  geom_abline() + 
  geom_smooth(method ="lm") +
  coord_cartesian(xlim=c(0.1,1.4),ylim=c(0.1,1.4)) + 
  labs(title ='3m Euribor 2012', y='Opazovano')


g6 <- ggplot(term_obr_mere4, aes(x=Napoved, y=term_obr_mere4$'3m')) + 
  geom_point(aes(colour = leto)) +
  geom_abline() + 
  geom_smooth(method ="lm") +
  coord_cartesian(xlim=c(0.19,0.25),ylim=c(0.19,0.25)) + 
  labs(title ='3m Euribor 2012', y='Opazovano')

#(e)

#Da bi hipoteza pričakovanj trga veljala, 
#bi morali biti napovedana in opazovana obrestna mera
#enaki; torej bi točke ležale na simetrali lihih kvadrantov
#oziroma vsaj v okolici. V mojem primeru empirični podatki ne potrjujejo hipoteze,
#še najbljižje hipotezi so v letu 2013.



