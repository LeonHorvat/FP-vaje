#1
#a (potreben je paket Quandl)
tabela <- Quandl("LBMA/GOLD", collapse="monthly", start_date="2012-12-07") %>%
  subset(select = c(1,6))
tabela <- tabela[-c(1),]

#b
cene <- rev(tabela[,2])
casovna_vrsta <- ts(cene, start = c(2012,12), frequency=12)


graf1 <- ts.plot(casovna_vrsta, xlab='Leto', ylab ='Vrednost v evrih', main = 'Vrednost zlata', lwd = 3)


#2
#a
G <- function(vrsta, k){
  dolzina <- length(vrsta)
  zglajene_vrednosti <- c()
  for (i in 1:(dolzina-k)){
    zglajene_vrednosti[i] <- sum(vrsta[i:(k+i-1)])/k
  }
  zac_leto <- ceiling(2012 + k/12)
  zac_mesec <- (k/12 - floor(k/12)) * 12
  zglajena_vrsta <- ts(zglajene_vrednosti, start = c(zac_leto, zac_mesec), frequency = 12)
  return(zglajena_vrsta)
}

#b
glajenje7 <- G(casovna_vrsta, 7)

napoved <- function(vrsta, k){
  dolzina <- length(vrsta)
  return(sum(vrsta[(dolzina-k+1):dolzina])/k)
}
napoved7 <- napoved(casovna_vrsta, 7)

#c
graf2<- ts.plot(casovna_vrsta, glajenje7, xlab='Leto', ylab ='Vrednost v evrih', main = 'Drseče povprečje reda 7', col=c('black', 'orange'),lwd = 3)
legend('bottomright', 
       legend = c('vrsta', 'glajena7'),
       col = c('black', 'orange'),
       lwd = 1:1,
       bty = 'n')

#d
skn <- function(vrsta, glajena ,k){
  dolzina <- length(vrsta)
  napaka <- 0
  for (i in (k+1):dolzina){
    napaka <- napaka + (vrsta[i] - glajena[i-k])^2
  }
  return(napaka/(dolzina - k))
}

skn7 <- skn(casovna_vrsta, glajenje7, 7)

#e
glajenje14 <- G(casovna_vrsta, 14)
glajenje30 <- G(casovna_vrsta, 30)

skn14 <- skn(casovna_vrsta, glajenje14, 14)
skn30 <- skn(casovna_vrsta, glajenje30, 30)

par(mfrow(c(2,2)))

ts.plot(casovna_vrsta, xlab='Leto', ylab ='Vrednost v evrih', main = 'Vrednost zlata', lwd = 3)

ts.plot(casovna_vrsta, glajenje7, xlab='Leto', ylab ='Vrednost v evrih', main = 'Drseče povprečje reda 7', col=c('black', 'orange'),lwd = 3)
legend('bottomright', 
       legend = c('vrsta', 'glajena7'),
       col = c('black', 'orange'),
       lwd = 1:1,
       bty = 'n')

ts.plot(casovna_vrsta, glajenje14,  xlab='Leto', ylab ='Vrednost v evrih', main = 'Drseče povprečje reda 14', col=c('black', 'green'),lwd = 2)
legend('bottomright', 
       legend = c('vrsta','glajena14'),
       col = c('black','green'),
       lwd = 1:1,
       bty = 'n')

ts.plot(casovna_vrsta, glajenje30,  xlab='Leto', ylab ='Vrednost v evrih', main = 'Drseče povprečje reda 30', col=c('black', 'blue'),lwd = 2)
legend('bottomright', 
       legend = c('vrsta','glajena30'),
       col = c('black','blue'),
       lwd = 1:1,
       bty = 'n')

#3
#a
EG <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  zglajene_vrednosti <- vrsta[1]
  for (i in 2:dolzina){
    zglajene_vrednosti[i] <- alpha*vrsta[i] + (1-alpha)*zglajene_vrednosti[i-1]
  }
  zglajena_vrsta <- ts(zglajene_vrednosti, start = c(2013,1), frequency = 12)
  return(zglajena_vrsta)
}

#b
glajena_e <- EG(casovna_vrsta, 0.5)
naslednji_mesec <- glajena_e[60]

graf5<- ts.plot(casovna_vrsta, glajena_e, xlab='Leto', ylab ='Vrednost v evrih', main = 'Eksponentno glajenje', col=c('black', 'red'),lwd = 2)
legend('bottomright', 
       legend = c('vrsta', 'glajenje'),
       col = c('black', 'red'),
       lwd = 1:1,
       bty = 'n')

#c
skn_e <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, alpha)
  for (i in 1:(dolzina-1)){
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return(napaka/(dolzina-1))
}

opt_alpha <- optimize(skn_e, c(0,1), vrsta = casovna_vrsta)

#d
glajena_e_opt <- EG(casovna_vrsta, opt_alpha$minimum)
naslednji_mesec_opt <- glajena_e_opt[60]

graf6<- ts.plot(casovna_vrsta, glajena_e_opt, xlab='Leto', ylab ='Vrednost v evrih', main = 'Eksponentno glajenje min napaka', col=c('black', 'red'),lwd = 2)
legend('bottomright', 
       legend = c('vrsta', 'glajenje z min napako'),
       col = c('black', 'red'),
       lwd = 1:1,
       bty = 'n')




