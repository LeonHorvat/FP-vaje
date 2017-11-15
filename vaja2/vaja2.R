#1
#a
vzorec <- read.table('vaja2/vzorec5.txt')
histogram <- hist(vzorec$V1, 
                  breaks = 20, 
                  main = 'Histogram odškodnin',
                  xlab = 'Višina odškodnine',
                  ylab = 'Frekvenca',
                  probability = TRUE)

#b
parametri <- mde(vzorec$V1, 
              pweibull,
              start = list(shape = 1, scale = 1),
              measure = 'CvM')

#c
k <- as.numeric(parametri$estimate[1])
lambda <- as.numeric(parametri$estimate[2])
curve(dweibull(x, shape=k, scale = lambda),
               from = 0, 
               to = 2, 
               add = TRUE)

plot(ecdf(vzorec$V1),
     main = 'Porazdelitvena funkcija odškodnin',
     ylab = 'porazdelitvena funkcija',
     xlab = 'višina odškodnin'
     )
curve(pweibull(x, shape=k, scale = lambda),
      from = 0, 
      to = 2, 
      add = TRUE,
      col = 'red',
      lwd = 2)
legend("bottomright", legend=c('empirična porazdelitev', 'Weibullova porazdelitev'), col=c('black','red'),lty=1:1, cex=0.8)
      
#d
upanje_y <- lambda * gamma(1 + 1/k)
upanje_S <- 15 * upanje_y
varianca_y <- lambda^2 * (gamma(1 + 2/k) - gamma(1 + 1/k)^2)
varianca_s <- varianca_y * 15 + upanje_y^2 * 15

#2
#a
diskretno <- discretize(pweibull(x, shape=k, scale = lambda),
                       step = 0.1,
                       from = 0,
                       to = 3,
                       method = 'rounding')
             

#b
g1 <- curve(pweibull(x, shape=k, scale = lambda),
            from = 0, 
            to = 3,
            main = 'Weibullova porazdelitev',
            ylab = 'porazdelitvena funkcija',
            lwd = 2)
f <- stepfun(seq(0,2.9,0.1), diffinv(diskretno))
plot(f, add = TRUE, col = 'orange')
legend("bottomright", legend=c('diskretizacija', 'Weibullova porazdelitev'), col=c('orange','black'),lty=1:1, cex=0.8)

#c
Fs <- aggregateDist(method = 'recursive',
              model.freq = 'poisson',
              model.sev = diskretno,
              lambda = 15,
              x.scale = 0.1,
              maxit = 1000000,
              tol = 0.01)
plot(Fs)





