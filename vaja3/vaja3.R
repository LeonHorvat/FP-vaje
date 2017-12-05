S0 <- 50
u <- 1.05
d <- 0.95
U <- 5
T <- 3
R <- 0.03

#1
#a


#b
izplacilo <- function(vrsta, T, type){
  if (type == 'call'){
    m <- - max(vrsta[1:T]) + max(vrsta[(T+1):length(vrsta)])
  } else if (type == 'put'){
    m <- - min(vrsta[1:T]) + min(vrsta[(T+1):length(vrsta)])
  }
  if (m > 0){
    return(m)
  } else {
    return(0)
  }
}

#2
#a
binomski <- function(S0, u, d, U, R, T, type){
  q <- (1+R-d)/(u-d)
  E <- 0
  vrsta <- c(S0)
  kocka <- hcube(c(rep(2,U)), translation = -1)
  for (i in 1:(2^U)) {
    for (j in 1:U){
      vrsta[j+1] <- vrsta[j]*(u*kocka[i,j] + d*(1-kocka[i,j]))
    }
    gor <- rowSums(kocka)[i]
    izplacilo <- izplacilo(vrsta, T, type)
    E <- E + izplacilo*q^(gor)*(1-q)^(U-gor)
  }
  c <- E/(1+R)^(U)
  return(c)
}

#b
monte <- function(S0, u, d, U, R, T, type, N){
  q <- (1+R-d)/(u-d)
  vrednosti <- c(0)
  vrsta <- c(S0)
  for (i in 1:N){
    kombinacija <- rbinom(U,1,q)
    for (j in 1:U) {
      vrsta[j+1] <- vrsta[j]*(u*kombinacija[j] + d*(1-kombinacija[j]))
    }
    vrednosti[i] <- izplacilo(vrsta, T, type)
  }
  c <- mean(vrednosti)/(1+R)^U
  return(c)
}

N1 <- 10
N2 <- 100
N3 <- 1000 

vrednostN1 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N1)
vrednostN2 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N2)
vrednostN3 <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N3)

#3
#a
bin_pov <- binomski(60, 1.05, 0.95, 15, 0.01, 8, 'put')

M <- 100
oceneN1 <-  c(0)
oceneN2 <-  c(0)
oceneN3 <-  c(0)

for (i in 1:M){
  oceneN1[i] <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N1)
  oceneN2[i] <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N2)
  oceneN3[i] <- monte(60, 1.05, 0.95, 15, 0.01, 8, 'put', N3)
}

odklon1 <- var(oceneN1)^0.5
odklon2 <- var(oceneN2)^0.5
odklon3 <- var(oceneN3)^0.5

histogram1 <- hist(oceneN1, 
              main = 'Monte Carlo, N = 10',
              xlim = c(0,11),
              col = 'orange',
              xlab = 'Premija')
abline(v = mean(oceneN1), col = 'purple', add = TRUE, lwd = 2)
abline(v = bin_pov, col = 'green', add = TRUE)
legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('purple', 'green'),
       lwd = 1:1,
       bty = 'n')
arrows(mean(oceneN1), 0, mean(oceneN1) + odklon1, 0, add = TRUE)
arrows(mean(oceneN1), 0, mean(oceneN1) - odklon1, 0, add = TRUE)

histogram2 <- hist(oceneN2, 
                   main = 'Monte Carlo, N = 100',
                   xlim = c(0,11),
                   col = 'orange',
                   xlab = 'Premija')
abline(v = mean(oceneN2), col = 'purple', add = TRUE, lwd = 2)
abline(v = bin_pov, col = 'green', add = TRUE)
legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('purple', 'green'),
       lwd = 1:1,
       bty = 'n')
arrows(mean(oceneN2), 0, mean(oceneN2) + odklon2, 0, add = TRUE)
arrows(mean(oceneN2), 0, mean(oceneN2) - odklon2, 0, add = TRUE)

histogram3 <- hist(oceneN3, 
                   main = 'Monte Carlo, N = 1000',
                   xlim = c(0,11),
                   col = 'orange',
                   xlab = 'Premija')
abline(v = mean(oceneN3), col = 'purple', add = TRUE)
abline(v = bin_pov, col = 'green', add = TRUE)
legend('topright', 
       legend = c('Monte Carlo', 'analiza modela'),
       col = c('purple', 'green'),
       lwd = 1:1,
       bty = 'n')
arrows(mean(oceneN3), 0, mean(oceneN3) + odklon3, 0, add = TRUE)
arrows(mean(oceneN3), 0, mean(oceneN3) - odklon3, 0, add = TRUE)

