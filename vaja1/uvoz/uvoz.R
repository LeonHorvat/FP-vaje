euribor2011 <- read_csv('vaja1/podatki/hist_EURIBOR_2011.csv') %>%
  subset(select=c('X1','03/01/2011', '01/02/2011', '01/03/2011','01/04/2011','02/05/2011','01/06/2011','01/07/2011','01/08/2011','01/09/2011','03/10/2011','01/11/2011','01/12/2011'))%>%
  t()

imena <- as.character(unlist(euribor2011[1,]))
euribor2011 <- euribor2011[-1,]
colnames(euribor2011) <- imena

euribor2012 <- read_csv('vaja1/podatki/hist_EURIBOR_2012.csv') %>%
  subset(select=c('X1','02/01/2012', '01/02/2012', '01/03/2012','02/04/2012','02/05/2012','01/06/2012','02/07/2012','01/08/2012','03/09/2012','01/10/2012','01/11/2012','03/12/2012'))%>%
  t()
euribor2012 <- euribor2012[-1,]
euribor2013 <- read_csv('vaja1/podatki/hist_EURIBOR_2013.csv') %>%
  subset(select=c('X1','02/01/2013', '01/02/2013', '01/03/2013','02/04/2013','02/05/2013','03/06/2013','01/07/2013','01/08/2013','02/09/2013','01/10/2013','01/11/2013','02/12/2013'))%>%
  t()
euribor2013 <- euribor2013[-1,]

euribor <- rbind(euribor2011, euribor2012, euribor2013) %>%
  subset(select=c(1,2,4,5,6,9,12,15)) %>%
  as.data.frame()
  
euribor[] <- lapply(euribor, as.character) %>% lapply(as.numeric)
  