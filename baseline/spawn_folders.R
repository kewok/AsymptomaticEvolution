# Host infection risks
r12 <- 10^seq(0,2,length=3)
r21 <- 10^seq(-2,0,length=3) # Note this is a constant of proportionality

# strain-specific differences in asymptomatic infection potential
at1 <- 0.1
at2 <- 0.1

# Asymptomaticity
p11 <- 0.075 # SARS-1 data, HCWs https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3371799/
p12 <- 10^seq(0,2,length=3) 
p21 <- 0.075
p22 <- 10^seq(0,2,length=3)

parameter_space <- expand.grid(r12, r21, at1, at2, p11, p12, p21, p22)
colnames(parameter_space) <- c('r12', 'r21', 'at1', 'at2', 'p11', 'p12', 'p21', 'p22')

for (i in 1:nrow(parameter_space))
  {
  directory <- paste('Scenario',i,sep='')
  system(paste('mkdir', directory))
  setwd(directory)
  write.table(parameter_space[i,], file='parameter_combo.txt', col.names=T, row.names=F)
  system('cp ../covid_2strains.R .')
  system('cp ../covid.R .')
  system('cp ../simulate.R .')
  setwd('..')
  }
