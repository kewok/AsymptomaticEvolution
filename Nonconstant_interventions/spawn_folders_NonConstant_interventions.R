options(scipen=999) # temporarily avoid using scientific notation

# Host infection risks
r12 <- 10^seq(0,2,length=3)
r21 <- 10^seq(-2,0,length=3) # Note this is a constant of proportionality

# strain-specific differences in asymptomatic infection potential
at1 <- 0.1
at2 <- 0.1

# Asymptomaticity
p11 <- 0.075 # SARS-1 data, HCWs https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3371799/
p12 <- c(1.1,5,13) # Note that the net "asymptomaticity" is p11*p12, so this becomes approx. 0.08,0.375,0.975
p21 <- 0.075
p22 <- c(1.1,5,13)

# Intervention parameters
MD <- 1+c(0.05,0.85)  # Additional recovery rate while under quarantine or isolation
q1 <- 10^seq(-2,1,length=15)
q2 <- c(1,0.001) # relative quarantine efficacy by strain
is1 <- 10^seq(-2,1,length=15)
is2 <- c(1,0.001) # relative isolation efficacy by strain
Ig <- c(0.1,1) # relative efficacy for isolation/quarantine in host 1

parameter_space <- expand.grid(r12, r21, at1, at2, p11, p12, p21, p22, MD, q1, q2, is1, is2,Ig)
colnames(parameter_space) <- c('r12', 'r21', 'at1', 'at2', 'p11', 'p12', 'p21', 'p22','MD','q1','q2','is1','is2','Ig')

args <- (commandArgs(trailingOnly=TRUE))
i = as.numeric(args[1])

# Get the value to the nearest 10000th and create a subdirectory
subset <- round(i,-4)
subdir <- paste('subdir', subset, sep='')
if (sum(subdir!=list.files()))
  {
  system(paste('mkdir', subdir))
  }

setwd(subdir)
directory <- paste('Scenario',i,sep='')
system(paste('mkdir', directory))
setwd(directory)
write.table(parameter_space[i,], file='parameter_combo.txt', col.names=T, row.names=F)
system('cp ../../covid_2strains_NonConstant_interventions.R .')
system('cp ../../covid_NonConstant_interventions.R .')
system('cp ../../simulate_NonConstant_interventions.R .')
source('simulate_NonConstant_interventions.R')
