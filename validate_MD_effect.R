# Parameter combinations are:
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
MD <- c(0.05,0.85) # Additional recovery rate while under quarantine or isolation
q1 <- 10^seq(-2,1,length=15)
q2 <- c(1,0.001) # relative quarantine efficacy by strain
is1 <- 10^seq(-2,1,length=15)
is2 <- c(1,0.001) # relative isolation efficacy by strain
Ig <- c(0.1,1) # relative efficacy for isolation/quarantine in host 1

parameter_space <- expand.grid(r12, r21, at1, at2, p11, p12, p21, p22, MD, q1, q2, is1, is2,Ig)
colnames(parameter_space) <- c('r12', 'r21', 'at1', 'at2', 'p11', 'p12', 'p21', 'p22','MD','q1','q2','is1','is2','Ig')
pop_sizes <- read.table('final_frequencies_with_interventions.txt',header=T)
# pop_sizes <- read.table('../Nonconstant_interventions/final_frequencies_Nonconst.txt',header=T)
# Confirm that but for MD, the order of the other parameters are identical in the parameter matrix:
md5 <- which(parameter_space[,'MD']==0.05)
md85 <- which(parameter_space[,'MD']==0.85)
sum(parameter_space[md5,-which(colnames(parameter_space)=="MD")]-parameter_space[md85,-which(colnames(parameter_space)=="MD")])

colnames(pop_sizes) <- c('S_1', 'I_1_1_A', 'I_1_1_s', 'I_1_2_A', 'I_1_2_s', 'S_2', 'I_2_1_A', 'I_2_1_s', 'I_2_2_A', 'I_2_2_s', 'Q_1_1', 'Q_1_2', 'Q_2_1', 'Q_2_2', 'Is_1_1', 'Is_1_2', 'Is_2_1', 'Is_2_2')

strain2_prevalence <- function(x)
 {
 sum(x[c('I_1_2_A','I_1_2_s','I_2_2_A','I_2_2_s')])/sum(x[c('S_1','S_2','I_1_1_A','I_1_1_s','I_2_1_A','I_2_1_s','I_1_2_A','I_1_2_s','I_2_2_A','I_2_2_s')])
 }

md5_prev <- apply(pop_sizes[md5,], 1, strain2_prevalence)
md85_prev <- apply(pop_sizes[md85,], 1, strain2_prevalence)

quantile(md5_prev-md85_prev,c(0.01,seq(0.05,0.95,length=10), 0.99),na.rm=T)

