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

which_rows <- function(MD, Ig, r12, r21, p12, p22, q2, is2)
  {
  return(which(parameter_space[,'MD']==MD & parameter_space[,'Ig']==Ig & parameter_space[,'r12']==r12 & parameter_space[,'r21']==r21 & parameter_space[,'p12']==p12 & parameter_space[,'p22']==p22 & parameter_space[,'q2']==q2 & parameter_space[,'is2']==is2)  )
  }
f <- which_rows(MD=0.05,Ig=0.1,r12=100,r21=0.01,p12=13,p22=1.1,q2=0.001,is2=0.001)

popsizes <- read.table('final_frequencies_Nonconst.txt', row.names=NULL,header=T)

# Draw the results
colnames(popsizes) <- c('S_1', 'I_1_1_A', 'I_1_1_s', 'I_1_2_A', 'I_1_2_s', 'S_2', 'I_2_1_A', 'I_2_1_s', 'I_2_2_A', 'I_2_2_s', 'Q_1_1', 'Q_1_2', 'Q_2_1', 'Q_2_2', 'Is_1_1', 'Is_1_2', 'Is_2_1', 'Is_2_2')

freq2 <- function(x)
	{
sum(x[c('I_1_2_A','I_1_2_s','I_2_2_A','I_2_2_s')])/sum(x[c('I_1_1_A','I_1_1_s','I_2_1_A','I_2_1_s','I_1_2_A','I_1_2_s','I_2_2_A','I_2_2_s')])
	}

strain2_prevalence <- function(x)
	{
	sum(x[c('I_1_2_A','I_1_2_s','I_2_2_A','I_2_2_s')])/sum(x[c('S_1','S_2','I_1_1_A','I_1_1_s','I_2_1_A','I_2_1_s','I_1_2_A','I_1_2_s','I_2_2_A','I_2_2_s')])
	}

strain2_prevalence_h1 <- function(x)
{
  sum(x[c('I_1_2_A','I_1_2_s')])/sum(x[c('S_1','S_2','I_1_1_A','I_1_1_s','I_2_1_A','I_2_1_s','I_1_2_A','I_1_2_s','I_2_2_A','I_2_2_s')])
}

strain2_prevalence_h2 <- function(x)
{
  sum(x[c('I_2_2_A','I_2_2_s')])/sum(x[c('S_1','S_2','I_1_1_A','I_1_1_s','I_2_1_A','I_2_1_s','I_1_2_A','I_1_2_s','I_2_2_A','I_2_2_s')])
}

freqs <- apply(popsizes, 1, freq2)
s2 <- apply(popsizes, 1, strain2_prevalence)
s2_h1 <- apply(popsizes, 1, strain2_prevalence_h1)
s2_h2 <- apply(popsizes, 1, strain2_prevalence_h2)

get_subset <- function(x)
	{
	return(which(parameter_space[,'r12']==x[1] & parameter_space[,'r21']==x[2] & parameter_space[,'p12']==x[3] & parameter_space[,'p22']==x[4] & parameter_space[,'MD']==x[5] & parameter_space[,'Ig']==x[6] & parameter_space[,'q2']==x[7] & parameter_space[,'is2']==x[8]))
	}

library(fields)

draw <- function(r1,r2,p12,p22,q2,is2)
	{
	for (i in 1:2)
		for (j in 1:2)
			{
	image(x=log(q1,10),y=log(is1,10),z=matrix(s2[get_subset(c('r12'=r1,'r21'=r2,'p12'=p12,'p22'=p22, 'MD'=MD[i],'Ig'=Ig[j],'q2'=q2,'is2'=is2))], nrow=15,byrow=T),xlab='Log 10 isolation effort',ylab='Log 10 quarantine effort',col=tim.colors(64),main=paste('med help=',MD[i],'Ignored=',Ig[j],'h1 risk=',r1,'mixing=',r2,'\nphen h1=',p12,'phen h2=',p22,'q help=',q2,'is help=',is2),cex.main=0.8,zlim=range(s2,na.rm=T))
			}
	}

par(mfrow=c(2,2))

# Change these values to facilitate analysis
draw(r1=10,r2=0.01,p12=5,p22=13,q2=1,is2=0.001)


