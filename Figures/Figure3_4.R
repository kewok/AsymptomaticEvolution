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
MD <- c(0.05,0.85)  # Additional recovery rate while under quarantine or isolation
q1 <- 10^seq(-2,1,length=15)
q2 <- c(1,0.001) # relative quarantine efficacy by strain
is1 <- 10^seq(-2,1,length=15)
is2 <- c(1,0.001) # relative isolation efficacy by strain
Ig <- c(0.1,1) # relative efficacy for isolation/quarantine in host 1

parameter_space <- expand.grid(r12, r21, at1, at2, p11, p12, p21, p22, MD, q1, q2, is1, is2,Ig)
colnames(parameter_space) <- c('r12', 'r21', 'at1', 'at2', 'p11', 'p12', 'p21', 'p22','MD','q1','q2','is1','is2','Ig')

which_rows <- function(MD, Ig, r12, r21, p12, p22, q2, is2, is1_v, q1_v)
  {
  return(which(parameter_space[,'MD']==MD & parameter_space[,'Ig']==Ig & parameter_space[,'r12']==r12 & parameter_space[,'r21']==r21 & parameter_space[,'p12']==p12 & parameter_space[,'p22']==p22 & parameter_space[,'q2']==q2 & parameter_space[,'is2']==is2 & parameter_space[,'is1']==is1[is1_v] & parameter_space[,'q1']==q1[q1_v])  )
  }
f <- which_rows(MD=0.05,Ig=0.1,r12=100,r21=0.01,p12=13,p22=1.1,q2=0.001,is2=0.001, is1_v=8, q1_v=3)


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
	sum(x[c('I_1_2_A','I_1_2_s')])/sum(x[c('S_1','I_1_1_A','I_1_1_s','I_1_2_A','I_1_2_s')])
	}

strain2_prevalence_h2 <- function(x)
	{
	sum(x[c('I_2_2_A','I_2_2_s')])/sum(x[c('S_2','I_2_1_A','I_2_1_s','I_2_2_A','I_2_2_s')])
	}

popsizes <- read.table('../Nonconstant_interventions/final_frequencies_Nonconst.txt', row.names=NULL,header=T)
colnames(popsizes) <- c('S_1', 'I_1_1_A', 'I_1_1_s', 'I_1_2_A', 'I_1_2_s', 'S_2', 'I_2_1_A', 'I_2_1_s', 'I_2_2_A', 'I_2_2_s', 'Q_1_1', 'Q_1_2', 'Q_2_1', 'Q_2_2', 'Is_1_1', 'Is_1_2', 'Is_2_1', 'Is_2_2')
freqs <- apply(popsizes, 1, freq2)
s2 <- apply(popsizes, 1, strain2_prevalence)
s2_h1 <- apply(popsizes, 1, strain2_prevalence_h1)
s2_h2 <- apply(popsizes, 1, strain2_prevalence_h2)


get_subset <- function(x)
	{
	return(which(parameter_space[,'r12']==x[1] & parameter_space[,'r21']==x[2] & parameter_space[,'p12']==x[3] & parameter_space[,'p22']==x[4] & parameter_space[,'MD']==x[5] & parameter_space[,'Ig']==x[6] & parameter_space[,'q2']==x[7] & parameter_space[,'is2']==x[8]))
	}

library(fields)

md <- 0.05
draw <- function(r1,r2,p12,p22,q2,is2, ig, main=NA)
	{	
	# use MD[1] = 0.05
image(x=log(q1,10),y=log(is1,10),z=matrix(s2[get_subset(c('r12'=r1,'r21'=r2,'p12'=p12,'p22'=p22, 'MD'=md,'Ig'=ig,'q2'=q2,'is2'=is2))], nrow=15,byrow=T),xlab=expression('Log'[10]*' isolation effort'),ylab=expression('Log'[10]*' quarantine effort'),col=tim.colors(64),main=ifelse(!is.na(main), main, paste('med help=',md,'Ignored=',ig,'h1 risk=',r1,'mixing=',r2,'\nphen h1=',p12,'phen h2=',p22,'q help=',q2,'is help=',is2)),cex.main=0.8,zlim=range(s2,na.rm=T),cex.lab=1.2,cex.main=2,cex.axis=1.3)
	}

par(las=1,pty='s',mfrow=c(2,4))
#nf <- layout(matrix(c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8),nrow=2,byrow=T),)
draw(r1=r12[3], r2=r21[1],p12=p12[1],p22=p22[1],q2=q2[1],is2=is2[1], ig=Ig[1],main='(A)')
draw(r1=100,r2=0.1,p12=5,p22=5,q2=0.001,is2=1,ig=0.1,main='(B)')
draw(r1=100,r2=0.1,p12=5,p22=5,q2=1,is2=0.001,ig=0.1,main='(C)')
draw(r1=r12[2], r2=r21[1],p12=p12[2],p22=p22[3],q2=q2[2],is2=is2[2], ig=Ig[1],main='(D)')
draw(r1=r12[2], r2=r21[3],p12=p12[1],p22=p22[3],q2=q2[2],is2=is2[1], ig=Ig[2],main='(E)')
draw(r1=r12[2], r2=r21[1],p12=p12[1],p22=p22[1],q2=q2[2],is2=is2[1], ig=Ig[1],main='(F)')
draw(r1=r12[1], r2=r21[1],p12=p12[1],p22=p22[1],q2=q2[2],is2=is2[2], ig=Ig[1],main='(G)')
draw(r1=r12[1], r2=r21[2],p12=p12[1],p22=p22[2],q2=q2[2],is2=is2[2], ig=Ig[2],main='(H)')
dev.copy2pdf(file='Fig3.pdf')

par(las=1,pty='s')
nf <- layout(matrix(c(1,1,2,2,3,3,0,4,4,5,5,0),ncol=6,nrow=2,byrow=T))
draw(r1=10,r2=0.1,p12=5,p22=5,q2=1,is2=1,ig=1,main='(A)')
draw(r1=10,r2=0.1,p12=5,p22=5,q2=0.001,is2=1,ig=1,main='(B)')
draw(r1=10,r2=0.1,p12=5,p22=5,q2=1,is2=0.001,ig=1,main='(C)')
draw(r1=100,r2=0.1,p12=5,p22=5,q2=0.001,is2=0.001,ig=1,main='(D)')
draw(r1=r12[2], r2=r21[3],p12=p12[1],p22=p22[2],q2=q2[2],is2=is2[1], ig=Ig[2],main='(E)')
dev.copy2pdf(file='Fig4.pdf')


