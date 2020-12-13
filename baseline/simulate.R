source('covid.R')
parameter_space <- read.table('parameter_combo.txt', header=T)
otpt <- simulate(parameter_space[c('r12','r21')], parameter_space[c('at1','at2')], matrix(parameter_space[c('p11','p12','p21','p22')],nrow=2,byrow=T))
write.table(otpt,file='results.txt')