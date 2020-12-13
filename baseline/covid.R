source('covid_2strains.R')

extinction_Threshold <- 1e-50

params <- c()
times <- seq(0,1e5,length=1001)

simulate <- function(r_h_H, aT, Pi)
  {
  params <- c(
    # asymptomaticity
    pi_1_1_A = as.numeric(Pi[1,1]),
    pi_2_1_A = as.numeric(Pi[2,1]),
    
    # Assume that strain 2 is consistently less symptomatic than strain 1
    pi_1_2_A = as.numeric(Pi[1,2]) * as.numeric(Pi[1,1]),
    pi_2_2_A = as.numeric(Pi[2,2]) * as.numeric(Pi[2,1]),
    
    # Host mixing terms:
    r_1_1 = as.numeric(r_h_H[1]), # enhanced infection risk in group 1
    r_1_2 = as.numeric(r_h_H[1]) * as.numeric(r_h_H[2]), # assume infection risk in group 1 from 2 is mediated by social segregation term r_h_H[2]
    r_2_1 = as.numeric(r_h_H[2]), 
    r_2_2 = 1,
    
  # Strain-specific differences in propensity of asymptomatics to transmit infection
  aT_1 = as.numeric(aT[1]),
  aT_2 = as.numeric(aT[2])
  )
  out <- lsoda(init, times, propInfected, parms=params, atol=extinction_Threshold)
  return(out)
}

