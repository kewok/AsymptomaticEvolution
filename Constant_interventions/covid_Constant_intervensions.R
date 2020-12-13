source('covid_2strains_Constant_interventions.R')

extinction_Threshold <- 1e-50

params <- c()
times <- seq(0,1e5,length=1001)

simulate <- function(r_h_H, aT, Pi, MD, Q, Is, Ig)
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
  aT_2 = as.numeric(aT[2]),
  
  # Intervention parameters
  
  # Effect of medication on lowering mortality if isolated or quarantined (vary):
  MD = as.numeric(MD),
  
  # Ability to successfully quarantine asymptomatics, strain 1
  Q_1 = as.numeric(Q[1]),
  # Ability to successfully quarantine asymptomatics, strain 2 as proportion of strain 1
  Q_2 = as.numeric(Q[1])*as.numeric(Q[2]),
  # Ability to successfully isolate symptomatics, strain 1
  Is_1 = as.numeric(Is[1]),
  # Ability to successfully isolate symptomatics, strain 2 as proportion of strain 1
  Is_2 = as.numeric(Is[2])*as.numeric(Is[1]),
  # Cost to host for lower access to testing or from being ignored
  Ig = as.numeric(Ig)
  )
  out <- lsoda(init, times, propInfected_interventions, parms=params, atol=extinction_Threshold)
  return(out)
}
