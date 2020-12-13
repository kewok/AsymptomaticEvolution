subset <- seq(0,29)*1e4

final_matrix <- matrix(ncol=19)
k <- 1
for (i in 1:30)
	{
	setwd(paste('subdir',subset[i],sep=''))
	lasts <- matrix(ncol=19)
	if (sum('Final_Population_Sizes.txt'==list.files()))
		{
		for (j in 1:(length(list.files())-1))
			{
			setwd(paste('Scenario',k,sep=''))
			otcm <- read.table('results_with_interventions.txt')
			lasts <- rbind(lasts, as.numeric(otcm[nrow(otcm),]))
			setwd("..")
			k <- k + 1
			}
		}
	else
		{
		for (j in 1:length(list.files()))
			{
			setwd(paste('Scenario',k,sep=''))
			otcm <- read.table('results_with_interventions.txt')
			lasts <- rbind(lasts, as.numeric(otcm[nrow(otcm),]))
			setwd("..")
			k <- k + 1
			}
		}
	write.table(lasts[-1,], file='Final_Population_SizesB.txt')
	print(subset[i])
	setwd('..')
	}

args <- (commandArgs(trailingOnly=TRUE))
i <- as.numeric(args[1])-1

setwd('Nonconstant_interventions')
subdir <- paste('subdir',i*10000,sep='')
setwd(subdir)
lasts <- matrix(ncol=19)
scenarios <- list.files()
numbers = as.numeric(regmatches(scenarios, regexpr("[0-9]+", scenarios)))
dat <- scenarios[order(numbers)]
for (scenario in dat)
    {
    setwd(scenario)
    otcm <- read.table('results_with_interventions.txt')
    lasts <- rbind(lasts, as.numeric(otcm[nrow(otcm),]))
    setwd("..")
   }

write.table(lasts, file='Final_Population_Sizes.txt')

