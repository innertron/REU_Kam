If the file pairwise.causality.region.CSD.RData is not present, then you need to do the following:
	1. download kamal_data3.h5 from https://drive.google.com/open?id=0BzyCB-i-aKDWTDJCYUdQYkxRRHM
	2. run the following scripts in order:
		1. h5ReadIn.R
		2. lump_CSD_by_region.R
		3. calculate_pairwise_causality_CSD_full_experiment.R
		4. app.R
Else if the file pairwise.causality.region.CDS.RData is present, then simply run the app.R file using RStudio.