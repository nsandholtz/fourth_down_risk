**<p align="center">  Learning Risk Preferences in Markov Decision Processes: an Application to the Fourth Down Decision in Football</p>**

*<p align="center"> A Companion Repository </p>*

All of the R code to produce the results presented in our paper [Learning Risk Preferences in Markov Decision Processes: an Application to the Fourth Down Decision in Football](https://arxiv.org/abs/2309.00756) are included in this repository.  The data which this analysis relies on can be accessed in R via the [nflfastR](https://www.nflfastr.com/) and [nfl4th](https://www.nfl4th.com/index.html) packages.  The script `r_scripts/data_preprocessing.R` loads the data using these packages, cleans the data for our analysis, and saves the cleaned data as an .rds file, which is used in the other analysis scripts.  We also have included the model output necessary to recreate the figures in our paper, for those interested in taking a closer look at the results without rerunning the analysis.  
