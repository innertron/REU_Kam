
The main package we will be using will be the [rEDM](https://cran.r-project.org/web/packages/rEDM/vignettes/rEDM_tutorial.html) package, which was developed by Sugihara's group. The mCCR package was used in the past, but will no loger be used. The code and data that we will be developing will be in the rEDM file for now.

#Requirements
The following packages are required to completely run all scripts within this project:

-rEDM
-igraph
-rhdf5
-shiny
-parallel
-snow
-eegkit

each of these packages can be installed by the following command

```
install.packages("PACKAGE_NAME")
```
with the esception of the rhdf5 package which must be installed by these commands.

```
source("https://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```

#Project structure
The project is split by folders into logical threads of work. Each folder represents a contingent body of work or a result of such work. Analysis folders and ploting folders are separated. The following is a short description of each folder:

- App: contains scripts to run a Shiny app that shows the final output
- causality plots: contains plots that represent simple causality plots between a given pair
- for channels: constains scripts specifically for dealing with causality between channels. 
- for regions: contains scripts specifically for dealing with causality between regions.
- lag plots: contains plots that show the laged causality between two pairs
- network plots: contains sample plots used in paper for simple graph results
- signficiant causality plots: contains overlayed causality plots that represent the significance of the causality detected.
- simple plots: contains simple ad-hoc plots produced for a paper or presentation
