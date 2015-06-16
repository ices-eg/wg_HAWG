## Installation ##

The most reliable way to install FLSAM is to download the release from this site and install it manually - click on the "Downloads" tab above and select the version for your operating system. This is where you will find the latest official release.

Once you have downloaded the file, you can install it in Windows by selecting  Packages/"Install from Local Zip file" from the menus in R and selecting the appropriate zip file.

Alternatively, for both Windows and Linux, you can also install from the R command prompt using:
```R
install.packages("<pathToFLSAM>/<FLSAM file>")```
or the following from the command line:
```bash
R CMD INSTALL <FLSAM package filename>```
### FLCore dependency ###
Please note that FLSAM will only run when FLCore (the main "core" package of the FLR library) is installed too! The current FLSAM release requires FLCore 2.4 or greater. If you don't already have it, you can install the latest FLCore release by issuing the following command in R.
```R
install.packages("FLCore", repos="http://flr-project.org/R")```

## More Info ##
More information on getting started with FLSAM is available from the internal documentation in FLSAM. In particular consult the "Getting Started with FLSAM" vignette
```R

library(FLSAM)
vignette("FLSAM")```
or the FLSAM manual
```R

library(FLSAM)
vignette("FLSAM-manual")```
for more details