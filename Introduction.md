# HOWTO: Use the HAWG Repository #
The following page describes how to setup and get started with using the HAWG stock assessment repository. The repository is a version control system for stock assessment code, allowing multiple users to collaborate on a common assessment project by sharing and synchronising their code. In addition, it also provides version control on a line-by-line basis - all versions are saved, and its always possible to go back!

## System Requirements ##
You will need the following software to use the HAWG repository.
  * R (at least version 2.13.0 or greater) for your system. [Available here](http://cran.r-project.org/).
  * An SVN (subversion) client. For Windows users, we recommend [TortoiseSVN](http://tortoisesvn.tigris.org/). Linux users will typically have access to such a client through their package distribution system.
  * Some way to write and edit R code. For Windows, try [RStudio](http://rstudio.org/) or [Tinn-R](http://sourceforge.net/projects/tinn-r/). For Linux, you almost certaintly already have [Vi](http://en.wikipedia.org/wiki/Vi)

## Getting the Code ##

Once you've got all the necessary bits and pieces, the next step is to fetch a "working copy" of the HAWG repository from the server. The SVN client that you have installed takes care of this. The general process is client specific, but generally involves two steps 1. Create a directory for your working copy (e.g. Desktop/HAWG\_assessments and 2. "Checking out" the repository.

We'll assume you can handle #1 yourself, but number 2 is a bit trickier, and is of course dependent on your SVN client. There are a number of good resources available e.g. for windows
  * On the [mseflr](http://code.google.com/p/mseflr/) project wiki

There are some key points to note here.
  * Firstly there are two different ways to checkout the code, depending on whether you want to commit the changes back to the repository or not. If you checkout the repository from `http://hawg.googlecode.com/svn/trunk/ ` it will not be possible to commit your changes back - however, the benefit is that **anyone** can get access to the repository, without the need for a google account.
  * Alternatively, you can check out the code from `https://hawg.googlecode.com/svn/trunk/ ` (note the **https**, rather than http), in which case you will be able to commit your changes back. The disadvantage is that this requires a google account (e.g. gmail.com).
  * Finally, there is a wee trap for beginners when logging in for the first time - make sure that you use the password for the project that googlecode supplies you - **not** your login password to google itself (slightly counter-intuitive, I know)! This is detailed on in the [mseflr](http://code.google.com/p/mseflr/wiki/UsingTortoiseSVN) page under "Project member checkout".

## Installing the FLR packages ##
Once you've checked our the source code from the repository, you're nearly ready to start running assessments. FLR is very modular in nature, and is designed as a series of lego-blocks that can be snapped in as needed. You most probably do not need everything that FLR offers. However, in any case you will need the FLCore package, which is the heart of the system. You can install the latest release for your machine by issuing the following command in R.
```
  install.packages("FLCore", repos="http://flr-project.org/R")
```
Other packages that you may wish to add can also be added in a similar manner, by replacing "FLCore" with the name of the package.