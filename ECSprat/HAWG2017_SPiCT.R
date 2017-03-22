#' ---
#' title: "SPiCT workshop - Exercise 1"
#' author: "Alexandros Kokkalis and Casper W. Berg"
#' date: "January 26 2017"
#' output: pdf_document
#' toc: true
#' ---
#' 
#+ echo = FALSE
knitr::opts_chunk$set(comment=NA, fig.width=6, fig.height=8)
#'
#' Learning objectives:
#' ------------------- 
#' * Installing SPiCT
#' * Examining an example data set
#' * Doing an assessment using SPiCT
#' * Looking at results (summary, plot, diagnostics)
#'
#' Notes
#' -----
#' * Use help to see the documentation of a function (e.g. ?get.par or 
#'   help("get.par", "spict"))
#' * The spict vignette is a long-form documentation that explains the 
#'   functionality of spict using code examples. You can open it using
#'   `vignette("vignette", "spict")`
#'
#' 1.1 Install TMB and SPiCT
#' --------------------------
#' 
#' Uncomment and run the next two lines if the spict package is not installed
# install.packages("TMB")
# devtools::install_github("mawp/spict/spict", build_vignettes = TRUE)
#'
#' 1.2 Load and look at the South Atlantic albacore data
#' -----------------------------------------------------
#' 
#' Load the spict package (it automatically loads TMB)
library(spict)

#' There are two messages shown after the successful loading of spict, showing 
#' that TMB was also loaded and the spict version and SHA1 code from the github 
#' repository. This code is useful to identify the exact version of the package 
#' used to produce results and is automatically saved with the results and added
#' to the default plots. The version used to create this exercise is shown below:
# Loading required package: TMB
# Welcome to spict_v1.1@9eccc2daae57dd739bc24792cfdc0d862501e7bf
#' One can install that exact same version of spict using
#'
## devtools::install_github("mawp/spict/spict", 
##                          ref = "9eccc2daae57dd739bc24792cfdc0d862501e7bf")
# Prepare my stock
spr <- read.csv("SPiCT/Spr_Ech.csv")

# I'm using ANNUAL LPUE or ANNUAL Effort, so the timing should be the 
# same of the catch (that however are in Q1-Q4). Shall we think about some
# seasonal SPiCT? 
#spr <- spr[10:32,]
inp <- list(list())
inp[["obsC"]] <- spr[,"Landings"]
inp[["timeC"]] <- spr[,"X"]
inp[["obsI"]] <- spr[,"LPUE_Rhours"]
inp[["timeI"]] <- spr[,"X"]
#inp[["obsI"]] <- spr[,"LPUE_Rdays"]
#inp[["timeI"]] <- spr[,"X"]
#inp[["timeI"]] <- list(spr$X, spr$X + 0.75)
#inp[["obsI"]] <- list(spr$LPUE_Rdays, spr$Echo)
# inp[["timeI"]] <- list(spr$X, spr$X + 0.75)
# inp[["obsI"]] <- list(spr$LPUE_Rhours, spr$Echo)
#inp[["timeI"]] <- spr$X + 0.75
#inp[["obsI"]] <- spr$Echo

# Effort Effort_all_Rest (annual)
#inp[["obsE"]] <- spr$Effort_all_Rest
#inp[["timeE"]] <- spr[,"X"]

#names(sprLs)[[1]]$obsC <- c("spr")

# Check if the input data are good enough
#inp <- check.inp(pol$albacore)
inp <- check.inp(inp)

#' Plot the input data
plotspict.data(inp)
# A lot of plotting function: they all starts with plotspict. something. 
# if you add "stamp" you can add note to that. 

# Fit the model
fit <- fit.spict(inp)

# Plot the results
plot(fit)
plotspict.diagnostic(fit)

inp$priors$logn <- c(2,2,0)
inp$priors$logalpha <- c(2,2,0)
inp$priors$logbeta <- c(2,2,0)
# INCREASE UNCERTAINTY
# Let's try to change uncertainty for the recent LPUE (only based on one vessel)
t <- inp$timeI[[1]] >= 2011 & inp$timeI[[1]] <= 2016
inp$stdevfacC[t] <- 2
# Fit the model
fit1 <- fit.spict(inp)

# Plot the results
plot(fit1)
plotspict.diagnostic(fit1)



