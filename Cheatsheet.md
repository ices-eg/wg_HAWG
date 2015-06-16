A quick reference guide for people using FLSAM. For more information about the individual functions, please see the help files e.g. type `help("FLSAM")` or `?FLSAM` in the command line. An overview of the package can also be found at `help(package="FLSAM")`

## Setup ##
#### Stock data VPA-suite style ####
`stck <- readFLStock(file.path(datapath,"index.txt"),no.discards=T/F)`

#### Tuning fleet data ####
`tun <- readFLIndices(file.path(datapath,"fleet.txt"),type)`

#### Create Control object ####
`ctrl <- FLSAM.control(stck,tun) `

## Run the assessment ##
  * `sam <- FLSAM(stck,tun,ctrl)`
  * `stck <- stck + sam`
  * `stck@stock <- computeStock(stck)`

## Look at the results ##
#### Plotting ####
  * `plot(stck)`
  * `plot(sam)`
  * `residual.diagnostics(sam)`

#### Extract fitted results ####
  * `ssb(sam)` _also possible with stck_
  * `tsb(sam)` _also possible with stck_
  * `fbar(sam)` _also possible with stck_
  * `rec(sam)` _also possible with stck_
  * `f(sam)`
  * `n(sam)`
  * `catchabilities(sam)`
  * `obs.var(sam)`
  * `power.law.exps(sam)`

## Compare model fits ##
  * `plot(FLStocks(FLSAM1 = sam,FLSAM2 = sam))` _identical_
  * `lr.test(sam1,sam2,...)` _log ratio test_
  * `AIC(sam)` _Aikaike information criteria_

## Document it! ##

## Glossary ##
  * `stck` An FLStock object containing the stock information
  * `tun` An FLIndices object containing tuning series information
  * `ctrl` An FLSAM.control object containing the configuration
  * `sam` An FLSAM object containing the results of the assessment