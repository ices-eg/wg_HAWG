# readVPAFile		{{{
readVPAFile <- function(file, sep = "", units = "NA", na.strings="NA",
                        quiet = TRUE) {	
  if (!file.exists(file)){
    if(quiet==TRUE) stop()
    if(quiet!=TRUE) stop(paste("VPA index file", file, "does not exist"))
  }
  
  switch (as.character(file.access(file)),
          "0"  = info <- read.table(file, colClasses = "character", 
                                    header = FALSE, fill = TRUE, skip = 1, 
                                    nrows = 4, sep = sep, comment.char='#',
                                    na.strings=na.strings),
          "-1" = info <- matrix(rep("0", 8), nrow = 4, ncol = 2))
  
  misc <- info[1, 1]
  type <- info[1, 2]
  dfor <- info[4, 1]
  
  # Switch for file type (dfor; e.g. matrix, scalar, vector)
  switch(misc,
         "1" = {range <- scan(file, skip = 2, nlines = 2, sep = sep, comment.char='#', na.strings=na.strings,
                              quiet=quiet)
         ages <- range[3:4]
         nages <- ages[2] - ages[1] + 1
         yrs <- range[1:2]
         nyrs <- yrs[2] - yrs[1] + 1
         dms <- list(age=as.character(ages[1]:ages[2]),year=as.character(yrs[1]:yrs[2]))
         switch(dfor,
                "1" = a. <- FLCore::as.FLQuant(matrix(t(read.table(file = file, skip = 5,
                                                           nrows = nyrs, sep = sep, comment.char='#', na.strings=na.strings)[, 1:nages]), nrow=nages,
                                              ncol=nyrs),dimnames= dms),
                "2" = a. <- FLCore::as.FLQuant(matrix(rep(scan(file, skip = 5, sep = sep,
                                                       comment.char='#', quiet=quiet, na.strings=na.strings)[1:nages], nyrs), nrow = nages,
                                              ncol = nyrs), dimnames = dms),
                "3" = a. <- FLCore::as.FLQuant(matrix(rep(scan(file, skip = 5, sep = sep,
                                                       comment.char='#', quiet=quiet, na.strings=na.strings)[1], nyrs * nages),nrow = nages,
                                              ncol = nyrs), dimnames = dms),
                "5" = {
                  dms <- list(age="all",year=as.character(yrs[1]:yrs[2]))
                  a. <- FLCore::as.FLQuant(matrix(t(read.table(file = file, skip = 5,
                                                       nrows = nyrs, sep = sep, na.strings=na.strings)[,1]), nrow = 1, ncol = nyrs), dimnames = dms)
                }
         )
         #needed to go from int to double
         a. <-  FLCore::FLQuant(as.numeric(a.),dimnames=dimnames(a.))
         return(a.)
         },
         "0" = cat("Invalid file. Cannot read file:-", file, "\n"),
         if(quiet != TRUE) cat("Tuning file", file, "not read", "\n")
  )
}	