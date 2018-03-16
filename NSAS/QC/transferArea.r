herTransferArea <- c("43F3","43F4","43F5","43F6","43F7",
                     "44F3","44F4","44F5","44F6",
                     "45F3","45F4","45F5","45F6",
                     "46F3","46F4","46F5","46F6",
                     "47F3","47F4","47F5","47F6")
lonLatTrans <- ICESrectangle2LonLat(herTransferArea,midpoint=T)
transPol    <- lonLat2SpatialPolygons(lst=lapply(as.list(1:nrow(lonLatTrans)),
                  function(x){return(
                     data.frame(SI_LONG=c(lonLatTrans[x,"SI_LONG"]-0.5,rep(lonLatTrans[x,"SI_LONG"]+0.5,2),lonLatTrans[x,"SI_LONG"]-0.5),
                                SI_LATI=c(rep(lonLatTrans[x,"SI_LATI"]-0.25,2),rep(lonLatTrans[x,"SI_LATI"]+0.25,2))))}))

                     
plot(eurPols,xlim=c(0,14),ylim=c(54,60),asp=1/lonLatRatio(7,57),col="grey")
abline(v=seq(0,14,1),lty=2)
abline(h=seq(54,60,0.5),lty=2)
axis(1,las=1);axis(2,las=1);box()
plot(transPol,add=T,col="lightblue")
plot(ICESareas,add=T,border="darkblue",lwd=3)
plot(eurPols,add=T,col="grey")
axis(1,las=1);axis(2,las=1);box()


