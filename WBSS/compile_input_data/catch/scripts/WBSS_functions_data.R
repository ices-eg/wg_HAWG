

#Functions for WBSS data handling

read_in_length<-function(path, sheets, firstLineLength){
  library(readxl)
  library(dplyr)
  library(sqldf)
  
  length<-data.frame()
  lengthStart<-firstLineLength-1
  lengthSlut<-lengthStart+74
  
  for(i in sheets) {
    
    length0<-read_excel(path, sheet=i)
    
    ctry<-data.frame(length0[2,2])
    sppName<-length0[3,2]
    year<-length0[4,2]
    area<-length0[2,6]
    fleet<-length0[3,6]
    clnum_unit<-length0[4,6]
    
    head<-data.frame(ctry,sppName,year,area,fleet,clnum_unit)
    colnames(head) <- c("ctry","sppName","year","area","fleet","clnum_unit")
    
    lengthq1<-length0[c(lengthStart:lengthSlut),c(1,3)]
    colnames(lengthq1)<-c("length","clnum")
    lengthq1$quarter<-1
    
    lengthq2<-length0[c(lengthStart:lengthSlut),c(1,4)]
    colnames(lengthq2)<-c("length","clnum")
    lengthq2$quarter<-2
    
    lengthq3<-length0[c(lengthStart:lengthSlut),c(1,5)]
    colnames(lengthq3)<-c("length","clnum")
    lengthq3$quarter<-3
    
    lengthq4<-length0[c(lengthStart:lengthSlut),c(1,6)]
    colnames(lengthq4)<-c("length","clnum")
    lengthq4$quarter<-4
    
    length1<-bind_rows(lengthq1,lengthq2,lengthq3,lengthq4)
    length2<-sqldf("select * from length1, head")
    
    length2$clnum<-as.numeric(length2$clnum)
    length2$length<-as.integer(length2$length)
    length2$length_unit<-"scm"
    
    length<-bind_rows(length,length2)
  }
  
  return(length)
  
}

read_in_catch<-function(path, sheets, noAreas){

  library(readxl)
  library(dplyr)
  library(sqldf)

  catch<-data.frame()
  areaStart<-16
  areaSlut<-areaStart+noAreas-1

  for(i in sheets){

    catch0<-read_excel(path, sheet=i)

    ctry<-data.frame(catch0[3,2])
    sppName<-catch0[4,2]
    year<-catch0[5,2]

    head<-data.frame(ctry,sppName,year)
    colnames(head) <- c("ctry","sppName","year")

    catchq1<-catch0[c(areaStart:areaSlut),c(1:5)]
    colnames(catchq1)<-c("area","landWt","unallocCatchWt","misRepCatchWt","disWt")
    catchq1$quarter<-1

    catchq2<-catch0[c(areaStart:areaSlut),c(1,6:9)]
    colnames(catchq2)<-c("area","landWt","unallocCatchWt","misRepCatchWt","disWt")
    catchq2$quarter<-2

    catchq3<-catch0[c(areaStart:areaSlut),c(1,10:13)]
    colnames(catchq3)<-c("area","landWt","unallocCatchWt","misRepCatchWt","disWt")
    catchq3$quarter<-3

    catchq4<-catch0[c(areaStart:areaSlut),c(1,14:17)]
    colnames(catchq4)<-c("area","landWt","unallocCatchWt","misRepCatchWt","disWt")
    catchq4$quarter<-4

    catch1<-bind_rows(catchq1,catchq2,catchq3,catchq4)
    
    catch2<-sqldf("select * from head, catch1")
    catch<-bind_rows(catch,catch2)

  }

   return(catch)

}

read_in_canum<-function(path, sheets, noAgeClasses){
  
  library(readxl)
  library(dplyr)
  library(sqldf)
  
  canum<-data.frame()
  ageStart<-11
  ageSlut<-ageStart+noAgeClasses-1
  
  for(i in sheets) {
    
    canum0<-read_excel(path, sheet=i)
    
    ctry<-data.frame(canum0[3,3])
    sppName<-canum0[4,3]
    year<-canum0[5,3]
    area<-canum0[3,9]
    fleet<-canum0[4,9]
    
    head<-data.frame(ctry,sppName,year,area,fleet)
    colnames(head) <- c("ctry","sppName","year","area","fleet")
    
    canumq1<-canum0[c(ageStart:ageSlut),c(2:5)]
    colnames(canumq1)<-c("wr","canum","leca","weca")
    canumq1$quarter<-1
    
    canumq2<-canum0[c(ageStart:ageSlut),c(2,6:8)]
    colnames(canumq2)<-c("wr","canum","leca","weca")
    canumq2$quarter<-2
    
    canumq3<-canum0[c(ageStart:ageSlut),c(2,9:11)]
    colnames(canumq3)<-c("wr","canum","leca","weca")
    canumq3$quarter<-3
    
    canumq4<-canum0[c(ageStart:ageSlut),c(2,12:14)]
    colnames(canumq4)<-c("wr","canum","leca","weca")
    canumq4$quarter<-4
    
    canumUnits<-canum0[10,c(3:5)]
    colnames(canumUnits)<-c("canum_unit","leca_unit","weca_unit")
    canumUnits$canum_unit<-ifelse(canumUnits$canum_unit!="(millions)","(1000)","(millions)")
    
    canum1<-bind_rows(canumq1,canumq2,canumq3,canumq4)
    canum1<-sqldf("select * from canum1, canumUnits")
    
    catchq1<-canum0[(ageSlut+3),4]
    colnames(catchq1)<-c("catch")
    catchq1$quarter<-1
    
    catchq2<-canum0[(ageSlut+3),7]
    colnames(catchq2)<-c("catch")
    catchq2$quarter<-2
    
    catchq3<-canum0[(ageSlut+3),10]
    colnames(catchq3)<-c("catch")
    catchq3$quarter<-3
    
    catchq4<-canum0[(ageSlut+3),13]
    colnames(catchq4)<-c("catch")
    catchq4$quarter<-4
    
    catchUnits<-canum0[(ageSlut+3),14]
    colnames(catchUnits)<-c("catch_unit")
    
    catch<-bind_rows(catchq1,catchq2,catchq3,catchq4)
    catch<-sqldf("select * from catch, catchUnits")
    
    canum2<-sqldf("select * from head, canum1")
    canum3<-full_join(canum2,catch)
    
    canum3$canum<-as.numeric(canum3$canum)
    canum3$leca<-as.numeric(canum3$leca)
    canum3$weca<-as.numeric(canum3$weca)
    canum3$catch<-as.numeric(canum3$catch)
    
    canum<-bind_rows(canum,canum3)
    
  }
  
  return(canum)
  
}

read_in_areaOfficial<-function(path, sheets, noRectangles){
  
  library(readxl)
  library(dplyr)
  library(sqldf)
  
  rect<-data.frame()
  rectangleStart<-17
  rectangleSlut<-rectangleStart+noRectangles-1
  
  for(i in sheets){
    
    rect0<-read_excel(path, sheet=i)
    
    ctry<-data.frame(rect0[3,2])
    sppName<-rect0[4,2]
    year<-rect0[5,2]
    fleet<-rect0[3,6]
    unit<-rect0[4,6]
    
    head<-data.frame(ctry,sppName,year,fleet,unit)
    colnames(head) <- c("ctry","sppName","year","fleet","unit")
    
    rectq1<-rect0[c(rectangleStart:rectangleSlut),c(1:2)]
    colnames(rectq1)<-c("rect","landWt")
    rectq1$quarter<-1
    
    rectq2<-rect0[c(rectangleStart:rectangleSlut),c(1,3)]
    colnames(rectq2)<-c("rect","landWt")
    rectq2$quarter<-2
    
    rectq3<-rect0[c(rectangleStart:rectangleSlut),c(1,4)]
    colnames(rectq3)<-c("rect","landWt")
    rectq3$quarter<-3
    
    rectq4<-rect0[c(rectangleStart:rectangleSlut),c(1,5)]
    colnames(rectq4)<-c("rect","landWt")
    rectq4$quarter<-4
    
    rect1<-bind_rows(rectq1,rectq2,rectq3,rectq4)
    rect1$landWt<-as.numeric(rect1$landWt)
    rect1$landWt[is.na(rect1$landWt)]<-0
    
    rect2<-sqldf("select * from head, rect1")
    rect<-bind_rows(rect,rect2)
    
  }
  
  return(rect)
  
}

read_in_samples<-function(path, sheets, firstLineSamples){
  
  library(readxl)
  library(dplyr)
  library(sqldf)
  
  samp<-data.frame()
  sampStart<-firstLineSamples-1
  sampSlut<-sampStart+2
  
  for(i in sheets) {
    
    samp0<-read_excel(path, sheet=i)
    ctry<-data.frame(samp0[3,3])
    sppName<-samp0[4,3]
    year<-samp0[5,3]
    area<-samp0[3,9]
    fleet<-samp0[4,9]
    
    head<-data.frame(ctry,sppName,year,area,fleet)
    colnames(head) <- c("ctry","sppName","year","area","fleet")
    
    sampq1<-data.frame(as.matrix(t(samp0[c(sampStart:sampSlut),c(4)])), stringsAsFactors=F)
    colnames(sampq1)<-c("noSample","noLength","noAge")
    sampq1$quarter<-1
    
    sampq2<-data.frame(as.matrix(t(samp0[c(sampStart:sampSlut),c(7)])), stringsAsFactors=F)
    colnames(sampq2)<-c("noSample","noLength","noAge")
    sampq2$quarter<-2
    
    sampq3<-data.frame(as.matrix(t(samp0[c(sampStart:sampSlut),c(10)])), stringsAsFactors=F)
    colnames(sampq3)<-c("noSample","noLength","noAge")
    sampq3$quarter<-3
    
    sampq4<-data.frame(as.matrix(t(samp0[c(sampStart:sampSlut),c(13)])), stringsAsFactors=F)
    colnames(sampq4)<-c("noSample","noLength","noAge")
    sampq4$quarter<-4
    
    samp1<-bind_rows(sampq1,sampq2,sampq3,sampq4)
    samp2<-sqldf("select * from head, samp1")
    
    samp2$noSample<-as.numeric(as.character(samp2$noSample))
    samp2$noLength<-as.numeric(as.character(samp2$noLength))
    samp2$noAge<-as.numeric(as.character(samp2$noAge))
    samp<-bind_rows(samp,samp2)
    
  }
  
  return(samp)
  
}
