library(rgdal)
library(lidR)
library(rgeos)

rm(list=ls())
gc()

plots<-readOGR("enter_path","file_name")

LASfile<-paste("*.laz",sep="")

LAS<-readLAS(LASfile,select = "xyzirnac")


for (i in 1:dim(plots)[1]){
  
  print(i)
  
  PL<-lasclip(LAS,plots[i,])
  
  tmp<-PL@data
  
  tmp<-tmp[tmp$Z>0 & tmp$Z<45,]
  
  #-------------------------------------------------------------------------------
  #Intensity correction
  tmp$Intensity<-tmp$Intensity*((1/cos(abs(tmp$ScanAngleRank)*pi/180))^2.5)
  #-------------------------------------------------------------------------------
  
  tmpF<-tmp[tmp$ReturnNumber==1,]
  tmpL<-tmp[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns,]
  
  vartmpF<-stdmetrics(x=tmpF$X, y=tmpF$Y, z=tmpF$Z, i=tmpF$Intensity, rn=tmpF$ReturnNumber, class=tmpF$Classification, dz = 1)
  vartmpF<-data.frame(vartmpF)
  vartmpF<-vartmpF[,c(1:48)]
  names(vartmpF)<-paste(names(vartmpF),"_F",sep="")
  
  vartmpL<-stdmetrics(x=tmpL$X, y=tmpL$Y, z=tmpL$Z, i=tmpL$Intensity, rn=tmpL$ReturnNumber, class=tmpL$Classification, dz = 1)
  vartmpL<-data.frame(vartmpL)
  vartmpL<-vartmpL[,c(1:48)]
  names(vartmpL)<-paste(names(vartmpL),"_L",sep="")     
  
  vartmp<-cbind(vartmpF,vartmpL)
  
  vartmp$AGB<-plots$Biomass[i]
    
  if (i==1){
      VAR<-vartmp
  }else{
      VAR<-rbind(VAR,vartmp)
  }
  
}

write.table(VAR,"*.csv",sep=";",col.names=T,row.names=F,quote=F)
