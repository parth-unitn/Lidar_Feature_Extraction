library(rgdal)
library(lidR)
library(rgeos)

rm(list=ls())
gc()

plots<-readOGR("") ## Input plots shapefiles

for (year in c(enter_year)){

  LASfile<-paste("*.laz",sep="")  
  LAS<-readLAS(LASfile)
  LAS<-data.frame(LAS@data)
  
  LAS<-LAS[,c(1:6)]
  
  B<-read.table(".csv",header=T,sep=",") ## Input Field Data
  
  gc()
  
  LP<-SpatialPoints(cbind(LAS$X,LAS$Y))
  proj4string(LP)<-proj4string(plots)
  
  print('tempte')
  
  for (r in seq(15,15,0.5)){
    print("Radius:")
    print(r)
    
    VAR1ha<-matrix(dim(plots)[1],46,data=NA)
    VAR1ha<-data.frame(VAR1ha)
    names(VAR1ha)<-c("Plot",paste("pf",seq(10,90,10),sep=""),paste("pl",seq(10,90,10),sep=""),"Hmaxf","Hmaxl","Hmeanf","Hmeanl","Hcvf","Hcvl",paste("df",seq(10,90,10),sep=""),paste("dl",seq(10,90,10),sep=""),"B4","B2","B2_TOT")
    
    for (i in 1:dim(plots)[1]){
      
      #print(i)
      
      # Plot center XY
      X<-coordinates(plots[i,])[1]
      Y<-coordinates(plots[i,])[2]
      
      # Filter all points in LAS falling inside a circular plot with radius r
      tmp<-LAS[(sqrt((LAS$X-X)^2)<=r) & (sqrt((LAS$Y-Y)^2)<=r),]
      tmp<-data.frame(tmp)
      tmp<-tmp[tmp$Z>2,] # remove points < 2m
      
      VAR1ha$Plot[i]<-as.character(plots$PlotID[i])
      
      VAR1ha$B2[i]<-B$B2[B$PlotID==plots$PlotID[i]]
      
      
      for (p in seq(10,90,10)){
        VAR1ha[i,paste("pf",p,sep="")]<-quantile(tmp$Z[tmp$ReturnNumber==1],p/100)
      }
      for (p in seq(10,90,10)){
        VAR1ha[i,paste("pl",p,sep="")]<-quantile(tmp$Z[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns],p/100)
      }
      
      VAR1ha$Hmaxf[i]<-max(tmp$Z[tmp$ReturnNumber==1])
      VAR1ha$Hmaxl[i]<-max(tmp$Z[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns])
      VAR1ha$Hmeanf[i]<-mean(tmp$Z[tmp$ReturnNumber==1])
      VAR1ha$Hmeanl[i]<-mean(tmp$Z[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns])
      VAR1ha$Hcvf[i]<-sd(tmp$Z[tmp$ReturnNumber==1])/mean(tmp$Z[tmp$ReturnNumber==1])
      VAR1ha$Hcvl[i]<-sd(tmp$Z[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns])/mean(tmp$Z[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns])
      
      for (p in seq(10,90,10)){
        VAR1ha[i,paste("df",p,sep="")]<-length(which(tmp$Z[tmp$ReturnNumber==1]>quantile(tmp$Z[tmp$ReturnNumber==1],p/100)))/length(tmp$Z[tmp$ReturnNumber==1])
      }
      for (p in seq(10,90,10)){
        VAR1ha[i,paste("dl",p,sep="")]<-length(which(tmp$Z[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns]>quantile(tmp$Z[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns],p/100)))/length(tmp$Z[tmp$ReturnNumber!=1 & tmp$ReturnNumber==tmp$NumberOfReturns])
      }
      
      
    }
    
    write.table(VAR1ha,paste("C:\\Parth\\41_Edited_plots\\Mraduis2.csv",sep=""),quote=F,col.names=T,row.names=F,sep=",")
    
    rm(VAR1ha)
    gc()
    
  }
  
}

