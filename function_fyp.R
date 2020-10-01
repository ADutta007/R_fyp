smi <- function(ndvi_data_path,lst_data_path,shapefile) {
  library(raster)
  library(sf)
  library(dplyr)
  library(data.table)
  library(rgdal)
  ndvi_hegged<-raster((ndvi_data_path))
  lst_hegged<-raster((lst_data_path))
  crop_extent <- readOGR((shapefile))
  crop_extent1 <-c(80.06015 ,88.2043 ,26.34742 ,30.47311)
  ndvi<-(crop(mask(ndvi_hegged,crop_extent),crop_extent1))
  ndvi<- aggregate(ndvi, fact = 0.0083/res(ndvi))
  ndvi<- ndvi/10000
  lst<-(crop(mask(lst_hegged,crop_extent),crop_extent1))
  #lst <- aggregate(lst, fact = 0.0083/res(lst_cropped))
  lst[lst <12392] <-NA
  lst <-lst*.02-273.15

  plot(lst,main="LST")
  plot(ndvi,main="NDVI")

  rf<-writeRaster(lst, filename="lst_2018_nov.tif", format="GTiff", overwrite=TRUE)
  rf<-writeRaster(ndvi, filename="ndvi_2018_nov.tif", format="GTiff", overwrite=TRUE)
  #plot(ndvi,lst,main="Scatterplot")

  lst_attr <- rasterToPoints(lst)
  write.table(lst_attr,file='lst_2018_nov.csv', append=FALSE, sep= ',', row.names = FALSE, col.names=TRUE)
  lst_attr <- fread("lst_2018_nov.csv", select = c(3))
  names(lst_attr)[names(lst_attr) == colnames(lst_attr)[1]] <- "lst_data"

  ndvi_attr <- rasterToPoints(ndvi)
  write.table(ndvi_attr,file='ndvi_2018_nov.csv', append=FALSE, sep= ',', row.names = FALSE, col.names=TRUE)
  ndvi_attr <- fread("ndvi_2018_nov.csv", select = c(3))
  names(ndvi_attr)[names(ndvi_attr) == colnames(ndvi_attr)[1]] <- "ndvi_data"
  ndvi_lst<- cbind(ndvi_attr, lst_attr)
  ndvi_lst<-as.data.table(ndvi_lst)
  ndvi_lst<-round(ndvi_lst,2)

  dry_edge<- ndvi_lst %>% group_by(ndvi_data) %>% top_n(1, lst_data)
  plot_dry_edge<- plot(dry_edge$lst_data~dry_edge$ndvi_data)

  reg_dry_edge<-lm(formula=dry_edge$lst_data~dry_edge$ndvi_data, data= dry_edge)
  print(reg_dry_edge)
  print(cor(dry_edge$lst_data,dry_edge$ndvi_data))

  png(filename="G:/dry_edge/2019_april_dry_edge.png")
  coeff<-coefficients(reg_dry_edge)
  eq <- paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
  plot(dry_edge,main = eq)
  abline(reg_dry_edge, col="blue")
  dev.off()

  wet_edge<- ndvi_lst %>% group_by(ndvi_data) %>% top_n(1, -lst_data)
  plot_wet_edge<- plot(wet_edge$lst_data~wet_edge$ndvi_data)

  reg_wet_edge<-lm(formula=wet_edge$lst_data~wet_edge$ndvi_data, data= wet_edge)
  print(reg_wet_edge)
  print(cor(wet_edge$lst_data,wet_edge$ndvi_data))
  png(filename="G:/wet_edge/2019_april_wet_edge.png")

  coeff<-coefficients(reg_wet_edge)
  eq <- paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
  plot(wet_edge,main = eq)
  abline(reg_wet_edge, col="blue")
  dev.off()
  png(filename = "G:/scatterplots/scatterplot_2019_april.png")
  plot(ndvi,lst, xlab="NDVI",ylab="LST",main="Scatter plot of LST and NDVI (April 2019)")
  dev.off()

}
#smi("G:/Final_year_project_ref_reps/ndvi_stitch_2020_jan.tif","G:/Final_year_project_ref_reps/lst_stitch_2020_jan.tif","G:/Final_year_project_ref_reps/nepal.shp")

 #smi<- (((coef(reg_dry_edge)["dry_edge$ndvi_data"])*(ndvi_attr)+(coef(reg_dry_edge)["(Intercept)"]))-(lst_attr))/
    #(((coef(reg_dry_edge)["dry_edge$ndvi_data"])*(ndvi_attr)+(coef(reg_dry_edge)["(Intercept)"]))-((coef(reg_wet_edge)["wet_edge$ndvi_data"])*(ndvi_attr)+(coef(reg_wet_edge)["(Intercept)"])))
  #print(smi)