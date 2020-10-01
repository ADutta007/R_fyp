library(raster)
library(rgdal)
#library(RStoolbox)

setwd('G:/Final_year_project_ref_reps/LST')
#pdf('lst_bagmati.pdf')
lst <- raster('lst_2019.tif')
plot (lst, main="LST")
print (lst)
 #dev.off()
#pdf('ndvi_plot.pdf')
#setwd('G:/Final_year_project_ref_reps/NDVI')
ndvi <- raster('ndvi_2019.tif')
plot (ndvi, main="NDVI")
print (ndvi)
#dev.off()
#pdf("stack.pdf")
lst_ndvi <- plot(ndvi,lst)
stck <- stack(ndvi,lst)
pairs(stck[[1:2]],main="lst-ndvi")
#dev.off()
crop_extent <- readOGR("G:/Final_year_project_ref_reps/nepal.shp")
plot(crop_extent)
ndvi_hegged <- raster("G:/Final_year_project_ref_reps/NDVI/ndvi_hegged.tif")
#plot(ndvi_hegged)
crop_extent1 <-c(80.06015 ,88.2043 ,26.34742 ,28.47311)

ndvi_cropped<-(crop(mask(ndvi_hegged,crop_extent),crop_extent1))
#ndvi_cropped<-(mask(crp,crop_extent))
ndvi_cropped <- aggregate(ndvi_cropped, fact = 0.0083/res(ndvi_cropped))
ndvi_cropped<- ndvi_cropped/10000


lst_hegged <-raster("G:/Final_year_project_ref_reps/LST_2019_MODIS_Grid_8Day_1km_LST.tif")
lst_cropped<-(crop(mask(lst_hegged,crop_extent),crop_extent1))
#ndvi_cropped<-(mask(crp,crop_extent))
lst_cropped <- aggregate(lst_cropped, fact = 0.0083/res(lst_cropped))
lst_cropped[lst_cropped <12435] <-NA
lst_cropped <-lst_cropped*.02-273.15

plot(ndvi_cropped,main="NDVI")
plot(lst_cropped,main="LST")
plot(ndvi_cropped,lst_cropped)