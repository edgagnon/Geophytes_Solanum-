################################################################################
# Script for calculating a world map of Annual Moisture index
# Original script from Peter W. Moonlight
# Modified by Edeline Gagnon, and published in Solanum Geophyte paper 2023
#
# The goal of this script is to split a 30 arcsec map into smaller tiles, so that
# it becomes more easier to calculate Annual MI for each tile, and merge them back together at the end.
###############################################################################

require(envirem)
require(raster)
require(SpaDES)

#setwd("C://Users/egagnon/Documents/OneDrive/Projets_Recherche/2019_Tuber_project/Input/Layer_data/")
setwd("E://Layer_data")
origin<-getwd()

#Load Altitude layer
alt<-raster("E:/Layer_data/SRTM_1km_ASC/srtm_1km.asc")


#Load Chelsa temperature and precipitation layers, which are the monthly temperature and precipitation averages from 1979 to 2013.

#temp_layers<-list.files(".", pattern="_1979-2013_V1.2_land.tif", full.names=T)
prec_layers<-list.files("./Chelsa_prec_monthly", pattern="_V1.2_land.tif", full.names=T)

#Create lists of raster layers of the temperature and precipitation data
tmp <- raster::stack(temp_layers)
tmp <-as.list(tmp)

prec <- raster::stack(prec_layers)
prec <-as.list(prec)

### ----------------------------------------------------------------
#Crop the lists to the extent of the altitude layer

#tmp2<-tmp
#for (i in 1:length(tmp))
#{tmp2[[i]]<-crop(tmp[[i]],extent(alt))
#print(i)}
#tmp2

tmp2<-lapply(tmp,crop,y=extent(alt))
print("tmp2 done")
prec2<-lapply(prec,crop,y=extent(alt))
print("prec2 done")

#prec2<-prec
#for (i in 1:length(prec))
#{prec2[[i]]<-crop(prec[[i]],extent(alt))
#print(i)}
#prec2

save.image("workspace_temp_prec.R")

### ----------------------------------------------------------------
#Split all the layers 

dir.create("splitlayers2/", showWarnings=T)
tmpdir <- file.path(getwd(), "splitlayers2")

#for (i )
lapply(tmp2,splitRaster,nx=10,ny=10,path=tmpdir)
lapply(prec2,splitRaster,nx=10,ny=10,path=tmpdir)


#Create directories corresponding to the number of tiles
nx<-10
ny<-10

toto<-paste(getwd(),"/tile_",1:(nx*ny),"/",sep="")
lapply(toto,dir.create)

setwd("C://Users/egagnon/Documents/OneDrive/Projets_Recherche/2019_Tuber_project/Input/Layer_data/splitlayers2/")


for (j in 2:(nx*ny))
{
#j<-2
grd<-paste("_tile",j,".grd",sep="")
gri<-paste("_tile",j,".gri",sep="")
tileN<-paste("tile_",j,sep="")

grd
gri
tileN

setwd(origin)

grd_layers<-list.files("./splitlayers2", pattern=grd, full.names=T)
gri_layers<-list.files("./splitlayers2", pattern=gri, full.names=T)

getwd()
grd_layers
gri_layers

destination<-paste(origin,"/",tileN,"/",sep="")
#setwd(paste(origin,"/splitlayers2/",sep=""))

destination
getwd()

file.copy(grd_layers, destination)
file.copy(gri_layers, destination)
print(paste(j,"is done!"))
}

#Next steps: copy these files onto the server
#Set up a script that can launch the calculation of all the layers and tiles

require(envirem)
library(raster)
require(SpaDES)
library(maptools)  ## For wrld_simpl

#setwd to origin
setwd(origin)

source("const.R")
source("evap.R")
source("etsrad.R")

#A list of which day of the year each month starts on
#For all twelve months
mthdy <- c(16, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)

#A list of how many days each month is
#For all twelve months
mthdys <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

alt<-raster("Q:/World/elevation_1km_resolution/SRTM_1km_ASC/srtm_1km.asc")

dir.create(paste(origin,"AnnualPET/",sep=""), showWarnings=T)
dir.create(paste(origin,"AnnualMI/",sep=""), showWarnings=T)
dir.create(paste(origin,"AnnualDD/",sep=""), showWarnings=T)

dir.create(paste(origin,"MonthlyMI/",sep=""), showWarnings=T)


#Choose a folder in which I will do all calculations

for (j in 1:(nx*ny))
{
j<-2
  tileN<-paste("tile_",j,sep="")
  destination<-paste(origin,"/",tileN,"/",sep="")
  setwd(destination)
  print(getwd())

#Then start everything else.
  
#Step 1 is getting the results file.  
  #raster(vals = sample(c(rep(NA,10),1), 100, TRUE), nrows = 10, ncols = 10, ext = extent(c(0,25,0,25)))->toto
  grd_layers<-list.files(".", pattern=".grd", full.names=T)
  gri_layers<-list.files(".", pattern=".gri", full.names=T)
  temp2_layers<-list.files(".", pattern="_1979-2013_V1.2_land.tif", full.names=T)
  prec2_layers<-list.files(".", pattern="CHELSA_prec_", full.names=T)
  
  toto3<-raster(gri_layers[1])
  
  #Read altitude data. Done before entering the index, because there is some croping that needs to get done.
  #Either getting the center of the coordinates, and then querying an elevation layer to get the center of the elevation.
  #If res(altitude) and res(r3) are different, then I will need to resample the resolution file so that they are the same.
  
  
  alt
  toto3
  print(res(alt)) 
  print(res(toto3)) # same resolution, so don't need to resample
  
  #crop alt to the extent of the tile 
  alt_t<-crop(alt,extent(toto3))
  
  
  # This csv should only include the row names for the cells that have data, i.e. those that are NOT in the sea.
  
  # This way, you don't waste time calculating values for cells in the sea
  index<-which(!is.na(values(toto3)))
  length(index)
  toto.matrix<-matrix(nrow=length(index),ncol=7)
  dim(toto.matrix)
  colnames(toto.matrix)<-c("latitude","altitude","3","4","5","6","7")
  rownames(toto.matrix)<-index
  head(toto.matrix)
  
  #Add in latitude data
  lat.data<-yFromCell(toto3,1:ncell(toto3))
  toto.matrix[,1]<-lat.data[index]
  head(toto.matrix)
  
  #r_resam <- resample(alt_t,toto3,method='bilinear')
  #altitude.r3<-r_resam[index]
  #toto.matrix[,2]<-altitude.r3
  #toto.matrix[,3]<-alt.data
  
  
  #Then, extract all the xy points from the cells of the new toto3
  xy.data<-xyFromCell(toto3,1:ncell(toto3))
  alt.data<-extract(alt_t,xy.data[index,])
  toto.matrix[,2]<-alt.data
  is.na(toto.matrix[,2])==TRUE->index.2
  toto.matrix[index.2,2]<-0
  toto.matrix
  results<-toto.matrix
  
  write.csv(toto.matrix,paste(tileN,"_temp_results.csv",sep=""))
  
  
  
  #This gets the latitude file that I need.https://stackoverflow.com/questions/35580730/raster-of-latitudes-and-longitudes
  latitude<-toto3
  xy<-coordinates(toto3)
  latitude[]<-xy[,2]
  plot(latitude)
  writeRaster(latitude, paste("latitude",tileN,".tif",sep="_"))

  
  #nb.na<-cellStats(toto,"countNA")#
  #nb.cells<-902016000
  #nb.rows<-nb.cells-nb.na #319588163
  
  dir.create(paste(destination,"PET/",sep=""), showWarnings=T)
  
  #Cycle through 12 months
  for(x in 1:12)
  {
    
      if(dir.exists(paste("PET/", x, sep="")))
        {next}
    {
      dir.create(paste(destination,"PET/",x, sep=""), showWarnings=T)
      
      writeLines(paste("...working on month", x))
      
      writeLines("   ...reading in the monthly mean temperature")
      
      #Need to read all the 12 chelsa files individually
      #Need to read all the 12 chelsa files individually
      
      #Crop all of them to the same extent as my altitude files
      
      #Divide all of them in the same number of tiles.
      
      tileN2<-paste("tile",j,sep="")
      results[,3] <- raster(paste("CHELSA_temp10_", x, "_1979.2013_V1.2_land_", tileN2,".gri", sep=""))[index]
      # most datasets have 10* the temperature in degrees. Divide your temperature by 10 to get the actual degrees.
      results[,3] <- results[,3]/10
      print(results[,3]) 
      
      writeLines("   ...calculating astronomical variables")
      # This works out where in its orbit the earth is on the month you are interested in.
      # All of this is important for calculating the amount of solar radiation in any given month at any given place on earth.
      
      # 01. Calculate the number of days in yeark (kN), days
      n <- mthdy[[x]]
      kN <- 365
      
      # 02. Calculate heliocentric longitudes (nu and lambda), degrees
      my_helio <- berger_tls(n, kN) #This requires a package stineb/rbeni, and the function is described here: https://rdrr.io/github/stineb/rbeni/src/R/etsrad.R#sym-berger_tls
      nu <- my_helio[1]
      lam <- my_helio[2]
      
      # 03. Calculate distance factor (dr), unitless
      kee <- ke^2
      rho <- (1 - kee)/(1 + ke*dcos(nu))
      dr <- (1/rho)^2
      
      # 04. Calculate the declination angle (delta), degrees
      delta <- asin(dsin(lam)*dsin(keps))
      delta <- delta/pir
      
      # 05. Calculate variable substitutes (u and v), unitless
      ru <- dsin(delta)*dsin(results[,1])
      rv <- dcos(delta)*dcos(results[,1])
      
      # 06. Calculate the sunset hour angle (hs), degrees
      hs <- -1.0*ru/rv
      hs <- base::acos(hs)
      hs <- hs / pir
      
      writeLines("   ...calculating radiation variables")
      
      # 07. Calculate daily extraterrestrial radiation (ra_d), J/m^2
      ra_d <- (86400/pi)*kGsc*dr*(ru*pir*hs + rv*dsin(hs))
      
      # 08. Calculate transmittivity (tau), unitless : transmissivity?? Conductance + aquifer thickness, what is kc and kd.
      tau_o <- (kc + kd*1)
      tau <- tau_o*(1 + (2.67e-5)*results[,2])
      
      # 09. Calculate daily photosynthetic photon flux density (ppfd_d), mol/m^2
      ppfd_d <- (1e-6)*kfFEC*(1 - kalb_vis)*tau*ra_d
      
      # 10. Estimate net longwave radiation (rnl), W/m^2
      rnl <- (kb + (1 - kb)*1)*(kA - results[,3])
      
      # 11. Calculate variable substitue (rw), W/m^2
      rw <- (1 - kalb_sw)*tau*kGsc*dr
      
      # 12. Calculate net radiation cross-over angle (hn), degrees
      hn <- base::acos((rnl - rw*ru)/(rw*rv))
      hn <- hn/pir
      
      writeLines("   ...calculating the radiation flux")
      
      # 13. Calculate daytime net radiation (rn_d), J/m^2
      rn_d <- (86400/pi)*(hn*pir*(rw*ru - rnl) + rw*rv*dsin(hn))
      results[,4] <- rn_d
      print(results[,4]) 
      
      writeLines("   ...calculating the water-to-energy conversion")
      # 14. Calculate atmospheric pressure for a given elevation
      patm <- kPo*(1 - kL*results[,2]/kTo)^(kG*kMa/(kR*kL))
      # Slope of saturation vap press temp curve, Pa/K
      s <- (17.269)*(237.3)*(610.78)*exp(17.269*results[,3]/(237.3 + results[,3]))/(237.3 + results[,3])^2
      # Enthalpy of vaporization, J/kg
      lv <- 1.91846e6*((results[,3] + 273.15)/(results[,3] + 273.15 - 33.91))^2
      # Density of water, kg/m^3
      pw <- density_h2o(results[,3], patm)
      # Psychrometric constant, Pa/K
      cp <- 1.0045714270 +
        (2.050632750e-3)*results[,3] -
        (1.631537093e-4)*results[,3]*results[,3] +
        (6.212300300e-6)*results[,3]*results[,3]*results[,3] -
        (8.830478888e-8)*results[,3]*results[,3]*results[,3]*results[,3] +
        (5.071307038e-10)*results[,3]*results[,3]*results[,3]*results[,3]*results[,3]
      cp <- (1e3)*cp
      lv <- 1.91846e6*((results[,3] + 273.15)/(results[,3] + 273.15 - 33.91))^2
      gam <- cp*kMa*patm/(kMv*lv)
      econ <- s/(lv*pw*(s + gam))
      results[,5] <- econ
      print(results[,5]) 
      
      
      writeLines("   ...calculating equilibrium evaportranspiration")
      eet_d <- (1e3)*econ*rn_d
      results[,6] <- eet_d
      print(results[,6]) 
      
      
      
      writeLines("   ...calculating potential evaportranspiration")
      pet_d <- (1 + kw)*eet_d
      pet_d <- pet_d*mthdys[[x]]
      results[,7] <- pet_d
      print(results[,7]) 
      
      mth_tmp <- raster(paste("latitude",tileN,".tif",sep="_"))
      mth_tmp[1:length(mth_tmp)] <- NA
      mth_tmp[index] <- results[,7]
      writeRaster(mth_tmp, file=paste("PET/", x,"/",x,".tif", sep=""))
      results[,3:7] <- NA
      #  removeTmpFiles(h=0.2)
    }
  }

  

  # From PET, we can now calculate the monthly PET and MI
  writeLines("... Reading PET Data")
  PET <- paste("PET/",1:12,"/",1:12, ".tif", sep="")
  PET <- stack(lapply(PET, raster))
  names(PET) <- paste("PET_", 1:12, sep="")
  
  writeLines("... Calculating Annual PET")
  annualPET <- sum(PET)
  writeRaster(annualPET, paste(origin,"/AnnualPET/",tileN,"_AnnualPET.tif",sep=""),overwrite=T)

  prec2_layers<-list.files(".", pattern="^.*prec*.*.gri$", full.names=T)
  prec <- stack(lapply(prec2_layers, raster))
  names(prec) <- paste("prec_", 1:12, sep="")
  
  writeLines("... Calculating Monthly MI")
  monthlyMI <- prec/PET #This creates a rasterBrick, which makes i
  names(monthlyMI) <- paste("MI_", 1:12, sep="")
#  writeRaster(monthlyMI, paste(origin,"/MonthlyMI/",tileN,"_MonthlyMI_non_adjusted.tif", sep=""))
  
  
  writeLines("... Calculating Annual MI")
  annualMI <- sum(monthlyMI)
  
  annualMI <- calc(monthlyMI,sum)
  writeRaster(annualMI, paste(origin,"/AnnualMI/",tileN,"_AnnualMI_non_adjusted.tif", sep=""))
  rm(annualMI)
  
  # You can then calcuate the annual drought days as below
  require(envirem)
  annualWetDays[[j]] <- growingDegDays(monthlyMI, baseTemp = 1)
  annualDroughtDays[[j]] <- 365 - annualWetDays
  writeRaster(annualDroughtDays, paste(origin,"/AnnualDD/",tileN,"_AnnualDroughtDays.tif",sep=""))

}

#setwd("E://Layer_data/AnnualMI")

setwd("/lustre04/scratch/gagnoned/climatic_variables3/AnnualMI")

library(raster)
tif_layers<-list.files(pattern="test.tif", full.names=T)
tmp <- lapply(tif_layers,raster)

pdf("AnnualMI_all.pdf")
lapply(tmp,plot)
dev.off()

AnnualMI_world<-do.call(merge,tmp)

pdf("AnnualMI_world.pdf")
plot(AnnualMI_world)
dev.off()

writeRaster(AnnualMI_world,"AnnualMI_world.tif")
