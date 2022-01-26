.# ---------------------------------------- #
# Script to generate .stl files from topo  #
# for 3d modeling / printing               #
# ---                                      #
# T Kukla (Stanford Univ. 2021)            #
# ---------------------------------------- #
library(elevatr)
library(rayshader)
library(pracma)
library(ggplot2)
library(data.table)
library(raster)

rm(list=ls())
setwd('PATH/HERE')


## NOTES: ------------------------------------------------ # 
## [1]: Requires XQuartz to display 3D model
## [2]: 3D XQuartz model must be active on screen for successful save of .stl. 
## [3]: This specific example is Angel's Landing in Zion National Park
## ------------------------------------------------------- # 


## ----- FUNCTION TO DECREASE RESOLUTION OF A 2D GRID
approx2d = function(mat, xyout, nx = 5, ny = 5){
  
  # Adapted from package fields (Douglas Nychka, Reinhard Furrer, Stephan Sain)
  
  if(missing(xyout)){
    xyout = expand.grid(seq(1,nrow(mat),len=nx), seq(1,ncol(mat),len=ny))
  }
  
  xmid = approx(1:nrow(mat), 1:nrow(mat), xyout[,1])$y
  ymid = approx(1:ncol(mat), 1:ncol(mat), xyout[,2])$y
  
  xlo = floor(xmid)
  ylo = floor(ymid)
  
  xdiff = xmid - xlo
  ydiff = ymid - ylo
  xdiff[xlo == nrow(mat)] = 1
  ydiff[ylo == ncol(mat)] = 1
  
  xlo[xlo == nrow(mat)] = nrow(mat) - 1
  ylo[ylo == ncol(mat)] = ncol(mat) - 1
  
  out = mat[cbind(xlo,ylo)] * (1-xdiff) * (1-ydiff) + mat[cbind(xlo+1,ylo)] * xdiff * (1-ydiff) + mat[cbind(xlo,ylo+1)] * (1-xdiff) * ydiff + mat[cbind(xlo+1, ylo+1)] * xdiff * ydiff
  return(out)
  
}


# ... set xy limits
min.lat <- 37.26 ; max.lat <- 37.28
min.lon <- -112.957314 ; max.lon <- -112.930664
# ... create grid
xvec <- seq(min.lon, max.lon, length=200)
yvec <- seq(min.lat, max.lat, length=200)
x.in <- rep(xvec, length(yvec))
y.in <- rep(yvec, each=length(xvec))
# ... merge to df
df.topo <- as.data.frame(cbind(x.in, y.in)) ; colnames(df.topo) <- c('x', 'y')

# ... pull out the elev data
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
pdf_elev <- get_elev_raster(df.topo, z=13, prj = prj_dd, src = "aws")

# plot(pdf_elev)

# crop to the region of interest ['xmin', 'xmax', 'ymin', 'ymax']
e <- extent(min.lon, max.lon, min.lat, max.lat)
pdf_crop <- crop(pdf_elev, e)
# plot(pdf_crop)
# get into matrix form
mat.topo <- as.matrix(pdf_crop)

## decrease resolution [NOTE: nx in approx2d corresponds with nrow in matrix; resolution must be same]
dfz.vec <- approx2d(mat.topo, nx=200, ny=200)
mat.topo.low <- matrix(dfz.vec, nrow=200, ncol=200, byrow=T)


# Printing generate 3d model
mat.topo.low %>%
  sphere_shade() %>%
  plot_3d(mat.topo.low,zscale=20)

## NOTE: MUST SAVE WHILE 3D MODEL IS BEING DISPLAYED --------- # 
# ------------------------------------------------------------ # 
# --- SAVE RESULT -------------------------------------------- #
save_3dprint("zion_angelsLanding.stl", maxwidth = 3, unit = "in")
# ------------------------------------------------------------ #


