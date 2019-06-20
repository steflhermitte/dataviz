# Load libraries
library(rayshader)
library(rgdal)
library(raster)
library(rgl)
library(stringr)

# Load raster data
bed = raster("~/Documents/Quantarctica3/Qreenland3/Bedmachine/BedMachine_bed.tif") 
top =  raster("~/Documents/Quantarctica3/Qreenland3/Bedmachine/BedMachine_surface.tif") 
mask =  raster("~/Documents/Quantarctica3/Qreenland3/Bedmachine/BedMachine_mask.tif") 

# Mask unncessary data
bed[mask[]==4]=cellStats(bed,stat=min)
top[mask[]==4]=cellStats(bed,stat=min)
top[is.na(mask[])]=NA

# Resample to 3000 m resolution and make a square (to allow circle croppin)g
bed <- aggregate(bed, fact=20)
bed <- extend(bed,c(0,204),value=cellStats(bed,stat=min))
bed <- crop(bed,extent(bed,1,918,1,918))
top <- aggregate(top, fact=20)
top <- extend(top,c(0,204),value=NA)
top <- crop(top,extent(top,1,918,1,918))
# Mask (for transparence) top data when it equals bed
top[top==bed]=NA
# Create common min/max for bed/top
bed[0:1]=cellStats(top,stat=max)
top[459]=cellStats(bed,stat=min)

# Rayshade the bed
dd_b = matrix(raster::extract(bed,raster::extent(bed),buffer=1000),nrow=ncol(bed),ncol=nrow(bed))
ss_b = sphere_shade(heightmap = dd_b, texture = 'desert' ,zscale=12) 

# Rayshade the top
pal = create_texture('#dcf3ff','#257ca3','#a2d2df','#5fa5c5','#ffffff')
dd_t = matrix(raster::extract(top,raster::extent(top),buffer=1000),nrow=ncol(top),ncol=nrow(top))
ss_t = sphere_shade(heightmap = dd_t,texture = pal,zscale=12) 
ss_t = add_shadow(ss_t,ray_shade(dd_t))
ss_t = add_shadow(ss_t,ambient_shade(dd_t))

# Set zoom factor for comparison with Antarctica based on rectangle width
zoom=2223/918.

# Rayshade for every 5 degrees
for (i in seq(0,360,5)){

  # Bed
  plot_3d(ss_b,dd_b,zscale=20,fov=0,theta=-i,phi=45,windowsize=c(1000,1000),zoom=0.7*zoom,
          water=TRUE, waterdepth = 0, wateralpha = 0.6,watercolor = "#4a7683",
          waterlinecolor = "white",waterlinealpha = 0.3,baseshape = "circle")
  # Save to file
  rgl.snapshot(filename=paste("/Users/steflhermitte/Downloads/rayshader_GrISAnt/Bedmac_",str_pad(i, 3, pad = "0"),"_o.png",sep=''),fmt="png")
  
  # Overlay top
  plot_3d(ss_t,dd_t,zscale=20,fov=0,theta=-i,phi=45,windowsize=c(1000,1000),zoom=0.7*zoom,
          water=FALSE, waterdepth = 0, wateralpha = 0,watercolor = "#4a7683",
          waterlinecolor = "white",waterlinealpha = 0, baseshape = "circle")
  # Label summit
  summit = rowColFromCell(top,which.max(flip(top,direction='y')))
  render_label(dd_t,x=summit[2],y=summit[2], z=8000,zscale=20, text = "3216 m",textsize = 1.2,linewidth = 1)
  # save to file
  rgl.snapshot(filename=paste("/Users/steflhermitte/Downloads/rayshader_GrISAnt/Bedmac_",str_pad(i, 3, pad = "0"),"_i.png",sep=''),fmt="png")
  rgl.close()
}
