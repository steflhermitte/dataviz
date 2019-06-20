# Load libraries
library(rayshader)
library(rgdal)
library(raster)
library(rgl)
library(stringr)

# Load raster data
bed = raster("~/Documents/Quantarctica3/Quantarctica3_BasemapScientific/TerrainModels/BEDMAP2/bedmap2_bed.tif") 
top = raster("~/Documents/Quantarctica3/Quantarctica3_BasemapScientific/TerrainModels/BEDMAP2/bedmap2_surface.tif")  

# Resample to 3000 m resolution and make a square (to allow circle croppin)g
bed <- aggregate(bed, fact=3)
bed[is.na(bed[])]=cellStats(bed,stat=min)
top <- aggregate(top, fact=3)
# Mask (for transparence) top data when it equals bed
top[top==bed]=NA
# Create common min/max for bed/top
bed[0:1]=cellStats(top,stat=max)
top[1111]=cellStats(bed,stat=min)

# Rayshade the bed
dd_b = matrix(raster::extract(bed,raster::extent(bed),buffer=1000),nrow=ncol(bed),ncol=nrow(bed))
ss_b = sphere_shade(heightmap = dd_b, texture = 'desert' ,zscale=12) 

# Rayshade the top
pal = create_texture('#dcf3ff','#257ca3','#a2d2df','#5fa5c5','#ffffff')
dd_t = matrix(raster::extract(top,raster::extent(top),buffer=1000),nrow=ncol(top),ncol=nrow(top))
ss_t = sphere_shade(heightmap = dd_t, texture = pal,zscale=12)
ss_t = add_shadow(ss_t,ray_shade(dd_t))
ss_t = add_shadow(ss_t,ambient_shade(dd_t))

# Rayshade for every 5 degrees
for (i in seq(0,360,5)){

  # Bed
  plot_3d(ss_b,dd_b,zscale=20,fov=0,theta=-i-45,phi=45,windowsize=c(1000,1000),zoom=0.7,
          water=TRUE, waterdepth = 0, wateralpha = 0.6,watercolor = "#4a7683",
          waterlinecolor = "white",waterlinealpha = 0.3,baseshape = 'circle')
  # Save to file
  rgl.snapshot(filename=paste("/Users/steflhermitte/Downloads/rayshader_GrISAnt/Bedmap_",str_pad(i, 3, pad = "0"),"_o.png",sep=''),fmt="png")
  
  # Overlay top
  plot_3d(ss_t,dd_t,zscale=20,fov=0,theta=-i-45,phi=45,windowsize=c(1000,1000),zoom=0.7,
          water=FALSE, waterdepth = 0, wateralpha = 0,watercolor = "#4a7683",
          waterlinecolor = "white",waterlinealpha = 0, baseshape = 'circle')
  # Label summit
  summit = rowColFromCell(top,which.max(flip(top,direction='y')))
  render_label(dd_t,x=summit[2],y=summit[1], z=8000,zscale=20, text = "4082 m",textsize = 1.2,linewidth = 1)
  # save to file
  rgl.snapshot(filename=paste("/Users/steflhermitte/Downloads/rayshader/Bedmap_",str_pad(i, 3, pad = "0"),"_i.png",sep=''),fmt="png")
  rgl.close()
}
