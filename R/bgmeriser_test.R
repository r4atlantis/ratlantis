user="Stephen"
rbgmerizer( map_location = paste("C:/Dropbox/", user, "/Experiments/FSU Projects/Atlantis Model/Deep C Atlantis Model", sep=""),
            map_name = "DeepCmap",
            boundary_boxes = c(0,52),
            get_bathymetry = TRUE,
            bathymetry_layer_location = paste("C:/Dropbox/", user, "/Experiments/FSU Projects/Atlantis Model/Deep C Atlantis Model/New Shape files I downloaded/GCOOS/w98e78n31s18_isobath_selected_5-4000m", sep=""),
            bathymetry_layer_name = "gom bath wgs84",
            bathymetry_cutoff = .9,
            bathymetry_levels = c(-10, -20,-50, -200,-1000, -2000,-4000),
            bgmerizer_location = paste("C:/Dropbox/", user, "/Experiments/FSU Projects/Atlantis Model/Deep C Atlantis Model", sep=""))


#source file or use profile to set library, etc
source("C:/Repositories/r_starter_code/R_starter.R")
require(rgdal)

map_location = paste("C:/Dropbox/", user, "/Experiments/FSU Projects/Atlantis Model/Deep C Atlantis Model", sep="")
map_name = "DeepCmap"
boundary_boxes = c(0,52)
get_bathymetry = TRUE
bathymetry_layer_location = paste("C:/Dropbox/", user, "/Experiments/FSU Projects/Atlantis Model/Deep C Atlantis Model/New Shape files I downloaded/GCOOS/w98e78n31s18_isobath_selected_5-4000m", sep="")
bathymetry_layer_name = "gom bath wgs84"
bathymetry_cutoff = .9
bathymetry_levels = c(-10, -20,-50, -200,-1000, -2000,-4000)
bgmerizer_location = paste("C:/Dropbox/", user, "/Experiments/FSU Projects/Atlantis Model/Deep C Atlantis Model", sep="")
