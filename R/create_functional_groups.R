#' create_functional_groups function
#'
#' This function uses data to suggest functional (focus) groups for an Atlantis model
#' @param species_data_location where csv file with species data is located (defaults to
#' working directory).
#' @param  species_input_csv name of csv file with the following column headers
#' (all provided by merging "gather" functions (gather_groups_for_species, gather_data_for_species,
#' gather_habitat_for_species):
#' \itemize{
#'  \item{"Genus"}
#'  \item{"species"}
#'  \item{"family"}
#'  \item{"order"}
#'  \item{"class"}
#'  \item{"superclass"}
#'  \item{"phylum"}
#'  \item{"Trophic level information"}(One of the following for trophic level:
#'  TrophicLevel (developed by user),FoodTroph (from gather_data_for_species), or
#'  DietTroph (from gather_data_for_species).  These will be averaged.)
#'  \item{Min_Depth}(highest depth at which a species is found; assumes fishbase style,
#'  with depths positive)
#'  \item{Max_Depth}(lowest depth at which a species is found; assumes fishbase style,
#'  with depths positive)
#'  }
#'  @param bathymetry_levels (also used in rbgmeriser function)
#'  @param map_location location of shape file used to create bgm; defaults to working
#' directory
#' @param map_name name of map used for bgm creation; can be produced by rbgmeriser
#' function or created manually; defaults to map_for_bgmeriser (produced by
#' rbgmeriser function)
#' @param habitat_list list of habitat types to be included in the model (see
#'  gather_habitat_for_species for default list and potential options)#'
#' @keywords biology prm
#' @details This function uses provided information to suggest functional group
#' classification for Atlantis models.  Species are separated based on taxonomy,
#' depth, habitat, and trophic-level groupings.  Given the available information,
#' this focuses mostly on fish.  For depth placement, one of the following must be provided:
#' bathymetry_levels OR map_location and map_name
#'
#' @export

create_functional_groups <- function(species_data_location = getwd(),  species_info_csv,
                                     bathymetry_levels, map_location = getwd(),
                                     map_name = "map_for_bgmeriser",
                                     habitat_list = c("Intertidal", "Sublittoral",
                                                      "Caves", "Oceanic", "Epipelagic",
                                                      "Mesopelagic", "Bathypelagic",
                                                      "Abyssopelagic", "Hadopelagic",
                                                      "Estuaries", "Mangroves", "MarshesSwamps",
                                                      "Stream", "Lakes", "Benthos","Sessile",
                                                      "Demersal", "Pelagic", "Endofauna",
                                                      "Megabenthos", "Meiobenthos", "SoftBottom",
                                                      "Sand", "Coarse", "Fine", "Level",
                                                      "Sloping", "Silt", "Mud", "Ooze", "HardBottom",
                                                      "Rocky", "Rubble", "SeaGrassBeds",
                                                      "BedsBivalve", "BedsRock", "CoralReefs",
                                                      "DropOffs", "ReefFlats", "Lagoons",
                                                      "DeepWaterCorals")){

  #read in the  species_input_csvt
  species_input <- read.csv(paste(species_data_location, "/",  species_info_csv, sep=""),
                            header=T)
  #make sure all required columns are present
  if ("Genus" %!in% names(species_input) |
      "species" %!in% names(species_input) |
      "common_name" %!in% names(species_input)
      ){
    stop(message = "Species list needs all of the following columns: Genus,  species, common_name")
  }

  if ("scientific_name" %!in% names (species_input)){
    #add scientific name for matching
    species_input$scientific_name <- paste (species_input$Genus, species_input$species,
                                            sep = " ")
  }
  if ("scientific_name_underscore" %!in% names (species_input)){
    species_input$scientific_name_underscore <- paste (species_input$Genus, species_input$species,
                                                       sep = "_")
  }

  #initially focus on fish

   species_input_fish <-  species_input[ species_input$superclass %in% c("Actinopterygii")|
                                     species_input$class %in% c("Chondrichthyes", "Myxini"),]

  #trophic level information
  #average provided information

   species_input_fish$TL <- rowMeans(cbind( species_input_fish$FoodTroph,
                                    species_input_fish$DietTroph), na.rm = T)


  #fill in with average for genus,family, order in gulf
  avgtlbygenus <- aggregate(TL~Genus,  species_input_fish, mean)
  names(avgtlbygenus)[2] <- "tlbygenus"
  avgtlbyfamily <- aggregate(TL~family,  species_input_fish, mean)
  names(avgtlbyfamily)[2] <- "tlbyfamily"
   species_input_fish <- merge( species_input_fish, avgtlbygenus, all.x = T)
   species_input_fish <- merge( species_input_fish, avgtlbyfamily, all.x = T)

   species_input_fish$TLfinal <-  species_input_fish$TL
   species_input_fish$TLcode <- NA

   species_input_fish$TLcode[is.na( species_input_fish$TLfinal) == F] <- 1

   species_input_fish$TLfinal[is.na( species_input_fish$TLfinal) == T &
                              is.na( species_input_fish$tlbygenus)==F] <-
     species_input_fish$tlbygenus[is.na( species_input_fish$TLfinal)==T & is.na( species_input_fish$tlbygenus)==F]
   species_input_fish$TLcode[is.na( species_input_fish$TLfinal) == T & is.na( species_input_fish$tlbygenus) == F]<-
    2

   species_input_fish$TLfinal[is.na( species_input_fish$TLfinal) == T & is.na( species_input_fish$tlbyfamily) == F] <-
     species_input_fish$tlbyfamily[is.na( species_input_fish$TLfinal) == T & is.na( species_input_fish$tlbyfamily) == F]
   species_input_fish$TLcode[is.na( species_input_fish$TLfinal) == T & is.na( species_input_fish$tlbyfamily) == F] <-
    3

  #for trophic level, do 1, 2, 3, 4, 5 bins
   species_input_fish$TLbin <- round( species_input_fish$TLfinal)

  #depth levels
  #make depths match up to Atlantis model cuts


  if(missing (bathymetry_levels)){
    map_area <- rgdal::readOGR(map_location, layer = map_name)
    bathymetry_levels <- sort(unique(map_area@data$botz))
   #bathymetry_levels <- sort(unique(map_area@data$botz), decreasing = T)
    bathymetry_levels <- bathymetry_levels[bathymetry_levels<=0]
    if(bathymetry_levels[length(bathymetry_levels)] != 0){
      bathymetry_levels[length(bathymetry_levels)+1] <- 0
    }
    bathymetry_levels <- c(-Inf, bathymetry_levels)
  }

   #multiply fish base depths
   species_input_fish$Min_depth <-  species_input_fish$Min_depth * -1
   species_input_fish$Max_depth <-  species_input_fish$Max_depth * -1


  #get ranges for each species
   #subtract one here in case Minimum depth falls on cut point
    species_input_fish$Min_depth [species_input_fish$Min_depth %in% bathymetry_levels] <-
      species_input_fish$Min_depth [species_input_fish$Min_depth %in% bathymetry_levels] -1
    species_input_fish$MinDepthbin <- bathymetry_levels[findInterval(species_input_fish$Min_depth,
                                                                     bathymetry_levels,
                                                                     rightmost.closed = T)+1]
    species_input_fish$MaxDepthbin= bathymetry_levels[findInterval(species_input_fish$Max_depth,
                                                                   bathymetry_levels)]
    #assume we want to include all species, so move maximum depth ones to lowest level
    species_input_fish$MaxDepthbin[!is.na(species_input_fish$Max_depth) &
                                    species_input_fish$Max_depth <  bathymetry_levels[2]]   <-
      bathymetry_levels[2]

    species_input_fish$DepthRangebin <- paste(species_input_fish$MinDepthbin,
                                             species_input_fish$MaxDepthbin, sep=",")

  #start making groupings
   species_input_fish$fish_type <- NA
   species_input_fish$atlantis_type <- NA

   #fish
   species_input_fish$fish_type[species_input_fish$superclass %in% c("Actinopterygii")] <-
     "fish"
   species_input_fish$atlantis_type [species_input_fish$superclass %in% c("Actinopterygii")] <-
     "fish"

   #shark
   species_input_fish$fish_type [species_input_fish$class %in% c("Chondrichthyes")] <-
     "shark"
   species_input_fish$atlantis_type[species_input_fish$class %in% c("Chondrichthyes")] <-
     "shark"

   #hagfish
   species_input_fish$fish_type [species_input_fish$class %in% c("Myxini")] <-
     "hagfish"
   species_input_fish$atlantis_type [species_input_fish$class %in% c("Myxini")] <-
     "hagfish"

   #make overall group, then see how they are habitat associated to determine if they
   #should be split. depth limits should fix some of these issues

   species_input_fish$group <- paste (species_input_fish$fish_type,
                                      species_input_fish$DepthRangebin,
                                      species_input_fish$TLbin,
                                      sep = ",")

   #birds

   species_input_birds <-  species_input[species_input$class %in% c("Aves"),]
   species_input_birds$atlantis_type <- "Bird"
   species_input_birds$group <- "Bird"

   #mammals

   species_input_mammals <-  species_input[species_input$class %in% c("Mammalia"),]
   species_input_mammals$atlantis_type <-  "Mammal"
   species_input_mammals$group <- "Mammal"

   #invertebrates


    #merge files
    species_input_combined <- merge(species_input_birds, species_input_fish,
                                    all.x = T, all.y = T)
    species_input_combined <- merge(species_input_combined, species_input_mammals, all.x = T, all.y = T)
    species_input_combined <- merge(species_input, species_input_combined[,names(species_input_combined) %!in%
                                                                    c("Max_depth", "Min_depth")], all.x  = T, all.y = T)

        #write .csv file showing how many are in each group and each habitat of interest
    auto_groups_number <- reshape::cast (species_input_combined, group~., length)


    names(auto_groups_number)[names(auto_groups_number) %in% c("(all)")] <- "Total"

    if (all(habitat_list %in% names(species_input_combined))){

      auto_groups_habitat <- reshape2::melt(species_input_combined,
                                            id =  "group", measure = habitat_list)
      auto_groups_habitat <- reshape::cast(auto_groups_habitat, group~variable, sum, value="value")

      auto_groups <- merge(auto_groups_number,auto_groups_habitat)

      write.csv(auto_groups, "recommended groups, totals, and habitat associations.csv")

     }else{

      print ("Habitat associations not returned since data is not present.  Please change groups to those found in species_info_csv input file")
      write.csv(auto_groups_number, "recommended groups and totals.csv")
     }

      return(species_input_combined)

}


