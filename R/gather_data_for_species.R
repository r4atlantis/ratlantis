#' gather_data_for_species function
#'
#' This function aids in gathering the species needed to create functional groups
#' and prm files in Atlantis
#' @param species_list_location where species list is located; defaults to working
#' director
#' @param species_list_csv name of csv file with the following column headers:
#' Genus, species,common_name
#' @keywords biology prm, functional groups,
#' @details This function aids in gathering the species needed to create functional
#' groups and prm files in Atlantis. User submits a list of potential species to
#'  be included in the model. The code gathers data on species from cloud-based,
#'  public data sources and offers potential functional groupings.
#' @export

gather_data_for_species <- function(species_list_location = getwd(), species_list_csv){
  #read in the species_list
  species <- read.csv(paste(species_list_location, "/", species_list_csv, sep=""),
                                 header=T)
  if ("Genus" %!in% names(species) |
        "species" %!in% names(species) |
        "common_name" %!in% names(species)){
      stop(message = "Species list needs all of the following columns: Genus,  species, common_name")
      }
  #add scientific name for matching
  species$scientific_name <- paste (species$Genus, species$species,
                                         sep = " ")
  species$scientific_name_underscore <- paste (species$Genus, species$species,
                                    sep = "_")

  #gather data from Fishbase
  #merge with main species list and remove specific traits from memory as function
  #progresses to minimize chance of "low memory" errors
  data(fishbase)

  #find species
  myfish <- rfishbase::findSpecies(species$scientific_name)

  #Depth
  Depth <- as.data.frame(rfishbase::getDepth(fish.data[myfish]))
  Depth$scientific_name_underscore=rownames(Depth)

  species <- merge(species,Depth, all.x = T)
  rm(Depth)


  #Length
  Length <- as.data.frame(rfishbase::getSize(fish.data[myfish], "length"))
  names(Length)[1] <- "length"
  Length$scientific_name_underscore=rownames(Length)
  species <- merge(species,Length, all.x=T)
  rm(Length)

  #Weight
  Weight <- as.data.frame(rfishbase::getSize(fish.data[myfish], "weight"))
  names(Weight)[1] <- "weight"
  Weight$scientific_name_underscore=rownames(Weight)
  species <- merge(species,Weight, all.x=T)
  rm(Weight)

  #Age
  Age <- as.data.frame(rfishbase::getSize(fish.data[myfish], "age"))
  names(Age)[1] <- "age"
  Age$scientific_name_underscore=rownames(Age)
  species <- merge(species,Age, all.x=T)
  rm(Age)


  #fishbase ID's to use for other functions

  IDS <- as.data.frame(rfishbase::getIds(fish.data[myfish]))
  IDS$scientific_name_underscore=rownames(IDS)
  names(IDS)[1] <- "idFB"
  species <- merge(species,IDS, all.x=T)
  rm(IDS)

 #using functions from Jorge Cornejo

  growthdata <- data.frame(NA)
  for(i in 1:nrow(species)){
    if(!is.na(species[i,"idFB"])){
      growth <- fb_growth (idFB = species$idFB[i] )
      growth$MestJen <- as.character(1.6*as.numeric(as.character(growth$k)))
      growth <- reshape::melt(data = growth, id.vars = c("idFB", "Genus", "Species"), measure.vars = c("Linf", "k", "t0", "M", "MestJen", "Lm", "phi"))
      growth$value <- as.numeric(as.character(growth$value))
      growth <- reshape::cast(idFB~variable, meannona, data = growth)
      growthdata <- merge(growthdata, growth, all.x = T, all.y = T)
      #this removes NA columns
      growthdata <- growthdata[names(growthdata)%in%names(growth)]
    }
  }
  species <- merge(species,growthdata, all.x=T)
  rm(growthdata)


  #length_weight gets a,b
  lwdata <- data.frame(NA)
  for(i in 1:nrow(species)){
    if(!is.na(species[i,"idFB"])){
      growth <- fb_lengthweight (idFB = species$idFB[i] )
      growth <- reshape::melt(data=growth, id.vars=c("idFB", "Genus", "Species"), measure.vars=c("a", "b"))
      growth$value <- as.numeric(as.character(growth$value))
      growth <- reshape::cast(idFB~variable, meannona, data=growth)
      lwdata <- merge(lwdata, growth, all.x=T, all.y=T)
      #this removes NA columns
      lwdata <- lwdata[names(lwdata)%in%names(growth)]
      Sys.sleep(2)
    }
  }
  species <- merge(species,lwdata, all.x=T)
  rm(lwdata)

  #to obtain trophic level
  tldata <- data.frame(NA)
  for(i in 1:nrow(species)){
    if(!is.na(species[i,"idFB"])){
      growth <- fb_tl2 (idFB = species$idFB[i], Genus = species[i,"Genus"],
                        Species = species[i,"species"] )
      growth <- reshape::melt(data = growth, id.vars = c("idFB", "Genus", "Species"),
                     measure.vars = c("tl"))
      growth$value <- as.numeric(as.character(growth$value))
      growth <- reshape::cast(idFB~variable, meannona, data = growth)
      tldata <- merge(tldata, growth, all.x = T, all.y = T)
      #this removes NA columns
      tldata <- tldata[names(tldata)%in%names(growth)]
    }
  }
  species <- merge(species,tldata, all.x=T)
  rm(tldata)

  #to obtain maturity data
  maturitydata <- data.frame(NA)
  for(i in 1:nrow(species)){
    if(!is.na(species[i,"idFB"])){
      growth <- fb_maturity (idFB = species$idFB[i])
      growth <- reshape::melt(data = growth, id.vars = c("idFB"),
                     measure.vars = c("lm", "tm"))
      #get rid of empty rows
      growth$value <- as.character(growth$value)
      growth <- growth[growth$value != "",]
      #get rid of empty rows
      if(dim(na.omit(growth))[1]>0){
      growth$value <- reshape::colsplit(growth$value, split=" ", names=c("value", "length.type"))$value
      growth$value <- as.numeric(as.character(growth$value))
      growth <- reshape::cast(idFB~variable, meannona, data = growth)
      maturitydata <- merge(maturitydata, growth, all.x = T, all.y = T)
      #this removes NA columns
      maturitydata <- maturitydata[names(maturitydata)%in%names(growth)]
      }
    }
  }
  species <- merge(species,maturitydata, all.x=T)
  rm(maturitydata)

  return (species)

}



