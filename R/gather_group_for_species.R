#' gather_group_for_species function
#'
#' This function aids in determining the functional group and higher order classification
#' information for species to be included in the Atlantis model
#' @param species_list_location where species list is located; defaults to working
#' directory
#' @param species_list_csv name of csv file with the following column headers:
#' Genus, species,common_name
#' @keywords biology prm, functional groups,
#' @details This function aids in gathering higher taxonomic information and Atlantis
#' group types for species to be included in an Atlantis model. User submits a list of potential species to
#'  be included in the model. The code gathers data on species from cloud-based,
#'  public data sources.
#' @export

gather_group_for_species <- function(species_list_location = getwd(), species_list_csv){
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

  #determine general grouping (e.g., fish, mammal)

  temp <- taxize::gnr_resolve(names = species$scientific_name)
  temp <- as.data.frame (temp$results)

  species$matched_name <- NA
  species$family <- NA
  species$order <- NA
  species$phylum <- NA
  species$kingdom <- NA

  for(i in 1:nrow(species)){
    temp_single <- temp[temp$submitted_name == species$scientific_name[i],]
    #for now, just take the first one that matches
    temp_single <- temp_single[temp_single$score == max(temp_single$score),][1,2]
    species$matched_name[i] <- temp_single
    a <- taxize::tax_name(query = species$matched_name[i], get = c("family", "order",
                                                                   "superclass", "class", "phylum",
                                                           "kingdom"), db = "ncbi")
    species$family[i] <- a[1,names(a) == "family"]
    species$order[i] <- a[1,names(a) == "order"]
    species$phylum[i] <- a[1,names(a) == "phylum"]
    species$kingdom[i] <- a[1,names(a) == "kingdom"]
  }


  return (species)

}



