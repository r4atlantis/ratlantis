#' gather_data_for_species function
#'
#' This function aids in gathering the species needed to create functional groups
#' and prm files in Atlantis
#' @param species_list_location where species list is located; defaults to working
#' directory
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

  #gather data from Fishbase using new fishbase R api (as of 9.4.2015)
  #merge with main species list and remove specific traits from memory as function
  #progresses to minimize chance of "low memory" errors


  species$scientific_name_validated <- rfishbase::validate_names(species$scientific_name)

  trophic_info <- rfishbase::ecology(species$scientific_name_validated,
          fields=c("SpecCode", "FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph"))

  #make sciname match column in species name
  names(trophic_info)[names(trophic_info) == 'sciname'] <- 'scientific_name_validated'

  species <- merge(species, trophic_info, all.x = T)

  #maturity information

  maturity_info <- rfishbase::maturity(species$scientific_name_validated)
  names(maturity_info)[names(maturity_info) == 'Speccode'] <- 'SpecCode'

  maturity_info_AgeMatMin <- cast (SpecCode~., value="AgeMatMin", data = maturity_info, minnona)
  names(maturity_info_AgeMatMin)[names(maturity_info_AgeMatMin) == '(all)'] <- 'Min_age_reprod'

  maturity_info_LengthMatMin <- cast (SpecCode~., value="LengthMatMin", data = maturity_info, minnona)
  names(maturity_info_LengthMatMin)[names(maturity_info_LengthMatMin) == '(all)'] <- 'Min_length_reprod'


  species <- merge(species, maturity_info_AgeMatMin, all.x = T)
    species <- merge(species, maturity_info_LengthMatMin, all.x = T)

  #population characteristics

  pop_char_info <- rfishbase::popchar(species$scientific_name_validated)
  names(pop_char_info)[names(pop_char_info) == 'Speccode'] <- 'SpecCode'

  pop_char_Wmax <- cast (SpecCode~., value="Wmax", data = pop_char_info, maxnona)
  names(pop_char_Wmax)[names(pop_char_Wmax) == '(all)'] <- 'Max_weight'

  pop_char_Lmax <- cast (SpecCode~., value="Lmax", data = pop_char_info, maxnona)
  names(pop_char_Lmax)[names(pop_char_Lmax) == '(all)'] <- 'Max_length'

  pop_char_tmax <- cast (SpecCode~., value="tmax", data = pop_char_info, maxnona)
  names(pop_char_tmax)[names(pop_char_tmax) == '(all)'] <- 'Max_age'

  species <- merge(species, pop_char_Wmax, all.x = T)
  species <- merge(species, pop_char_tmax, all.x = T)
  species <- merge(species, pop_char_Lmax, all.x = T)

  #population growth
  pop_growth_info <- rfishbase::popgrowth(species$scientific_name_validated)


  pop_growth_info_Loo <- cast (SpecCode~., value="Loo", data = pop_growth_info,
                               meannona)
  names( pop_growth_info_Loo)[names( pop_growth_info_Loo) == '(all)'] <- 'Mean_Loo'

  pop_growth_info_K <- cast (SpecCode~., value="K", data = pop_growth_info,
                               meannona)
  names( pop_growth_info_K)[names( pop_growth_info_K) == '(all)'] <- 'Mean_K'

  pop_growth_info_M <- cast (SpecCode~., value="M", data = pop_growth_info,
                             meannona)
  names( pop_growth_info_M)[names( pop_growth_info_M) == '(all)'] <- 'Mean_M'

  pop_growth_info_TL <- cast (SpecCode~., value="TL", data = pop_growth_info,
                             meannona)
  names( pop_growth_info_TL)[names( pop_growth_info_TL) == '(all)'] <- 'Mean_TL'

  pop_growth_info_to <- cast (SpecCode~., value="to", data = pop_growth_info,
                              meannona)
  names( pop_growth_info_to)[names( pop_growth_info_TL) == '(all)'] <- 'Mean_to'

  #Length max
  pop_growth_info_Lm <- cast (SpecCode~., value="Lm", data = pop_growth_info,
                              meannona)
  names( pop_growth_info_Lm)[names( pop_growth_info_Lm) == '(all)'] <- 'Mean_Lm'

  species <- merge(species, pop_growth_info_M, all.x = T)
  species <- merge(species, pop_growth_info_Lm, all.x = T)
  species <- merge(species, pop_growth_info_TL, all.x = T)
  species <- merge(species, pop_growth_info_K, all.x = T)
  species <- merge(species, pop_growth_info_Loo, all.x = T)
  species <- merge(species, pop_growth_info_to, all.x = T)


  #morphology (not currently used)
  morphology_info <- rfishbase::morphology(species$scientific_name_validated)

  #species info
  species_info <- rfishbase::species(species$scientific_name_validated)

  species_info_DepthRangeShallow <- cast (SpecCode~., value="DepthRangeShallow", data = species_info,
                              meannona)
  names(species_info_DepthRangeShallow)[names( species_info_DepthRangeShallow) == '(all)'] <- 'Min_depth'

  species_info_DepthRangeDeep <- cast (SpecCode~., value="DepthRangeDeep", data = species_info,
                                          meannona)
  names(species_info_DepthRangeDeep)[names( species_info_DepthRangeDeep) == '(all)'] <- 'Max_depth'

  species <- merge(species, species_info_DepthRangeDeep, all.x = T)
  species <- merge(species, species_info_DepthRangeShallow, all.x = T)


  return (species)

}



