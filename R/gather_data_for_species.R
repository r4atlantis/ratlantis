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
  species_input <- read.csv(paste(species_list_location, "/", species_list_csv, sep=""),
                                 header=T)
  if ("Genus" %!in% names(species_input) |
        "species" %!in% names(species_input) |
        "common_name" %!in% names(species_input)){
      stop(message = "Species list needs all of the following columns: Genus,  species, common_name")
      }
  #add scientific name for matching
  species_input$scientific_name <- paste (species_input$Genus, species_input$species,
                                         sep = " ")
  species_input$scientific_name_underscore <- paste (species_input$Genus, species_input$species,
                                    sep = "_")

  #gather data from Fishbase using new fishbase R api (as of 9.4.2015)
  #merge with main species list and remove specific traits from memory as function
  #progresses to minimize chance of "low memory" errors

  species_input$scientific_name_validated <- NA

  for (i in 1:nrow(species_input)){
    scientific_name_validated <- rfishbase::validate_names(species_input$scientific_name[i])
    if (!is.null(scientific_name_validated)){
    species_input$scientific_name_validated[i] <- scientific_name_validated
    }
    }

  #species info, do this first to get SpecCode since all other tables may not exist,
  #leading to NA SpecCode
  species_info <- rfishbase::species(species_input$scientific_name_validated)
  names(species_info)[names(species_info) == 'sciname'] <- 'scientific_name_validated'

  species_input <- merge(species_input, species_info[, c("scientific_name_validated",
                                                         "SpecCode")], all.x = T)

  species_info_DepthRangeShallow <- reshape::cast (SpecCode~., value="DepthRangeShallow", data = species_info,
                                                   meannona)
  names(species_info_DepthRangeShallow)[names( species_info_DepthRangeShallow) == '(all)'] <- 'Min_depth'

  species_info_DepthRangeDeep <- reshape::cast (SpecCode~., value="DepthRangeDeep", data = species_info,
                                                meannona)
  names(species_info_DepthRangeDeep)[names( species_info_DepthRangeDeep) == '(all)'] <- 'Max_depth'

  species_input <- merge(species_input, species_info_DepthRangeDeep, all.x = T)
  species_input <- merge(species_input, species_info_DepthRangeShallow, all.x = T)


  #get trophic info

  trophic_info <- rfishbase::ecology(species_input$scientific_name_validated,
          fields=c("FoodTroph", "FoodSeTroph", "DietTroph", "DietSeTroph"))

  #include all.x = T in case no ecology table
  species_input <- merge(species_input, trophic_info, all.x = T)

  #maturity information

  maturity_info <- rfishbase::maturity(species_input$scientific_name_validated)
  names(maturity_info)[names(maturity_info) == 'Speccode'] <- 'SpecCode'

  maturity_info_AgeMatMin <- reshape::cast (SpecCode~., value="AgeMatMin", data = maturity_info, minnona)
  names(maturity_info_AgeMatMin)[names(maturity_info_AgeMatMin) == '(all)'] <- 'Min_age_reprod'

  maturity_info_LengthMatMin <- reshape::cast (SpecCode~., value="LengthMatMin", data = maturity_info, minnona)
  names(maturity_info_LengthMatMin)[names(maturity_info_LengthMatMin) == '(all)'] <- 'Min_length_reprod'


  species_input <- merge(species_input, maturity_info_AgeMatMin, all.x = T)
    species_input <- merge(species_input, maturity_info_LengthMatMin, all.x = T)

  #population characteristics

  pop_char_info <- rfishbase::popchar(species_input$scientific_name_validated)
  names(pop_char_info)[names(pop_char_info) == 'Speccode'] <- 'SpecCode'

  pop_char_Wmax <- reshape::cast (SpecCode~., value="Wmax", data = pop_char_info, maxnona)
  names(pop_char_Wmax)[names(pop_char_Wmax) == '(all)'] <- 'Max_weight'
  pop_char_Wmax$Max_weight[!is.finite(pop_char_Wmax$Max_weight)] <- NA


  pop_char_Lmax <- reshape::cast (SpecCode~., value="Lmax", data = pop_char_info, maxnona)
  names(pop_char_Lmax)[names(pop_char_Lmax) == '(all)'] <- 'Max_length'
  pop_char_Wmax$Lmax[!is.finite(pop_char_Wmax$Lmax)] <- NA


  pop_char_tmax <- reshape::cast (SpecCode~., value="tmax", data = pop_char_info, maxnona)
  names(pop_char_tmax)[names(pop_char_tmax) == '(all)'] <- 'Max_age'

  species_input <- merge(species_input, pop_char_Wmax, all.x = T)
  species_input <- merge(species_input, pop_char_tmax, all.x = T)
  species_input <- merge(species_input, pop_char_Lmax, all.x = T)

  #population growth
  pop_growth_info <- rfishbase::popgrowth(species_input$scientific_name_validated)


  pop_growth_info_Loo <- reshape::cast (SpecCode~., value="Loo", data = pop_growth_info,
                               meannona)
  names( pop_growth_info_Loo)[names( pop_growth_info_Loo) == '(all)'] <- 'Mean_Loo'

  pop_growth_info_K <- reshape::cast (SpecCode~., value="K", data = pop_growth_info,
                               meannona)
  names( pop_growth_info_K)[names( pop_growth_info_K) == '(all)'] <- 'Mean_K'

  pop_growth_info_M <- reshape::cast (SpecCode~., value="M", data = pop_growth_info,
                             meannona)
  names( pop_growth_info_M)[names( pop_growth_info_M) == '(all)'] <- 'Mean_M'

   pop_growth_info_to <- reshape::cast (SpecCode~., value="to", data = pop_growth_info,
                              meannona)
  names( pop_growth_info_to)[names( pop_growth_info_to) == '(all)'] <- 'Mean_to'

  #Length max
  pop_growth_info_Lm <- reshape::cast (SpecCode~., value="Lm", data = pop_growth_info,
                              meannona)
  names( pop_growth_info_Lm)[names( pop_growth_info_Lm) == '(all)'] <- 'Mean_Lm'

  species_input <- merge(species_input, pop_growth_info_M, all.x = T)
  species_input <- merge(species_input, pop_growth_info_Lm, all.x = T)
  species_input <- merge(species_input, pop_growth_info_K, all.x = T)
  species_input <- merge(species_input, pop_growth_info_Loo, all.x = T)
  species_input <- merge(species_input, pop_growth_info_to, all.x = T)


  #morphology (not currently used)
  morphology_info <- rfishbase::morphology(species_input$scientific_name_validated)

  #length-weight
  length_weight_info <- rfishbase::length_weight(species_input$scientific_name_validated)

  length_weight_info_a <- reshape::cast (SpecCode~., value="a", data = length_weight_info,
                                                   meannona)
  names(length_weight_info_a)[names( length_weight_info_a) == '(all)'] <- 'Mean_a'

  length_weight_info_b <- reshape::cast (SpecCode~., value="b", data = length_weight_info,
                                         meannona)
  names(length_weight_info_b)[names( length_weight_info_b) == '(all)'] <- 'Mean_b'

  species_input <- merge(species_input, length_weight_info_b, all.x = T)
  species_input <- merge(species_input, length_weight_info_a, all.x = T)

  return (species_input)

}



