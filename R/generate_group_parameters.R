#' generate_group_parameters function
#'
#' This function calculates the group values needed for Atlantis
#' @param species_data_location where csv file with species data is located (defaults to
#' working directory).
#' @param  species_info_groups_csv name of csv file with the following column headers
#' (all provided by merging "gather" functions (gather_groups_for_species, gather_data_for_species,
#' gather_habitat_for_species):
#' \itemize{
#'  \item{Genus}
#'  \item{species}
#'  \item{family}
#'  \item{order}
#'  \item{class}
#'  \item{superclass}
#'  \item{phylum}
#'  \item{TLfinal}
#'  \item {Data on depth associations, specifically
#'  \itemize{
#'  \item {Min_Depth}
#'  \item {Max_Depth}
#'  }
#'  }
#'  \item {Growth information, specifically
#'  \itemize{
#'  \item {Mean_a}
#'  \item {Mean_b}
#'  \item {Mean_to}
#'  \item {Mean_K}
#'  \item {Mean_M}
#'  \item {max_Length}
#'  \item {mean_Loo}
#'  \item {max_Age}
#'  }
#'  }
#'  \item {atlantis_type} {classification for Atlantis group type.  For Vertebrates, these can be
#'  \itemize{
#'  \item{Fish}
#'  \item{Bird}
#'  \item{Mammal}
#'  \item{Shark}
#'  }
#'  for invertebrates, see a long list at https://wiki.csiro.au/display/Atlantis/AtlantisGroupTypes
#'  }
#'  \item {group_name} {a name for the specific functional or focus group each organism
#'  belongs in; these are suggested by create_functional_groups but may need to be
#'  modified for readabilty (pick names you can remember and choose between!)}
#'  \item {group_code} {a 3-letter abbreviation for group_name}
#'  }
#'
#'  @param bathymetry_levels (also used in rbgmeriser function)
#'  @param map_location location of shape file used to create bgm; defaults to working
#' directory
#' @param map_name name of map used for bgm creation; can be produced by rbgmeriser
#' function or created manually; defaults to map_for_bgmeriser (produced by
#' rbgmeriser function)
#' @param habitat_list list of habitat types to be included in the model (see
#'  gather_habitat_for_species for default list and potential options)
#' @keywords biology prm
#' @details This function uses provided information to obtain vaues for functional group,
#' generate Atlantis parameters, and build athe biology prm file needed by Atlantis.
#' @export

generate_group_parameters <- function(species_data_location = getwd(),  species_info_groups_csv){
  species_input <- read.csv(paste(species_data_location, "/", species_list_csv, sep=""),
                            header=T)
  #need to add check for rest of columns here
  if ("Genus" %!in% names(species_input) |
      "species" %!in% names(species_input) |
      "common_name" %!in% names(species_input)){
    stop(message = "Species list needs all of the following columns: Genus,
         species, common_name")
  }

  #add scientific name for matching
  species_input$scientific_name <- paste (species_input$Genus, species_input$species,
                                          sep = " ")
  species_input$scientific_name_underscore <- paste (species_input$Genus, species_input$species,
                                                     sep = "_")

  species_input <- reshape::melt(species_input, id.vars=c("atlantis_type",
                                                          "group_name", "group_code",
                                                          "scientific_name_underscore"))
  #melting above allows us to remove all NA rows
  species_input=cast(data=na.omit(species_input), atlantis_type + group_name +
                       group_code ~ variable, mean)

  #constant across all groups
  Ntoall <- 0.175438596  #ratio of N to all other elements
  drytowet=20  #ratio of wet to dry weight
  sNtN=0.273972603	#ratio of structural N to total N
  rNtN=0.726027397	#ratio of reserve N to total N
  gtot=1000000	#ratio of metric tons to grams

  #start making other columns in loop
  species_input$TmaxfromM=log(.01)/-species_input$mean_M
  species_input$MfromTmax=log(.01)/-species_input$mean_Tmax
  species_input$ypa_FUNC=NA


}




