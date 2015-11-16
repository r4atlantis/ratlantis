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
#'  \item{kingdom}
#'  \item{TL_final}
#'  \item {Data on depth associations, specifically
#'  \itemize{
#'  \item {min_depth}
#'  \item {max_depth}
#'  }
#'  }
#'  \item {Growth information, specifically
#'  \itemize{
#'  \item {mean_a}
#'  \item {mean_b}
#'  \item {mean_to}
#'  \item {mean_K}
#'  \item {mean_M}
#'  \item {max_length}
#'  \item {mean_Loo}
#'  \item {max_age}
#'  }
#'  }
#'  \item {atlantis_type} {classification for Atlantis group type.  For Vertebrates, these can be
#'  \itemize{
#'  \item{fish}
#'  \item{bird}
#'  \item{mammal}
#'  \item{shark}
#'  }
#'  for invertebrates, see a long list at https://wiki.csiro.au/display/Atlantis/AtlantisGroupTypes
#'  }
#'  \item {group_name} {a name for the specific functional or focus group each organism
#'  belongs in; these are suggested by create_functional_groups but may need to be
#'  modified for readabilty (pick names you can remember and choose between!)}
#'  \item {group_code} {a 2 or 3-letter abbreviation for group_name}
#'  }
#' @keywords biology prm
#' @details This function uses provided information to Atlantis parameters for each
#' functional group
#' @export

generate_group_parameters <- function(species_data_location = getwd(),  species_info_groups_csv){
  species_input <- read.csv(paste(species_data_location, "/", species_info_groups_csv, sep=""),
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

  species_input_groups <- reshape2::melt(species_input, id.vars=c("atlantis_type",
    "group_name", "group_code",
    "scientific_name_underscore"),
    measure.vars = c("TL_final", "mean_M", "max_age", "min_age_reprod", "mean_a",
      "mean_b", "mean_Loo", "mean_K", "min_depth", "max_depth"))
  #melting above allows us to remove all NA rows
  species_input_groups=reshape::cast(data=na.omit(species_input_groups), atlantis_type + group_name +
      group_code ~ variable, meannona)

  #add in any groups that were missed due to all species missing all data
  species_input <- merge (unique(species_input[,c("atlantis_type", "group_name", "group_code")]),
    species_input_groups, all.x = T, all.y = T)


  return(species_input)
}




