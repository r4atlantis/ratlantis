#' create_biology_prm function
#'
#' This function creates the biology prm file needed for Atlantis
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
#'  \item{Trophic level information}{ One of the following for trophic level:
#'  TrophicLevel (developed by user),FoodTroph (from gather_data_for_species), or
#'  DietTroph (from gather_data_for_species).  These will be averaged.}
#'  \item{Min_Depth}{ highest depth at which a species is found; assumes fishbase style,
#'  with depths positive}
#'  \item{Max_Depth}{ lowest depth at which a species is found; assumes fishbase style,
#'  with depths positive}
#'  \item {atlantis_type} {classification for Atlantis group type.  For Vertebrates, these can be
#'  \itemize{
#'  \item{Fish}
#'  \item{Bird}
#'  \item{Mammal}
#'  \item{Shark}
#'  }
#'  for invertebrates, see a long list at https://wiki.csiro.au/display/Atlantis/AtlantisGroupTypes
#'  }
#'  \item{group_name}{ a name for the specific functional or focus group each organism
#'  belongs in; these are suggested by create_functional_groups but may need to be
#'  modified for readabilty (pick names you can remember and choose between!)}
#'  }
#'  @param bathymetry_levels (also used in rbgmeriser function)
#'  @param map_location location of shape file used to create bgm; defaults to working
#' directory
#' @param map_name name of map used for bgm creation; can be produced by rbgmeriser
#' function or created manually; defaults to map_for_bgmeriser (produced by
#' rbgmeriser function)
#' @param habitat_list list of habitat types to be included in the model (see
#'  gather_habitat_for_species for default list and potential options)
#' @keywords biology prm
#' @details This function uses provided information to suggest functional group
#' classification for Atlantis models.  Species are separated based on taxonomy,
#' depth, habitat, and trophic-level groupings.  Given the available information,
#' this focuses mostly on fish.  For depth placement, one of the following must be provided:
#' bathymetry_levels OR map_location and map_name
#'#' @keywords biology prm
#' @details This function creates the biology prm file needed by Atlantis. User
#' create functional groupings using the create_functional_groups function or supply
#' their own list with needed parameters.
#' @export

create_biology_prm <- function(species_data_location = getwd(),  species_info_groups_csv){
}




