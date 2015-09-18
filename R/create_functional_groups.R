#' create_functional_groups function
#'
#' This function uses data to create functional (focus) groups for an Atlantis model
#' @param species_data_location where csv file with species data is located (defaults to
#' working directory).  This file must contain the following columns:
#' \itemize{
#'  \item{"Genus"}
#'  \item{"species"}
#'  \item{"Trophic level information"}{one of the following for trophic level:
#'  TrophicLevel (developed by user),FoodTroph (from gather_data_for_species), or
#'  DietTroph (from gather_data_for_species).  These will be averaged.}
#'  \
#'  \item{"parameter 2"}{Stuff}
#' }
#' Genus, species,
#' one of the following for trophic level (TrophicLevel (developed by user),
#' FoodTroph (from gather_data_for_species), DietTroph (from gather_data_for_species);
#' @param species_info_csv name of csv file with the following column headers:
#' Genus, species,common_name
#' @keywords biology prm
#' @details This function uses provided information to suggest functional group
#' classification for Atlantis models.  Species are separated based on taxonomy,
#' depth, habitat, and trophic-level groupings.  Given the available information,
#' this focuses mostly on fish
#' @export

create_functional_groups <- function(species_list_location, species_info_csv){}


