#' write_bio_prm function
#'
#' This function actually writes the bio.prm file
#' @param species_list_location where species list is located; defaults to working
#' directory
#' @param group_list_csv name of csv file with the following column headers:
#' Genus, species,common_name
#' @param traits_for_each_age_class_csv
#' @param flag_data_csv
#' @keywords biology prm, functional groups,
#' @details This function produced the bio.prm file needed for Atlantis
#' @export

write_the_bio_prm <- function(species_list_location = getwd(), group_list_csv,
                              traits_for_each_age_class_csv){
  #read in the provided data
  group_data <- read.csv(paste(species_list_location, "/",group_list_csv, sep=""),
                            header=T)
  age_class_data <- read.csv(paste(species_list_location, "/",traits_for_each_age_class_csv, sep=""),
                         header=T)
  flag_data <- read.csv(paste(species_list_location, "/",flag_data_csv, sep=""),
                        header=T)

  #grab map info from flag_data
  NumberofHabitats=as.numeric(as.character(flag_data[flag_data$Flag=="#NumberofHabitats","Value"]))
  MaxDepth=as.numeric(as.character(flag_data[flag_data$Flag=="#MaxDepth","Value"]))
  MinDepth=as.numeric(as.character(flag_data[flag_data$Flag=="#MinDepth","Value"]))
  NumberofBoxes=as.numeric(as.character(flag_data[flag_data$Flag=="#NumberofBoxes","Value"]))
  NumberofDepthLayers=as.numeric(as.character(flag_data[flag_data$Flag=="#NumberofDepthLayers","Value"]))
  MinTemp=as.numeric(as.character(flag_data[flag_data$Flag=="#MinTemp","Value"]))
  MaxTemp=as.numeric(as.character(flag_data[flag_data$Flag=="#MaxTemp","Value"]))
  MinSalt=as.numeric(as.character(flag_data[flag_data$Flag=="#MinSalt","Value"]))
  MaxSalt=as.numeric(as.character(flag_data[flag_data$Flag=="#MaxSalt","Value"]))

  #PRODUCE GROUPS FILE
  groups <- data.frame(GroupCode = group_data$group_code, Index = group_data$Index,
                       IsTurnedOn = group_data$turned_on, Name = group_data$group_code,
                       Long.Name = group_data$group_code, NumCohorts = group_data$num_of_age_classes,
                       VerticallyMigrates = group_data$vertically_migrates,
                       HorizontallyMigrates = group_data$horizontally_migrates,
                       IsFished = group_data$fished,
                       IsImpacted = group_data$impacted,
                       isTAC = group_data$TAC, GroupType = group_data$atlantis_type,
                       IsPredator = group_data$is_predator, groups$IsCover = group_data$cover,
                       IsSiliconDep = group_data$Is_silicon_dep,
                       IsAssessed = group_data$assessed,
                       IsCatchGrazer = group_data$catch_grazer,
                       isOverWinter = group_data$overwinter)
  write.csv(groups, "functionalgroups.csv")




}
