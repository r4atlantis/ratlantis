#' execute_atlantis function
#'
#' This function calls the terminal and executes an Atlantis model
#' @param atlantis_location location of atlantis file (location may not
#' have spaces (e.g. not C:/Desktop/John Doe/bgm))
#' @param atlantis_exe name of Atlantis executable file (typically atlantismain but
#' may differ based on build)
#' @param biology_nc name of biology initial conditions file (needs to be in same
#' location as the atlantis exe file)
#' @param output_file_nc desired name for output file
#' @param run_file_prm name of initial conditions run file (needs to be in same
#' location as the atlantis exe file)
#' @param forcing_time_series_prm name of file detailing initial forcings (needs
#'  to be in same location as the atlantis exe file)
#' @param physics_prm name of initial physics file detailing eddies and scaling
#' (needs to be in same location as the atlantis exe file)
#' @param biology_prm name of biology parameter file (needs to be in same
#' location as the atlantis exe file)
#' @param harvest_prm name of harvest file (needs to be in same
#' location as the atlantis exe file)
#' @param functional_group_csv list of functional groups (needs to be in same
#' location as the atlantis exe file)
#' @param fisheries_csv list delimiting targeted fisheries (needs to be in same
#' location as the atlantis exe file)
#' @param econ_prm (optional) name of econ file (needs to be in same location as the
#' atlantis exe file)
#' @param output_folder name of folder (in same directory as atlantis.exe file)
#' where should results be stored?
#' @keywords atlantis
#' @details This function acts as a wrapper for the Atlantis C++ program.
#' @export
#' @seealso rbgmeriser

execute_atlantis <- function (atlantis_location, atlantis_exe, biology_nc, output_file_nc,
                              run_file_prm, forcing_time_series_prm,physics_prm,
                              biology_prm, harvest_prm, functional_group_csv,
                              fisheries_csv, econ_prm, output_folder
                              ){

  #first do required commands

  command_argument <- paste("cd ", atlantis_location, " && ", atlantis_exe, " -i ", biology_nc," 0 -o ",
                            output_file_nc, " -r ", run_file_prm, " -f ", forcing_time_series_prm, " -p ",
                            physics_prm, " -b ", biology_prm, " -h  ", harvest_prm,
                            " -s ", functional_group_csv,
                            " -q ", fisheries_csv, " ", " -d ", output_folder,
                            sep = "")

  #for optional parameters

  if(missing(econ_prm) == F){
    command_argument <- paste(command_argument, " -e ", econ_prm, sep = "")
  }

  shell(command_argument, intern = T)

}
