#' create_biology_prm function
#'
#' This function creates the biology prm file needed for Atlantis
#' @param species_data_location where csv file with species data is located (defaults to
#' working directory).
#' @param  species_info_groups_csv name of csv file with species data.
#' \itemize{
#' \item {The following column headers are required
#' (all provided by merging generate_group_parameter function):
#' \itemize{
#'  \item{atlantis_type}
#'  \item{group_name}
#'  \item{group_code}
#'  \item{TL_final}
#'  \item{mean_M}
#'  \item{max_age}
#'  \item {min_age_reprod}
#'  \item {mean_a} {average coefficient from length-weight relationship}
#'  \item {mean_b} {average exponent from length-weight relationship}
#'  \item {mean_Loo}
#'  \item {mean_K}
#'  \item {min_depth}
#'  \item{max_depth}
#'  \item{larval_duration}
#'  \item{planktivore}{only used for vertebrates}
#'  \item{bear_live_young}{only used for vertebrates, (0 = no, 1 = yes) }
#'  \item{parental_care} {does the vertebrate group provides parental care for
#'   young until maturity; 0 = no, 1 = yes, -1 = semelparous so die after reproduction}
#'  \item{feed_while_spawning}{for vertebrates and age-structured inverts}
#'  \item{assessed}
#'  \item{cover}
#'  \item{silicon_dep}
#'  }
#'  }
#'  \item {The following columns are optional (will be filled in by function if not
#'  provided):
#'  \itemize{
#'  \item{num_of_age_classes}
#'  \item{catch_grazer} {is the group an opportunistic catch grazers (thief)?
#'  defaults to no = 0}
#'  \item{ca}
#'  \item{cb}
#'  \item{juv_eff}
#'  \item{adult_eff}
#'  \item{predator} {1 = yes, 0 = no, defaults based trophic level, needed for all
#'  groups}
#'  \item{jack_a}
#'  \item{jack_b}
#'  \item{optional for all living:
#'  \itemize{
#'  \item{flag_dem}{Preferred location trend (0 is top, 1 is demersal (?)));
#'  whether to weight vertical distributions towards surface or bottom layers
#'  when in depths where there were less than complete set of depth layers. defaults
#'  to 1}
#'  \item{ph_sensitive}{is species growth or non predation mortality is sensitive
#'   to pH, defaults to 0}
#'  \item{ph_sensitive_fecund}{is species fecundity sensitive to pH, defaults
#'   to 0}
#'  \item{salinity_impacts_nutrition}{Whether the species nutritional value is
#'    sensitive to salinity or pH (mainly an issue for phytoplankton), default to
#'    0}
#'  \item{ph_impacts_availablity}{# Whether the species availability to
#'    predators is sensitive to pH (mainly behaviour in fish), defaults to 0}
#'  \item{ph_impacts_growth_and_death}{# Whether the species growth or non-
#'     predation mortality is sensitive to pH, defaults to 0}
#'  \item{ph_impact_form}{Form of the pH effects model applied for the group,
#'   defaults to 0, 0 = no effect, 0 = monod, 0 = nonlinear (humped form as of
#'    Hinga 0000), 0 = linear}
#'  \item{monod_inflection_point}{Monod inflection point for pH impact function,
#'  defaults to 0}
#'  \item{optimal_pH_for_nonlinear_function}{Optimal pH for nonlinear pH
#'  impact function, defaults to 0}
#'  \item{ph_correction_scalar}{Correction scalar pH for nonlinear pH impact
#'   function, defaults to 1}
#'  \item{ph_function_coeff}{Coefficient pH for nonlinear pH impact function,
#'  defaults to 1}
#'  \item{ph_function_base}{Base coefficient pH for nonlinear pH impact function, defaults to 1}
#'  \item{ph_correction_scalar2}{Correction scalar pH for nonlinear pH impact function, defaults to 1}
#'  \item{salinity_sensitive}{Whether the species is sensitive to salinity, defaults to 0}
#'  \item{salinity_correction_scalar}{# Correction scalar salt for nonlinear salinity
#'   impact function, defaults to 1}
#'  }
#'  }
#'  \item{optional for primary producers:
#'  \itemize{
#'  \item{lysis_rate} {only primary producers defaults to 0}
#'  }
#'  }
#'  \item{optional for macrophytes:
#'  \itemize{
#'  \item{extra_mortality_macrophytes} {due to scour or nutrient input for mar, defaults to 0}
#'  }
#'  }
#'  \item{optional for catch grazers:
#'  \itemize{
#'  \item{prop_catch_available}{Availabilty of catch to opportunistic catch grazers (thieves);
#'  should be space separated vector with entry for each other functional group; defaults to 0}
#'  \item{prop_catch_exploitable}{How much of each catch is exploitable by opportunistic catch grazers (thieves);
#'  should be space separated vector with entry for each other functional group; defaults to 0}
#'  }
#'  }
#'
#'  \item{needed for all that horizontally migrate
#'  \itemize{
#'  \item{migrates_out_of_model} {defaults to 0, needed for all that horizontally
#'  migrate.  Maximum number of times juvenile OR adult stages must leave the
#'  model domain). For example if juvenile FVT leave once,but adults leave 3
#'   times then enter 3 here.  Hard group_code assumes juvenile inverts do not leave model.}
#'  \item{migrates_out_times_ad} { Migration dates (days of the year) for adults of groups
#'  with stages or for pools moving out of model domain - must be as many entries in these arrays as there
#'  are in the flagjXXXMigrate entry for that group, separated by a space. Note
#'  an entry of 0 for the flag still requires a single entry for the array (defaults
#'  to 364)}
#'  \item{migrates_in_times_ad} { Migration dates (days of the year) for adults of groups
#'  with stages or for pools returning to the model domain - must be as many entries in these arrays as there
#'  are in the migrates_out_model entry for that group, separated by a space. Note
#'  an entry of 0 for the flag still requires a single entry for the array (defaults
#'  to 0)}
#'  \item{migrates_out_times_juv} { Migration dates (days of the year) for juveniles of groups with stages
#'  moving out of model domain - must be as many entries in these arrays as there
#'  are in the flagjXXXMigrate entry for that group, separated by a space. Note
#'  an entry of 0 for the flag still requires a single entry for the array (defaults
#'  to 364)}
#'  \item{migrates_in_times_juv} { Migration dates (days of the year) for juveniles of groups with stages
#'  returning to the model domain - must be as many entries in these arrays as there
#'  are in the migrates_out_model entry for that group, separated by a space. Note
#'  an entry of 0 for the flag still requires a single entry for the array (defaults
#'  to 0)}
#'  \item{migrate_times_juv} {days entering and leaving models, separated by a space
#'  (defaults to leave 364, enter 1)}
#'  \item{migrate_period_ad} {defaults to 1 for those with horizontal migration, period of time adults of groups wiht stages of pools exit or enter model over;
#'  put to 0 to stop migration}
#'  \item{migrate_period_juv} {defaults to 1 for those wiht horizontal migration, period of time juveniles of groups with stages exit or enter model over;
#'  put to 0 to stop migration}
#'  \item{return_stock_ad} {defaults to 0, integer number identifying specific stock adult or pool migrants will return to; 0
#'  spreads uniformly across model}
#'  \item{return_stock_juv} {defaults to 0, integer number identifying specific stock juvenile migrants will return to; 0
#'  spreads uniformly across model}
#'  \item{migrants_return_ad} {proportion of migrating adult or pool biomass that return to the model domain, defaults to 1}
#'  \item{migrants_return_juv} {proportion of migrating adult or pool biomass that return to the model domain, defaults to 1}
#'  \item{prop_increase_migrant_size_ad}{Proportional increases in size while outside model domain for adults,
#'  defaults to 0}
#'  \item{prop_increase_migrant_size_juv}{Proportional increases in size while outside model domain for juveniles,
#'  defaults to 0}
#'  \item{density_depend} {is movement of group density-dependent, 0 = off,
#'  1 = sedentary, 2 = on, 3 = sticky, 4 = no explicit movement, defaults to 0 for
#'  vertebrates, 4 for others; needed for all that horizontally migrate}
#'  }
#'  }
#'  \item{optional for all stage structured inverts:
#'  \itemize{
#'  \item{seperate}{1 = seperate groups (or single pool), 0 = age structured
#'  single group, defaults to 0; at this point can only handle one recruit type
#'  for these groups}
#'  }
#'  }
#'  \item{optional for all vertebrates:
#'  \itemize{
#'  \item{reprod_strength}{# Vertebrate reproduction strength flags (1=very
#'  strong year classes possible, relative strength set using recruitRange and
#'  0=only moderate variation in year class strength possible, mainly for top
#'  predators with few young per reproductive event, relative strength set
#'  using recruitRangeFlat. defaults to 0)}
#'  \item{flag_q10}{Switch indicating whether or not efficiency of assimilation
#'  is temperature dependent, defaults 1; 0 = no (same efficiency regardless),
#'   1 = poorer when cooler, 2 = poorer when warmer}
#'  \item{habitat_depend}{defaults to 0, dependent on demersal habitat: 0 = no,
#'  1 = yes 0, defaults to 0}
#'  \item{channel}{indicating whether vertebrate group seeks channels during low
#'   tide - in tidal models}
#'  \item{Kcov_juv} {Exponent of refuge relationship with biogenic habitat, defaults to
#'  3, verts only}
#'  \item{Kcov_ad} {Exponent of refuge relationship with biogenic habitat, defaults to
#'  3, verts only}
#'  \item{Bcov_juv} {Coefficient of refuge relationship with biogenic habitat, defaults
#'  to .6, verts only}
#'  \item{Bcov_ad} {Coefficient of refuge relationship with biogenic habitat, defaults
#'  to .6, verts only}
#'  \item{Acov_juv} {scalar for relationship with biogenic habitat, defaults to 1, verts only}
#'  \item{Acov_ad} {scalar for relationship with biogenic habitat, defaults to 1, verts only}
#'  \item{swim_speed}{defaults to 12500 for fish and sharks, 15000 for birds, 20000 for sharks mammals}
#'  \item{min_move_temp} {defaults to minimum temp in model}
#'  \item{max_move_temp} {defaults to maximum temp in model}
#'  \item{prefer_allocate_reserves}{Vertebrate preference for rebuilding reserves over structure;
#'  defaults to 4 for fish, 3 for sharks, 3.5 for mammals, and 5 for birds}
#'  \item{min_length_reproduction}{if not provided, uses mininum age at reproduction and
#'  length weight relationship to estimate}
#'  \item{ fish_respiration_scaling_coefficient} {scaling of respiration vs weight, defaults .025 for fish, .021
#'  for mammals, .024 for birds, .014 for all others}
#'  \item{ fish_respiration_scaling_exponent} {exponent of respiration vs weight, defaults to .8}
#'  \item{minimim_oxygen_vertebrates}{minimum tolerated oxygen level for age
#'  structured groups (typically vertebrates)}
#'  \item{spawning_formulation_constant} {constant in spawning formulation, defaults to guild values}
#'  \item{spawning_formulation_fraction} {defaults to guild values}
#'  \item{stock_recruitment_scalar} {recruitment parameter for diffferent stocks of verts, defaults to 1
#'  for all stocks}
#'  \item{recovery_multiplier}{Recruitment modifiers to encourage recovery of
#'  stocks, defaults to 1}
#'  \item{recover_start_date}{day when recovery will start, only used when recruit_group_code
#'  is set to 8, defaults to 999 for dummy value}
#'  \item{min_spawn_temp} {defaults to minimum temp in model}
#'  \item{max_spawn_temp} {defaults to maximum temp in model}
#'  \item{min_spawn_salinity} {defaults to minimum temp in model}
#'  \item{max_spawn_salinity} {defaults to maximum temp in model}
#'  \item{stock_structure}{defaults to 1 for each box, not sure what this is, needs to
#'  be 1 entry for each box separated by space}
#'  }
#'  }
#'  \item{optional for all vertebrates and stage structured inverts:
#'  \itemize{
#'  \item{ext_reprod} {does group reproduce outside model area? 1 = yes, 0 = no,
#'  required for all vertebrates and stage-structured invertebrates, defaults to 0}
#'  \item{local_recruit} {defaults to 0,
#'  1 = demersal and piscivorous fish recruit at parental locations, 0 = independent distribution}
#'  item{flag_temp_sensitive}{Temperature sensitivty; defaults to 0 = no, 1 = yes}
#'  \item{recruit_group_code} {flag recruit, needed for verts and stage structured invertebrates,
#' Vertebrate reproduction related flags. The flagrecruit entries refer to the recruitment function used.
#' 1=const, 2=dependent on prim producers (Chla), 3=Beverton-Holt, 4=lognormal, 5=dependent on all plankton
#' groups not just Chla, 6=Bev-Holt with lognormal variation added, 7=Bev-Holt with encourage recovery
#' 8=Bev-Holt with perscribed recovery, 9=Ricker, 10=Standard Bev-Holt (no explict use of spawn included)
#' 11=pupping/calving linearly dependent on maternal condition, 12=pupping/calving a fixed number per adult
#' spawning, 13=forced timeseries of recruitment, defaults to  12 for birds and mammal, 10 for fish and sharks}
#' \item{recruits_per_individual_per_year}{needed for vertebrates and stage-structured invertebrates, but only used for
#' groups with recruit_group_code = 10 or 12; defaults to 1 (stable population) for each stock; represents biomass of new recruit for
#' stage-structured inverts}
#' \item{primary_production_impact_recruitment}{Coefficient relating primary
#'  production to recruitment, linked to recruit_group_code 2, defaults to
#'  999 for dummy value that is not used}
#'  \item{ricker_alpha}{parameter needed for recruit_group_code 9, defaults to
#'  999 here for dummy value}
#'  \item{ricker_beta}{parameter needed for recruit_group_code 9, defaults to
#'  999 here for dummy value}
#'  \item{beverton_holt_alpha}{parameter needed for recruit_group_code 3, defaults to
#'  999 here for dummy value}
#'  \item{beverton_holt_beta}{parameter needed for recruit_group_code 3, defaults to
#'  999 here for dummy value}
#'  \item{structural and reserve weight of recruits is calculated internally;
#'  results are output to "parameters_for_age_classes_of_groups.csv"}
#'  }
#'  }
#'  \item{optional for all fish:
#'  \itemize{
#'  \item{other_fish_mortality} {mortality due to fish not included
#'  in model), value per quarter, separated by a space; defaults to 0}
#'  \item{mammal_bird_mortality} {Static seabird and mammal induced mortality of
#'  each fish group, per quarter, separated by a space; defaults to 0}
#'  }
#'  }
#'  \item{optional for all predators:
#'  \itemize{
#'  \item{gape_lower_limit} {defaults to .01, lower Gape size for predators, to determine available prey fish groups.  Required for all
#'  predators}
#'  \item{gape_upper_limit} {defaults to 1, upper Gape size for predators, to determine available prey fish groups.  Required for all
#'  predators}
#'  \item{population_refuge}{Seed bank/population refuge based on consumer intake, if available
#'   food below this feeding slows/stops so can't eat out all food. only used by
#'    model for predcase =4; defaults to 1 for filler}
#'   \item{saturation_level}{Saturation levels for consumer food intake. only used by model for predcase = 4; defaults to 1 for filler}
#'   \item{search_volume_invert}{Search volume for invertebrate predators -
#'   used by size specific Holling type III (predcase 5); defaults to 999 here for filler value  }
#'   \item{search_volume_coefficient}{Search volume coefficient for vertebrate predators-
#'   used by size specific Holling type III (predcase 5);
#'   defaults to planktivorous = 2, pelagic piscivores = 5, demersal/reef = 1}
#'   \item{search_volume_exponent}{Search volume exponenent for vertebrate predators -
#'   used by size specific Holling type III (predcase 5);
#'   defaults to .5 for birds and sharks and .35 for mammals and fish}
#'   \item{handling_time_invert}{handling time for invertebrate predators -
#'   used by size specific Holling type III (predcase 5); defaults to 999 here for filler value  }
#'   \item{handling_time_coefficient}{handling time coefficient for vertebrate predators-
#'   used by size specific Holling type III (predcase 5);
#'   ; defaults to 999 here for filler value  }
#'   \item{handling_time_exponent}{handling time exponenent for vertebrate predators -
#'   used by size specific Holling type III (predcase 5);
#'   ; defaults to 999 here for filler value  }
#'  }
#'  }
#'  \item{optional for all consumers:
#'  \itemize{
#'  \item{active} { 2 = no preference, 1 = day, 0 = night,defaults to 2, needed
#'  for all consumers}
#'  \item {sediment_penetration_depth}{Depth consumers can dig into, are found down to in the sediment;
#'  defaults to .1 for lg_inf; .1 for fish, mammals, sharks, and birds; and .001 for others }
#'  \item {assimilation_efficiency_on_plants}{defaults to .5 for invertebrates,
#'  .1 for vertebrates}
#'  \item {assimilation_efficiency_on_live_food_and_carrion}{defaults to .5 for invertebrates,
#'  .1 for vertebrates}
#'  \item {assimilation_efficiency_on_labile_detritus}{defaults to .3 for bacteria,
#'  .2 for filter feeders, and .1 for everything else}
#'  \item {assimilation_efficiency_on_refractory_detritus}{defaults to .5 for bacteria,
#'  .3 for filter feeders, and .1 for everything else}
#'  \item{food_lost_to_detritus}{Fraction of non-assimilated lost to detritus, defaults to 0}
#'  \item{labile_detritus_lost_to_detritus}{Fraction of non-assimilate from
#'  labile detrital food lost to detritu, defaults to 0}
#'  \item{refractory_detritus_lost_to_detritus}{Fraction of non-assimilate from
#'   refractory detrital food lost to detritus, defaults to 0}
#'  \item{mortality_contribution_to_detritus}{Fraction of mortality that goes to
#'  detritus, defaults to .3}
#'  }
#'  }
#'  \item{k_tur} {defaults to .1}{needed for epibenthic groups that are mobile or infaunal (SM_INF, LG_INF, MOB_EP_OTHER)}
#'  \item{k_irr} {defaults to 1, needed for epibenthic groups that are infaunal
#'   (SM_INF, LG_INF))}
#'  \item{temp_coefft_a} {defaults to .851, needed for all living things,
#'  Coefficient A in Gary Griffith temperature function}
#'  \item{q10} {defaults to 2, Exponent in temperature effect on rate parameters}
#'  \item{q10_method} {defaults to 0, method of calculating Q10 0 is the
#'  'normal' way of calculating it. 1 is the 'new' climate change method from
#'  Gary G.}
#'  \item{q10_optimal_temp} {defaults to 0, optimum temperature of each
#'  functional group - this is only read in for groups where the q10_method is 1.}
#'  \item{q10_correction} {defaults to 0, the q10 correction factor for each
#'  functional group - this is only read in for groups where the q10_method is 1.}
#'  \item{wc} {defaults to .01}
#'  \item{turned_on} {defaults to 1 (=yes, 0=no)  groups$IsTurnedOn=1}
#'  \item{overwinter} {defaults to 0}
#'  \item{max}{space restrictions for basal (epibenthic and some infauna) groups, defaults to
#'  5000}
#'  \item{low}{Threshold spatial factors for filter feeders if using ERSEM formulation, Little space limitation (pop too small) }
#'  \item{thresh}{Threshold spatial factors for filter feeders if using ERSEM formulation }
#'  \item{sat}{Threshold spatial factors for filter feeders if using ERSEM formulation, Interference to uptake due to shading }
#'  \item{home_range} {defaults to 1, all epxcept primary producers}
#'  \item{overlap} {defaults to 1, all except primary producers}
#'  \item{stock_availability} {verts only, scalars to determine stock availablity for adults, defaults to 1 assuming no
#'  stocks considered (just one big group, should be vector the same length as number of stocks)}
#'  \item{stock_availability_juv} {verts only, scalars to determine availablity for juveniles, defaults to 1
#'  assuming no stocks considered (just one big group), should be vector the same length as number of stocks}
#'  \item{linear_mortality} {tuning parameter needed for each stage for all living organisms; defaults to 0}
#'  \item{quadratic_mortality} {tuning parameter needed for each stage for all living organisms; defaults to 0}
#'  \item{juvenile_quadratic_mortality} {tuning parameter, defaults to 0, needed for stage-structured inverts
#'  and vertebrates}
#'  \item{starve} {mortality due to starvation, only for vertebrates, defaults to 0}
#'  \item{oxygen_depth_mortality} {tuning parameter, Half oxygen mortality depth, inverts except
#'  primary producers,defaults to .001}
#'  \item{ambient_oxygen_mortality} {tuning parameter, oxygen dependent mortality due to ambient conditions, inverts except
#'  primary producers, defaults to .01}
#'  \item{lethal_oxygen_level} {lethal oxgyen level, inverts except primary producers,defaults to .5}
#'  \item{limiting_oxygen_level} {limiting oxygen level, inverts except primary producers, defults to 10}
#'  \item{other_fish_mortality} {fish only, defaults to 0 (mortatlity due to fish not included
#'  in model), value for each season, separated by a space (defaults to 0)}
#'  \item{mS_SB} {verts only, defaults to 0 (mortatlity due to birds and mammals
#'  not included in model), value for each season, separated by a space}
#'  \item{FSP} {defaults to guild values, verts only}
#'  \item{vert_stock_struct} {verts only, defaults to 1,input as number for each box
#'  separated by a space}
#'  \item{pop_ratio_stock} {defaults to 1}
#'  \item{remin_contrib}{ for small_infaunal, defaults to 0}
#'  \item{in_WC} {defaults to 0}
#'  \item{in_sed} {defaults to 0}
#'  \item{epi} {default to 0}
#'  \item{vertically_migrates} {defaults to 0}
#'  \item{horizontally_migrates} {defaults to 0}
#'  \item{fished} {defaults to 0, is it targeted in fisheries}
#'  \item{impacted} {defaults to 0, is it impacted by fisheries}
#'  \item{TAC} {defaults to 0, is it controlled by a TAC}
#'  \item{pred_case} {needed for all predators, defaults to 0, Predation
#'  formulation switches. 0=Holling type II, 1=Holling type I, 2=Holling type
#'  III, 3=ECOSIM (currently disabled), 4=min-max, 5=Size specific Holling type III}
#'  \item{KI} {default to 0, light saturation, needed for primary producers}
#'  \item{KS} {default to 0, Half-sat const for PL growth on Si mg Si m-3}
#'  \item{KF} {default to 0, Half-sat const for PL growth on Micro-nutrient}
#'  \item{KN} {default to 0, Primary producer nutrient requirements   }
#'  \item{num_of_genotypes}{defaults to 1}
#'  \item{num_of_stages}{defaults to 2 for vertebrates, 1 for others}
#'  \item{num_of_stocks}{defaults to 1}
#'  \item{num_of_spawns}{{defaults to 1}}
#'  \item{num_of_age_class_size}{set by internal function using maximum age and
#'  number of cohorts}
#'  \item{cultured}{defaults to 0}
#'  }
#'  }
#'  }
#  need to add file here wiht mum, clearance, and other values. for now these just default
#  to value in group_code to keep everything clean
#  @param invert_mum_and_clearance_csv name of csv file that must contain the
#  following column headers
#  \itemize{
#  \item{group_code}
#  \item{mum}
#  \item{clearance}
#  \item{KI}{Light saturation}
#  \item{KN}{Half-sat const for PL growth on Micro-nutrient}
#  \item{KS}{Half-sat const for PL growth on Si mg Si m-3}
#  \item{KF}{Half-sat const for PL growth on Micro-nutrient}
#   item{max}{space restrictions for basal (epibenthic and some infauna) groups}
#   \item{in_WC} {defaults to 0}
#  \item{in_sed} {defaults to 0}
#  \item{epi} {default to 0}
#  \item{vertically_migrates} {defaults to 0}
#  \item{horizontally_migrates} {defaults to 0}
#  }
#' @param flag_data_csv csv file containing major flag values for model
#' @details This function creates the biology prm file needed by Atlantis.
#' @keywords biology prm
#' @export

create_biology_prm <- function(species_data_location = getwd(),  species_info_groups_csv,
                               flag_data_csv){

  species_input <- read.csv(paste(species_data_location, "/", species_info_groups_csv, sep=""),
    header = T, strip.white = T)

  flag_data <- read.csv(paste(species_data_location, "/",flag_data_csv, sep=""),
                        header=T)

  #grab map info from flag_data
  NumberofHabitats <- as.numeric(as.character(flag_data[flag_data$Flag == "#NumberofHabitats","Value"]))
  MaxDepth <- as.numeric(as.character(flag_data[flag_data$Flag == "#MaxDepth","Value"]))
  MinDepth <- as.numeric(as.character(flag_data[flag_data$Flag == "#MinDepth","Value"]))
  NumberofBoxes <- as.numeric(as.character(flag_data[flag_data$Flag == "#NumberofBoxes","Value"]))
  NumberofDepthLayers <- as.numeric(as.character(flag_data[flag_data$Flag == "#NumberofDepthLayers","Value"]))
  MinTemp <- as.numeric(as.character(flag_data[flag_data$Flag == "#MinTemp","Value"]))
  MaxTemp <- as.numeric(as.character(flag_data[flag_data$Flag == "#MaxTemp","Value"]))
  MinSalt <- as.numeric(as.character(flag_data[flag_data$Flag == "#MinSalt","Value"]))
  MaxSalt <- as.numeric(as.character(flag_data[flag_data$Flag == "#MaxSalt","Value"]))



  #constant across all groups
  Ntoall <- 0.175438596  #ratio of N to all other elements
  drytowet <- 20  #ratio of wet to dry weight
  sNtN <- 0.273972603	#ratio of structural N to total N
  rNtN <- 0.726027397	#ratio of reserve N to total N
  gtot <- 1000000	#ratio of metric tons to grams

  #start making other columns in loop
  species_input$T_max_from_M <- log(.01)/-species_input$mean_M
  species_input$M_from_T_max <- log(.01)/-species_input$max_age
  species_input$ypa_FUNC <- NA
  species_input$mat_FUNC <- NA


  #set number of age classes if not provided by user
    if( "num_of_age_classes" %!in% names(species_input)){

    for (i in 1:nrow(species_input)){
      if(species_input$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
        species_input$num_of_age_classes[i] <- 10
      }else {species_input$num_of_age_classes[i] <- 1}
    }
    } else {
      #make sure user provided all values
      if( !is.numeric(species_input$num_of_age_classes)){
        stop(message = "Provided number of age classes are not all numeric")
      }
      if( any(is.na(species_input$num_of_age_classes))){
        stop(message = "Provided number of age classes contains an NA")
      }
    }


  #use independent M estimate if it exists to calculate years per class
  # for vertebrate groups
  for (i in 1:nrow(species_input)){
    if(species_input$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      if(is.na(species_input$mean_M[i]) == T){
        species_input$ypa_FUNC[i] <- species_input$max_age[i]/species_input$num_of_age_classes[i]
      }

      else{species_input$ypa_FUNC[i] <- species_input$T_max_from_M[i]/species_input$num_of_age_classes[i]}
    }
  }

  #for vertebrates, determine age class where maturity starts
  for (i in 1:nrow(species_input)){
    if(species_input$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      species_input$mat_FUNC[i]=min(ceiling(species_input$min_age_reprod[i]/species_input$ypa_FUNC[i]), species_input$num_of_age_classes[i])
    }
    #set maturity at 0 for inverts, other that are just pools
    else{species_input$mat_FUNC[i]=0}
  }

  #energetics from Hanson 1997 (Horne 2010)
  #maximum feeding rate (clearance) based on allometric relationships between weight and feeding
  #see Anderson 2010 for example
  cageneral <- .3
  cbgeneral <- .7

  if( "ca" %!in% names(species_input)){
  species_input$ca <- cageneral
  }
  if( "cb" %!in% names(species_input)){
  species_input$cb <- cbgeneral
  }

  #calculate mum and clearance for vertebrates, also need weights, all for average individual in each size class

  #for classes 1-10
  #produce separate table with info on size classses for vertebrates
  #list variables to be included here
  to_be_included <- c("atlantis_type", "AgeClass", "ActualAge", "WetWeight",
    "StructN", "ResN", "mum", "clearance", "Decay", "PropAgeDist", "PropJuv", "PropAdults",
    "PropBiomass", "PropSpawning", "length")

  #make matrix to hold data
  #take number of ages classes plus a 0 class for all vertebrate
  mean_individual_morphology <- matrix(NA, sum(sum(species_input[species_input$atlantis_type
    %in% c("bird", "fish", "mammal", "shark"),]$num_of_age_classes),
    length(species_input[species_input$atlantis_type %in% c("bird", "fish", "mammal", "shark"),]$group_code)),
    length(to_be_included)+1)
  mean_individual_morphology <- as.data.frame(mean_individual_morphology)
  names(mean_individual_morphology)=c("group_code", to_be_included)

  #loop over species, variables, and age classes
  #consider recruits as 0 age class

  #to create this, loop over group_code, age classes, and use counter
  counter <- 1

  for(i in 1:nrow(species_input)){
    #add one for larval group
    if(species_input$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      for (j in 1:(species_input[species_input$group_code ==
          species_input$group_code[i], "num_of_age_classes"]+1)){
        mean_individual_morphology$atlantis_type[counter] =
          as.character(species_input[species_input$group_code ==
              species_input$group_code[i], "atlantis_type"])
        mean_individual_morphology$group_code[counter] <- as.character(species_input$group_code[i])
        mean_individual_morphology$AgeClass[counter] <- j-1
        counter <- counter+1
      }}
  }

  #fill in information with loops

  #do age classes and wet weights for verts
  #for each age class, this finds actual age, uses VB equation to estimate L, and
  #then use length-weight relationship to estimate wet weight

  for (i in 1:nrow(mean_individual_morphology)){
    if(mean_individual_morphology$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      mean_individual_morphology$ActualAge[i] <- as.numeric(mean_individual_morphology$AgeClass[i]*
        species_input[species_input$group_code == mean_individual_morphology$group_code[i], "ypa_FUNC"])
      if(mean_individual_morphology$AgeClass[i] > 0){
        mean_individual_morphology$WetWeight[i]  <- species_input[species_input$group_code ==
            mean_individual_morphology$group_code[i], "mean_a"]*
          (species_input[species_input$group_code == mean_individual_morphology$group_code[i],
            "mean_Loo"]*(1-exp(-1
            *species_input[species_input$group_code == mean_individual_morphology$group_code[i],
              "mean_K"]*
              mean_individual_morphology$ActualAge[i]
          )))^species_input[species_input$group_code == mean_individual_morphology$group_code[i],
            "mean_b"]

        mean_individual_morphology$length[i] <-  (mean_individual_morphology$WetWeight[i]/
                                                    species_input[species_input$group_code == mean_individual_morphology$group_code[i],
                                                                  "mean_a"])^(1/species_input[species_input$group_code == mean_individual_morphology$group_code[i],
                                                                                              "mean_b"])


        if(mean_individual_morphology$atlantis_type[i] %in% c("fish", "shark")){
          mean_individual_morphology$Decay[i]<- exp(-1*species_input[species_input$group_code
            == mean_individual_morphology$group_code[i], "mean_M"]*
              (mean_individual_morphology$AgeClass[i]-1)*species_input[species_input$group_code
                == mean_individual_morphology$group_code[i], "ypa_FUNC"])
        } else if(mean_individual_morphology$atlantis_type[i] == "mammal"){
          #need to add silers group_code
          mean_individual_morphology$Decay[i] <- exp(-1*species_input[species_input$group_code
            == mean_individual_morphology$group_code[i], "mean_M"]*
              (mean_individual_morphology$AgeClass[i]-1)*species_input[species_input$group_code
                == mean_individual_morphology$group_code[i], "ypa_FUNC"])
        }else if(mean_individual_morphology$atlantis_type[i] == "bird"){
        #birds shoudl be constant
          mean_individual_morphology$Decay[i] <- exp(-1*species_input[species_input$group_code ==
              mean_individual_morphology$group_code[i], "mean_M"]*
              (mean_individual_morphology$AgeClass[i]-1)*species_input[species_input$group_code ==
                  mean_individual_morphology$group_code[i], "ypa_FUNC"])
        }


      } else {
        mean_individual_morphology$WetWeight[i] <- species_input[species_input$group_code ==
            mean_individual_morphology$group_code[i], "mean_a"] *

          (species_input[species_input$group_code == mean_individual_morphology$group_code[i],
            "mean_Loo"]*(1-exp(-1
            *species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mean_K"]*
              species_input[species_input$group_code == mean_individual_morphology$group_code[i],
                "larval_duration"]/365
          )))^species_input[species_input$group_code == mean_individual_morphology$group_code[i],
            "mean_b"]
      }
    }
  }



  mean_individual_morphology$StructN <- mean_individual_morphology$WetWeight*Ntoall/drytowet*sNtN*1000
  mean_individual_morphology$ResN <- mean_individual_morphology$WetWeight*Ntoall/drytowet*rNtN*1000

  #mum and clearance for verts

  for (i in 1:nrow(mean_individual_morphology)){
    if(mean_individual_morphology$AgeClass[i] > 0){
      mean_individual_morphology$clearance[i] <- species_input[species_input$group_code ==
                                                                 mean_individual_morphology$group_code[i],
                                                               "ca"]*(mean_individual_morphology$StructN[i] +
          mean_individual_morphology$ResN[i])^species_input[species_input$group_code == mean_individual_morphology$group_code[i], "cb"] * .8 * 3.3
      mean_individual_morphology$mum[i] <- 10 * mean_individual_morphology$clearance[i]
      mean_individual_morphology$PropAgeDist[i] <- mean_individual_morphology$Decay[i]/
        sum(mean_individual_morphology[mean_individual_morphology$AgeClass > 0 & mean_individual_morphology$group_code == mean_individual_morphology$group_code[i],
          "Decay"])
      if(mean_individual_morphology$AgeClass[i] < species_input[species_input$group_code ==
          mean_individual_morphology$group_code[i], "mat_FUNC"]){
        mean_individual_morphology$PropJuv[i] <- mean_individual_morphology$Decay[i]/
          sum(mean_individual_morphology[mean_individual_morphology$AgeClass > 0 & mean_individual_morphology$group_code == mean_individual_morphology$group_code[i] &
              mean_individual_morphology$AgeClass < species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mat_FUNC"],
            "Decay"])
        mean_individual_morphology$PropAdults[i] = 0
        if(mean_individual_morphology$AgeClass[i] < (species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mat_FUNC"] - 1)){
          mean_individual_morphology$PropSpawning[i] = 0}
        else{mean_individual_morphology$PropSpawning[i] = .2}

      } else{
        mean_individual_morphology$PropJuv[i] <- 0
        mean_individual_morphology$PropAdults[i] <- mean_individual_morphology$Decay[i]/
          sum(mean_individual_morphology[mean_individual_morphology$AgeClass>0 & mean_individual_morphology$group_code == mean_individual_morphology$group_code[i]&
              mean_individual_morphology$AgeClass >= species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mat_FUNC"],
            "Decay"])
        if(mean_individual_morphology$AgeClass[i] == species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mat_FUNC"]){
          mean_individual_morphology$PropSpawning[i] = .8}
        else{mean_individual_morphology$PropSpawning[i]  = 1}
      }


    } else{
      mean_individual_morphology$mum[i] <- (mean_individual_morphology$StructN[i]+
          mean_individual_morphology$ResN[i])/(species_input[species_input$group_code == mean_individual_morphology$group_code[i], "larval_duration"]/365)/365
    }
  }


  #figure out invert mum and clearance formulas, KI, KN, KS, KF
  #for now, use values from other models
  #invert_mum_clearance <- read.csv("invert_mum_and_clearance.csv")
#   mean_individual_morphology <- merge (mean_individual_morphology, invert_mum_clearance,
#     all.x=T, all.y=T)


  #merge not vert species in model with species_input sheet
  # invert_params <- merge(unique(species_input[
   #species_input$atlantis_type %!in% c("fish", "mammal", "bird", "shark"),c(
  #  "group_code", "atlantis_type")]), INVERTSHET)
  #merge this info into mean_individual_morphology

  mean_individual_morphology <- merge (unique(species_input[,c("group_code", "atlantis_type")]),
                                              mean_individual_morphology,all.x = T,
                                              all.y = T)
  mean_individual_morphology$clearance[is.na(mean_individual_morphology$clearance)] <- 1
  mean_individual_morphology$mum[is.na(mean_individual_morphology$mum)] <- 1

  #parameters needed for availability calculator
    if ("juv_eff" %!in% names(species_input)) {species_input$juv_eff <- .1}
    if ("adult_eff" %!in% names(species_input)) {species_input$adult_eff <- .1}

  for (i in 1:nrow(species_input)){
    average_individual <- mean_individual_morphology[mean_individual_morphology$group_code ==
  species_input$group_code[i],]
    average_juvenile=mean_individual_morphology[mean_individual_morphology$group_code ==
        species_input$group_code[i] &
        mean_individual_morphology$AgeClass<species_input$mat_FUNC[i]
      & mean_individual_morphology$AgeClass!=0,]
    average_adult=mean_individual_morphology[mean_individual_morphology$group_code ==
        species_input$group_code[i]&
        mean_individual_morphology$AgeClass>=species_input$mat_FUNC[i],]

    #juvenile calculations
    species_input$avgjuvN[i] <- sum(average_juvenile$WetWeight*average_juvenile$PropJuv) *
      Ntoall * 1000/drytowet
    species_input$JuvGrowthRate[i] <- sum(average_juvenile$mum * average_juvenile$PropJuv) /
      species_input$avgjuvN[i]
    species_input$Juvclearance[i] <- sum(average_juvenile$clearance * average_juvenile$PropJuv)

    #adult calculations
    species_input$avgadultN[i] <- sum(average_adult$WetWeight*average_adult$PropAdult)*Ntoall*1000/drytowet
    species_input$AdultGrowthRate[i] <- sum(average_adult$mum*average_adult$PropAdult)/species_input$avgadultN[i]
    species_input$Adultclearance[i] <- sum(average_adult$clearance*average_adult$PropAdult)

  }

  species_input$JuvSj <- .3*species_input$JuvGrowthRate/species_input$juv_eff
  species_input$AdultSj <- .3*species_input$AdultGrowthRate/species_input$adult_eff

  #make columns for  type of recruitment, local recruitment, and year class variation
  if ("recruit_group_code" %!in% names (species_input)){
    species_input$recruit_group_code <- NA
    species_input$recruit_group_code[species_input$atlantis_type %in% c("bird", "mammal")] <- 12
    species_input$recruit_group_code[species_input$atlantis_type %in% c("fish", "shark")] <- 10
  }
  if ("local_recruit" %!in% names (species_input)){
    species_input$local_recruit <- 0
  }
  if ("reprod_strength" %!in% names (species_input)){
    species_input$reprod_strength <- NA
    species_input$reprod_strength[species_input$atlantis_type %in% c("bird", "mammal")] <- 0
    species_input$reprod_strength[species_input$atlantis_type %in% c("fish", "shark")] <- 0
  }

  if( "predator" %!in% names(species_input)){
    species_input$predator <- NA
    species_input$predator[species_input$TL_final > 2.5] <- 1
    species_input$predator[is.na(species_input$predator)] <- 0
  }

  if( "pred_case" %!in% names(species_input)){
    species_input$pred_case <- NA
    species_input$pred_case[species_input$predator == 1] <- 0
  }

  if("horizontally_migrates" %!in% names(species_input)){
    species_input$horizontally_migrates <- 1
  }

   if( "jack_a" %!in% names(species_input)){
    species_input$jack_a <- 0
  }

  if( "jack_b" %!in% names(species_input)){
    species_input$jack_b <- 0
  }

  if( "recovery_multiplier" %!in% names(species_input)){
    species_input$recovery_multiplier <- NA
    species_input$recovery_multiplier[species_input$atlantis_type %in% c("fish",
                                                                         "mammal",
                                                                         "bird", "shark")] <- 999
  }

  if( "recover_start_date" %!in% names(species_input)){
    species_input$recover_start_date <- NA
    species_input$recover_start_date[species_input$atlantis_type %in% c("fish",
                                                                         "mammal",
                                                                         "bird", "shark")] <- 999
  }


  if( "flag_dem" %!in% names(species_input)){
    species_input$flag_dem <-1
  }
  if( "flag_q10" %!in% names(species_input)){
    species_input$q10 <-1
  }
  if( "flag_temp_sensitive" %!in% names(species_input)){
    species_input$flag_temp_sensitive <- 0
  }
#   if( "flag_hab_depend" %!in% names(species_input)){
#     species_input$flag_hab_depend <- 0
#   }
  if( "channel" %!in% names(species_input)){
    species_input$flag_channel <- 0
  }
  if( "active" %!in% names(species_input)){
    species_input$active <- 2
  }

  if( "k_tur" %!in% names(species_input)){
    species_input$k_tur <- .1
  }
  if( "k_irr" %!in% names(species_input)){
    species_input$k_irr <- 1
  }

  if( "temp_coefft_a" %!in% names(species_input)){
      species_input$temp_coefft_a <- .851
  }
  if( "q10" %!in% names(species_input)){
    species_input$q10 <- 2
  }
  if( "q10_method" %!in% names(species_input)){
    species_input$q10_method <- 0
  }
  if( "q10_optimal_temp" %!in% names(species_input)){
    species_input$q10_optimal_temp <- 0
  }
  if( "q10_correction" %!in% names(species_input)){
    species_input$q10_correction <- 0
  }
  if( "wc" %!in% names(species_input)){
    species_input$wc <- 0.01
  }

  if("turned_on" %!in% names(species_input)){
    species_input$turned_on <- 1
  }

  if("low" %!in% names(species_input)){
    species_input$low <- 500
  }
  if("thresh" %!in% names(species_input)){
    species_input$thresh <- 2500
  }
  if("sat" %!in% names(species_input)){
    species_input$sat <- NA
    species_input$sat[species_input$atlantis_type %in% c("fish")] <- 4000
  }

  if("Kcov_juv" %!in% names(species_input)){
    species_input$Kcov_juv <- NA
    species_input$Kcov_juv[species_input$atlantis_type %in% c("fish", "mammal", "shark",
                                                         "bird")] <- 3
  }

  if("Kcov_ad" %!in% names(species_input)){
    species_input$Kcov_ad <- NA
    species_input$Kcov_ad[species_input$atlantis_type %in% c("fish", "mammal", "shark",
                                                         "bird")] <- 3
  }

  if("Bcov_juv" %!in% names(species_input)){
    species_input$Bcov_juv <- NA
    species_input$Bcov_juv[species_input$atlantis_type %in% c("fish", "mammal", "shark",
                                                              "bird")] <- .6
  }

  if("Bcov_ad" %!in% names(species_input)){
    species_input$Bcov_ad <- NA
    species_input$Bcov_ad[species_input$atlantis_type %in% c("fish", "mammal", "shark",
                                                             "bird")] <- .6
  }
  if("Acov_juv" %!in% names(species_input)){
    species_input$Acov_juv <- NA
    species_input$Acov_juv[species_input$atlantis_type %in% c("fish", "mammal", "shark",
                                                              "bird")] <- 1
  }

  if("Acov_ad" %!in% names(species_input)){
    species_input$Acov_juv <- NA
    species_input$Acov_juv[species_input$atlantis_type %in% c("fish", "mammal", "shark",
                                                              "bird")] <- 1
  }
  if("swim_speed" %!in% names(species_input)){
    species_input$swim_speed <- NA
    species_input$swim_speed[species_input$atlantis_type %in% c("fish", "shark")] <- 12500
    species_input$swim_speed[species_input$atlantis_type %in% c("mammal")] <- 20000
    species_input$swim_speed[species_input$atlantis_type %in% c("bird")] <- 15000
  }

  if("home_range" %!in% names(species_input)){
    species_input$home_range <- 1
  }
  if("overlap" %!in% names(species_input)){
    species_input$overlap <- 1
  }

  if("migrates_out_of_model" %!in% names(species_input)){
    species_input$migrates_out_of_model <- NA
    species_input$migrates_out_of_model[species_input$horizontally_migrates == 1] <- 0
    }
  if("migrates_out_times_ad" %!in% names(species_input)){
    species_input$migrates_out_times_ad <- 364
  }
  if("migrates_out_times_juv" %!in% names(species_input)){
    species_input$migrates_out_times_juv <- 364
  }
  if("migrates_in_times_juv" %!in% names(species_input)){
    species_input$migrates_in_times_juv <- 0
  }
  if("migrates_in_times_ad" %!in% names(species_input)){
    species_input$migrates_in_times_ad <- 0
  }
  if("migrate_period_ad" %!in% names(species_input)){

    species_input$migrate_period_ad <- NA
    species_input$migrate_period_ad[species_input$horizontally_migrates == 0] <- 0
    species_input$migrate_period_ad[species_input$horizontally_migrates != 0] <- 1

  }
  if("migrate_period_juv" %!in% names(species_input)){

    species_input$migrate_period_juv <- NA
    species_input$migrate_period_juv[species_input$horizontally_migrates == 0] <- 0
    species_input$migrate_period_juv[species_input$horizontally_migrates != 0] <- 1

  }

  if("return_stock_ad" %!in% names(species_input)){
    species_input$return_stock_ad <- 0
  }
  if("return_stock_juv" %!in% names(species_input)){
    species_input$return_stock_juv <- 0
  }
  if("migrants_return_ad" %!in% names(species_input)){
    species_input$migrants_return_ad <- 1
  }
  if("migrants_return_juv" %!in% names(species_input)){
    species_input$migrants_return_juv <- 1
  }
  if("prop_increase_migrant_size_ad" %!in% names(species_input)){
    species_input$prop_increase_migrant_size_ad <- 0
  }
  if("prop_increase_migrant_size_juv" %!in% names(species_input)){
    species_input$prop_increase_migrant_size_juv <- 0
  }

 if("fsp" %!in% names(species_input)){
    species_input$fsp <- 0
  }
  if("stock_availabilty" %!in% names(species_input)){
    species_input$stock_availability <- 1
  }
  if("stock_availabilty_juv" %!in% names(species_input)){
    species_input$stock_availability_juv <- 1
  }

  if("gape_lower_limit" %!in% names(species_input)){
    species_input$gape_lower_limit <- NA
    species_input$gape_lower_limit[species_input$predator == 1] <- .01
  }

  if("gape_upper_limit" %!in% names(species_input)){
    species_input$gape_upper_limit <- NA
    species_input$gape_upper_limit[species_input$predator == 1] <- 1
  }

  if("population_refuge" %!in% names(species_input)){
    species_input$population_refuge <- NA
    species_input$population_refuge[species_input$predator == 1] <- 1
  }

  if("saturation_level" %!in% names(species_input)){
    species_input$saturation_level <- NA
    species_input$saturation_level[species_input$predator == 1] <- 1
  }
  if("search_volume_invert" %!in% names(species_input)){
    species_input$search_volume_invert <- NA
    species_input$search_volume_invert[species_input$predator == 1 &
                                  species_input$atlantis_type %!in% c("fish",
                                                                         "mammal", "bird", "shark")] <- 999
  }


  if("search_volume_coefficient" %!in% names(species_input)){
    species_input$search_volume_coefficient <- NA
    species_input$search_volume_coefficient[species_input$predator == 1 &
                                              species_input$atlantis_type %in% c("fish",
                                                                                    "mammal", "bird", "shark")
                                            & species_input$TL_final < 3.1] <- 2

    species_input$search_volume_coefficient[species_input$predator == 1 &
                                         species_input$atlantis_type %in% c("fish",
                                                                               "mammal", "bird", "shark")
                                       & species_input$TL_final >= 3.1] <- 5
    }

  if("search_volume_exponent" %!in% names(species_input)){
    species_input$search_volume_exponent <- NA
    species_input$search_volume_exponent[species_input$predator == 1 &
                                              species_input$atlantis_type %in% c("bird", "shark")] <- .5
    species_input$search_volume_exponent[species_input$predator == 1 &
                                              species_input$atlantis_type %in% c("fish",
                                                                                    "mammal")] <- .35
  }




  if("handling_time_invert" %!in% names(species_input)){
    species_input$handling_time_invert <- NA
    species_input$handling_time_invert[species_input$predator == 1 &
                                         species_input$atlantis_type %!in% c("fish",
                                                                                "mammal", "bird", "shark")] <- 999
  }


  if("handling_time_coefficient" %!in% names(species_input)){
    species_input$handling_time_coefficient <- NA
    species_input$handling_time_coefficient[species_input$predator == 1 &
                                         species_input$atlantis_type %in% c("fish",
                                                                                "mammal", "bird", "shark")] <- 999
  }

  if("handling_time_exponent" %!in% names(species_input)){
    species_input$handling_time_exponent <- NA
    species_input$handling_time_exponent[species_input$predator == 1 &
                                              species_input$atlantis_type %in% c("fish",
                                                                                    "mammal", "bird", "shark")] <- 999
  }

  if("pr" %!in% names(species_input)){
    species_input$pr <- NA
    species_input$pr[species_input$atlantis_type %in% c("fish", "mammal")] <- 3
    species_input$pr[species_input$atlantis_type %in% c("bird", "shark")] <- 5
  }
  if("min_length_reprod" %!in% names(species_input)){
    species_input$min_length_reprod <- 0
  }
  if(" fish_respiration_scaling_coefficient" %!in% names(species_input)){
    species_input$ fish_respiration_scaling_coefficient <- NA
    species_input$ fish_respiration_scaling_coefficient[species_input$atlantis_type == "fish"] <- .025
    species_input$ fish_respiration_scaling_coefficient[species_input$atlantis_type == "mammal"] <- .021
    species_input$ fish_respiration_scaling_coefficient[species_input$atlantis_type == "bird"] <- .024
    species_input$ fish_respiration_scaling_coefficient[species_input$atlantis_type %!in% c("fish", "mammal", "bird")] <- .014
  }

  if("prefer_allocate_reserves" %!in% names(species_input)){
    species_input$prefer_allocate_reserves <- NA
    species_input$prefer_allocate_reserves[species_input$atlantis_type == "fish"] <- 4
    species_input$prefer_allocate_reserves[species_input$atlantis_type == "shark"] <- 2
    species_input$prefer_allocate_reserves[species_input$atlantis_type == "mammal"] <- 3.5
    species_input$prefer_allocate_reserves[species_input$atlantis_type == "bird"] <- 5
  }

  if(" fish_respiration_scaling_exponent" %!in% names(species_input)){
    species_input$ fish_respiration_scaling_exponent <- .8
  }
  if("lysis_rate" %!in% names(species_input)){
    species_input$lysis_rate <- NA
    species_input$lysis_rate[species_input$atlantis_type %in% c("phytoben", "microphytobenthos",
                                                                   "seagrass", "lg_phy", "sm_phy")] <- 0
  }

  if("starve" %!in% names(species_input)){
    species_input$prefer_allocate_reserves <- NA
    species_input$starve[species_input$atlantis_type %in% c("fish", "mammal", "bird",
                                                            "shark")] <- 0
  }
  if("oxygen_depth_mortality" %!in% names(species_input)){
    species_input$oxygen_depth_mortality <- NA
    species_input$oxygen_depth_mortality[species_input$atlantis_type %!in%
                                           c("fish", "mammal", "bird", "shark",
                                             "microphytobenthos", "phytoben",
                                             "sm_phy", "lg_phy", "dinoflag",
                                             "seagrass")] <- 0.001
  }
  if("ambient_oxygen_mortality" %!in% names(species_input)){
    species_input$ambient_oxygen_mortality <- NA
    species_input$ambient_oxygen_mortality[species_input$atlantis_type %!in%
                                           c("fish", "mammal", "bird", "shark",
                                             "microphytobenthos", "phytoben",
                                             "sm_phy", "lg_phy", "dinoflag",
                                             "seagrass")] <- 0.01
  }

  if("lethal_oxygen_level" %!in% names(species_input)){
    species_input$lethal_oxygen_level <- NA
    species_input$lethal_oxygen_level[species_input$atlantis_type %!in%
                                             c("fish", "mammal", "bird", "shark",
                                               "microphytobenthos", "phytoben",
                                               "sm_phy", "lg_phy", "dinoflag",
                                               "seagrass")] <- 0.5
  }

  if("limiting_oxygen_level" %!in% names(species_input)){
    species_input$limiting_oxygen_level <- NA
    species_input$limiting_oxygen_level[species_input$atlantis_type %!in%
                                        c("fish", "mammal", "bird", "shark",
                                          "microphytobenthos", "phytoben",
                                          "sm_phy", "lg_phy", "dinoflag",
                                          "seagrass")] <- 10
  }

   if("minimim_oxygen_vertebrates" %!in% names(species_input)){
     species_input$minimim_oxygen_vertebrates <-NA
     species_input$minimim_oxygen_vertebrates[species_input$atlantis_type %in%
                                                c("fish", "mammal", "bird",
                                                  "shark")] <- 0
   }

  if("other_fish_mortality" %!in% names(species_input)){
    species_input$other_fish_mortality <-NA
    species_input$other_fish_mortality[species_input$atlantis_type %in%
                                               c("fish")] <-  c("0 0 0 0")
  }
  if("mammal_bird_mortality" %!in% names(species_input)){
    species_input$mammal_bird_mortality <-NA
    species_input$mammal_bird_mortality[species_input$atlantis_type %in%
                                         c("fish")] <-  c("0 0 0 0")
  }

  if("mS_SB" %!in% names(species_input)){
    species_input$mS_SB <-  c("0, 0, 0, 0")
  }
  if("spawning_formulation_constant" %!in% names(species_input)){
    species_input$spawning_formulation_constant <- NA
    species_input$spawning_formulation_constant[species_input$atlantis_type == "shark"] <- 2763.3
    species_input$spawning_formulation_constant[species_input$atlantis_type == "fish"  &
                                                  species_input$TL_final <= 2.5] <- 31.93
    species_input$spawning_formulation_constant[species_input$atlantis_type == "fish"  &
                                                  species_input$TL_final > 2.5] <- 569.65
    species_input$spawning_formulation_constant[species_input$atlantis_type == "bird"] <- 193.9
    species_input$spawning_formulation_constant[species_input$atlantis_type == "mammal"] <- 38376.9
  }

  if("spawning_formulation_fraction" %!in% names(species_input)){
    species_input$spawning_formulation_fraction <- NA
    species_input$spawning_formulation_fraction[species_input$atlantis_type == "shark"] <- .27
    species_input$spawning_formulation_fraction[species_input$atlantis_type == "fish"  &
                                                  species_input$TL_final <= 2.5] <- .246
    species_input$spawning_formulation_fraction[species_input$atlantis_type == "fish"  &
                                                  species_input$TL_final > 2.5] <- .425
    species_input$spawning_formulation_fraction[species_input$atlantis_type == "bird"] <- .2
    species_input$spawning_formulation_fraction[species_input$atlantis_type == "mammal"] <- .16
  }

  if("FSP" %!in% names(species_input)){
    species_input$FSP <- 0
  }
  if("min_spawn_temp" %!in% names(species_input)){
    species_input$min_spawn_temp <- NA
    species_input$min_spawn_temp[species_input$atlantis_type %in% c("fish",
                                                                        "mammal",
                                                                        "bird",
                                                                        "shark")] <- MinTemp
  }
  if("min_spawn_salinity" %!in% names(species_input)){
    species_input$min_spawn_salinity <- NA
    species_input$min_spawn_salinity[species_input$atlantis_type %in% c("fish",
                                                                     "mammal",
                                                                     "bird",
                                                                     "shark")] <- MinSalt
  }
  if("max_move_temp" %!in% names(species_input)){
    species_input$max_move_temp <- MaxTemp
  }
  if("min_move_temp" %!in% names(species_input)){
    species_input$min_move_temp <- MinTemp
  }
  if("max_spawn_temp" %!in% names(species_input)){
    species_input$max_spawn_temp <- NA
    species_input$max_spawn_temp [species_input$atlantis_type %in% c("fish",
                                                                        "mammal",
                                                                        "bird",
                                                                        "shark")] <- MaxTemp
  }
  if("max_spawn_salinity" %!in% names(species_input)){
    species_input$max_spawn_salinity <- NA
    species_input$max_spawn_salinity[species_input$atlantis_type %in% c("fish",
                                                                        "mammal",
                                                                        "bird",
                                                                        "shark")] <- MaxSalt
  }
  if("stock_structure" %!in% names(species_input)){
    species_input$stock_structure <- NA
    species_input$stock_structure <-  gsub(",", rep = "", toString(rep(1,NumberofBoxes)))

  }
  if("vert_stock_struct" %!in% names(species_input)){
    species_input$vert_stock_struct <- 1
  }
  if("pop_ratio_stock" %!in% names(species_input)){
    species_input$pop_ratio_stock <- 1
  }
  if("remin_contrib" %!in% names(species_input)){
    species_input$remin_contrib <- 0
  }
  if("vertically_migrates" %!in% names(species_input)){
    species_input$vertically_migrates <- 0
  }
  if("density_depend" %!in% names(species_input)){
    species_input$density_depend <- NA
    species_input$density_depend[species_input$atlantis_type %in%
        c("bird", "fish", "mammal", "shark")] <- 0
    species_input$density_depend[species_input$atlantis_type %!in%
        c("bird", "fish", "mammal", "shark") & species_input$horizontally_migrates ==
        1] <- 4
  }
  if("in_WC" %!in% names(species_input)){
    species_input$in_WC <- 0
  }
  if("in_sed" %!in% names(species_input)){
    species_input$in_sed <- 0
  }
  if("epi" %!in% names(species_input)){
    species_input$epi <- 0
  }
  if("fished" %!in% names(species_input)){
      species_input$fished <- 0
  }
  if("seperate" %!in% names(species_input)){
    species_input$seperate <- 0
  }
  if("ext_reprod" %!in% names(species_input)){
    species_input$ext_reprod <- 0
  }
    if("impacted" %!in% names(species_input)){
      species_input$impacted <- 0
    }
    if("TAC" %!in% names(species_input)){
      species_input$TAC <- 0
    }

  if("KI" %!in% names(species_input)){
    species_input$KI <- 0
  }
  if("KS" %!in% names(species_input)){
    species_input$KS <- 0
  }
  if("KF" %!in% names(species_input)){
    species_input$KF <- 0
  }
  if("KN" %!in% names(species_input)){
    species_input$KN <- 0
  }
  if("overwinter" %!in% names(species_input)){
    species_input$overwinter <- 0
  }

  if("num_of_genotypes" %!in% names(species_input)){
    species_input$num_of_genotypes <- 1
  }
  if("num_of_stages" %!in% names(species_input)){
    species_input$num_of_stages <- NA
    species_input$num_of_stages[species_input$atlantis_type %in% c("fish","shark",
      "mammal", "bird")] <- 2
    species_input$num_of_stages[is.na(species_input$num_of_stages)] <- 1
  }
  if("linear_mortality" %!in% names(species_input)){
    species_input$linear_mortality <- NA
    for (i in 1:nrow (species_input)){
      if(species_input$atlantis_type[i] %!in% c("lab_det",
                                             "ref_det",
                                             "carrion")){
    species_input$linear_mortality[i] <-  gsub(",", rep = "", toString(rep(0, species_input$num_of_stages[i])))
      }
    }
  }

  if("quadratic_mortality" %!in% names(species_input)){
    species_input$quadratic_mortality <- NA
    for (i in 1:nrow (species_input)){
      if(species_input$atlantis_type[i] %!in% c("lab_det",
                                                "ref_det",
                                                "carrion")){
        species_input$quadratic_mortality[i] <-  gsub(",", rep = "",
                                                      toString(rep(0, species_input$num_of_stages[i])))
      }
    }
  }


  if("extra_mortality_macrophytes" %!in% names(species_input)){
    species_input$extra_mortality_macrophytes <- NA
    species_input$extra_mortality_macrophytes[species_input$atlantis_type %!in% c("phytoben", "seagrass")] <- 0
  }

  if("num_of_stocks" %!in% names(species_input)){
    species_input$num_of_stocks <- 1
  }

  if("stock_recruitment_scalar" %!in% names(species_input)){
    species_input$stock_recruitment_scalar <- NA
    for (i in 1:nrow(species_input)){
      if(species_input$atlantis_type[i] %in% c("fish",
                                               "mammal", "bird", "shark")){
        species_input$stock_recruitment_scalar[i] <-  gsub(",", rep = "", toString(rep(1, species_input$num_of_stocks[i])))
      }
    }
  }

  if("recruits_per_individual_per_year" %!in% names(species_input)){
    species_input$recruits_per_individual_per_year <- NA
    for(i in 1:nrow(species_input)){
      if(is.na(species_input$recruit_group_code[i]) == F){
    species_input$recruits_per_individual_per_year[i] <-  gsub(",", rep = "", toString(rep(1, species_input$num_of_stocks[i])))
      }
    }
  }

  if("primary_production_impact_recruitment" %!in% names(species_input)){
    species_input$primary_production_impact_recruitment <- NA
    species_input$primary_production_impact_recruitment[species_input$num_of_stages > 1] <- 999
  }

  if("ricker_alpha" %!in% names(species_input)){
    species_input$ricker_alpha <- NA
    species_input$ricker_alpha[species_input$num_of_stages > 1] <- 999
  }
  if("ricker_beta" %!in% names(species_input)){
    species_input$ricker_beta <- NA
    species_input$ricker_beta[species_input$num_of_stages > 1] <- 999
  }
  if("beverton_holt_alpha" %!in% names(species_input)){
    species_input$beverton_holt_alpha <- NA
    species_input$beverton_holt_alpha[species_input$num_of_stages > 1] <- 999
  }
  if("beverton_holt_beta" %!in% names(species_input)){
    species_input$beverton_holt_beta <- NA
    species_input$beverton_holt_beta[species_input$num_of_stages > 1] <- 999
  }

  if("num_of_spawns" %!in% names(species_input)){
    species_input$num_of_spawns <- 1
  }
  if("cultured" %!in% names(species_input)){
    species_input$cultured <- 0
  }
  if("habitat_depend" %!in% names(species_input)){
    species_input$habitat_depend <- 0
  }

  if("ph_sensitive" %!in% names(species_input)){
    species_input$ph_sensitive <- 0
  }

  if("ph_sensitive_fecund" %!in% names(species_input)){
    species_input$ph_sensitive_fecund <- 0
  }

  if("salinity_impacts_nutrition" %!in% names(species_input)){
    species_input$salinity_impacts_nutrition <- 0
  }

  if("ph_impacts_availability" %!in% names(species_input)){
    species_input$ph_impacts_availability <- 0
  }

  if("ph_impacts_growth_and_death" %!in% names(species_input)){
    species_input$ph_impacts_growth_and_death <- 0
  }

  if("ph_impact_form" %!in% names(species_input)){
    species_input$ph_impact_form <- 0
  }
  if("monod_inflection_point" %!in% names(species_input)){
    species_input$monod_inflection_point <- 0
  }
  if("optimal_pH_for_nonlinear_function" %!in% names(species_input)){
    species_input$optimal_pH_for_nonlinear_function <- 0
  }
  if("ph_correction_scalar" %!in% names(species_input)){
    species_input$ph_correction_scalar <- 1
  }
  if("ph_function_coeff" %!in% names(species_input)){
    species_input$ph_function_coeff <- 1
  }

  if("ph_function_base" %!in% names(species_input)){
    species_input$ph_function_base <- 1
  }

  if("ph_correction_scalar2" %!in% names(species_input)){
    species_input$ph_correction_scalar2 <- 1
  }

  if("salinity_sensitive" %!in% names(species_input)){
    species_input$salinity_sensitive <- 0
  }
  if("salinity_correction_scalar" %!in% names(species_input)){
    species_input$salinity_correction_scalar <- 1
  }
  if("max" %!in% names(species_input)){
    species_input$max <- 5000
  }
  if("catch_grazer" %!in% names(species_input)){
    species_input$catch_grazer <- 0
  }

  if("prop_catch_available" %!in% names(species_input)){
    species_input$prop_catch_available <- NA
    species_input$prop_catch_available[species_input$catch_grazer == 1] <-
      gsub(",", rep = "", toString(rep(0,nrow(species_input))))
  }

  if("prop_catch_exploitable" %!in% names(species_input)){
    species_input$prop_catch_exploitable <- NA
    species_input$prop_catch_exploitable[species_input$catch_grazer == 1] <-
      gsub(",", rep = "", toString(rep(0,nrow(species_input))))
  }
  if("sediment_penetration_depth" %!in% names(species_input)){
    species_input$sediment_penetration_depth <- NA
    species_input$sediment_penetration_depth[species_input$atlantis_type %!in%
                                               c("phytoben", "microphytobenthos",
                                               "seagrass", "lg_phy", "sm_phy")] <- .001
        species_input$sediment_penetration_depth[species_input$atlantis_type %in%
                                               c("lg_inf")] <- .1
    species_input$sediment_penetration_depth[species_input$atlantis_type %in%
                                               c("fish", "mammal", "shark", "bird")] <- 1
  }
  if("assimilation_efficiency_on_plants" %!in% names(species_input)){
    species_input$assimilation_efficiency_on_plants <- NA
    species_input$assimilation_efficiency_on_plants[species_input$atlantis_type %!in%
                                               c("phytoben", "microphytobenthos",
                                                 "seagrass", "lg_phy", "sm_phy")] <- .5
    species_input$assimilation_efficiency_on_plants[species_input$atlantis_type %in%
                                               c("fish", "mammal", "shark", "bird")] <- .1
  }

  if("assimilation_efficiency_on_live_food_and_carrion" %!in% names(species_input)){
    species_input$assimilation_efficiency_on_live_food_and_carrion <- NA
    species_input$assimilation_efficiency_on_live_food_and_carrion[species_input$atlantis_type %!in%
                                                      c("phytoben", "microphytobenthos",
                                                        "seagrass", "lg_phy", "sm_phy")] <- .5
    species_input$assimilation_efficiency_on_live_food_and_carrion[species_input$atlantis_type %in%
                                                      c("fish", "mammal", "shark", "bird")] <- .1
  }

  if("assimilation_efficiency_on_labile_detritus" %!in% names(species_input)){
    species_input$assimilation_efficiency_on_labile_detritus <- NA
    species_input$assimilation_efficiency_on_labile_detritus[species_input$atlantis_type %!in%
                                                      c("phytoben", "microphytobenthos",
                                                        "seagrass", "lg_phy", "sm_phy")] <- .1
    species_input$assimilation_efficiency_on_labile_detritus[species_input$atlantis_type %in%
                                                      c("sed_ep_ff")] <- .2
    species_input$assimilation_efficiency_on_labile_detritus[species_input$atlantis_type %in%
                                                               c("pl_bact", "sed_bact")] <- .3
  }

  if("assimilation_efficiency_on_refractory_detritus" %!in% names(species_input)){
    species_input$assimilation_efficiency_on_refractory_detritus <- NA
    species_input$assimilation_efficiency_on_refractory_detritus[species_input$atlantis_type %!in%
                                                               c("phytoben", "microphytobenthos",
                                                                 "seagrass", "lg_phy", "sm_phy")] <- .1
    species_input$assimilation_efficiency_on_refractory_detritus[species_input$atlantis_type %in%
                                                               c("sed_ep_ff")] <- .3
    species_input$assimilation_efficiency_on_refractory_detritus[species_input$atlantis_type %in%
                                                               c("pl_bact", "sed_bact")] <- .5
  }

  if("food_lost_to_detritus" %!in% names(species_input)){
    species_input$food_lost_to_detritus <- NA
    species_input$food_lost_to_detritus[species_input$atlantis_type %!in%
                                                                   c("phytoben", "microphytobenthos",
                                                                     "seagrass", "lg_phy", "sm_phy")] <- 0
  }
  if("labile_detritus_lost_to_detritus" %!in% names(species_input)){
    species_input$labile_detritus_lost_to_detritus <- NA
    species_input$labile_detritus_lost_to_detritus[species_input$atlantis_type %!in%
                                          c("phytoben", "microphytobenthos",
                                            "seagrass", "lg_phy", "sm_phy")] <- 0
  }
  if("refractory_detritus_lost_to_detritus" %!in% names(species_input)){
    species_input$refractory_detritus_lost_to_detritus <- NA
    species_input$refractory_detritus_lost_to_detritus[species_input$atlantis_type %!in%
                                                     c("phytoben", "microphytobenthos",
                                                       "seagrass", "lg_phy", "sm_phy")] <- 0
  }


  if("mortality_contribution_to_detritus" %!in% names(species_input)){
    species_input$mortality_contribution_to_detritus <- NA
    species_input$mortality_contribution_to_detritus[species_input$atlantis_type %!in%
                                                         c("phytoben", "microphytobenthos",
                                                           "seagrass", "lg_phy", "sm_phy")] <- .3
  }

  #add index
  species_input=species_input[order(species_input$atlantis_type, species_input$group_code),]
  species_input$index=seq(from=0, to=(nrow(species_input)-1))

  #write files for eventual review

  write.csv(species_input, "final_group_paramaters_used_in_model.csv")
  write.csv(mean_individual_morphology, "parameters_for_age_classes_of_groups.csv")


# start producing actual input files

  species_input$group_code <- toupper(species_input$group_code)
  mean_individual_morphology$group_code <- toupper(mean_individual_morphology$group_code)

    #PRODUCE GROUPS FILE
  groups <- data.frame("Code" = species_input$group_code,
                       "Index" = species_input$index,
                       "IsTurnedOn" = species_input$turned_on,
                       "Name" = species_input$group_code,
                       "LongName" = species_input$group_code,
                       "NumCohorts" = species_input$num_of_age_classes,
                       "NumGeneTypes" = species_input$num_of_genotypes,
                       "NumStages" = species_input$num_of_stages,
                       "NumSpawns" = species_input$num_of_spawns,
                       "NumAgeClassSize" = species_input$ypa_FUNC,
                       "NumStocks" = species_input$num_of_stocks,
                       "VerticallyMigrates" = species_input$vertically_migrates,
                       "HorizontallyMigrates" = species_input$horizontally_migrates,
                       "IsFished" = species_input$fished,
                       "IsImpacted" = species_input$impacted,
                       "isTAC" = species_input$TAC,
                       "GroupType" = toupper(species_input$atlantis_type),
                       "IsPredator" = species_input$predator,
                       "IsCover" = species_input$cover,
                       "IsSiliconDep" = species_input$silicon_dep,
                       "IsAssessed" = species_input$assessed,
                       "IsCatchGrazer" = species_input$catch_grazer,
                       "OverWinters" = species_input$overwinter,
                       "isCultured" = species_input$cultured,
                       "isHabDepend" = species_input$habitat_depend)

  write.csv(groups, "set_as_groups.csv", row.names = F)

  #produce biology.prm file

  sink("Biology_prm.prm")
  cat("#Biology prm file for Deep-C model \n")
  cat(paste("#",Sys.Date(), "\n"))
  #group_codes for reference
  cat("#list group_codes for reference \n")
  for (i in 1:nrow(species_input)){cat(paste("#", as.character(species_input$group_name[i]),
                                             as.character(species_input$group_code[i]), "\n"))}
  #start adding flags, check https://wiki.csiro.au/display/Atlantis/Finding+all+the+options+for+the+flags

  #read in file with general flags (not tied to groups), this has explanations on it
  cat("\n")
  #use default values for flags

  for (i in 1:nrow(flag_data)){
    cat(paste(as.character(flag_data[i,1]), as.character(flag_data[i,2]), sep=" "))
    cat("\n")
  }

  #Array indicating cells effected by coastal degradation (this assumes no degradation)
  cat (paste ("Box_degraded", NumberofBoxes,"\n", sep=" "))
  cat (rep(0,NumberofBoxes))
  cat("\n")

  #Set option for regional reporting here, defaults to no regions

  cat(paste("regID ", NumberofBoxes, "\n", sep=" "))
  cat(rep(0,NumberofBoxes))
  cat("\n")

  # Differential scaling with depth (must be as many entries as
  # number of water column layers * number of changes)
  cat(paste("vertTchange_mult ", NumberofDepthLayers, "\n", sep=""))
  cat(rep(1,NumberofDepthLayers))
  cat("\n")

  # Differential scaling with depth (must be as many entries as
  # number of water column layers * number of changes)
  cat(paste("vertSchange_mult ", NumberofDepthLayers, "\n", sep=""))
  cat(rep(1,NumberofDepthLayers))
  cat("\n")

  # Differential scaling with depth (must be as many entries as
  # number of water column layers * number of changes)
  cat(paste("vertpHchange_mult ", NumberofDepthLayers, "\n", sep=""))
  cat(rep(1,NumberofDepthLayers))
  cat("\n")

  #start putting in flags for each group
  #design is to make one block per group since comparisons easier to see
  #in excel

  for (i in 1:nrow(species_input)){
    cat(paste("# parameters for ", species_input$group_code[i]), "\n")

    #for all living

    if (species_input$atlantis_type[i] %!in% c("lab_det", "carrion",
      "ref_det")){

        #flagdem <- flag_dem
      if(species_input$num_of_stages[i] > 1){
      if (species_input$atlantis_type[i] %in% c("fish", "bird", "mammal",
        "shark")){
      cat(paste("flagdem",as.character(species_input$group_code[i]), " ",
        species_input$flag_dem[i],"\n", sep=""))
        } else {
          cat(paste("flagdem",as.character(species_input$group_code[i]), " ",
            species_input$flagdem[i],"\n", sep=""))
          cat(paste("flagdem j",as.character(species_input$group_code[i]), " ",
            species_input$flagdem[i],"\n", sep=""))
        }} else {
          cat(paste("flagdem",as.character(species_input$group_code[i]), " ",
            species_input$flag_dem[i],"\n", sep=""))
        }


      #temperature


      cat(paste("temp_coefftA_",as.character(species_input$group_code[i]), " ",
                species_input$temp_coefft_a[i],"\n", sep=""))
      cat(paste("q10_",as.character(species_input$group_code[i]), " ",
                species_input$q10[i],"\n", sep=""))
      cat(paste("q10_method_",as.character(species_input$group_code[i]), " ",
                species_input$q10_method[i],"\n", sep=""))
      cat(paste("q10_optimal_temp_",as.character(species_input$group_code[i]), " ",
                species_input$q10_optimal_temp[i],"\n", sep=""))

      cat(paste("q10_correction_",as.character(species_input$group_code[i]), " ",
                species_input$q10_correction[i],"\n", sep=""))

      cat(paste("flagpHsensitive_",as.character(species_input$group_code[i]), " ",
                species_input$ph_sensitive[i],"\n", sep=""))
      cat(paste("flagfecundsensitive_",as.character(species_input$group_code[i]),
                " ", species_input$ph_sensitive_fecund[i],"\n", sep=""))

      cat(paste("flagnutvaleffect_",as.character(species_input$group_code[i]), " ",
                species_input$salinity_impacts_nutrition[i],"\n", sep=""))

      cat(paste("flagpredavaileffect_",as.character(species_input$group_code[i]),
                " ", species_input$ph_impacts_availability[i],"\n", sep=""))

      cat(paste("flagcontract_tol_",as.character(species_input$group_code[i]),
                " ", species_input$ph_impacts_growth_and_death[i],"\n", sep=""))
      cat(paste("pHsensitive_model_",as.character(species_input$group_code[i]),
                " ", species_input$ph_impact_form[i],"\n", sep=""))
      cat(paste("KN_pH_",as.character(species_input$group_code[i]),
                " ", species_input$monod_inflection_point[i],"\n", sep=""))
      cat(paste("optimal_pH_",as.character(species_input$group_code[i]),
                " ", species_input$optimal_pH_for_nonlinear_function[i],"\n", sep=""))

      cat(paste("pH_correction_",as.character(species_input$group_code[i]),
                " ", species_input$ph_correction_scalar[i],"\n", sep=""))
      cat(paste("pH_constA_",as.character(species_input$group_code[i]),
                " ", species_input$ph_function_coeff[i],"\n", sep=""))
      cat(paste("pH_constB_",as.character(species_input$group_code[i]),
                " ", species_input$ph_function_base[i],"\n", sep=""))
      cat(paste("contract_tol_",as.character(species_input$group_code[i]),
                " ", species_input$ph_correction_scalar2[i],"\n", sep=""))
      cat(paste("flagSaltSensitive_",as.character(species_input$group_code[i]),
                " ", species_input$salinity_sensitive[i],"\n", sep=""))
      cat(paste("salt_correction_",as.character(species_input$group_code[i]),
                " ", species_input$salinity_correction_scalar[i],"\n", sep=""))


      if(species_input$num_of_stages[i] > 1){
        cat(paste(as.character(species_input$group_code[i]), "_mL ", species_input$num_of_stages[i],
                  "\n", sep=""))
        cat(paste(species_input$linear_mortality[i], "\n", sep="" ))
      } else {
      cat(paste(as.character(species_input$group_code[i]), "_mL ",
                        species_input$linear_mortality[i],sep="" ))
      }


      if(species_input$num_of_stages[i] > 1){
        cat(paste(as.character(species_input$group_code[i]), "_mQ ", species_input$num_of_stages[i],
                  "\n", sep=""))
        cat(paste(species_input$quadratic_mortality[i], "\n", sep="" ))
      } else {
        cat(paste(as.character(species_input$group_code[i]), "_mQ ",
                  species_input$quadratic_mortality[i],sep="" ))
      }

    if(species_input$atlantis_type[i] %in% c("phytoben", "seagrass")){
        cat(paste("mS_", as.character(species_input$group_code[i]), "_T15 ",
                  species_input$extra_mortality_macrophyte[i],"\n", sep="" ))
      }

      if(species_input$atlantis_type[i] %in% c("fish", "mammal", "bird", "shark")){
        cat(paste("mStarve_", as.character(species_input$group_code[i]), " ",
                  species_input$starve[i],"\n", sep="" ))
      }

      if(species_input$atlantis_type[i] %!in% c("fish", "mammal", "bird", "shark",
           "microphytobenthos", "phytoben", "sm_phy", "lg_phy", "dinoflag",
           "seagrass")){
        cat(paste("mD_", as.character(species_input$group_code[i]), " ",
                  species_input$oxygen_depth_mortality[i],"\n", sep="" ))
        cat(paste("mO_", as.character(species_input$group_code[i]), " ",
                  species_input$ambient_oxygen_mortality[i],"\n", sep="" ))

        cat(paste("KO2_", as.character(species_input$group_code[i]), " ",
                  species_input$lethal_oxygen_level[i],"\n", sep="" ))
        cat(paste("KO2lim_", as.character(species_input$group_code[i]), " ",
                  species_input$limiting_oxygen_level[i],"\n", sep="" ))
      }




    }

    #for all except pelagic bacteria
    if (species_input$atlantis_type[i] != "pl_bact"){

      cat(paste(as.character(species_input$group_code[i]), "_mindepth ",species_input$min_depth[i],"\n", sep=""))
      cat(paste(as.character(species_input$group_code[i]), "_maxdepth ",species_input$max_depth[i],"\n", sep=""))

    }


    #for all predators
    if (species_input$predator[i] == 1){
      cat(paste("predcase_", as.character(species_input$group_code[i]), " ",
                species_input$pred_case[i], "\n", sep=""))
        cat(paste("KLP_", as.character(species_input$group_code[i]), " ",
                  species_input$gape_lower_limit[i], "\n", sep=""))
        cat(paste("KUP_", as.character(species_input$group_code[i]), " ",
                  species_input$gape_upper_limit[i], "\n", sep=""))
        cat(paste("KL_", as.character(species_input$group_code[i]), " ",
                  species_input$population_refuge[i], "\n", sep=""))
        cat(paste("KU_", as.character(species_input$group_code[i]), " ",
                  species_input$saturation_level[i], "\n", sep=""))
        if (species_input$atlantis_type[i] %!in% c("fish", "mammal", "bird", "shark")){
        cat(paste("vl_", as.character(species_input$group_code[i]), " ",
                  species_input$search_volume_invert[i], "\n", sep=""))
        cat(paste("ht_", as.character(species_input$group_code[i]), " ",
                    species_input$handling_time_invert[i], "\n", sep=""))
        }else{
          cat(paste("vla_", as.character(species_input$group_code[i]), "_T15 ",
                    species_input$search_volume_coefficient[i], "\n", sep=""))
          cat(paste("vlb_", as.character(species_input$group_code[i]), " ",
                    species_input$search_volume_exponent[i], "\n", sep=""))
          cat(paste("hta_", as.character(species_input$group_code[i]), " ",
                    species_input$handling_time_coefficient[i], "\n", sep=""))
          cat(paste("htb_", as.character(species_input$group_code[i]), " ",
                    species_input$handling_time_exponent[i], "\n", sep=""))
          }
    }

    #for all consumeer
    if(species_input$atlantis_type[i] %!in% c("phytoben", "microphytobenthos",
                                           "seagrass", "lg_phy", "sm_phy")){

      cat(paste("flag",as.character(species_input$group_code[i]), "day ",
                species_input$active[i],"\n", sep = ""))


     if (species_input$atlantis_type[i] %!in% c("fish", "mammal", "bird", "shark") |
           species_input$num_of_stages[i] == 1){
        cat(paste("mum_", as.character(species_input$group_code[i]), "_T15 ",
                  mean_individual_morphology[mean_individual_morphology$group_code ==
                                               species_input$group_code[i], "mum"], "\n", sep=""))
      cat(paste("C_", as.character(species_input$group_code[i]), "_T15 ",
                mean_individual_morphology[mean_individual_morphology$group_code ==
                                             species_input$group_code[i], "clearance"], "\n", sep=""))
    } else {

      #clearance
      cat(paste("C_", as.character(species_input$group_code[i]), " ",
                species_input$num_of_age_classes[i], "\n", sep=""))
      cat(mean_individual_morphology[mean_individual_morphology$group_code ==
                                       species_input$group_code[i] &
                                       mean_individual_morphology$AgeClass>0,"clearance"])
      cat("\n")

      #mum

      cat(paste("mum_", as.character(species_input$group_code[i]), " ",
                species_input$num_of_age_classes[i], "\n", sep=""))
      cat(mean_individual_morphology[mean_individual_morphology$group_code ==
                                       species_input$group_code[i] & mean_individual_morphology$AgeClass>0,"mum"])
      cat("\n")
    }


      cat(paste("KDEP_", as.character(species_input$group_code[i]), " ",
                species_input$sediment_penetration_depth[i],"\n", sep=""))

      cat(paste("EPlant_", as.character(species_input$group_code[i]), " ",
                species_input$assimilation_efficiency_on_plants[i],"\n", sep=""))

      cat(paste("E_", as.character(species_input$group_code[i]), " ",
                species_input$assimilation_efficiency_on_live_food_and_carrion[i],"\n", sep=""))
      cat(paste("EDL_", as.character(species_input$group_code[i]), " ",
                species_input$assimilation_efficiency_on_labile_detritus[i],"\n", sep=""))
      cat(paste("EDR_", as.character(species_input$group_code[i]), " ",
                species_input$assimilation_efficiency_on_refractory_detritus[i],"\n", sep=""))
      cat(paste("FDG_", as.character(species_input$group_code[i]), " ",
                species_input$food_lost_to_detritus[i],"\n", sep=""))
      cat(paste("FDGDL_", as.character(species_input$group_code[i]), " ",
                species_input$labile_detritus_lost_to_detritus[i],"\n", sep=""))
      cat(paste("FDGDR_", as.character(species_input$group_code[i]), " ",
                species_input$refractory_detritus_lost_to_detritus[i],"\n", sep=""))
      cat(paste("FDM_", as.character(species_input$group_code[i]), " ",
                species_input$mortality_contribution_to_detritus[i],"\n", sep=""))

    }

    #for all catch grazers
    if(species_input$catch_grazer[i] == 1){

      cat(paste("pFC",as.character(species_input$group_code[i]), " ",
                nrow(species_input),"\n", sep = ""))
      cat(as.character(species_input$prop_catch_available[i]))
      cat("\n")
      cat(paste("PropCatch_",as.character(species_input$group_code[i]), " ",
                nrow(species_input),"\n", sep = ""))
      cat(as.character(species_input$prop_catch_exploitable[i]))
      cat("\n")
    }

    #for primary producers
    if(species_input$atlantis_type[i] %in% c("phytoben", "microphytobenthos",
                                             "seagrass", "lg_phy", "sm_phy")){

    cat(paste("mum_", as.character(species_input$group_code[i]), "_T15 ",
              mean_individual_morphology[mean_individual_morphology$group_code ==
                                           species_input$group_code[i], "mum"],
              "\n", sep=""))

      cat(paste("KI_", as.character(species_input$group_code[i]), "_T15 ",
                species_input$KI[i], "\n", sep=""))
      cat(paste("KN_", as.character(species_input$group_code[i]), "_T15 ",
                species_input$KN[i], "\n", sep=""))
      cat(paste("KS_", as.character(species_input$group_code[i]), "_T15 ",
                species_input$KS[i], "\n", sep=""))
      cat(paste("KF_", as.character(species_input$group_code[i]), "_T15 ",
                species_input$KS[i], "\n", sep=""))
      cat(paste("KLYS_", as.character(species_input$group_code[i]), " ",
                species_input$lysis_rate[i], "\n", sep=""))
    }

    #for all fish
    if(species_input$atlantis_type[i] %in% c("fish")){
      cat(paste("mS_FD", as.character(species_input$group_code[i]), " 4",
                "\n", sep=""))
      cat(as.character(species_input$other_fish_mortality[i]))
      cat("\n")
      cat(paste("mS_SB", as.character(species_input$group_code[i]), " 4",
                "\n", sep=""))
      cat(as.character(species_input$mammal_bird_mortality[i]))
      cat("\n")
    }

    #all except primary producers
    if(species_input$atlantis_type[i] %!in% c("phytoben", "microphytobenthos",
                                             "seagrass", "lg_phy", "sm_phy")){
      cat(paste(as.character(species_input$group_code[i]), "_homerange ",species_input$home_range[i],"\n", sep=""))

      cat(paste(as.character(species_input$group_code[i]), "_overlap ",species_input$overlap[i],"\n", sep=""))

    }


    #for mobile epibenthic groups that are mobile or infaunal

    if(species_input$atlantis_type[i] %in% c("sm_inf", "lg_inf", "mob_ep_other")){
      cat(paste("KTUR_",as.character(species_input$group_code[i]), " ",
                species_input$k_tur[i]))
      }

      #for epibenthic infaunal groups

    if(species_input$atlantis_type[i] %in% c("sm_inf", "lg_inf")){
       cat(paste("KIRR_",as.character(species_input$group_code[i]), " ",
                 species_input$k_irr[i], sep = ""))
    }

    #for epibenthic and large and small infaunal groups
    if(species_input$atlantis_type[i] %in% c("sed_ep_ff", "sed_ep_other", "mob_ep_other", "phytoben", "microphytobenthos", "seagrass", "sm_inf", "lg_inf")){
      cat(paste(as.character(species_input$group_code[i]), "max ",species_input$max[i],"\n", sep=""))

    }

    #for filter feeders
    if(species_input$atlantis_type[i] %in% c("SED_EP_FF")){
      cat(paste(as.character(species_input$group_code[i]), "_low ", ,species_input$low[i], "\n", sep=""))
      cat(paste(as.character(species_input$group_code[i]), "thresh ",species_input$thresh[i],"\n", sep=""))
      cat(paste(as.character(species_input$group_code[i]), "_sat ",species_input$sat[i],"\n", sep=""))
    }
    #for species that horizontally migrate
    if(species_input$horizontally_migrates[1] == 1){

      cat(paste(as.character(species_input$group_code[i]), "_ddepend_move"," ",
                species_input$density_depend[i], "\n", sep=""))

      if(species_input$num_of_stages[i] > 1){
        if (species_input$atlantis_type[i] %in% c("fish", "bird", "mammal",
                                                  "shark")){
          cat(paste("flag", as.character(species_input$group_code[i]), "Migrate"," ",
                    species_input$migrates_out_of_model[i], "\n", sep=""))
        } else {

          cat(paste("flag", as.character(species_input$group_code[i]), "Migrate"," ",                     species_input$migrates_out_of_model[i], "\n", sep=""))
          cat(paste("flagj", as.character(species_input$group_code[i]), "Migrate"," ",                     species_input$migrates_out_of_model[i], "\n", sep=""))
        }} else {
          cat(paste("flag", as.character(species_input$group_code[i]), "Migrate"," ",
                    species_input$migrates_out_of_model[i], "\n", sep=""))

        }


      if(species_input$num_of_stages[i] > 1){
        cat(paste("j", as.character(species_input$group_code[i]), "_Migrate_Time 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrates_out_times_juv[i]))
        cat("\n")

        cat(paste("j", as.character(species_input$group_code[i]), "_Migrate_Return 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrates_in_times_juv[i]))
        cat("\n")

        cat(paste("j", as.character(species_input$group_code[i]), "_Migrate_Period 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrate_period_juv[i]))
        cat("\n")

        cat(paste("j", as.character(species_input$group_code[i]), "_ReturnStock 1",
                  "\n", sep=""))
        cat(as.character(species_input$return_stock_juv[i]))
        cat("\n")

        cat(paste("j", as.character(species_input$group_code[i]), "_FSM 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrants_return_juv[i]))
        cat("\n")

        cat(paste("j", as.character(species_input$group_code[i]), "_FSMG 1",
                  "\n", sep=""))
        cat(as.character(species_input$prop_increase_migrant_size_juv[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_Migrate_Time 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrates_out_times_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_Migrate_Return 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrates_in_times_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_Migrate_Period 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrate_period_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_ReturnStock 1",
                  "\n", sep=""))
        cat(as.character(species_input$return_stock_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_FSM 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrants_return_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_FSMG 1",
                  "\n", sep=""))
        cat(as.character(species_input$prop_increase_migrant_size_ad[i]))
        cat("\n")
      }  else {

        cat(paste(as.character(species_input$group_code[i]), "_Migrate_Time 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrates_out_times_ad[i]))
        cat("\n")


        cat(paste(as.character(species_input$group_code[i]), "_Migrate_Return 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrates_in_times_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_Migrate_Period 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrate_period_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_ReturnStock 1",
                  "\n", sep=""))
        cat(as.character(species_input$return_stock_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_FSM 1",
                  "\n", sep=""))
        cat(as.character(species_input$migrants_return_ad[i]))
        cat("\n")

        cat(paste(as.character(species_input$group_code[i]), "_FSMG 1",
                  "\n", sep=""))
        cat(as.character(species_input$prop_increase_migrant_size_ad[i]))
        cat("\n")
          }
      }

    #for all vertebrates and stage-structured inverts
    if (species_input$atlantis_type[i] %in% c("fish", "mammal", "bird", "shark") |
        species_input$num_of_stages[i] > 1){

      #flagrecruit <- flag_recruit
      if (species_input$atlantis_type[i] %in% c("fish", "mammal", "bird", "shark")){
      cat(paste("flagrecruit",as.character(species_input$group_code[i]), " ",
        species_input$recruit_group_code[i], "\n", sep=""))
      } else{
        cat(paste("flagrecruit",as.character(species_input$group_code[i]), " ",
          species_input$recruit_group_code[i], "\n", sep=""))
        cat(paste("flagseperate",as.character(species_input$group_code[i]), " ",
          species_input$seperate[i], "\n", sep=""))
      }
      cat(paste("KDENR_",as.character(species_input$group_code[i]), " ",species_input$num_of_stocks[i],
                "\n", sep=""))
      cat(paste(species_input$recruits_per_individual_per_year[i], "\n", sep=""))
      cat(paste("PP_", as.character(species_input$group_code[i]), " ",
                species_input$primary_production_impact_recruitment[i], "\n", sep=""))
      cat(paste("Ralpha_", as.character(species_input$group_code[i]), " ",
                species_input$ricker_alpha[i], "\n", sep=""))
      cat(paste("Rbeta_", as.character(species_input$group_code[i]), " ",
                species_input$ricker_beta[i], "\n", sep=""))
      cat(paste("BHalpha_", as.character(species_input$group_code[i]), " ",
                species_input$beverton_holt_alpha[i], "\n", sep=""))
      cat(paste("BHbeta_", as.character(species_input$group_code[i]), " ",
                species_input$beverton_holt_beta[i], "\n", sep=""))

      #external reproduction
      cat(paste("flagext_reprod",as.character(species_input$group_code[i]), " ",
        species_input$ext_reprod[i], "\n", sep=""))

      #local recruit
      cat(paste("flaglocalrecruit",as.character(species_input$group_code[i]), " ", species_input$local_recruit[i], "\n", sep=""))

      cat(paste("feed_while_spawn",as.character(species_input$group_code[i]), " ", species_input$feed_while_spawning[i], "\n", sep=""))

      cat(paste("flagtempsensitive",as.character(species_input$group_code[i]),
        " ",species_input$flag_temp_sensitive[i],"\n", sep=""))

      cat(paste(as.character(species_input$group_code[i]), "_AgeClassSize",  " ", max(1, round(species_input$ypa_FUNC[i])), "\n", sep=""))
      cat(paste(as.character(species_input$group_code[i]), "_age_mat",  " ", species_input$mat_FUNC[i], "\n", sep=""))

      cat(paste("FSPB_", as.character(species_input$group_code[i]), " ", species_input$NumofAgeClasses[i], "\n", sep=""))
      cat(mean_individual_morphology[mean_individual_morphology$group_code==species_input$group_code[i] & mean_individual_morphology$AgeClass>0,
                                      "PropSpawning"])
      cat("\n")

      cat(paste("KSWR_", as.character(species_input$group_code[i]),  " ",
                mean_individual_morphology[mean_individual_morphology$group_code
                                           ==  species_input$group_code[i] & mean_individual_morphology$AgeClass==1,
                                            "StructN"], "\n", sep=""))

            cat(paste("KWWR_", as.character(species_input$group_code[i]),  " ",
                mean_individual_morphology[mean_individual_morphology$group_code
                                           == species_input$group_code[i] & mean_individual_morphology$AgeClass==1,
                                            "ResN"], "\n", sep=""))
      cat("\n")
      }


    #for all vertebrates

    if (species_input$atlantis_type[i] %in% c("fish", "mammal", "bird", "shark")){
      cat(paste("flagplankfish",as.character(species_input$group_code[i]), " ", species_input$planktivore[i], "\n", sep=""))

    cat(paste("flagrecpeak",as.character(species_input$group_code[i]), " ", species_input$reprod_strength[i], "\n", sep=""))

    cat(paste("flagbearlive",as.character(species_input$group_code[i]), " ", species_input$bear_live_young[i], "\n", sep=""))

    cat(paste("flagmother",as.character(species_input$group_code[i]), " ", species_input$parental_care[i], "\n", sep=""))

    cat(paste("flaq10eff",as.character(species_input$group_code[i]), " ",species_input$flag_q10[i],"\n", sep=""))

    cat(paste("flaghabdepend",as.character(species_input$group_code[i]), " ",species_input$habitat_depend[i],"\n", sep=""))

    cat(paste("flagchannel",as.character(species_input$group_code[i]), " ",species_input$channel[i],"\n", sep=""))

    cat(paste("Kcov_juv_",as.character(species_input$group_code[i]), " ",species_input$Kcov_juv[i],"\n", sep=""))

    cat(paste("Bcov_juv_",as.character(species_input$group_code[i]), " ",species_input$Bcov_juv[i],"\n", sep=""))

    cat(paste("Acov_juv_",as.character(species_input$group_code[i]), " ",species_input$Acov_juv[i],"\n", sep=""))

    cat(paste("Kcov_ad_",as.character(species_input$group_code[i]), " ",species_input$Kcov_ad[i],"\n", sep=""))

    cat(paste("Bcov_ad_",as.character(species_input$group_code[i]), " ",species_input$Bcov_ad[i],"\n", sep=""))

    cat(paste("Acov_ad_",as.character(species_input$group_code[i]), " ",species_input$Acov_ad[i],"\n", sep=""))

    cat(paste("Speed_",as.character(species_input$group_code[i]), " ",species_input$swim_speed[i],"\n", sep=""))

    cat(paste(as.character(species_input$group_code[i]), "_min_move_temp ",species_input$min_move_temp[i],"\n", sep=""))

    cat(paste(as.character(species_input$group_code[i]), "_max_move_temp ",species_input$max_move_temp[i],"\n", sep=""))

    cat(paste(as.character(species_input$group_code[i]), "_min_move_salt ",species_input$min_move_salt[i],"\n", sep=""))

    cat(paste(as.character(species_input$group_code[i]), "_max_move_salt",species_input$max_move_salt[i],"\n", sep=""))

    cat(paste("pSTOCK_", as.character(species_input$group_code[i]), " 1","\n", sep=""))
    cat(as.character(species_input$stock_availability[i]))
    cat("\n")

    cat(paste("pSTOCK_j", as.character(species_input$group_code[i]), " 1","\n", sep=""))
    cat(as.character(species_input$stock_availability_juv[i]))
    cat("\n")

    cat(paste("pR_", as.character(species_input$group_code[i]), " ",
              species_input$prefer_allocate_reserves[i],"\n", sep=""))

    cat(paste("li_a_", as.character(species_input$group_code[i]), " ",
              species_input$mean_a[i],"\n", sep=""))
    cat(paste("li_b_", as.character(species_input$group_code[i]), " ",
              species_input$mean_b[i],"\n", sep=""))

    if("min_length_reproduction" %!in% names(species_input)){
      cat(paste("min_li_mat_", as.character(species_input$group_code[i]), " ",
                minnona(mean_individual_morphology[mean_individual_morphology$group_code
                                                   == species_input$group_code[i] &
                                                     mean_individual_morphology$Prop_Adult > 0,
                                                   "length"]),"\n", sep=""))
    }else {cat(paste("min_li_mat_", as.character(species_input$group_code[i]), " ",
                     species_input$min_length_reproduction[i]), "\n", sep="")
    }

    cat(paste("KA_", as.character(species_input$group_code[i]), " ",
              species_input$fish_respiration_scaling_coefficient[i], "\n", sep=""))
    cat(paste("KB_", as.character(species_input$group_code[i]), " ",
              species_input$fish_respiration_scaling_exponent[i], "\n", sep=""))
    cat(paste(as.character(species_input$group_code[i]), "_min_O2 ",
              species_input$minimim_oxygen_vertebrates[i], "\n", sep = ""))
    cat(paste("KSPA_", as.character(species_input$group_code[i]), " ",
              species_input$spawning_formulation_constant[i], "\n", sep = ""))
    cat(paste("FSP_", as.character(species_input$group_code[i]), " ",
              species_input$spawning_formulation_fraction[i], "\n", sep = ""))
    cat(paste("recSTOCK_"), as.character(species_input$group_code[i]), " ",
        species_input$num_of_stocks[i], "\n", sep = "")
    cat(paste(species_input$stock_recruitment_scalar[i], "\n", sep="" ))
    cat(paste("recover_mult_", as.character(species_input$group_code[i]), " ",
              species_input$recovery_multiplier[i], "\n", sep = ""))
    cat(paste("recover_start_", as.character(species_input$group_code[i]), " ",
              species_input$recover_start_date[i], "\n", sep = ""))
    cat(paste(as.character(species_input$group_code[i]), "_min_spawn_temp ",species_input$min_spawn_temp[i],"\n", sep=""))
    cat(paste(as.character(species_input$group_code[i]), "_max_spawn_temp ",species_input$max_spawn_temp[i],"\n", sep=""))
    cat(paste(as.character(species_input$group_code[i]), "_min_spawn_temp ",species_input$min_spawn_salinity[i],"\n", sep=""))
    cat(paste(as.character(species_input$group_code[i]), "_max_spawn_temp ",species_input$max_spawn_salinity[i],"\n", sep=""))
    cat (paste (as.character(species_input$group_code[i]), "_stock_struct ", NumberofBoxes,"\n", sep=""))
    cat(paste(species_input$stock_structure[i]), "\n")
     }

    cat("\n")
  }
  sink()

}
