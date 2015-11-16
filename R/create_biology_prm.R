#' create_biology_prm function
#'
#' This function creates the biology prm file needed for Atlantis
#' @param species_data_location where csv file with species data is located (defaults to
#' working directory).
#' @param  species_info_groups_csv name of csv file that must contain the following column headers
#' (all provided by merging generate_group_parameter function):
#' \itemize{
#'  \item{atlantis_type}
#'  \item{group_name}
#'  \item{group_code}
#'  \item{TL_final}
#'  \item{mean_M}
#'  \item{max_age}
#'  \item {min_age_reprod}
#'  \item {mean_a}
#'  \item {mean_b}
#'  \item {mean_Loo}
#'  \item {mean_K}
#'  \item {min_depth}
#'  \item{max_depth}
#'  \item{larval_duration}
#'  \item{planktivore}{only used for vertebrates}
#'  \item{bear_live_young}{only used for vertebrates, (0 = no, 1 = yes) }
#'  \item{parental_care} {does the vertebrate group provides parental care for
#'   young until maturity; 0 = no, 1 = yes, -1 = semelparous so die after reproduction)
#'  \item{feed_while_spawning}{for vertebrates and age-structured inverts}
#'  \item{catch_grazer}
#'  \item{assessed}
#'  \item{cover}
#'  \item{silicon_dep}
#'  \item{}
#'  \item{}
#'  }
#'  The following columns are optional (will be filled in by function if not
#'  provided)
#'  \itemize{
#'  \item{num_of_age_classes}
#'  \item{ca}
#'  \item{cb}
#'  \item{juv_eff}
#'  \item{adult_eff}
#'  \item{recruit_code} {flag recruit, needed for verts and stage structured invertebrates,
#' Vertebrate reproduction related flags. The flagrecruit entries refer to the recruitment function used.
#' 1=const, 2=dependent on prim producers (Chla), 3=Beverton-Holt, 4=lognormal, 5=dependent on all plankton
#' groups not just Chla, 6=Bev-Holt with lognormal variation added, 7=Bev-Holt with encourage recovery
#' 8=Bev-Holt with perscribed recovery, 9=Ricker, 10=Standard Bev-Holt (no explict use of spawn included)
#' 11=pupping/calving linearly dependent on maternal condition, 12=pupping/calving a fixed number per adult
#' spawning, 13=forced timeseries of recruitment, defaults to  }
#'  \item{predator} {1 = yes, 0 = no, defaults based trophic level, needed for all
#'  groups}
#'  \item{jack_a}
#'  \item{jack_b}
#'  \item{recover_mult}
#'  \item{recover_start}
#'  \item{PP}

#'  \itemize{needed for all living
#'  \item{flag_dem}{Preferred location trend (0 is top, 1 is demersal (?)));
#'  whether to weight vertical distributions towards surface or bottom layers
#'  when in depths where there were less than complete set of depth layers. defaults
#'  to 1}
#'  \item{ph_sensitive}{is species growth or non predation mortality is sensitive
#'   to pH, defaults to 0}
#'   \item{ph_sensitive_fecund}{is species fecundity sensitive to pH, defaults
#'   to 0}
#'   \item{salinity_impacts_nutrition}{Whether the species nutritional value is
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
#'  \itemize{needed for all that horizontally migrate
#'  \item{migrates_out_of_model} {defaults to 0, needed for all that horizontally
#'  migrate.  Maximum number of times juvenile OR adult stages must leave the
#'  model domain). For example if juvenile FVT leave once,but adults leave 3
#'   times then enter 3 here.  Hard code assumes juvenile inverts do not leave model.}
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
#'  }
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
#'  \itemize{needed for all stage structured inverts
#'  \item{seperate}{1 = seperate groups (or single pool), 0 = age structured
#'  single group, defaults to 0; at this point can only handle one recruit type
#'  for these groups}
#'  }
#'  \itemize{needed for all vertebrates
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
#'  }
#'  \itemize{needed for all vertebrates and stage structured inverts
#'  \item{ext_reprod} {does group reproduce outside model area? 1 = yes, 0 = no,
#'  required for all vertebrates and stage-structured invertebrates, defaults to 0}
#'  \item{local_recruit} {defaults to 0,
#'  1 = demersal and piscivorous fish recruit at parental locations, 0 = independent distribution}
#'  item{flag_temp_sensitive}{Temperature sensitivty; defaults to 0 = no, 1 = yes}
#'  }
#'  \item{flag_X_day}
#'  \item{active} { 2 = no preference, 1 = day, 0 = night,defaults to 2, needed
#'  for all consumers}
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
#'  \item{max}{pace restrictions for basal (epibenthic and some infauna) groups, defaults to
#'  5000}
#'  \item{low}{Threshold spatial factors for filter feeders if using ERSEM formulation, Little space limitation (pop too small) }
#'  \item{thresh}{Threshold spatial factors for filter feeders if using ERSEM formulation }
#'  \item{sat}{Threshold spatial factors for filter feeders if using ERSEM formulation, Interference to uptake due to shading }
#'  \item{home_range} {defaults to 1, all epxcept primary producers)
#'  \item{overlap} {defaults to 1, all except primary producers}
#'  \item{p_stock} {verts only, scalars to determine, defaults to 1 assuming no
#'  stocks considered (just one big group)}
#'  \item{p_stock_juv} {verts only, scalars to determine, defaults to 1
#'  assuming no stocks considered (just one big group)}
#'  \item{kup} {defaults to 1 ,deals with gape limitations, required for all predators}
#'  \item{klp} {defaults to .01, deals with gape limitations, required for all
#'  predators}
#'  \item{pr} {preference for rebuilding, defaults to 3 for fish and mammals,
#'  5 for bird and fish}
#'  \item{min_length_reprod} {defaults to 0}
#'  \item{kdep} {depth organisms can dig into, not needed for primary producers,
#'  defaults to .1}
#'  \item{ka} {scaling of respiration vs weight, defaults .025 for fish, .021
#'  for mammals, .024 for birds, .014 for all others}
#'  \item{kb} {exponent of respiration vs weight, defaults to .8}
#'  \item{klys} {only primary producers defaults to 0}
#'  \item{m_L} {tuning parameter, defaults to 0}
#'  \item{m_Q} {tuning parameter, needed for all living except primary producers,
#'  defaults to 0}
#'  \item{m_S} {tuning parameter for phytobenthos and seagrass,defaults to 0}
#'  \item{m_Starve} {only for verts, defaults to 0}
#'  \item{m_D} {tuning parameter, oxygen mortality due to ambient, inverts except
#'  primary producers,tuning parameter, oxygen mortality due to depth, inverts except
#'  primary producers,defaults to .001}
#'  \item{m_O} {defaults to .01}
#'  \item{k_02} {lethal oxgyen level, inverts except primary producers,defaults to .5}
#'  \item{k_O2_lim} {defults to 10}
#'  \item(min_02){verts only, defaults to 0}
#'  \item{mS_FD} {verts only, defaults to 0 (mortatlity due to fish not included
#'  in model), value for each season, separated by a space (defaults to 0)}
#'  \item{mS_SB} {verts only, defaults to 0 (mortatlity due to birds and mammals
#'  not included in model), value for each season, separated by a space}
#'  \item{KSPA} {defaults to guild values, verts only}
#'  \item{FSM} {ddefaults to guild values, verts only}
#'  \item{FSP} {ddefaults to guild values, verts only}
#'  \item{rec_stock} {recruitment param for diffferent stocks of verts, defaults to 1
#'  (no stocks)}
#'  \item{min_spawn_temp} {defaults to minimum temp in model}
#'  \item{max_spawn_temp} {defaults to maximum temp in model}
#'  \item{stock_struct} {verts only, defaults to 1, input as number for each box
#'  separated by a space}
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
#'  \item{number_of_stocks}{defaults to 1}
#'  \item{number_of_spawns}{{defaults to 1}}
#'  \item{num_of_age_class_size}{set by internal function using maximum age and
#'  number of cohorts}
#'  \item{cultured}{defaults to 0}
#'  }
#'
#'
#  need to add file here wiht mum, clearance, and other values. for now these just default
#  to value in code to keep everything clean
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
#'  @param flag_data_csv
#' @details This function creates the biology prm file needed by Atlantis.
#' @keywords biology prm
#' @export

create_biology_prm <- function(species_data_location = getwd(),  species_info_groups_csv,
                               flag_data_csv){

  species_input <- read.csv(paste(species_data_location, "/", species_info_groups_csv, sep=""),
    header = T)

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
      species_input$mat_FUNC[i]=ceiling(species_input$min_age_reprod[i]/species_input$ypa_FUNC[i])
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
    "PropBiomass", "PropSpawning")

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
  if ("recruit_code" %!in% names (species_input)){
    species_input$recruit_code <- NA
    species_input$recruit_code[species_input$atlantis_type %in% c("bird", "mammal")] <- 12
    species_input$recruit_code[species_input$atlantis_type %in% c("fish", "shark")] <- 10
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

  if( "recover_mult" %!in% names(species_input)){
    species_input$recover_mult <- 1
  }

  if( "recover_start" %!in% names(species_input)){
    species_input$recover_mult <- 10000000
  }

  if( "PP" %!in% names(species_input)){
    species_input$PP <-2000
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
  if("p_stock" %!in% names(species_input)){
    species_input$p_stock <- 1
  }
  if("p_stock_juv" %!in% names(species_input)){
    species_input$p_stock_juv <- 1
  }
  if("kup" %!in% names(species_input)){
    species_input$kup[species_input$predator == 1] <- 1
  }
  if("klp" %!in% names(species_input)){
    species_input$klp[species_input$predator == 1] <- .01
  }
  if("pr" %!in% names(species_input)){
    species_input$pr <- NA
    species_input$pr[species_input$atlantis_type %in% c("fish", "mammal")] <- 3
    species_input$pr[species_input$atlantis_type %in% c("bird", "shark")] <- 5
  }
  if("min_length_reprod" %!in% names(species_input)){
    species_input$min_length_reprod <- 0
  }
  if("kdep" %!in% names(species_input)){
    species_input$kdep <- .1
  }
  if("ka" %!in% names(species_input)){
    species_input$ka <- NA
    species_input$ka[species_input$atlantis_type == "fish"] <- .025
    species_input$ka[species_input$atlantis_type == "mammal"] <- .021
    species_input$ka[species_input$atlantis_type == "bird"] <- .024
    species_input$ka[species_input$atlantis_type %!in% c("fish", "mammal", "bird")] <- .014
  }

  if("kb" %!in% names(species_input)){
    species_input$kb <- .8
  }
  if("klys" %!in% names(species_input)){
    species_input$klys <- 0
  }
  if("m_L" %!in% names(species_input)){
    species_input$m_L <- 0
  }
  if("m_Q" %!in% names(species_input)){
    species_input$m_Q <- 0
  }
  if("m_S" %!in% names(species_input)){
    species_input$m_S <- 0
  }
  if("m_Starve" %!in% names(species_input)){
    species_input$m_Starve <- 0
  }
  if("m_D" %!in% names(species_input)){
    species_input$m_D <- 0.001
  }
  if("m_O" %!in% names(species_input)){
    species_input$m_O <- .01
  }
  if("k_O2" %!in% names(species_input)){
    species_input$k_O2 <- .5
  }
  if("k_O2_lim" %!in% names(species_input)){
    species_input$k_O2_lim <- 10
  }
  if("min_O2" %!in% names(species_input)){
    species_input$min_O2 <- 0
  }
  if("mS_FD" %!in% names(species_input)){
    species_input$mS_FD <- c("0, 0, 0, 0")
  }
  if("mS_SB" %!in% names(species_input)){
    species_input$mS_SB <-  c("0, 0, 0, 0")
  }
  if("KSPA" %!in% names(species_input)){
    species_input$KSPA <- 0
  }
  if("FSP" %!in% names(species_input)){
    species_input$FSP <- 0
  }
  if("rec_stock" %!in% names(species_input)){
    species_input$rec_stock <- 1
  }
  if("min_spawn_temp" %!in% names(species_input)){
    species_input$min_spawn_temp <- MinTemp
  }
  if("max_move_temp" %!in% names(species_input)){
    species_input$max_move_temp <- MaxTemp
  }
  if("min_move_temp" %!in% names(species_input)){
    species_input$min_move_temp <- MinTemp
  }
  if("max_spawn_temp" %!in% names(species_input)){
    species_input$max_spawn_temp <- MaxTemp
  }
  if("stock_struct" %!in% names(species_input)){
    species_input$stock_struct <- 1
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
  if("num_of_stocks" %!in% names(species_input)){
    species_input$num_of_stocks <- 1
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
  groups <- data.frame("group_code" = species_input$group_code,
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
  #codes for reference
  cat("#list codes for reference \n")
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
    }

    #for all consumeer
    if(species_input$atlantis_type[i] %!in% c("phytoben", "microphytobenthos",
                                           "seagrass", "lg_phy", "sm_phy")){

      cat(paste("flag",as.character(species_input$group_code[i]), "day ",
                species_input$active[i],"\n", sep = ""))
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

    #for all vertebrates and stage structured inverts
    if (species_input$atlantis_type[i] %in% c("fish", "mammal", "bird", "shark") |
        species_input$num_of_stages[i] > 1){

      #flagrecruit <- flag_recruit
      if (species_input$atlantis_type[i] %in% c("fish", "mammal", "bird", "shark")){
      cat(paste("flagrecruit",as.character(species_input$group_code[i]), " ",
        species_input$Recruitcode[i], "\n", sep=""))
      } else{
        cat(paste("flagrecruit",as.character(species_input$group_code[i]), " ",
          species_input$Recruitcode[i], "\n", sep=""))
        cat(paste("flagseperate",as.character(species_input$group_code[i]), " ",
          species_input$seperate[i], "\n", sep=""))
      }

      #external reproduction
      cat(paste("flagext_reprod",as.character(species_input$group_code[i]), " ",
        species_input$ext_reprod[i], "\n", sep=""))

      #local recruit
      cat(paste("flaglocalrecruit",as.character(species_input$group_code[i]), " ", species_input$local_recruit[i], "\n", sep=""))

      cat(paste("feed_while_spawn",as.character(species_input$group_code[i]), " ", species_input$feed_while_spawning[i], "\n", sep=""))

      cat(paste("flagtempsensitive",as.character(species_input$group_code[i]),
        " ",species_input$flag_temp_sensitive[i],"\n", sep=""))
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

    }
    cat("\n")
  }
  sink()

}
