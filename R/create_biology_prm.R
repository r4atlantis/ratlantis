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
#'  \item{larval_duration}
#'  \item{planktivore}
#'  \item{bear_live_young}
#'  \item{parental_care} 
#'  \item{feed_while_spawning}
#'  \item{ext_reprod}{does group reproduce outside model area? 1 = yes, 0 = no}
#'  \item{predator}{1 = yes, 0 = no}
#'  \itme{KI}
#'  \item{KS}
#'  \item{KF}
#'  \item{max}
#'  \item{thresh}
#'  }
#'  The following columns are optional (will be filled in by function if not 
#'  provided)
#'  \itemize{
#'  \item{num_of_age_classes}
#'  \item{ca}
#'  \item{cb}
#'  \item{juv_eff}
#'  \item{adult_eff}
#'  \item{recruit_code}{flag recruit}
#'  \item{reprod_stength}
#'  \item{local_recruit}
#'  \item{predator}{1 = yes, 0 = no}
#'  \item{jack_a}
#'  \item{jack_b}
#'  \item{recover_mult}
#'  \item{recover_start}
#'  \item{PP}
#'  \item{flag_dem}
#'  \item{flag_X_day}
#'  \item{active}{ 2 = no preference, 1 = day, 0 = night,defaults to 2}
#'  \item{k_tur} {defaults to .1}
#'  \item{k_irr} {defaults to 1}
#'  }
#'  @param invert_mum_and_clearance_csv name of csv file that must contain the following column headers
#'  \itemize{
#'  \item{group_code}
#'  \item{mum}
#'  \item{clearance}

#'  }
#' @details This function creates the biology prm file needed by Atlantis. 
#' @keywords biology prm
#' @export

create_biology_prm <- function(species_data_location = getwd(),  group_data_csv){
  
  species_input <- read.csv(paste(species_data_location, "/", group_data_csv, sep=""),
    header = T)  
  
  
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
  
  #for vertebrates, determine age class where starts
  for (i in 1:nrow(species_input)){  
    if(species_input$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      species_input$mat_FUNC[i]=ceiling(species_input$min_age_reprod[i]/species_input$ypa_FUNC[i])
    }
    #set maturity at 0 for inverts, other that are just pools
    else{species_input$mat_FUNC[i]=0}
  }
  
  species_input$mat_FUNC[is.na(species_input$mat_FUNC)] <- 2
  
  
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
  
  for(i in 1:nlevels(species_input$group_code)){
    #add one for larvale group
    if(species_input$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      for (j in 1:(species_input[species_input$group_code == 
          levels(species_input$group_code)[i], "num_of_age_classes"]+1)){
        mean_individual_morphology$atlantis_type[counter] = 
          as.character(species_input[species_input$group_code == 
              levels(species_input$group_code)[i], "atlantis_type"])
        mean_individual_morphology$group_code[counter]=levels(species_input$group_code)[i]
        mean_individual_morphology$AgeClass[counter]=j-1
        counter <- counter+1
      }}
  }
  
  #fill in information with loops
  
  #do age classes and wet weights for verts
  #for each age class, this finds actual age, uses VB equation to estimate L, and 
  #then use length-weight relationship to estimate wet weight
  
  for (i in 1:nrow(mean_individual_morphology)){
    if(mean_individual_morphology$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      mean_individual_morphology$ActualAge[i] <- mean_individual_morphology$AgeClass[i]*
        species_input[species_input$group_code == mean_individual_morphology$group_code[i], "ypa_FUNC"]
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
  
  #mum and clearance for verts, below will be NA for others due to lack of wetweights
  
  for (i in 1:nrow(mean_individual_morphology)){
    if(mean_individual_morphology$AgeClass[i]>0){
      mean_individual_morphology$clearance[i]=species_input[species_input$group_code == mean_individual_morphology$group_code[i], "ca"]*(mean_individual_morphology$StructN[i]+
          mean_individual_morphology$ResN[i])^species_input[species_input$group_code == mean_individual_morphology$group_code[i], "cb"]*.8*3.3
      mean_individual_morphology$mum[i]=10 * mean_individual_morphology$clearance[i]
      mean_individual_morphology$PropAgeDist[i]=mean_individual_morphology$Decay[i]/ 
        sum(mean_individual_morphology[mean_individual_morphology$AgeClass>0 & mean_individual_morphology$group_code == mean_individual_morphology$group_code[i],
          "Decay"])
      if(mean_individual_morphology$AgeClass[i] < species_input[species_input$group_code == 
          mean_individual_morphology$group_code[i], "mat_FUNC"]){
        mean_individual_morphology$PropJuv[i]=mean_individual_morphology$Decay[i]/ 
          sum(mean_individual_morphology[mean_individual_morphology$AgeClass>0 & mean_individual_morphology$group_code == mean_individual_morphology$group_code[i]&
              mean_individual_morphology$AgeClass<species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mat_FUNC"],
            "Decay"])
        mean_individual_morphology$PropAdults[i]=0
        if(mean_individual_morphology$AgeClass[i]<(species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mat_FUNC"]-1)){
          mean_individual_morphology$PropSpawning[i]=0}
        else{mean_individual_morphology$PropSpawning[i]=.2}
        
      } else{
        mean_individual_morphology$PropJuv[i]=0
        mean_individual_morphology$PropAdults[i]=mean_individual_morphology$Decay[i]/ 
          sum(mean_individual_morphology[mean_individual_morphology$AgeClass>0 & mean_individual_morphology$group_code == mean_individual_morphology$group_code[i]&
              mean_individual_morphology$AgeClass>=species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mat_FUNC"],
            "Decay"])
        if(mean_individual_morphology$AgeClass[i] == species_input[species_input$group_code == mean_individual_morphology$group_code[i], "mat_FUNC"]){
          mean_individual_morphology$PropSpawning[i] = .8}
        else{mean_individual_morphology$PropSpawning[i]  =1}
      }
      
      
    } else{
      mean_individual_morphology$mum[i]=(mean_individual_morphology$StructN[i]+
          mean_individual_morphology$ResN[i])/(species_input[species_input$group_code == mean_individual_morphology$group_code[i], "larval_duration"]/365)/365
    }
  }
  
  
  #figure out invert mum and clearance formula
    #for now, use values from other models
  invert_mum_clearance <- read.csv("invert_mum_and_clearance.csv")
  mean_individual_morphology <- merge (mean_individual_morphology, invert_mum_clearance, 
    all.x=T, all.y=T)
    
#   
  #parameters needed for availability calculator 
    if ("juv_eff" %in% names(species_input)) {species_input$juv_eff <- .1}
    if ("adult_eff" %in% names(species_input)) {species_input$adult_eff <- .1}

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
    species_input$recruit_code[species$input$atlantis.type %in% c("bird", "mammal")] <- 12
    species_input$recruit_code[species$input$atlantis.type %in% c("fish", "shark")] <- 10
  }
  if ("local_recruit" %!in% names (species_input)){
    species_input$local_recruit <- NA
    species_input$local_recruit[species$input$atlantis.type %in% c("bird", "mammal",
      "fish", "shark")] <- 0
  }
  if ("rep_strength" %!in% names (species_input)){
    species_input$rep_strength <- NA
    species_input$rep_strength[species$input$atlantis.type %in% c("bird", "mammal")] <- 0
    species_input$rep_strength[species$input$atlantis.type %in% c("fish", "shark")] <- 0
  }
  
  for ( i in 1:nrow(species_input)){
    
    if(species_input$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      species_input$LocalRec[i] <- 0    
      if(species_input$Vertebrate.Type[i] == "mammal"){
        species_input$Recruitgroup_code[i] = 12; 
      species_input$RepStrength[i]=0}
      else if(species_input$Vertebrate.Type[i]=="bird"){
        species_input$Recruitgroup_code[i] = 12
        species_input$RepStrength[i]=0}
      else if(species_input$Vertebrate.Type[i]=="turtle"){
        species_input$Recruitgroup_code[i] = 12
        species_input$RepStrength[i]=0}
      #else here catches fish and inverts with age classes
      else {species_input$Recruitgroup_code[i]=10
      species_input$RepStrength[i]=1}
    }
  }
  
  if( "predator" %!in% names(species_input)){
    species_input$predator <- NA
    if (species_input$predator[species_input$TL_final > 2.5] <- 1
    if (species_input$predator[is.na(species_input$predator)] <- 0
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
  if( "flag_hab_depend" %!in% names(species_input)){
    species_input$flag_hab_depend <- 0
  }
  if( "flag_channel" %!in% names(species_input)){
    species_input$flag_channel <- 0
  }
  
  species_input$pred_case <- 0

  if( "flag_X_day" %!in% names(species_input)){
    species_input$flag_X_day <- 2
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
 

#   #primary producer  requirments, needed for PHYTOBEN, MICROPHYTOBEN, LG_ AND SMALL_PHY, DINOFLAG, SEAGRASS, )
#   #KI, KN, KS, KF (light, nutrients, silicon, micronutrients)
#   #onlyneeds KS if IsSiliconDep=1
#   #use midrange of suggested values if not supplied
#   #PropSpawning (in average_individualmorpholgy, for verts, defaults to .2 of last juvenile, .8 of first adult, all thereafter)
#   #max (maximum biomass for epibenthic and large inflaunal (SM_INF AND LG_INF, not plankton or detrisus) groups
#   #_low and _thresh and _sat (filter feeders)
#   #defaults to 500, 2500, 4000
#   #habitat parameters for verts only
#   #Kcov_juv and Kcov_ad Exponent of refuge relationship with biogenic habitat, defaults to 3 for adults and juveniles
#   #Bcov-juv and Bcov_ad Coefficient of refuge relationship with biogenic habitat, defaults to .6 for fish and .3 for other inverts 
#   #Acov_juv scalar for relationship with biogenic habitat, defaults to 1
#   #min and max depth (can be pulled from specieslist data, defaults to 0 and maxdepth of model for all living species
#   #except primary producer, PHYTOBEN, MICROPHYTOBEN, LG_ AND SMALL_PHY, DINOFLAG, SEAGRASS))
#   #homerange (all except primary prodcuer, defaults to 1)
#   #overlap (whether home ranges overlap, defaults to 0, all except primary producer)
#   #swimspeed (vertsonly, defaults to 12500 for fish, 15000 for birds, 1000 for inverts, 20000 others
#   #min and max move temp and salt (vertsonly, defaults to bounds outsdie of model to have no effect)
#   #Migration parameters (for any speie you want to consider moving out of model and has HorizMigrates=1)
#   #flagXMigrate(number of times leaving model domain ) (defaults to 0)
#   #for no age class, just 1 (yes) or 0
#   #for ages classes (Verts), max number of times adults or juveniles must leave the model area
#   #Migrate_Times or Migrate_Times_Juv (verts)
#   #days entering and leaving models, separated by a space (defaults to leave 364, enter 1)
#   #Migrate_Returns
#   #days returning to models, separated by a space
#   #Migrate Periods
#   #defaults to 0
#   #ReturnStock 
#   #defaults to 0
#   #FSM (external migration survivorship)
#   #defaults to 1
#   #FSP (Proportional increase in size  while outside model domain)
#   #pSTOCK and pSTOCK_j(verts only, scalars to determine, defaults to 1 assuming no stocks considered (just one big group) )
#   #KUP and KLP (for all isPredator=1)
#   #klp defaults to 0.01
#   #kup defaults to 1    
#   #these defaults effectively remove say you can eat anything smaller than you (I think)
#   #pR (preference for rebuilding, verts only, defaults to 3 for fish and mammals and inverts, 5 for others)
#   #min_li_mat (verts only, minimum length at maturity, defaults to 0)
#   #KDEP (depth organisms can dig into, not needed for primary producers, defaults to .1 but see Beth's recommendations)
#   #KA (scaling of resp vs weight, verts only, defaults to .025 for fish, .01 reptiles, .024 birds, .021 mammals, .014 inverts)
#   #KB (expoent of resp vs weight, verts only, defaults to .8)
#   #KLYS (only primary producers, defaults to 0)
#   #mL (major tuning parameter, defaults to 0)
#   #mQ (turnign parameter, all living except primary producers, set to 0)
#   #mS (tuning parameter, phytobenthos and seagrass, set to 0)
#   #mStarve (verts only, defaults to 0)
#   #mD (tuning parameter, oxygen mortality due to depth, inverts except primary producers, defautls to .001)
#   #mO (tuning parameter, oxygen mortality due to ambient, inverts except primary producers, defautls to .01)
#   #KO2 (lethal oxgyen level, inverts except primary producers, defautls to .5, )
#   #KO2lim (defaults to 10)
#   #min_O2, verts only, defaults to 0
#   #mS_FD, verts only, defaults to 0 (mortatlity due to fish not included in model)
#   #value for each season, separated by a space (defaults to 0)
#   #mS_SB, verts only, defaults to 0 (mortatlity due to birds and mammals not included in model)
#   #value for each season, separated by a space (defaults to 0)
#   #KSPA and FSP, defaults to guild values, verts only
#   #recSTOCK, recruitment param for diffferent stocks of verts, defaults to 1 (no stocks)
#   #min_spawn_temp, max_spawn_temp, verts, defaults to min and max temps
#   #stock_struct and vert_stock_struct, vertsonly, defaults to 1 across all boxes or layers, 
#   #input as number for each box separated by a space
#   #popratioStock
#   #group_code here assumes no stocks and uses dummy values here , all verts
#   #remin_contrib, for small_infaunal, defautls to 0
#   
#   
#   
#   
#   #is movement of group density dependendent?  
#   #for now, use cimage 
#   ddepend=read.csv("CIMAGE ddepend.csv")
#   species_input=merge(species_input, ddepend, all.x=T)
#   
#   
#   #add columsn needed for groups.csv file
#   #notes here https://wiki.csiro.au/display/Atlantis/Functional+Group+Definition+File
#   #first, order species_input so indexes make sense
#   species_input=species_input[order(species_input$Vertebrate.Type, species_input$Invert.Type,species_input$group_code),]
#   species_input$Index=seq(from=0, to=(nrow(species_input)-1))
#   #for migration and sediment info, defaults to csv file that says no migration if in sediment or epibenthic or not alive (detritus)
#   #columsn produced here:  inWC, inSed, Epi, VerticallyMigrates, HorizontallyMigrates
#   
#   invertdefaults=read.csv("Invert.types.csv")
#   #this was added to put in defaults (such as in wc, epi, hmigrate, vmigrate), but I think some of these may be more precisely added earlier 
#   #remove any already added
#   invertdefaults=invertdefaults[,names(invertdefaults) %!in% names(cimageflagsforalloroddgroupsfinal)[2:dim(cimageflagsforalloroddgroupsfinal)[2]]]     
#   species_input=merge(species_input,invertdefaults, all.x=T)
#   
#   #for isFished, eventually connect to list of fisheries directly
#   species_input$isFished=0
#   species_input$isImpacted=0
#   species_input$isTAC=0
#   #these can eventually be tied directly to trophic interactions
#   #below sets predcase from isPredator and vice versa, assuming one is entered
#   for (i in 1:nrow(species_input)){
#     if("predcase" %in% colnames(species_input)){
#       if( is.na (species_input$predcase[i])==F) species_input$isPredator[i]=1
#       else species_input$isPredator[i]=0
#     }
#     
#     else if ("isPredator" %in% colnames (species_input))
#       if(species_input$isPredator[i]==1) species_input$predcase[i]=0
#   }
#   
#   species_input$isCatchGrazer=0
#   #these need to be corrected manually
#   species_input$isAssessed=0
#   species_input$isCover=0
#   
#   species_input$IsSiliconDep=0
#   species_input[!is.na(species_input$KS)& species_input$KS>0,]$KS=1
#   species_input$isOverWinter=0
#   
#   #make sure you have MigratesoutofModel value for all HorizMigrates=1
#   for (i in 1:nrow(species_input)){
#     if(species_input$HorizontallyMigrates[i]==1 & is.na(species_input$Migratesoutofmodel[i])==T) species_input$Migratesoutofmodel[i]=0
#   }
#   
#   
#   #write the output, then modify any columns as needed
#   write.csv(species_input, "Biological parameters for focus groups.csv", row.names=F)
#   write.csv(mean_individual_morphology, "Rates for individuals and age classes.csv", row.names=F)
#   
#   #also make habitat file
#   #read in habitat file with invert.type, and other changes, (same as for species_input)
  
  
  
  

  return(species_input)
  
}




