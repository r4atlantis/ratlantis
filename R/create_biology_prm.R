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
#'  \item{group_group_code}
#'  \item{TL_final}
#'  \item{mean_M}
#'  \item{max_age}
#'  \item {min_age_reprod}
#'  \item {mean_a}
#'  \item {mean_b}
#'  \item {mean_Loo}
#'  \item {mean_K}
#'  \item{larval_duration}
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
  
  #calculate mum and clearance, also need weights, all for average individual in each size class
  
  #for classes 1-10
  #produce separate table with info on size classses for vertebrates
  #list variables to be included here
  to_be_included <- c("atlantis_type", "AgeClass", "ActualAge", "WetWeight",
    "StructN", "ResN", "Mum", "Clearance", "Decay", "PropAgeDist", "PropJuv", "PropAdults",
    "PropBiomass", "PropSpawning")
  
  #make matrix to hold data
  #take number of ages classes plus a 0 class for all vertebrate
  mean_individual_morphology <- matrix(NA, sum(sum(species_input[species_input$atlantis_type
    %in% c("bird", "fish", "mammal", "shark"),]$num_of_age_classes),
    length(species_input[species_input$atlantis_type %in% c("bird", "fish", "mammal", "shark"),]$group_group_code)),
    length(to_be_included)+1)
  mean_individual_morphology <- as.data.frame(mean_individual_morphology)
  names(mean_individual_morphology)=c("group_group_code", to_be_included)
  
  #loop over species, variables, and age classes
  #consider recruits as 0 age class
  
  #to create this, loop over group_group_code, age classes, and use counter
  counter <- 1                                   
  
  for(i in 1:nlevels(species_input$group_group_code)){
    #add one for larvale group
    if(species_input$atlantis_type[i] %in% c("bird", "fish", "mammal", "shark")){
      for (j in 1:(species_input[species_input$group_group_code == 
          levels(species_input$group_group_code)[i], "num_of_age_classes"]+1)){
        mean_individual_morphology$atlantis_type[counter] = 
          as.character(species_input[species_input$group_group_code == 
              levels(species_input$group_group_code)[i], "atlantis_type"])
        mean_individual_morphology$group_group_code[counter]=levels(species_input$group_group_code)[i]
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
        species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "ypa_FUNC"]
      if(mean_individual_morphology$AgeClass[i] > 0){
        mean_individual_morphology$WetWeight[i]  <- species_input[species_input$group_group_code == 
            mean_individual_morphology$group_group_code[i], "mean_a"]*
          (species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], 
            "mean_Loo"]*(1-exp(-1
            *species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], 
              "mean_K"]*
              mean_individual_morphology$ActualAge[i]
          )))^species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], 
            "mean_b"]
        
        if(mean_individual_morphology$atlantis_type[i] %in% c("fish", "shark")){
          mean_individual_morphology$Decay[i]<- exp(-1*species_input[species_input$group_group_code 
            == mean_individual_morphology$group_group_code[i], "mean_M"]*
              (mean_individual_morphology$AgeClass[i]-1)*species_input[species_input$group_group_code 
                == mean_individual_morphology$group_group_code[i], "ypa_FUNC"])
        } else if(mean_individual_morphology$atlantis_type[i] == "mammal"){
          #need to add silers group_group_code
          mean_individual_morphology$Decay[i] <- exp(-1*species_input[species_input$group_group_code 
            == mean_individual_morphology$group_group_code[i], "mean_M"]*
              (mean_individual_morphology$AgeClass[i]-1)*species_input[species_input$group_group_code 
                == mean_individual_morphology$group_group_code[i], "ypa_FUNC"])
        }else if(mean_individual_morphology$atlantis_type[i] == "bird"){
        #birds shoudl be constant
          mean_individual_morphology$Decay[i] <- exp(-1*species_input[species_input$group_group_code == 
              mean_individual_morphology$group_group_code[i], "mean_M"]*
              (mean_individual_morphology$AgeClass[i]-1)*species_input[species_input$group_group_code == 
                  mean_individual_morphology$group_group_code[i], "ypa_FUNC"])
        }
        
        
      } else {
        mean_individual_morphology$WetWeight[i] <- species_input[species_input$group_group_code == 
            mean_individual_morphology$group_group_code[i], "mean_a"] *
          
          (species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], 
            "mean_Loo"]*(1-exp(-1
            *species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "mean_K"]*
              species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], 
                "larval_duration"]/365
          )))^species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], 
            "mean_b"]
      }
    }
  }
  
  
  
  mean_individual_morphology$StructN <- mean_individual_morphology$WetWeight*Ntoall/drytowet*sNtN*1000
  mean_individual_morphology$ResN <- mean_individual_morphology$WetWeight*Ntoall/drytowet*rNtN*1000
  
  #mum and clearance for verts, below will be NA for others due to lack of wetweights
  
  for (i in 1:nrow(mean_individual_morphology)){
    if(mean_individual_morphology$AgeClass[i]>0){
      mean_individual_morphology$Clearance[i]=species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "ca"]*(mean_individual_morphology$StructN[i]+
          mean_individual_morphology$ResN[i])^species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "cb"]*.8*3.3
      mean_individual_morphology$Mum[i]=10 * mean_individual_morphology$Clearance[i]
      mean_individual_morphology$PropAgeDist[i]=mean_individual_morphology$Decay[i]/ 
        sum(mean_individual_morphology[mean_individual_morphology$AgeClass>0 & mean_individual_morphology$group_group_code == mean_individual_morphology$group_group_code[i],
          "Decay"])
      if(mean_individual_morphology$AgeClass[i] < species_input[species_input$group_group_code == 
          mean_individual_morphology$group_group_code[i], "mat_FUNC"]){
        mean_individual_morphology$PropJuv[i]=mean_individual_morphology$Decay[i]/ 
          sum(mean_individual_morphology[mean_individual_morphology$AgeClass>0 & mean_individual_morphology$group_group_code == mean_individual_morphology$group_group_code[i]&
              mean_individual_morphology$AgeClass<species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "mat_FUNC"],
            "Decay"])
        mean_individual_morphology$PropAdults[i]=0
        if(mean_individual_morphology$AgeClass[i]<(species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "mat_FUNC"]-1)){
          mean_individual_morphology$PropSpawning[i]=0}
        else{mean_individual_morphology$PropSpawning[i]=.2}
        
      } else{
        mean_individual_morphology$PropJuv[i]=0
        mean_individual_morphology$PropAdults[i]=mean_individual_morphology$Decay[i]/ 
          sum(mean_individual_morphology[mean_individual_morphology$AgeClass>0 & mean_individual_morphology$group_group_code == mean_individual_morphology$group_group_code[i]&
              mean_individual_morphology$AgeClass>=species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "mat_FUNC"],
            "Decay"])
        if(mean_individual_morphology$AgeClass[i] == species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "mat_FUNC"]){
          mean_individual_morphology$PropSpawning[i] = .8}
        else{mean_individual_morphology$PropSpawning[i]  =1}
      }
      
      
    } else{
      mean_individual_morphology$Mum[i]=(mean_individual_morphology$StructN[i]+
          mean_individual_morphology$ResN[i])/(species_input[species_input$group_group_code == mean_individual_morphology$group_group_code[i], "larval_duration"]/365)/365
    }
  }
  
  
#   #figure out invert mum and clearance
    #for now
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
    species_input$JuvGrowthRate[i] <- sum(average_juvenile$Mum * average_juvenile$PropJuv) /
      species_input$avgjuvN[i]
    species_input$JuvClearance[i] <- sum(average_juvenile$Clearance * average_juvenile$PropJuv)
    
    #adult calculations
    species_input$avgadultN[i] <- sum(average_adult$WetWeight*average_adult$PropAdult)*Ntoall*1000/drytowet
    species_input$AdultGrowthRate[i] <- sum(average_adult$Mum*average_adult$PropAdult)/species_input$avgadultN[i]
    species_input$AdultClearance[i] <- sum(average_adult$Clearance*average_adult$PropAdult) 
    
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
  
#   #NEED TO MANUALLY PRODUCE SOME COLUMNS (OR PULL FROM FISHBASE, ETC, BUT LIKELY NOT WORTH CODING)
#   
#   #FOR VERTS ONLY
#   
#   #is vertebrate primarly a planktivore? 0=no, yes=1
#   #could pull from diet matrix
#   #does vertebrate bearliveyoung? 0=no, yes=1
#   #does vertebrate demonstrate parental care? 0=no, yes=1, -1 = semelparous so die after reproduction
#   #does vertebrate feed while spawning? 0=no, yes=1
#   #time spawning
#   #spawning period
#   #recruit_time
#   
#   #for now, use cimage assignments
#   verts=read.csv("CIMAGE vert assignments.csv")
#   species_input=merge(species_input, verts, all.x=T)
#   
#   #FOR OTHER GROUPINGS
#   
#   #ExtReprod
#   #does group reproduce outside?  
#   #don't do this for epifauna but all other verts, benthos
#   #1 = can repoduce outside model
#   
#   
#   #need to set isPredator or predcase
#   #may also just set based on trophic level
#   
#   #for now, use cimage 
#   #this has NA's (or blanks in csv file) where flags not needed or wanted
#   cimageflagsforalloroddgroupsfinal=read.csv("cimageflagsforalloroddgroupsfinal.csv")
#   #drop extra columns
#   cimageflagsforalloroddgroupsfinal=cimageflagsforalloroddgroupsfinal[,names(cimageflagsforalloroddgroupsfinal) %!in% c("Category",
#     "Functional.group", "Group.type")]
#   species_input=merge(species_input, cimageflagsforalloroddgroupsfinal, all.x=T)
#   
#   #MAY ALSO ADD THESE COLUMNS (ELSE DEFAULTS ARE USED IN WRITING THE BIO PRM FUNCTION)
#   #flag recruit set above, but
#   #default here is no relationship between primary productivity and recruitment, no standard number of pups (need to be turned on for mammals), and no ricker 
#   #(PP_, KDENR_, Ralpha and Rbeta set to 2000 as filler ) verts only, determined by flag recruit)
#   #recover_mult, verts only, defaults to 1
#   #recover_start, verts only, defaults to 10000000
#   #jack_a and jack_b default to 0
#   #flagdem (default set to 1 for all living groups (not detritus)
#   #flagq10 (vertsonly, default set to 1)
#   #flagtempsensitive (vertsonly, default set to 0)
#   #flaghabdepend (verts only, default set to 0)
#   #flagchannel (verts only, default set to 0)
#   #predcase
#   #you need to set predcase (and have isPredator set by that), or set isPredator and automatically set all predcase to 0 (default)
#   #needed for all predators 
#   #0=Holling type II, 1=Holling type I, 2=Holling type III,              
#   # 3=ECOSIM (currently disabled), 4=min-max, 5=Size specific Holling type III 
#   #if not present, defaults to 0
#   #if predcase =4 or 5 , you need to change parameters, odd defaults set for vl, vla,vlb, ht, hta,htb, ku,kl, 
#   
#   #flagXday (all consumers, so no detritus or phytobenthos, defaults to 2)
#   #when is consumer speices active
#   #Active: 2 = no preference, 1 = day, 0 = night 2
#   #k_Tur (bioturbation rates)(defaults to 1/10 max for epibenthic groups that are mobile or infaunal (SM_INF, LG_INF, MOB_EP_OTHER))
#   #k-Irr (irrigation) defaults to 1 for infaunal groups (SM_INF, LG_INF)
#   #Gary Griffith's climate change (for all living,not detritus)
#   #defaults temp_coefftA to .851
#   #defaults q10 to 2 (note this is not the same flagq10eff)
#   #defaults q10_method to 0
#   #defaults q10_optimal_temp to 0 (this is only read if q10_method=1)
#   #defaults q10_correction to 0 (only read if q10_method=1)
#   #wc (#Scalar for microphytobenthos growth in wc (reduced as not on substrate), defaults to .01
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




