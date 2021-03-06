% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/create_biology_prm.R
\name{create_biology_prm}
\alias{create_biology_prm}
\title{create_biology_prm function}
\usage{
create_biology_prm(species_data_location = getwd(), species_info_groups_csv,
  flag_data_csv)
}
\arguments{
\item{species_data_location}{where csv file with species data is located (defaults to
working directory).}

\item{species_info_groups_csv}{name of csv file with species data.
\itemize{
\item {The following column headers are required
(all provided by merging generate_group_parameter function):
\itemize{
 \item{atlantis_type}
 \item{group_name}
 \item{group_code}
 \item{TL_final}
 \item{mean_M}
 \item{max_age}
 \item {min_age_reprod}
 \item {mean_a} {average coefficient from length-weight relationship}
 \item {mean_b} {average exponent from length-weight relationship}
 \item {mean_Loo}
 \item {mean_K}
 \item {min_depth}
 \item{max_depth}
 \item{larval_duration}
 \item{planktivore}{only used for vertebrates}
 \item{bear_live_young}{only used for vertebrates, (0 = no, 1 = yes) }
 \item{parental_care} {does the vertebrate group provides parental care for
  young until maturity; 0 = no, 1 = yes, -1 = semelparous so die after reproduction}
 \item{feed_while_spawning}{for vertebrates and age-structured inverts}
 \item{assessed}
 \item{cover}
 \item{silicon_dep}
 }
 }
 \item {The following columns are optional (will be filled in by function if not
 provided):
 \itemize{
 \item{num_of_age_classes}
 \item{catch_grazer} {is the group an opportunistic catch grazers (thief)?
 defaults to no = 0}
 \item{ca}
 \item{cb}
 \item{juv_eff}
 \item{adult_eff}
 \item{recruit_group_code} {flag recruit, needed for verts and stage structured invertebrates,
Vertebrate reproduction related flags. The flagrecruit entries refer to the recruitment function used.
1=const, 2=dependent on prim producers (Chla), 3=Beverton-Holt, 4=lognormal, 5=dependent on all plankton
groups not just Chla, 6=Bev-Holt with lognormal variation added, 7=Bev-Holt with encourage recovery
8=Bev-Holt with perscribed recovery, 9=Ricker, 10=Standard Bev-Holt (no explict use of spawn included)
11=pupping/calving linearly dependent on maternal condition, 12=pupping/calving a fixed number per adult
spawning, 13=forced timeseries of recruitment, defaults to  }
 \item{predator} {1 = yes, 0 = no, defaults based trophic level, needed for all
 groups}
 \item{jack_a}
 \item{jack_b}
 \item{recover_mult}
 \item{recover_start}
 \item{PP}
 \item{optional for all living:
 \itemize{
 \item{flag_dem}{Preferred location trend (0 is top, 1 is demersal (?)));
 whether to weight vertical distributions towards surface or bottom layers
 when in depths where there were less than complete set of depth layers. defaults
 to 1}
 \item{ph_sensitive}{is species growth or non predation mortality is sensitive
  to pH, defaults to 0}
 \item{ph_sensitive_fecund}{is species fecundity sensitive to pH, defaults
  to 0}
 \item{salinity_impacts_nutrition}{Whether the species nutritional value is
   sensitive to salinity or pH (mainly an issue for phytoplankton), default to
   0}
 \item{ph_impacts_availablity}{# Whether the species availability to
   predators is sensitive to pH (mainly behaviour in fish), defaults to 0}
 \item{ph_impacts_growth_and_death}{# Whether the species growth or non-
    predation mortality is sensitive to pH, defaults to 0}
 \item{ph_impact_form}{Form of the pH effects model applied for the group,
  defaults to 0, 0 = no effect, 0 = monod, 0 = nonlinear (humped form as of
   Hinga 0000), 0 = linear}
 \item{monod_inflection_point}{Monod inflection point for pH impact function,
 defaults to 0}
 \item{optimal_pH_for_nonlinear_function}{Optimal pH for nonlinear pH
 impact function, defaults to 0}
 \item{ph_correction_scalar}{Correction scalar pH for nonlinear pH impact
  function, defaults to 1}
 \item{ph_function_coeff}{Coefficient pH for nonlinear pH impact function,
 defaults to 1}
 \item{ph_function_base}{Base coefficient pH for nonlinear pH impact function, defaults to 1}
 \item{ph_correction_scalar2}{Correction scalar pH for nonlinear pH impact function, defaults to 1}
 \item{salinity_sensitive}{Whether the species is sensitive to salinity, defaults to 0}
 \item{salinity_correction_scalar}{# Correction scalar salt for nonlinear salinity
  impact function, defaults to 1}
 }
 }
 \item{optional for primary producers:
 \itemize{
 \item{lysis_rate} {only primary producers defaults to 0}
 }
 }
 \item{optional for macrophytes:
 \itemize{
 \item{extra_mortality_macrophytes} {due to scour or nutrient input for mar, defaults to 0}
 }
 }
 \item{optional for catch grazers:
 \itemize{
 \item{prop_catch_available}{Availabilty of catch to opportunistic catch grazers (thieves);
 should be space separated vector with entry for each other functional group; defaults to 0}
 \item{prop_catch_exploitable}{How much of each catch is exploitable by opportunistic catch grazers (thieves);
 should be space separated vector with entry for each other functional group; defaults to 0}
 }
 }

 \item{needed for all that horizontally migrate
 \itemize{
 \item{migrates_out_of_model} {defaults to 0, needed for all that horizontally
 migrate.  Maximum number of times juvenile OR adult stages must leave the
 model domain). For example if juvenile FVT leave once,but adults leave 3
  times then enter 3 here.  Hard group_code assumes juvenile inverts do not leave model.}
 \item{migrates_out_times_ad} { Migration dates (days of the year) for adults of groups
 with stages or for pools moving out of model domain - must be as many entries in these arrays as there
 are in the flagjXXXMigrate entry for that group, separated by a space. Note
 an entry of 0 for the flag still requires a single entry for the array (defaults
 to 364)}
 \item{migrates_in_times_ad} { Migration dates (days of the year) for adults of groups
 with stages or for pools returning to the model domain - must be as many entries in these arrays as there
 are in the migrates_out_model entry for that group, separated by a space. Note
 an entry of 0 for the flag still requires a single entry for the array (defaults
 to 0)}
 \item{migrates_out_times_juv} { Migration dates (days of the year) for juveniles of groups with stages
 moving out of model domain - must be as many entries in these arrays as there
 are in the flagjXXXMigrate entry for that group, separated by a space. Note
 an entry of 0 for the flag still requires a single entry for the array (defaults
 to 364)}
 \item{migrates_in_times_juv} { Migration dates (days of the year) for juveniles of groups with stages
 returning to the model domain - must be as many entries in these arrays as there
 are in the migrates_out_model entry for that group, separated by a space. Note
 an entry of 0 for the flag still requires a single entry for the array (defaults
 to 0)}
 \item{migrate_times_juv} {days entering and leaving models, separated by a space
 (defaults to leave 364, enter 1)}
 \item{migrate_period_ad} {defaults to 1 for those with horizontal migration, period of time adults of groups wiht stages of pools exit or enter model over;
 put to 0 to stop migration}
 \item{migrate_period_juv} {defaults to 1 for those wiht horizontal migration, period of time juveniles of groups with stages exit or enter model over;
 put to 0 to stop migration}
 \item{return_stock_ad} {defaults to 0, integer number identifying specific stock adult or pool migrants will return to; 0
 spreads uniformly across model}
 \item{return_stock_juv} {defaults to 0, integer number identifying specific stock juvenile migrants will return to; 0
 spreads uniformly across model}
 \item{migrants_return_ad} {proportion of migrating adult or pool biomass that return to the model domain, defaults to 1}
 \item{migrants_return_juv} {proportion of migrating adult or pool biomass that return to the model domain, defaults to 1}
 \item{prop_increase_migrant_size_ad}{Proportional increases in size while outside model domain for adults,
 defaults to 0}
 \item{prop_increase_migrant_size_juv}{Proportional increases in size while outside model domain for juveniles,
 defaults to 0}
 \item{density_depend} {is movement of group density-dependent, 0 = off,
 1 = sedentary, 2 = on, 3 = sticky, 4 = no explicit movement, defaults to 0 for
 vertebrates, 4 for others; needed for all that horizontally migrate}
 }
 }
 \item{optional for all stage structured inverts:
 \itemize{
 \item{seperate}{1 = seperate groups (or single pool), 0 = age structured
 single group, defaults to 0; at this point can only handle one recruit type
 for these groups}
 }
 }
 \item{optional for all vertebrates:
 \itemize{
 \item{reprod_strength}{# Vertebrate reproduction strength flags (1=very
 strong year classes possible, relative strength set using recruitRange and
 0=only moderate variation in year class strength possible, mainly for top
 predators with few young per reproductive event, relative strength set
 using recruitRangeFlat. defaults to 0)}
 \item{flag_q10}{Switch indicating whether or not efficiency of assimilation
 is temperature dependent, defaults 1; 0 = no (same efficiency regardless),
  1 = poorer when cooler, 2 = poorer when warmer}
 \item{habitat_depend}{defaults to 0, dependent on demersal habitat: 0 = no,
 1 = yes 0, defaults to 0}
 \item{channel}{indicating whether vertebrate group seeks channels during low
  tide - in tidal models}
 \item{Kcov_juv} {Exponent of refuge relationship with biogenic habitat, defaults to
 3, verts only}
 \item{Kcov_ad} {Exponent of refuge relationship with biogenic habitat, defaults to
 3, verts only}
 \item{Bcov_juv} {Coefficient of refuge relationship with biogenic habitat, defaults
 to .6, verts only}
 \item{Bcov_ad} {Coefficient of refuge relationship with biogenic habitat, defaults
 to .6, verts only}
 \item{Acov_juv} {scalar for relationship with biogenic habitat, defaults to 1, verts only}
 \item{Acov_ad} {scalar for relationship with biogenic habitat, defaults to 1, verts only}
 \item{swim_speed}{defaults to 12500 for fish and sharks, 15000 for birds, 20000 for sharks mammals}
 \item{min_move_temp} {defaults to minimum temp in model}
 \item{max_move_temp} {defaults to maximum temp in model}
 \item{prefer_allocate_reserves}{Vertebrate preference for rebuilding reserves over structure;
 defaults to 4 for fish, 3 for sharks, 3.5 for mammals, and 5 for birds}
 \item{min_length_reproduction}{if not provided, uses mininum age at reproduction and
 length weight relationship to estimate}
 \item{ fish_respiration_scaling_coefficient} {scaling of respiration vs weight, defaults .025 for fish, .021
 for mammals, .024 for birds, .014 for all others}
 \item{ fish_respiration_scaling_exponent} {exponent of respiration vs weight, defaults to .8}
 }
 }
 \item{optional for all vertebrates and stage structured inverts:
 \itemize{
 \item{ext_reprod} {does group reproduce outside model area? 1 = yes, 0 = no,
 required for all vertebrates and stage-structured invertebrates, defaults to 0}
 \item{local_recruit} {defaults to 0,
 1 = demersal and piscivorous fish recruit at parental locations, 0 = independent distribution}
 item{flag_temp_sensitive}{Temperature sensitivty; defaults to 0 = no, 1 = yes}
 }
 }
 #'  \item{optional for all predators:
 \itemize{
 \item{gape_lower_limit} {defaults to .01, lower Gape size for predators, to determine available prey fish groups.  Required for all
 predators}
 \item{gape_upper_limit} {defaults to 1, upper Gape size for predators, to determine available prey fish groups.  Required for all
 predators}
 \item{population_refuge}{Seed bank/population refuge based on consumer intake, if available
  food below this feeding slows/stops so can't eat out all food. only used by
   model for predcase =4; defaults to 1 for filler}
  \item{saturation_level}{Saturation levels for consumer food intake. only used by model for predcase = 4; defaults to 1 for filler}
  \item{search_volume_invert}{Search volume for invertebrate predators -
  used by size specific Holling type III (predcase 5); defaults to 999 here for filler value  }
  \item{search_volume_coefficient}{Search volume coefficient for vertebrate predators-
  used by size specific Holling type III (predcase 5);
  defaults to planktivorous = 2, pelagic piscivores = 5, demersal/reef = 1}
  \item{search_volume_exponent}{Search volume exponenent for vertebrate predators -
  used by size specific Holling type III (predcase 5);
  defaults to .5 for birds and sharks and .35 for mammals and fish}
  \item{handling_time_invert}{handling time for invertebrate predators -
  used by size specific Holling type III (predcase 5); defaults to 999 here for filler value  }
  \item{handling_time_coefficient}{handling time coefficient for vertebrate predators-
  used by size specific Holling type III (predcase 5);
  ; defaults to 999 here for filler value  }
  \item{handling_time_exponent}{handling time exponenent for vertebrate predators -
  used by size specific Holling type III (predcase 5);
  ; defaults to 999 here for filler value  }
 }
 }
 \item{optional for all consumers:
 \itemize{
 \item{active} { 2 = no preference, 1 = day, 0 = night,defaults to 2, needed
 for all consumers}
 \item {sediment_penetration_depth}{Depth consumers can dig into, are found down to in the sediment;
 defaults to .1 for lg_inf; .1 for fish, mammals, sharks, and birds; and .001 for others }
 \item {assimilation_efficiency_on_plants}{defaults to .5 for invertebrates,
 .1 for vertebrates}
 \item {assimilation_efficiency_on_live_food_and_carrion}{defaults to .5 for invertebrates,
 .1 for vertebrates}
 \item {assimilation_efficiency_on_labile_detritus}{defaults to .3 for bacteria,
 .2 for filter feeders, and .1 for everything else}
 \item {assimilation_efficiency_on_refractory_detritus}{defaults to .5 for bacteria,
 .3 for filter feeders, and .1 for everything else}
 }
 }
 \item{flag_X_day}
 \item{k_tur} {defaults to .1}{needed for epibenthic groups that are mobile or infaunal (SM_INF, LG_INF, MOB_EP_OTHER)}
 \item{k_irr} {defaults to 1, needed for epibenthic groups that are infaunal
  (SM_INF, LG_INF))}
 \item{temp_coefft_a} {defaults to .851, needed for all living things,
 Coefficient A in Gary Griffith temperature function}
 \item{q10} {defaults to 2, Exponent in temperature effect on rate parameters}
 \item{q10_method} {defaults to 0, method of calculating Q10 0 is the
 'normal' way of calculating it. 1 is the 'new' climate change method from
 Gary G.}
 \item{q10_optimal_temp} {defaults to 0, optimum temperature of each
 functional group - this is only read in for groups where the q10_method is 1.}
 \item{q10_correction} {defaults to 0, the q10 correction factor for each
 functional group - this is only read in for groups where the q10_method is 1.}
 \item{wc} {defaults to .01}
 \item{turned_on} {defaults to 1 (=yes, 0=no)  groups$IsTurnedOn=1}
 \item{overwinter} {defaults to 0}
 \item{max}{space restrictions for basal (epibenthic and some infauna) groups, defaults to
 5000}
 \item{low}{Threshold spatial factors for filter feeders if using ERSEM formulation, Little space limitation (pop too small) }
 \item{thresh}{Threshold spatial factors for filter feeders if using ERSEM formulation }
 \item{sat}{Threshold spatial factors for filter feeders if using ERSEM formulation, Interference to uptake due to shading }
 \item{home_range} {defaults to 1, all epxcept primary producers}
 \item{overlap} {defaults to 1, all except primary producers}
 \item{stock_availability} {verts only, scalars to determine stock availablity for adults, defaults to 1 assuming no
 stocks considered (just one big group, should be vector the same length as number of stocks)}
 \item{stock_availability_juv} {verts only, scalars to determine availablity for juveniles, defaults to 1
 assuming no stocks considered (just one big group), should be vector the same length as number of stocks}
 \item{linear_mortality} {tuning parameter needed for all living organisms, defaults to 0}
 \item{juvenile_linear_mortality} {tuning parameter, defaults to 0, needed for stage-structured inverts and
 vertebrates}
 \item{quadratic_mortality} {tuning parameter, needed for all living except primary producers,
 defaults to 0}
 \item{juvenile_quadratic_mortality} {tuning parameter, defaults to 0, needed for stage-structured inverts
 and vertebrates}
 \item{starve} {mortality due to starvation, only for vertebrates, defaults to 0}
 \item{oxygen_depth_mortality} {tuning parameter, Half oxygen mortality depth, inverts except
 primary producers,defaults to .001}
 \item{ambient_oxygen_mortality} {tuning parameter, oxygen dependent mortality due to ambient conditions, inverts except
 primary producers, defaults to .01}
 \item{lethal_oxygen_level} {lethal oxgyen level, inverts except primary producers,defaults to .5}
 \item{limiting_oxygen_level} {limiting oxygen level, inverts except primary producers, defults to 10}
 \item(min_02){verts only, defaults to 0}
 \item{mS_FD} {verts only, defaults to 0 (mortatlity due to fish not included
 in model), value for each season, separated by a space (defaults to 0)}
 \item{mS_SB} {verts only, defaults to 0 (mortatlity due to birds and mammals
 not included in model), value for each season, separated by a space}
 \item{KSPA} {defaults to guild values, verts only}
 \item{FSM} {ddefaults to guild values, verts only}
 \item{FSP} {ddefaults to guild values, verts only}
 \item{rec_stock} {recruitment param for diffferent stocks of verts, defaults to 1
 (no stocks)}
 \item{min_spawn_temp} {defaults to minimum temp in model}
 \item{max_spawn_temp} {defaults to maximum temp in model}
 \item{stock_struct} {verts only, defaults to 1, input as number for each box
 separated by a space}
 \item{vert_stock_struct} {verts only, defaults to 1,input as number for each box
 separated by a space}
 \item{pop_ratio_stock} {defaults to 1}
 \item{remin_contrib}{ for small_infaunal, defaults to 0}
 \item{in_WC} {defaults to 0}
 \item{in_sed} {defaults to 0}
 \item{epi} {default to 0}
 \item{vertically_migrates} {defaults to 0}
 \item{horizontally_migrates} {defaults to 0}
 \item{fished} {defaults to 0, is it targeted in fisheries}
 \item{impacted} {defaults to 0, is it impacted by fisheries}
 \item{TAC} {defaults to 0, is it controlled by a TAC}
 \item{pred_case} {needed for all predators, defaults to 0, Predation
 formulation switches. 0=Holling type II, 1=Holling type I, 2=Holling type
 III, 3=ECOSIM (currently disabled), 4=min-max, 5=Size specific Holling type III}
 \item{KI} {default to 0, light saturation, needed for primary producers}
 \item{KS} {default to 0, Half-sat const for PL growth on Si mg Si m-3}
 \item{KF} {default to 0, Half-sat const for PL growth on Micro-nutrient}
 \item{KN} {default to 0, Primary producer nutrient requirements   }
 \item{num_of_genotypes}{defaults to 1}
 \item{num_of_stages}{defaults to 2 for vertebrates, 1 for others}
 \item{num_of_stocks}{defaults to 1}
 \item{num_of_spawns}{{defaults to 1}}
 \item{num_of_age_class_size}{set by internal function using maximum age and
 number of cohorts}
 \item{cultured}{defaults to 0}
 }
 }
 }}

\item{flag_data_csv}{csv file containing major flag values for model}
}
\description{
This function creates the biology prm file needed for Atlantis
}
\details{
This function creates the biology prm file needed by Atlantis.
}
\keyword{biology}
\keyword{prm}

