library(tidyverse)
library(rCTOOL)

# C inputs -----
## Crop rotation from Troels ----

# rotations

scenario <- c("Now", "Equal", "More")

stock <- c("conv.high", "conv.low", "org")

field <- c("4ha", "7ha", "9ha")

cinp <- c("allo", "fixed")

mod_desing <-
  expand.grid(
    "scenario" = scenario,
    "stock" = stock,
    "field" = field,
    "cinp" = cinp
  ) |>
  mutate(
    farm = paste(
      "scenario" = scenario,
      "stock" = stock,
      "cinp" = cinp,
      sep = "_"
    ),
    field_info = paste(
      "field" = field,
      "scenario" = scenario,
      "stock" = stock,
      "cinp" = cinp,
      sep = "_"
    )
  ) |>
  rownames_to_column(var = "id_mod")


crop_rot <- read.table("data/rot_feb_yield.txt", 
                       header =T, dec= ".", sep="\t",
                       na.strings = ".") |> 
  mutate(farm=paste("scenario"=scenario,"stock"=stock, sep="_"),
         field_info=paste("field"=field,"scenario"=scenario,"stock"=stock,"cinp"=cinp, sep="_")) |> 
  select(!c(rot_cycle)) 

crop_rot_list <-
  merge(crop_rot, 
        mod_desing[c("id_mod", "field_info")], by = "field_info") #|>
  #mutate(mod_id=paste(field_info, sep = "_")) |> 
  #group_by(id_mod) |> group_spl
  #group_split(field_info)

crop_rot_list <- split(crop_rot_list, crop_rot_list$field_info)

cinp_list <- apply(mod_desing, 1, function(i) {
  df <-
    dplyr::bind_rows(
      c(rep(crop_rot_list[paste(i["field"], 
                                "Now", 
                                i["stock"], 
                                i["cinp"], 
                                sep = "_")], 
            6),
        rep(crop_rot_list[i["field_info"]], 
            20))) 
  
  df$year = as.numeric(row.names(df))
  df$id_mod = i['id_mod']
  df$scenario = i["scenario"]
  df$stock = i["stock"]
  df$field = i["field"]
  df$cinp = i["cinp"]
  df$farm = i["farm"]
  df$field_info = i["field_info"]
  df$period = ifelse(df$year>30, 'crop_rot', 'burnout_Now')
  
  df
  })

## Allometric functions

cinp_df <- do.call("rbind", cinp_list) |> mutate(
    HI = as.numeric(
      recode(
        Crop,
        'Grain' = 0.75,
        'Grass' = 0.70,
        'Maize' = 0.85,
        'Pulses' = 0.42 #from Soy bean
      )
    ),
    SB = as.numeric(recode(
      Crop,
      'Grain' = 0,
      'Grass' = 0,
      'Maize' = 0,
      'Pulses' = 0.5 #from Soy bean
    )),
    RB = as.numeric(
      recode(
        Crop,
        'Grain' = 0.17,
        'Grass' = 0.45,
        'Maize' = 0.15,
        'Pulses' = 0.1 #from Soy bean
      )
    ),
    RE = as.numeric(
      recode(
        Crop,
        'Grain' = 0.8,
        'Grass' = 0.9,
        'Maize' = 0.8,
        'Pulses' = 0.8 #from Soy bean
      )
    ),
    yield_MC = ifelse(is.na(yield_MC), 0 , yield_MC / 1000),
    
    C_manure = if_else(is.na(C_manure), 0 , C_manure / 1000),
    
    C_CC = if_else(is.na(C_CC), 0 , C_CC)
    
  ) |> mutate('Cresid' = as.numeric(((1 / HI) - 1 - SB) * (yield_MC * 0.43)),
         #'Cresid_cc' = as.numeric(((1 / HI) - 1 - SB) * (yield_CC * 0.43)),
         
         'Cbelow' = as.numeric((RB / ((
           1 - RB) * HI)) * (yield_MC * 0.43))#,
  ) |> 
         mutate(
           'Ctop' = ifelse(Cresid < 0, 0 + (RE * Cbelow),
                           Cresid + (RE * Cbelow)) + C_CC,
           
           'Csub' = (1 - RE) * Cbelow ,
           
           'Cman' = C_manure
         )
#write.table(cinp_df, "cinp_kl24.txt", sep="\t")

#writexl::write_xlsx(cinp_df, "cinp_kl24.xlsx")

# Monthly distribution of C inputs

month_prop_grain <- c(0,0,0,8,12,16,64,0,0,0,0,0)/100

month_prop_grass <-c(1,1,2,7,12,15,17,16,14,9,5,1)/100

month_man <- c(0,0,100,0,0,0,0,0,0,0,0,0)/100

# Simulations
# Parametrisation ----

## Temperature data ----

temp_df <- data.frame(
  "month" = seq(1, 12, 1),
  "temp_range" = c(4, 5, 6, 8, 9, 9, 9, 8, 7, 6, 5, 5),
  "temp_ave" = c(0.26,0.08, 1.96,5.89,10.72,14.27,16.19,15.86,12.80,8.61,4.55,1.70)
)

T_ave <- rep(temp_df$temp_ave,130)

T_range <- rep(temp_df$temp_range, 130)

## Fraction of manure that we consider is already Humidified ----
fman <-0.192

# Soil data ----
# Parameters referring to site-specific soil conditions 

# Initial C stock at 1m depth 
Cinit <- 1.98*25*1.39 + # 0-25
  1.24*1.47*25 + # 25-50
  0.590*1.48*25 + # 50-75
  0.2*1.48*25# 75-100

# Proportion of the total C allocated in topsoil

Cproptop <- 1.98*25*1.39 /Cinit #0.47

clay_top <- 0.0619 

clay_sub <- (0.0646+0.0735+0.047)/3

# Diffusion index 
phi <- 0.065984 # average soils 1 to 40.035 

# respiration fraction
fco2 <- 0.628

# romification fraction
fromi <- 0.012

## decomposition rates ----
kFOM <- 0.12

kHUM <- 0.0028

kROM <- 3.858e-05

# transport rate
ftr <- 0#0.003

# initial pool distribution  bjarne
fHUM_top <- 0.533

fROM_top <- 0.405 

fHUM_sub <- 0.387

fROM_sub <- 0.61 

# CN relation
CN_top <- 14.2

CN_sub <- (15.4+13.1+10)/3

## Pre-Processing for time 0 ----
# initial_values

startCAmount_top <- Cinit * Cproptop
startCAmount_sub <- Cinit * (1-Cproptop)

init_pool_top <-pool_cn(cn=CN_top,
                        HUM_frac = fHUM_top,
                        ROM_frac = fROM_top,
                        C_0=startCAmount_top)|> t()

colnames(init_pool_top) <- paste(colnames(init_pool_top), "top", sep = "_")

init_pool_sub <-pool_cn(cn=CN_sub,
                        HUM_frac = fHUM_sub,
                        ROM_frac = fROM_sub,
                        C_0=startCAmount_sub) |> t()

colnames(init_pool_sub)<-paste(colnames(init_pool_sub),"sub",sep="_")

# time period ----
# 100 years complete simulation 

y=seq(1,130,1) 
m=seq(1,12,1)

nsteps <- as.list(seq(1, length(y) * length(m), 1))

# Initial configuration ----

initial_value <-
  cbind(
    "step" = 1,
    "yr" = y[1],
    "mth" = 1,
    init_pool_top,
    init_pool_sub,
    "C_topsoil" = NA,
    "C_subsoil" = NA,
    
    "FOM_tr" = NA,
    "HUM_tr" = NA,
    "ROM_tr" = NA,
    
    "C_tr" = NA,
    
    "CO2_FOM_top" = NA,
    "CO2_HUM_top" = NA,
    "CO2_ROM_top" = NA,
    
    "CO2_FOM_sub" = NA,
    "CO2_HUM_sub" = NA,
    "CO2_ROM_sub" = NA,
    
    "C_CO2_top" = NA,
    "C_CO2_sub" = NA
  )

# Run ----

scn_list <- list()

for(id in 1:54) {

#id=1

C_input_top <- cinp_df[which(cinp_df$id_mod == id),'Ctop']

C_input_sub <- cinp_df[which(cinp_df$id_mod == id),'Csub']

C_input_man <- cinp_df[which(cinp_df$id_mod == id),'Cman']

Crop <- cinp_df[which(cinp_df$id_mod == id),'Crop']


result_pools <-
  matrix(ncol = length(initial_value), nrow = length(nsteps))

colnames(result_pools) <- colnames(initial_value)

result_pools[1, ] <- initial_value

#system.time(
  for (i in 2:length(nsteps)) {
    result_pools[i, ] <- turnoverkl(i)
  }
#)

scn_list[[id]] <- result_pools |> as.data.frame() |>
  mutate(id_mod = paste(id))

}

field_results <-
  do.call("rbind", scn_list) |> filter(mth == 12) |> 
  bind_cols(cinp_df, .name_repair = "unique")

writexl::write_xlsx(field_results, "field_results_kl24.xlsx")


farm_results_wide <- field_results |>
  pivot_wider(
    names_from = field,
    values_from = c("C_topsoil", "C_subsoil", "C_CO2_top", "C_CO2_sub")
  )

farm_results_summ <- farm_results_wide  |> #unique() |>
  group_by(farm, yr,stock,cinp, scenario) |>
  mutate(
    "C_topsoil"=
      mean(`C_topsoil_4ha`,na.rm=TRUE)*0.2 +
      mean(`C_topsoil_7ha`,na.rm=TRUE)*0.35 +
      mean(`C_topsoil_9ha`,na.rm=TRUE)*0.45,
    
    "C_subsoil"=
      mean(`C_subsoil_4ha`,na.rm=TRUE)*0.2 +
      mean(`C_subsoil_7ha`,na.rm=TRUE)*0.35 +
      mean(`C_subsoil_9ha`,na.rm=TRUE)*0.45
  ) |> 
  select(farm, yr,stock,cinp, scenario,C_topsoil,C_subsoil) |> 
  unique() |> as.data.frame()

writexl::write_xlsx(farm_results_summ, "farm_results_summ.xlsx")

delta_field <- data.frame(
  field_results[which(field_results$year==1),
                c('farm','stock','cinp', 'scenario','field')],

  "delta_Ctopspoil_25yr" =
  field_results[which(field_results$year==30+25),'C_topsoil']-
  field_results[which(field_results$year==30),'C_topsoil'],
  
  "delta_Ctopspoil_100yr" =
    field_results[which(field_results$year==130),'C_topsoil']-
    field_results[which(field_results$year==30),'C_topsoil'],
  
  "delta_C1mt_25yr" =
    (field_results[which(field_results$year==30+25),'C_topsoil']+
    field_results[which(field_results$year==30+25),'C_subsoil'])-
    (field_results[which(field_results$year==30),'C_topsoil']+
    field_results[which(field_results$year==30),'C_subsoil']),
  
  "delta_C1mt_100yr" =
    (field_results[which(field_results$year==130),'C_topsoil']+
       field_results[which(field_results$year==130),'C_subsoil'])-
    (field_results[which(field_results$year==30),'C_topsoil']+
       field_results[which(field_results$year==30),'C_subsoil'])
  )
  
writexl::write_xlsx(delta_field, "delta_field.xlsx")

  
delta_farm <- data.frame(
  farm_results_summ[which(farm_results_summ$yr==1),
                    c('farm','stock','cinp', 'scenario')],
  
  "delta_Ctopspoil_25yr" =
    farm_results_summ[which(farm_results_summ$yr==30+25),'C_topsoil']-
    farm_results_summ[which(farm_results_summ$yr==30),'C_topsoil'],
  
  "delta_Ctopspoil_100yr" =
    farm_results_summ[which(farm_results_summ$yr==130),'C_topsoil']-
    farm_results_summ[which(farm_results_summ$yr==30),'C_topsoil'],
  
  "delta_C1mt_25yr" =
    (farm_results_summ[which(farm_results_summ$yr==30+25),'C_topsoil']+
       farm_results_summ[which(farm_results_summ$yr==30+25),'C_subsoil'])-
    (farm_results_summ[which(farm_results_summ$yr==30),'C_topsoil']+
       farm_results_summ[which(farm_results_summ$yr==30),'C_subsoil']),
  
  "delta_C1mt_100yr" =
    (farm_results_summ[which(farm_results_summ$yr==130),'C_topsoil']+
       farm_results_summ[which(farm_results_summ$yr==130),'C_subsoil'])-
    (farm_results_summ[which(farm_results_summ$yr==30),'C_topsoil']+
       farm_results_summ[which(farm_results_summ$yr==30),'C_subsoil'])
)


writexl::write_xlsx(delta_farm, "delta_farm.xlsx")
