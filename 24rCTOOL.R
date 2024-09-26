library(tidyverse)
#devtools::install_github("francagiannini/rCTOOL")
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
            20),
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
  df$period = ifelse(df$year>100, 'crop_rot', 'burnout_Now')
  
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

writexl::write_xlsx(cinp_df, "cinp_df.xlsx")

# Simulations
# Parametrisation ----

## Time period ----
period = rCTOOL::define_timeperiod(yr_start = 1, yr_end = 200)

## Managment an C inputs ----
## Monthly allocation
## Fraction of manure that we consider is already Humidified

management = management_config(
  f_man_humification = 0.12,
  manure_monthly_allocation = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  plant_monthly_allocation = c(0, 0, 0, 8, 12, 16, 64, 0, 0, 0, 0, 0) / 100#,
  #grass_monthly_allocation = c(1,1,2,7,12,15,17,16,14,9,5,1)/100
) # set to default

# C input calculation has been made previously and is allocated in plot_Cinp

## Soil ----

# Initial C stock at 1m depth
Cinit_kl <- 1.98*25*1.39 + # 0-25
  1.24*1.47*25 + # 25-50
  0.590*1.48*25 + # 50-75
  0.2*1.48*25 # 75-100

# Proportion of the total C allocated in topsoil
Cproptop_kl <- 1.98*25*1.39 /Cinit_kl #

soil = soil_config(Csoil_init = Cinit_kl, # Initial C stock at 1m depth
                   f_hum_top =  0.533,#
                   f_rom_top =  0.405,#
                   f_hum_sub =  0.387,#
                   f_rom_sub =  0.610,#
                   Cproptop = Cproptop_kl, # landmarkensite report askov
                   clay_top = 0.0619,
                   clay_sub = (0.0646+0.0735+0.047)/3,
                   phi = 0.065984,
                   f_co2 = 0.628,# average soils JB1 to JB4 
                   f_romi = 0.012,
                   k_fom  = 0.12,
                   k_hum = 0.0028,
                   k_rom = 3.85e-5,
                   ftr = 0.0025
)

## Temperature ----

T_ave <- rep(c(0.26,0.08, 1.96,5.89,10.72,14.27,16.19,15.86,12.80,8.61,4.55,1.70),200)
T_range <- rep(c(4, 5, 6, 8, 9, 9, 9, 8, 7, 6, 5, 5),200)

temp = bind_cols(month=period$timeperiod$mon,
                 yr=period$timeperiod$yrs,
                 Tavg =T_ave,
                 Range = T_range)


soil_pools = initialize_soil_pools(soil_config = soil,
                                   cn = 14.2 # CN relation
)

# Run ----

scn_list <- list()

for(id in 1:54) {
  
  #id=1
  
  cin_id = define_Cinputs(
    Cin_top = cinp_df[which(cinp_df$id_mod == id),'Ctop'],
    Cin_sub = cinp_df[which(cinp_df$id_mod == id),'Csub'],
    Cin_man = cinp_df[which(cinp_df$id_mod == id),'Cman'],
    time_config = period
  )
  
  
  scn_list[[id]] <- run_ctool(time_config = period,
                              cin_config = cin_id,
                              m_config = management,
                              t_config = temp,
                              s_config = soil,
                              soil_pools = soil_pools) |>
    mutate(id_mod = paste(id))
  
}

field_results <-
  do.call("rbind", scn_list) |> filter(mon == 12) |> 
  bind_cols(cinp_df, .name_repair = "unique")

writexl::write_xlsx(field_results, "field_results_kl24_100.xlsx")


farm_results_wide <- field_results |>
  pivot_wider(
    names_from = field,
    values_from = c("C_topsoil", "C_subsoil", "em_CO2_top", "em_CO2_sub")
  )

farm_results_summ <- farm_results_wide  |> #unique() |>
  group_by(farm, yrs,stock,cinp, scenario) |>
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
  select(farm, yrs,stock,cinp, scenario,C_topsoil,C_subsoil) |> 
  unique() |> as.data.frame()

writexl::write_xlsx(farm_results_summ, "farm_results_summ_100.xlsx")

delta_field <- data.frame(
  field_results[which(field_results$year==1),
                c('farm','stock','cinp', 'scenario','field')],
  
  "delta_Ctopspoil_25yr" =
    field_results[which(field_results$year==100+25),'C_topsoil']-
    field_results[which(field_results$year==100),'C_topsoil'],
  
  "delta_Ctopspoil_100yr" =
    field_results[which(field_results$year==200),'C_topsoil']-
    field_results[which(field_results$year==100),'C_topsoil'],
  
  "delta_C1mt_25yr" =
    (field_results[which(field_results$year==100+25),'C_topsoil']+
       field_results[which(field_results$year==100+25),'C_subsoil'])-
    (field_results[which(field_results$year==100),'C_topsoil']+
       field_results[which(field_results$year==100),'C_subsoil']),
  
  "delta_C1mt_100yr" =
    (field_results[which(field_results$year==200),'C_topsoil']+
       field_results[which(field_results$year==200),'C_subsoil'])-
    (field_results[which(field_results$year==100),'C_topsoil']+
       field_results[which(field_results$year==100),'C_subsoil'])
)

writexl::write_xlsx(delta_field, "delta_field_100.xlsx")


delta_farm <- data.frame(
  farm_results_summ[which(farm_results_summ$yrs==1),
                    c('farm','stock','cinp', 'scenario')],
  
  "delta_Ctopspoil_25yr" =
    farm_results_summ[which(farm_results_summ$yr==100+25),'C_topsoil']-
    farm_results_summ[which(farm_results_summ$yr==100),'C_topsoil'],
  
  "delta_Ctopspoil_100yr" =
    farm_results_summ[which(farm_results_summ$yr==200),'C_topsoil']-
    farm_results_summ[which(farm_results_summ$yr==100),'C_topsoil'],
  
  "delta_C1mt_25yr" =
    (farm_results_summ[which(farm_results_summ$yr==100+25),'C_topsoil']+
       farm_results_summ[which(farm_results_summ$yr==100+25),'C_subsoil'])-
    (farm_results_summ[which(farm_results_summ$yr==100),'C_topsoil']+
       farm_results_summ[which(farm_results_summ$yr==100),'C_subsoil']),
  
  "delta_C1mt_100yr" =
    (farm_results_summ[which(farm_results_summ$yr==200),'C_topsoil']+
       farm_results_summ[which(farm_results_summ$yr==200),'C_subsoil'])-
    (farm_results_summ[which(farm_results_summ$yr==100),'C_topsoil']+
       farm_results_summ[which(farm_results_summ$yr==100),'C_subsoil'])
)


writexl::write_xlsx(delta_farm, "delta_farm_100.xlsx")
