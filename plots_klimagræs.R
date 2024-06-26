library(tidyverse)
library(ggthemes)

# My ggplot theme 
theme_set(theme_bw())
theme_update(panel.grid = element_blank())

ggthemes::theme_calc()


# Cinputs 
Carbon_inputs <- 
cinp_df |> 
  filter(between(year,25,35)) |> 
  ggplot(aes(y=Ctop+Csub,x=year, group=field_info,
             col=cinp#scenario#, 
             #fill=interaction(Crop,CC)
             )
             )+ 
  #geom_col()+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = c(seq(25,35,1)))+
  geom_vline(xintercept = 30, linetype="dotted")+
  scale_color_calc()+
  facet_grid(scenario+field~stock)



table(cinp_df$farm)

Carbon_inputs <- 
cinp_df |> 
  filter(between(year,25,35)) |> 
  ggplot(aes(y=Ctop+Csub+Cman,x=year, group=field_info,
             col=cinp#scenario#, 
             #fill=interaction(Crop,CC)
  )
  )+ 
  #geom_col()+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = c(seq(25,35,1)))+
  geom_vline(xintercept = 30, linetype="dotted")+
  scale_color_calc()+
  facet_grid(scenario+field~stock)

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/Carbon_inputs.pdf",
       Carbon_inputs,
       device = "pdf")
# Climate data 
temp <- 
temp_df |> ggplot(aes(x=month,y=temp_ave))+
  geom_line()+
  geom_point()+
  geom_point(aes(y=temp_range, col="Temperature range"))+
  geom_line(aes(y=temp_range, col="Temperature range"))+
  scale_y_continuous(name='Temperature C*',
                     #sec.axis = sec_axis(~.*1, name = "")
                     )+
  scale_x_continuous(breaks = seq(1,12,1))
  
ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/temp.pdf",
       temp,
       device = "pdf")

example_pool_dist <- 
  scn_list[[30]] |> 
  as.data.frame() |> 
  pivot_longer(
    cols = c("FOM_top","HUM_top","ROM_top",
             "FOM_sub","HUM_sub","ROM_sub"), 
    names_to = "pool",
    values_to = "SOC" )|>  
  mutate(pool = fct_relevel(pool, 
                            "FOM_top", "FOM_sub", 
                            "HUM_top", "HUM_sub",
                            "ROM_top", "ROM_sub")) |> 
  ggplot(aes(x=make_date(year=yrs,month=mon),y=SOC,fill=pool))+
  geom_col(position = "stack")

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/example_pool_dist.pdf",
       example_pool_dist,
       device = "pdf")
# results -----
field_results <- 
field_results |> 
  ggplot(aes(y= C_topsoil#+C_subsoil
             ,
             x=yrs, 
             group=field_info,
             col=cinp
             #scenario#, 
             #fill=interaction(Crop,CC)
  )
  )+ 
  #geom_col()+
  #geom_point()+
  geom_line()+
  geom_smooth(se=FALSE)+
  #scale_x_continuous(breaks = c(seq(25,35,1)))+
  geom_vline(xintercept = 30, linetype="dotted")+
  facet_grid(stock~scenario+field)+
  scale_x_continuous(breaks = seq(0,130,30))+
  scale_color_hc()+
  theme(legend.position = "bottom")

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/Cstocks_perfield_topsoil_horizontal.pdf",
       field_results,
       device = "pdf")

delta_field <- 
delta_field |> select(!contains("C1mt")) |>
  pivot_longer(cols = starts_with("delta_"),
                            names_to = 'variable',
                            values_to = "Delta C") |> 
   
  ggplot(aes(x=variable,y=`Delta C`, fill=variable))+
  geom_col()+
  facet_grid(stock~scenario+field)+
  scale_fill_calc()+
  theme(axis.text.x = element_text(angle=90), legend.text = element_blank())

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/delta_field.pdf",
       delta_field,
       device = "pdf")

# field_results |> 
#   ggplot(aes(y= Ctop+Csub+Cman,
#              x=yr, 
#              group=field_info,
#              col=cinp
#              #scenario#, 
#              #fill=interaction(Crop,CC)
#   )
#   )+ 
#   #geom_col()+
#   geom_point()+
#   geom_line()+
#   geom_smooth(se=FALSE)+
#   #scale_x_continuous(breaks = c(seq(25,35,1)))+
#   geom_vline(xintercept = 30, linetype="dotted")+
#   facet_grid(stock~scenario+field)+
#   scale_x_continuous(breaks = seq(0,100,10))+
#   scale_color_hc()+
#   theme(legend.position = "bottom")

farm_results <- 
farm_results_summ|> 
  ggplot(aes(y= C_topsoil,
             x=yrs, 
             group=farm,
             col=cinp
             #scenario#, 
             #fill=interaction(Crop,CC)
  )
  )+
  #geom_col()+
  geom_line()+
  geom_point()+
  geom_smooth(se=FALSE)+
  # geom_line(aes(y=C_subsoil))+
  # geom_point(aes(y=C_subsoil))+
  # geom_smooth(aes(y=C_subsoil),se=FALSE)+
  #scale_x_continuous(breaks = c(seq(25,35,1)))+
  geom_vline(xintercept = 30, linetype="dotted")+
  facet_grid(stock~scenario)+
  scale_x_continuous(breaks = seq(0,130,10))+
  scale_color_hc()+
  theme(legend.position = "bottom")

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/Ctopsoil_perfarm.pdf",
       farm_results,
       device = "pdf")

delta_farm <- 
delta_farm |> select(!contains("C1mt")) |>
  pivot_longer(cols = starts_with("delta_"),
               names_to = 'variable',
               values_to = "Delta C") |> 
  
  ggplot(aes(x=variable,y=`Delta C`, fill=variable))+
  geom_col()+
  facet_grid(stock~scenario)+
  scale_fill_calc()+
  theme(axis.text.x = element_text(angle=90), legend.text = element_blank())

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/delta_farm.pdf",
       delta_farm,
       device = "pdf")
