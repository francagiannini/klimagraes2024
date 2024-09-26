library(tidyverse)
library(ggthemes)
library(readxl)

# ggplot theme ----
theme_set(theme_bw())
theme_update(panel.grid = element_blank())

# results reading ----

ggthemes::theme_calc()

cinp_df <- read_excel("cinp_df.xlsx") |> 
  mutate(scenario = fct_relevel(scenario, c("Now", "Equal", "More")))

field_results <- read_excel("field_results_kl24_100.xlsx")|> 
  mutate(scenario = fct_relevel(scenario, c("Now", "Equal", "More")))

farm_results_summ <- read_excel("farm_results_summ_100.xlsx")|> 
  mutate(scenario = fct_relevel(scenario, c("Now", "Equal", "More")))

delta_field <- read_excel("delta_field_100.xlsx")|> 
  mutate(scenario = fct_relevel(scenario, c("Now", "Equal", "More")))

delta_farm <- read_excel("delta_farm_100.xlsx")|> 
  mutate(scenario = fct_relevel(scenario, c("Now", "Equal", "More")))

# Cinputs 
# Carbon_inputs <- 
# cinp_df |> 
#   filter(between(year,25,35)) |> 
#   ggplot(aes(y=Ctop+Csub,x=year, group=field_info,
#              col=cinp#scenario#, 
#              #fill=interaction(Crop,CC)
#              )
#              )+ 
#   #geom_col()+
#   geom_point()+
#   geom_line()+
#   scale_x_continuous(breaks = c(seq(25,35,1)))+
#   geom_vline(xintercept = 30, linetype="dotted")+
#   scale_color_calc()+
#   facet_grid(scenario+field~stock)

# C inputs plots ----

table(cinp_df$field_info)

Carbon_inputs <- 
cinp_df |> 
  filter(between(year,95,105)) |> # & cinp=="fixed") |> 
  ggplot(aes(y=Ctop+Csub+Cman,x=year, group=field_info,
             col= scenario,
             linetype=cinp
             #scenario#, 
             #fill=interaction(Crop,CC)
  )
  )+ 
  #geom_col()+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = c(seq(90,110,1)))+
  geom_vline(xintercept = 100, linetype="dotted")+
  scale_color_colorblind()+
  facet_grid(field~stock)
  #facet_grid(scenario+field~stock)


cinp_df |> group_by(field_info) |> arrange(year) |> 
  mutate(cumsumCinp=cumsum(Ctop+Csub+Cman)) |> 
  ggplot(aes(y=cumsumCinp,x=year, group=field_info,
             col= scenario,
             linetype = cinp
             #scenario#, 
             #fill=interaction(Crop,CC)
  )
  )+ 
  geom_line()+
  geom_vline(xintercept = 100, linetype="dotted")+
  scale_color_colorblind()+
  facet_grid(field~stock)

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

# field results -----

field_results_plot <- 
field_results |> 
  ggplot(aes(y= C_topsoil,
             x=yrs, 
             group=farm,
             col= scenario,
             linetype =cinp
             #scenario#, 
             #fill=interaction(Crop,CC)
  )
  )+
  #geom_col()+
  #geom_line()+
  #geom_point(size=0.01)+
  geom_smooth(se=FALSE)+
  # geom_line(aes(y=C_subsoil))+
  # geom_point(aes(y=C_subsoil))+
  # geom_smooth(aes(y=C_subsoil),se=FALSE)+
  #scale_x_continuous(breaks = c(seq(25,35,1)))+
  geom_vline(xintercept = 100, linetype="dotted")+
  facet_grid(field~stock)+
  scale_x_continuous(breaks = seq(0,200,20))+
  scale_color_colorblind()+
  theme(legend.position = "bottom")

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/Cstocks_perfield_topsoil_horizontal100.pdf",
       field_results_plot,
       device = "pdf")

delta_field_plot <- 
delta_field |> select(!contains("C1mt")) |>
  pivot_longer(cols = starts_with("delta_"),
               names_to = 'variable',
               values_to = "Delta C") |> 
  filter(variable=="delta_Ctopspoil_100yr") |> 
  ggplot(aes(x=scenario,y=`Delta C`, fill=scenario))+
  geom_col()+
  geom_text(aes(label = round(`Delta C`, digits = 2)), vjust = -0.5, size = 5)+
  #scale_y_continuous(limits = c(-8,8))+
  facet_grid(cinp~stock+field)+
  scale_fill_colorblind()+
  labs(y='delta topsoil C (Mg/ha)')+
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom"
        #, legend.text = element_blank()
  )

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/delta_field100.pdf",
       delta_field_plot,
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

# farm results ----

farm_results_plot <- 
farm_results_summ|> 
  ggplot(aes(y= C_topsoil,
             x=yrs, 
             group=farm,
             col= scenario,
             linetype =cinp
             #scenario#, 
             #fill=interaction(Crop,CC)
  )
  )+
  #geom_col()+
  #geom_line()+
  #geom_point(size=0.01)+
  geom_smooth(se=FALSE)+
  # geom_line(aes(y=C_subsoil))+
  # geom_point(aes(y=C_subsoil))+
  # geom_smooth(aes(y=C_subsoil),se=FALSE)+
  #scale_x_continuous(breaks = c(seq(25,35,1)))+
  geom_vline(xintercept = 100, linetype="dotted")+
  facet_grid(.~stock)+
  scale_x_continuous(breaks = seq(0,200,20))+
  scale_color_colorblind()
  

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/Ctopsoil_perfarm100.pdf",
       farm_results_plot,
       device = "pdf")

delta_farm_plot <- 
delta_farm |> select(!contains("C1mt")) |>
  pivot_longer(cols = starts_with("delta_"),
               names_to = 'variable',
               values_to = "Delta C") |> 
  filter(variable=="delta_Ctopspoil_100yr") |> 
  ggplot(aes(x=scenario,y=`Delta C`, fill=scenario))+
  geom_col()+
  geom_text(aes(label = round(`Delta C`, digits = 2)), vjust = -0.5, size = 5)+
  scale_y_continuous(limits = c(-1,6))+
  facet_grid(cinp~stock)+
  scale_fill_colorblind()+
  labs(y='delta topsoil C (Mg/ha)')+
  theme(axis.text.x = element_text(angle=90), legend.position = "bottom"
        #, legend.text = element_blank()
        )

ggsave("O:/Tech_AGRO/Jornaer/Franca/Klimagræs_simulation_results_2024/delta_farm100.pdf",
       delta_farm_plot,
       device = "pdf")
