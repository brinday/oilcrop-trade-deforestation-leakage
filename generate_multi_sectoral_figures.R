
#### FIGURES FOR MULTI-SECTORAL IMPACTS (FOLLOW ON PAPER)

library("reshape2")
library("stringr")
library("scales")
library("plyr")
library("readr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("jgcricolors")
library("grid")
library("rgcam")


# SET WORKING DIRECTORY -----------------------------------------------------------------------------
## REQUIRED: Manually set working directory
setwd("C:/palm_soy_trade/runs_02202024")

PLOT_FOLDER <- paste(getwd(), "/plots/", sep = "")

# Create the specified output directory inside the current working directory
dir.create(PLOT_FOLDER)

# FUNCTIONS ---------------------------------------------------------------
#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )

#Fill annual (fill to all GCAM years). If CUMULATIVE = T, get cumulative
Fill_annual <- function(.df, CUMULATIVE = FALSE,
                        CUM_YEAR_START = 2020,
                        CUM_OUT_STEP = 5){
  YEAR_START <- min(unique(.df$year))
  YEAR_END <- max(unique(.df$year))
  .df %>% mutate(year = as.integer(year)) -> .df


  .df %>% filter(year >= YEAR_START) %>%
    bind_rows(
      .df %>%
        #assuming YEAR_END has values for all
        filter(year == YEAR_END) %>% select(-year) %>%
        mutate(value = NA) %>%
        gcamdata::repeat_add_columns(tibble(year = setdiff(seq(YEAR_START,YEAR_END), unique(.df$year))))
    ) %>% arrange(year) %>%
    mutate(value = gcamdata::approx_fun(year, value, rule = 2)) -> .df1

  if (CUMULATIVE == TRUE ) {
    assertthat::assert_that(CUM_YEAR_START >= YEAR_START)
    .df1 %>% filter(year >= CUM_YEAR_START) %>%
      mutate(value = cumsum(value)) %>% filter(year >= CUM_YEAR_START) %>%
      filter(year %in% seq(YEAR_START,YEAR_END, CUM_OUT_STEP))-> .df1
  }
  return(.df1)
}


aggregate_rows <- function(df, filter_var, var_name, filter_group, ...) {
  group_var <- quos(...)
  filter_var <- enquo(filter_var)
  filter_var_name <- quo_name(filter_var)
  df %>%
    filter(!!filter_var %in% filter_group) %>%
    group_by(!!!group_var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(!!filter_var_name := !!var_name)
}

#parse the output for scenarios with just the one "scenarios" parameter
parse_output_scenario <- function (df) {
  #remove the duplicated headers and columnn names
  df <- df[!(duplicated(df) | duplicated(df, fromLast = TRUE)), ]

  #remove 1990
  try(df <- select(df, -c("1990")))
  try(df <- select(df, -c("1980", "1985", "1995", "2000")))

  #remove columns with NA
  df <- df[,!apply(is.na(df), 2, any)]


  #separate scenario name and date
  df <- separate(df, col = "scenario", into = c("scenario", "date"), sep = c(","))

  #tidy data
  YEARS <- as.character(seq(2005,2100,by=5))
  df <- gather(df, YEARS, key = "year", value = "value") %>%
    dplyr::mutate(year = as.integer(year)) %>%
    select(-date)

  return (df)
}


#returns difference from a scenario
diff_from_scen <- function(df, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_df <- df %>%
    filter(scenario %in% diff_scenarios)

  ref_df <- df %>%
    filter(scenario %in% ref_scenario)

  output_df <- diff_df %>%
    full_join(ref_df, by = join_var,
              suffix = c(".diff", ".ref")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.ref = if_else(is.na(value.ref),0,value.ref),
           value = value.diff - value.ref)

  net_df <- output_df %>%
    group_by(scenario.diff, !!!net_join_var) %>%
    dplyr::summarise(value.net = sum(value)) %>%
    ungroup()

  output_df_new <- output_df %>%
    left_join(net_df,
              suffix = c("", ".net"))

  return(output_df_new)
}



#returns % difference from a scenario
pct_diff_from_scen <- function(df, diff_scenarios, ref_scenario, join_var){

  diff_df <- df %>%
    filter(scenario %in% diff_scenarios)

  ref_df <- df %>%
    filter(scenario %in% ref_scenario)

  output_df <- diff_df %>%
    full_join(ref_df, by = join_var,
              suffix = c(".diff", ".ref")) %>%
    mutate(value = ((value.diff - value.ref)/value.ref) * 100)

  return(output_df)
}


#returns difference from 2015
diff_from_2015 <- function(df, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_2015 <- df %>%
    filter(scenario == ref_scenario,year == 2015)

  output_df <- df %>%
    full_join(diff_2015, by = join_var,
              suffix = c(".diff", ".2015")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.2015 = if_else(is.na(value.2015),0,value.2015),
           value = value.diff - value.2015)

  net_df <- output_df %>%
    group_by(scenario.diff, !!!net_join_var) %>%
    dplyr::summarise(value.net = sum(value)) %>%
    ungroup()

  output_df_new <- output_df %>%
    left_join(net_df,
              suffix = c("", ".net"))

  return(output_df_new)
}

#returns pct difference from 2015
pct_diff_from_2015 <- function(df, diff_scenarios, ref_scenario, join_var, ...){
  net_join_var <- quos(...)

  diff_2015 <- df %>%
    filter(scenario == ref_scenario,year == 2015)

  output_df <- df %>%
    full_join(diff_2015, by = join_var,
              suffix = c(".diff", ".2015")) %>%

    mutate(value.diff = if_else(is.na(value.diff),0,value.diff),
           value.2015 = if_else(is.na(value.2015),0,value.2015),
           value = ((value.diff - value.2015)/value.2015)*100)

  net_df <- output_df %>%
    group_by(scenario.diff, !!!net_join_var) %>%
    dplyr::summarise(value.net = sum(value)) %>%
    ungroup()

  output_df_new <- output_df %>%
    left_join(net_df,
              suffix = c("", ".net"))

  return(output_df_new)
}

# COLORS, LABELS, LEVELS ------------------------------------------------------------------

#SCENARIOS
REF_scenario <- c("A_REF")
diff_scenarios <- c( "B_EU_freeze",
                     "D_HighInc_freeze",
                     "E_EUTop5_freeze",
                     "F_EU_ban")
forest_scenarios <- c("G_LUCpa")

scenario_bar_labels_2030 <- c("A_REF.2015" = "2015",
                              "blank1" = "",
                              "A_REF.2030" = 'REF',
                              "B_EU_freeze.2030" = "EU freeze",
                              "D_HighInc_freeze.2030" = "HighInc freeze",
                              "E_EUTop5_freeze.2030" = "EU/Top 5 freeze",
                              "F_EU_ban.2030" = "EU ban",
                              "G_LUCpa.2030" = "Land protect")

scenario_bar_labels_2050 <- c("A_REF.2015" = "2015",
                              "blank1" = "",
                              "A_REF.2050" = 'REF',
                              "B_EU_freeze.2050" = "EU freeze",
                              "D_HighInc_freeze.2050" = "HighInc freeze",
                              "E_EUTop5_freeze.2050" = "EU/Top 5 freeze",
                              "F_EU_ban.2050" = "EU ban",
                              "G_LUCpa.2050" = "Land protect")

#REGIONS
agg_region_levels <- c("ROW",
                       "Russia",
                       "Canada",
                       "USA",
                       "Argentina",
                       "Brazil",
                       "Indonesia",
                       "RO SEAsia",
                       "Africa",
                       "India",
                       "China",
                       "EU",
                       "domestic")

agg_region_levels_w_global <- c("Global",
                                agg_region_levels)


#WATER
water_levels <- c("OilPalm",
                  "Soybean",
                  "Other oil crops",
                  "Purpose-grown energy crops",
                  "Other crops",
                  "Livestock",
                  "Municipal",
                  "Energy")

water_colors <- c("Other oil crops" = "darkorchid1",
                  "OilPalm" = "dodgerblue",
                  "Soybean" = "goldenrod1",
                  "Other crops" = "#e68613",
                  "Purpose-grown energy crops" = "#7cae00",
                  "Livestock" = "#ad440c",
                  "Municipal" = "grey40",
                  "Energy" = "grey70")

#FERTILIZER
fert_levels <- c("OilPalm",
                 "Soybean",
                 "Other oil crops",
                 "Purpose-grown energy crops",
                 "Other crops",
                 "Exports")

fert_colors <- c("Other oil crops" = "darkorchid1",
                 "OilPalm" = "dodgerblue",
                 "Soybean" = "goldenrod1",
                 "Other crops" = "#e68613",
                 "Purpose-grown energy crops" = "#7cae00",
                 "Exports" = "grey40")


# SELECT DATABASE ---------------------------------------------------------


prj <- rgcam::loadProject("bi_soy_palm.dat")

prj_water <- rgcam::loadProject("bi_soy_palm_water.dat")


# READ IN MAPPING FILES -----------------------------------------------------------

grouped_region_mapping <- readr::read_csv("../mapping/grouped_region_mapping_v3.csv")
grouped_region_mapping_palm_import <- readr::read_csv("../mapping/grouped_region_mapping_palm_import.csv")
grouped_region_mapping_soy_import <- readr::read_csv("../mapping/grouped_region_mapping_soy_import.csv")
grouped_region_mapping_palm_export <- readr::read_csv("../mapping/grouped_region_mapping_palm_export.csv")
grouped_region_mapping_palm_export_poster <- readr::read_csv("../mapping/grouped_region_mapping_palm_export_poster.csv")
grouped_region_mapping_soy_export <- readr::read_csv("../mapping/grouped_region_mapping_soy_export.csv")
land_alloc_mapping <- readr::read_csv("../mapping/land_alloc_mapping_v3.csv")
refining_mapping <- readr::read_csv("../mapping/refining_mapping.csv")
commodity_alloc_mapping <- readr::read_csv("../mapping/commodity_alloc_mapping.csv") %>%
  mutate(input = tolower(input),
         agg_input = tolower(agg_input))

GWP <- readr::read_csv("../mapping/GWP.csv")


# READ IN FILES -----------------------------------------------------------
GCAM_YEARS <- as.character(c("1990","2005", seq(2010,2100,by=5)))

GDP <- readr::read_csv("../mapping/GDP.csv", skip = 1)

GDP = parse_output_scenario(GDP) %>%
  mutate(value = value * 10^6 * 108.595965/60.81836633,
         Units = "2020$")


pop <- readr::read_csv("../mapping/pop.csv") %>%
  pivot_longer(cols = as.character(c(1990, 2005, seq(2010, 2100, by = 5))), names_to = "year")

# RETREIVE QUERIES  --------------------------------------------------------
## Retrieve queries for all scenarios in the dataset,
## formatted as a single table


water_wd_tech <- getQuery(prj, "water withdrawals by tech")
water_runoff <- getQuery(prj_water, "Basin level available runoff")

fert_cons <- getQuery(prj, "fertilizer consumption by crop type")

fert_cons_tech <- getQuery(prj, "fertilizer consumption by ag tech") %>%
  separate(subsector, into = c("subsector", "basin"), sep = "_")
# WATER WITHDRAWALS -------------------------------------------------------

livestock_wd <- water_wd_tech %>%
  filter(sector %in% c("Beef", "Dairy", "Pork", "Poultry", "SheepGoat")) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Livestock") %>%
  ungroup()


agriculture_wd <- water_wd_tech %>%
  filter(sector %in% c("Corn", "MiscCrop", "OtherGrain", "Rice", "RootTuber", "SugarCrop", "Wheat", "FodderHerb", "FodderGrass",
                       "Fruits", "Legumes", "NutsSeeds", "Vegetables")) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Other crops") %>%
  ungroup()

energy_wd_tech <- water_wd_tech %>%
  filter(sector %in% c("electricity", "elec_biomass (conv CCS)", "elec_biomass (IGCC CCS)", "elec_coal (conv pul CCS)", "elec_gas (CC CCS)",
                       "nuclearFuelGenII", "nuclearFuelGenIII",
                       "total coal", "total natural gas", "total oil", "unconventional oil production",
                       "H2 central production", "H2 industrial", "H2 wholesale dispensing","other industry", "industry")) %>%
  select(-sector, -subsector) %>%
  group_by(Units, scenario, region, technology, year) %>%
  dplyr::summarise(value = sum(value))

energy_wd <- water_wd_tech %>%
  filter(sector %in% c("electricity", "elec_biomass (conv CCS)", "elec_biomass (IGCC CCS)", "elec_coal (conv pul CCS)", "elec_gas (CC CCS)",
                       "nuclearFuelGenII", "nuclearFuelGenIII",
                       "total coal", "total natural gas", "total oil", "unconventional oil production",
                       "H2 central production", "H2 industrial", "H2 wholesale dispensing","other industry", "industry")) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Energy") %>%
  ungroup()

other_oil_crop_wd <- water_wd_tech %>%
  filter(sector %in% c("FiberCrop", "OilCrop")) %>%
  group_by(scenario, region, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "Other oil crops") %>%
  ungroup()

biomass_wd <-  water_wd_tech %>%
  filter(sector %in% c("biomass")) %>%
  mutate(sector = "Purpose-grown energy crops")

municipal_wd <- water_wd_tech %>%
  filter(sector %in% c("municipal water")) %>%
  mutate(sector = "Municipal")

agg_water_wd <- water_wd_tech %>%
  filter(sector %in% c("OilPalm","Soybean")) %>%
  group_by(scenario, sector, region, year, Units) %>%
  dplyr::summarise(value = sum(value)) %>%
  bind_rows(livestock_wd, agriculture_wd, energy_wd, other_oil_crop_wd, biomass_wd, municipal_wd) %>%
  ungroup()

grouped_agg_water_wd <- agg_water_wd %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(Units, scenario, grouped_region,sector, year) %>%
  dplyr::summarise(value = sum(value))

global_agg_water_wd <- grouped_agg_water_wd %>%
  group_by(Units, scenario, sector, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(grouped_region = "Global") %>%
  bind_rows(grouped_agg_water_wd)

water_wd_reg_basin <- water_wd_tech %>%
  separate(subsector, into = c("subsector", "basin"), sep = "_") %>%
  group_by(Units, scenario, region, basin, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#basin only
water_wd_basin <- water_wd_reg_basin %>%
  group_by(Units, scenario, basin, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

water_runoff_basin <- water_runoff %>%
  separate(basin, into = c("basin", NA), sep = "_water") %>%
  group_by(Units, scenario, basin, year) %>%
  dplyr::summarise(value = sum(value))

#water scarcity = demand/supply
water_scarcity <- water_wd_basin %>%
  left_join(water_runoff_basin, by = c("Units", "scenario", "basin", "year"), suffix = c(".demand", ".supply")) %>%
  na.omit() %>%
  mutate(value = value.demand/value.supply)


# CUMULATIVE WATER WITHDRAWALS --------------------------------------------

cum_water_wd_reg_basin <- water_wd_reg_basin %>%
  filter(year >= 2015) %>%
  group_by(Units, scenario, region, basin) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2015)

cum_water_wd_basin <- water_wd_basin %>%
  filter(year >= 2015) %>%
  group_by(Units, scenario, basin) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2015)

cum_agg_water_wd <- global_agg_water_wd %>%
  filter(year >= 2015) %>%
  group_by(Units, scenario, grouped_region, sector) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2015)


# DIFF CUMULATIVE WATER WITHDRAWALS ---------------------------------------

diff_cum_water_wd_reg_basin <- diff_from_scen(cum_water_wd_reg_basin,
                                              diff_scenarios = c(diff_scenarios, forest_scenarios),
                                              ref_scenario = REF_scenario,
                                              join_var = c("Units", "region", "basin", "year"),
                                              Units, region, basin, year)

diff_cum_water_wd_basin <- diff_from_scen(cum_water_wd_basin,
                                          diff_scenarios = c(diff_scenarios, forest_scenarios),
                                          ref_scenario = REF_scenario,
                                          join_var = c("Units", "basin", "year"),
                                          Units, basin, year)

diff_cum_agg_water_wd <- diff_from_scen(cum_agg_water_wd,
                                        diff_scenarios = c(diff_scenarios, forest_scenarios),
                                        ref_scenario = REF_scenario,
                                        join_var = c("Units", "sector", "year", "grouped_region"))

# PLOT DIFF WATER WITHDRAWALS (LINE PLOT) --------------------------------------------------


diff_grouped_agg_water_wd$grouped_region <- factor(diff_grouped_agg_water_wd$grouped_region,
                                                   levels = agg_region_levels_w_global)
diff_grouped_agg_water_wd$sector <- factor(diff_grouped_agg_water_wd$sector,
                                           levels = water_levels)

ggplot(data = filter(diff_grouped_agg_water_wd,
                     year == 2030),
       aes(x = grouped_region, y = value, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Water withdrawals", x = "", y = "diff. km^3") +
  facet_wrap(~interaction(scenario.diff, year), nrow = 1, scales = "fixed", labeller = as_labeller(scenario_bar_labels_2030)) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = water_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"diff_water_wd_2030.png", sep = ""),width=20, height=5, units="in")

ggplot(data = filter(diff_grouped_agg_water_wd,
                     year == 2050),
       aes(x = grouped_region, y = value, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Water withdrawals", x = "", y = "diff. km^3") +
  facet_wrap(~interaction(scenario.diff, year), nrow = 1, scales = "fixed", labeller = as_labeller(scenario_bar_labels_2050)) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = water_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"diff_water_wd_2050.png", sep = ""),width=20, height=5, units="in")



# PLOT CUM DIFF WATER WITHDRAWALS (LINE PLOT) --------------------------------------------------


diff_cum_agg_water_wd$grouped_region <- factor(diff_cum_agg_water_wd$grouped_region,
                                                   levels = agg_region_levels_w_global)
diff_cum_agg_water_wd$sector <- factor(diff_cum_agg_water_wd$sector,
                                           levels = water_levels)

ggplot(data = filter(diff_cum_agg_water_wd,
                     year == 2050),
       aes(x = grouped_region, y = value, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Cumulative water withdrawals", x = "", y = "diff. km^3") +
  facet_wrap(~interaction(scenario.diff, year), nrow = 1, scales = "fixed", labeller = as_labeller(scenario_bar_labels_2050)) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = water_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"cum_diff_water_wd_2050.png", sep = ""),width=20, height=5, units="in")



# FERT CONS --------------------------------------------------

other_crop_fert_cons <- fert_cons %>%
  filter(sector %in% c("Corn", "FodderGrass", "FodderHerb", "Fruits", "Legumes",
                       "MiscCrop", "NutsSeeds", "OtherGrain", "Rice", "RootTuber", "SugarCrop",
                       "Vegetables", "Wheat")) %>%
  group_by(Units, scenario, region, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(sector = "Other crops")

other_oil_crop_fert_cons <- fert_cons %>%
  filter(sector %in% c("FiberCrop", "OilCrop")) %>%
  group_by(Units, scenario, region, input, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup() %>%
  mutate(sector = "Other oil crops")

biomass_fert_cons <- fert_cons %>%
  filter(sector %in% c("biomass")) %>%
  mutate(sector = "Purpose-grown energy crops")

export_fert_cons <- fert_cons %>%
  filter(sector %in% c("Exports_fertilizer")) %>%
  mutate(sector = "Exports")

agg_fert_cons <- fert_cons %>%
  filter(sector %in% c("OilPalm", "Soybean")) %>%
  bind_rows(other_crop_fert_cons, other_oil_crop_fert_cons, biomass_fert_cons, export_fert_cons)

grouped_agg_fert_cons <- agg_fert_cons %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(Units, scenario, grouped_region,sector, year) %>%
  dplyr::summarise(value = sum(value))

global_agg_fert_cons <- grouped_agg_fert_cons %>%
  group_by(Units, scenario, sector, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(grouped_region = "Global") %>%
  bind_rows(grouped_agg_fert_cons)

fert_cons_reg_basin <- fert_cons_tech %>%
  group_by(Units, scenario, region, basin, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  ungroup()

#basin only
fert_cons_basin <- fert_cons_tech %>%
  group_by(Units, scenario, basin, year) %>%
  dplyr::summarise(value = sum(value))

# DIFF FERT CONS  ---------------------------------------------------------

diff_grouped_agg_fert_cons <- diff_from_scen(global_agg_fert_cons,
                                             diff_scenarios = c(diff_scenarios, forest_scenarios),
                                             ref_scenario = REF_scenario,
                                             join_var = c("Units", "grouped_region", "sector", "year"),
                                             Units, grouped_region, year)

diff_grouped_agg_fert_cons$grouped_region <- factor(diff_grouped_agg_fert_cons$grouped_region,
                                                    levels = agg_region_levels_w_global)
diff_grouped_agg_fert_cons$sector <- factor(diff_grouped_agg_fert_cons$sector,
                                            levels = fert_levels)

# PLOT DIFF FERT CONS -----------------------------------------------------

ggplot(data = filter(diff_grouped_agg_fert_cons,
                     year == 2030),
       aes(x = grouped_region, y = value*100, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Fertilizer consumption", x = "", y = "diff. Mt N") +
  facet_wrap(~interaction(scenario.diff, year), nrow = 1, scales = "fixed", labeller = as_labeller(scenario_bar_labels_2030)) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = fert_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"diff_fert_cons_2030.png", sep = ""),width=20, height=5, units="in")

ggplot(data = filter(diff_grouped_agg_fert_cons,
                     year == 2050),
       aes(x = grouped_region, y = value*100, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Fertilizer consumption", x = "", y = "diff. Mt N") +
  facet_wrap(~interaction(scenario.diff, year), nrow = 1, scales = "fixed", labeller = as_labeller(scenario_bar_labels_2050)) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = fert_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"diff_fert_cons_2050.png", sep = ""),width=20, height=5, units="in")



# CUMULATIVE FERT CONS --------------------------------------------

cum_fert_cons_reg_basin <- fert_cons_reg_basin %>%
  filter(year >= 2015) %>%
  group_by(Units, scenario, region, basin) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2015)

cum_fert_cons_basin <- fert_cons_basin %>%
  filter(year >= 2015) %>%
  group_by(Units, scenario, basin) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2015)

cum_agg_fert_cons <- global_agg_fert_cons %>%
  filter(year >= 2015) %>%
  group_by(Units, scenario, sector, grouped_region) %>%
  Fill_annual(CUMULATIVE = T, CUM_YEAR_START = 2015)

# DIFF CUMULATIVE FERT CONS  ---------------------------------------

diff_cum_fert_cons_reg_basin <- diff_from_scen(cum_fert_cons_reg_basin,
                                               diff_scenarios = c(diff_scenarios, forest_scenarios),
                                               ref_scenario = REF_scenario,
                                               join_var = c("Units", "region", "basin", "year"),
                                               Units, region, basin, year)

diff_cum_fert_cons_basin <- diff_from_scen(cum_fert_cons_basin,
                                           diff_scenarios = c(diff_scenarios, forest_scenarios),
                                           ref_scenario = REF_scenario,
                                           join_var = c("Units", "basin", "year"),
                                           Units, basin, year)




diff_cum_agg_fert_cons <- diff_from_scen(cum_agg_fert_cons,
                                        diff_scenarios = c(diff_scenarios, forest_scenarios),
                                        ref_scenario = REF_scenario,
                                        join_var = c("Units", "sector", "year", "grouped_region"))

# PLOT CUM FERT CONS (LINE PLOT) ------------------------------------------------------


diff_cum_agg_fert_cons$grouped_region <- factor(diff_cum_agg_fert_cons$grouped_region,
                                               levels = agg_region_levels_w_global)
diff_cum_agg_fert_cons$sector <- factor(diff_cum_agg_fert_cons$sector,
                                       levels = water_levels)


ggplot(data = filter(diff_cum_agg_fert_cons,
                     year == 2050),
       aes(x = grouped_region, y = value*100, fill = sector, group = sector))+
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Cumulative fertilizer consumption", x = "", y = "diff. Mt N") +
  facet_wrap(~interaction(scenario.diff, year), nrow = 1, scales = "fixed", labeller = as_labeller(scenario_bar_labels_2050)) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = fert_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"diff_cum_fert_cons_2050.png", sep = ""),width=20, height=5, units="in")

