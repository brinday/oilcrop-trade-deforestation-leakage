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
library("RColorBrewer")
library("circlize")
library("magick")
library("rmap")
library( "zoo" )

# SET WORKING DIRECTORY -----------------------------------------------------------------------------
## REQUIRED: Manually set working directory
setwd("C:/palm_soy_trade/metarepo")

PLOT_FOLDER <- paste(getwd(), "/figures/", sep = "")

# Create the specified output directory inside the current working directory
dir.create(PLOT_FOLDER)

# FUNCTIONS ---------------------------------------------------------------
#Function "is not an element of" (opposite of %in%)
'%!in%' <- function( x, y ) !( '%in%'( x, y ) )

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

# SET FACTOR COLORS, LEVELS AND LABELS ------------------------------------------------------------------

  # LAND GROUPINGS ------------------------------------------------------------------
land_levels <- c("Oil palm",
                 "Soybean",
                 "Other oil crops",
                 "Purpose-grown energy crops",
                 "Other crops",
                 "Pasture",
                 "Grass and Shrub",
                 "Forest")

land_colors <- c("Oil palm" = "dodgerblue",
                 "Soybean" = "goldenrod1",
                 "Other oil crops" = "darkorchid1",
                 "Other crops"= "#e68613",
                 "Purpose-grown energy crops" = "#7cae00",
                 "Pasture" = "burlywood",
                 "Grass and Shrub" = "grey50",
                 "Forest" = "springgreen4")

  # REGION GROUPINGS ------------------------------------------------------------------

agg_region_levels_palm <- c("domestic",
                            "EU",
                            "China",
                            "India",
                            "Africa",
                            "RO SEAsia",
                            "Indonesia",
                            "ROW")

agg_region_levels_soy <- c("domestic",
                           "EU",
                           "China",
                           "Brazil",
                           "Argentina",
                           "USA",
                           "ROW")

agg_region_levels_circle <- c("domestic",
                              "USA",
                              "Argentina",
                              "Brazil",
                              "Indonesia",
                              "RO SEAsia",
                              "Africa",
                              "India",
                              "China",
                              "EU",
                              "ROW")

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

agg_region_colors <- c("EU" = "#E69F00",
                       "China" = "#7fc97f",
                       "India" = "#80b1d3",
                       "Africa" = "plum1",
                       "RO SEAsia" = "#f7988f",
                       "Indonesia" = "#b3de69",
                       "Brazil" = "#f781bf",
                       "Argentina" = "#FFCC00",
                       "USA" = "#b3cde3",
                       "ROW" = "grey70",
                       "domestic" = "grey40")


  # SCENARIOS ---------------------------------------------------------------

paper_scenarios <- c("A_REF",
                     "B_EU_freeze",
                     "E_EUTop5_freeze",
                     "D_HighInc_freeze",
                     "F_EU_ban",
                     "G_LUCpa")

scenario_levels <- c(paper_scenarios)

scenario_labels <- c("A_REF" = "REF",
                     "B_EU_freeze" = "EU freeze",
                     "D_HighInc_freeze" = "HighInc freeze",
                     "E_EUTop5_freeze" = "EU/Top 5 freeze",
                     "F_EU_ban" = "EU ban",
                     "G_LUCpa" = "Land protect")

scenario_colors <- c("A_REF" = "gray20",
                     "B_EU_freeze" = "#fdd67b",
                     "D_HighInc_freeze" = "#f7988f",
                     "E_EUTop5_freeze" = "#56B4E9",
                     "F_EU_ban" = "#88c892",
                     "G_LUCpa" = "#999999")

#REF SCENARIO
REF_scenario <- c("A_REF")

REF_scenario_bar_limits_2050 <- c("A_REF.2015",
                                  "A_REF.2050")

REF_scenario_bar_labels_2050 <- c("A_REF.2015" = "2015",
                                  "A_REF.2050" = "2050 REF")

#DIFF SCENARIOS
diff_scenarios <- c( "B_EU_freeze",
                     "D_HighInc_freeze",
                     "E_EUTop5_freeze",
                     "F_EU_ban")

scenario_bar_labels_2050_diff <- c("B_EU_freeze.2050" = "EU freeze",
                                   "D_HighInc_freeze.2050" = "HighInc freeze",
                                   "E_EUTop5_freeze.2050" = "EU/Top 5 freeze",
                                   "F_EU_ban.2050" = "EU ban")

#FOREST PROTECT SCENARIO
forest_scenarios <- c("G_LUCpa")

scenario_forest_bar_labels_2050_diff <- c("B_EU_freeze.2050" = "EU freeze - REF",
                                          "D_HighInc_freeze.2050" = "HighInc freeze - REF",
                                          "E_EUTop5_freeze.2050" = "EU/Top 5 freeze - REF",
                                          "F_EU_ban.2050" = "EU ban - REF",
                                          "G_LUCpa.2050" = "Land protect - REF")


# READ IN MAPPING FILES -----------------------------------------------------------

grouped_region_mapping <- readr::read_csv("./mapping/grouped_region_mapping.csv")
grouped_region_mapping_palm_import <- readr::read_csv("./mapping/grouped_region_mapping_palm_import.csv")
grouped_region_mapping_soy_import <- readr::read_csv("./mapping/grouped_region_mapping_soy_import.csv")
grouped_region_mapping_palm_export <- readr::read_csv("./mapping/grouped_region_mapping_palm_export.csv")
grouped_region_mapping_soy_export <- readr::read_csv("./mapping/grouped_region_mapping_soy_export.csv")
land_alloc_mapping <- readr::read_csv("./mapping/land_alloc_mapping.csv")
commodity_alloc_mapping <- readr::read_csv("./mapping/commodity_alloc_mapping.csv") %>%
  mutate(input = tolower(input),
         agg_input = tolower(agg_input))

GWP <- readr::read_csv("../mapping/GWP.csv")


# SELECT DATABASE ---------------------------------------------------------

prj <- rgcam::loadProject("bi_soy_palm.dat")

# RETREIVE QUERIES  --------------------------------------------------------
## Retrieve queries for all scenarios in the dataset,
## formatted as a single table

#supply
forest_import_domestic <- getQuery(prj, "forest import vs. domestic supply (Regional Armington competition)") %>%
  separate(subsector, into = c("trade", "subsector"), sep = " ") %>%
  filter(trade %in% c("domestic", "imported"), subsector != "iron") %>%
  select(-input)

ag_import_domestic <- getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)") %>%
  separate(subsector, into = c("trade", "subsector"), sep = " ") %>%
  filter(trade != "total") %>%
  filter(!grepl("import-ban", input)) %>%
  select(-input) %>%
  mutate(subsector = if_else(subsector == "nuts_seeds", "nutsseeds", subsector),
         subsector = if_else(subsector == "root_tuber", "roottuber", subsector)) %>%
  bind_rows(forest_import_domestic)

ag_import <- ag_import_domestic %>%
  filter(trade == "imported")
ag_domestic <- ag_import_domestic %>%
  filter(trade == "domestic")
ag_export <- getQuery(prj, "ag export to the world center (USA) (Intl. Armington competition)") %>%
  dplyr::rename(import_region = region) %>%
  filter(!grepl("export-ban", input),
         !grepl("import_constraint", input),
         !grepl("iron and steel", input)) %>%
  separate(subsector, into = c("region", "subsector"), sep = " traded ") %>%
  mutate(trade = "exported")

#prices
forest_import_domestic_prices <- getQuery(prj, "forest import vs. domestic prices") %>%
  separate(subsector, into = c("trade", "subsector"), sep = " ") %>%
  # filter(!grepl("import-ban", input)) %>%
  filter(trade %in% c("domestic", "imported"), subsector != "iron") %>%
  mutate(value = value*3.8,
         Units = "2020$/kg")

ag_import_domestic_prices <- getQuery(prj, "ag import vs. domestic prices") %>%
  separate(subsector, into = c("trade", "subsector"), sep = " ") %>%
  # filter(!grepl("import-ban", input)) %>%
  mutate(value = value*3.8,
         Units = "2020$/kg") %>%
  mutate(subsector = if_else(subsector == "nuts_seeds", "nutsseeds", subsector),
         subsector = if_else(subsector == "root_tuber", "roottuber", subsector)) %>%
  bind_rows(forest_import_domestic_prices)

ag_import_prices <- ag_import_domestic_prices %>%
  filter(trade == "imported")
ag_domestic_prices <- ag_import_domestic_prices %>%
  filter(trade == "domestic")
ag_export_prices <- getQuery(prj, "ag export prices (at world center USA)") %>%
  dplyr::rename(import_region = region) %>%
  separate(subsector, into = c("region", "subsector"), sep = " traded ") %>%
  mutate(trade = "exported",
         value = value*3.8,
         Units = "2020$/kg") %>%
  mutate(subsector = if_else(subsector == "nuts_seeds", "nutsseeds", subsector),
         subsector = if_else(subsector == "root_tuber", "roottuber", subsector))

crop_land_alloc <- getQuery(prj, "land allocation by crop") 

LUC_emissions <- getQuery(prj, "LUC emissions by region") %>%
  mutate(value = value*(44/12),
         Units = "MtCO2/yr")

# DATA PROCESSING ---------------------------------------------------------
  # PALM AND SOY SUPPLY (BILATERAL TRADE + DOMESTIC) [Mt]---------------------------------------------------------

#bilateral trade
oil_palm_bilateral <- ag_export %>%
  filter(sector == "traded oilpalm", trade == "exported")

soy_bilateral <- ag_export %>%
  filter(sector == "traded soybean", trade == "exported")

grouped_oil_palm_bilateral <- oil_palm_bilateral %>%
  left_join(grouped_region_mapping_palm_export, by = c("region")) %>%
  group_by(Units, scenario, grouped_region, import_region, input, year, trade) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(grouped_region_export = grouped_region) %>%
  left_join(grouped_region_mapping_palm_import, by = c("import_region" = "region")) %>%
  group_by(Units, scenario, grouped_region_export, grouped_region, input, year, trade) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(grouped_region_import = grouped_region) %>%
  mutate(grouped_region_import = factor(grouped_region_import, levels = agg_region_levels_palm),
         grouped_region_export = factor(grouped_region_export, levels = agg_region_levels_palm),
         input = tolower(input))

grouped_soy_bilateral <- soy_bilateral %>%
  left_join(grouped_region_mapping_soy_export, by = c("region")) %>%
  group_by(Units, scenario, grouped_region, import_region, input, year, trade) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(grouped_region_export = grouped_region) %>%
  left_join(grouped_region_mapping_soy_import, by = c("import_region" = "region")) %>%
  group_by(Units, scenario, grouped_region_export, grouped_region, input, year, trade) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(grouped_region_import = grouped_region) %>%
  mutate(grouped_region_import = factor(grouped_region_import, levels = agg_region_levels_soy),
         grouped_region_export = factor(grouped_region_export, levels = agg_region_levels_soy),
         input = tolower(input))

#domestic
grouped_oil_palm_domestic <- ag_domestic %>%
  filter(subsector == "oilpalm") %>%
  left_join(grouped_region_mapping_palm_export, by = c("region")) %>%
  group_by(Units, scenario, grouped_region, subsector, year, trade) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(grouped_region_export = grouped_region,
                input = subsector) %>%
  dplyr::mutate(grouped_region_import = "domestic")

grouped_soy_domestic <- ag_domestic %>%
  filter(subsector == "soybean") %>%
  left_join(grouped_region_mapping_soy_export, by = c("region")) %>%
  group_by(Units, scenario, grouped_region, subsector, year, trade) %>%
  dplyr::summarise(value = sum(value)) %>%
  dplyr::rename(grouped_region_export = grouped_region,
                input = subsector) %>%
  dplyr::mutate(grouped_region_import = "domestic")


#bilateral trade + domestic
grouped_oil_palm_bilateral_domestic <- bind_rows(grouped_oil_palm_bilateral,
                                                 grouped_oil_palm_domestic) %>%
  mutate(grouped_region_import = factor(grouped_region_import, levels = agg_region_levels_palm),
         grouped_region_export = factor(grouped_region_export, levels = agg_region_levels_palm))


grouped_soy_bilateral_domestic <- bind_rows(grouped_soy_bilateral,
                                            grouped_soy_domestic) %>%
  mutate(grouped_region_import = factor(grouped_region_import, levels = agg_region_levels_soy),
         grouped_region_export = factor(grouped_region_export, levels = agg_region_levels_soy))

grouped_oil_palm_bilateral_domestic_circle <- grouped_oil_palm_bilateral_domestic %>%
  mutate(grouped_region_import = if_else(trade == "domestic", grouped_region_export, grouped_region_import)) %>%
  group_by(Units, scenario, grouped_region_export, grouped_region_import, input, year) %>%
  dplyr::summarise(value = sum(value))

grouped_soy_bilateral_domestic_circle <- grouped_soy_bilateral_domestic %>%
  mutate(grouped_region_import = if_else(trade == "domestic", grouped_region_export, grouped_region_import)) %>%
  group_by(Units, scenario, grouped_region_export, grouped_region_import, input, year) %>%
  dplyr::summarise(value = sum(value))

# production % calculations
grouped_oil_palm_production_total <- grouped_oil_palm_bilateral_domestic %>%
  group_by(scenario, grouped_region_export, input, year) %>%
  dplyr::summarise(value = sum(value))

grouped_soy_production_total <- grouped_soy_bilateral_domestic %>%
  group_by(scenario, grouped_region_export, input, year) %>%
  dplyr::summarise(value = sum(value))

pct_grouped_oil_palm_production_total <- grouped_oil_palm_production_total %>%
  left_join(filter(grouped_oil_palm_production_total, scenario == "A_REF"), by = c("grouped_region_export", "input", "year"), suffix = c(".scen", ".ref")) %>%
  mutate(pct = value.scen*100/value.ref)

pct_grouped_soy_production_total <- grouped_soy_production_total %>%
  left_join(filter(grouped_soy_production_total, scenario == "A_REF"), by = c("grouped_region_export", "input", "year"), suffix = c(".scen", ".ref")) %>%
  mutate(pct = value.scen*100/value.ref)

  # DIFF FROM REF PALM AND SOY SUPPLY [Mt] ----------------------------------

#ban case (add 0s for EU)
grouped_oil_palm_bilateral_domestic_ban_fillout <- grouped_oil_palm_bilateral_domestic %>%
  filter(scenario == "A_REF") %>%
  ungroup() %>%
  select(-scenario, -value) %>%
  distinct() %>%
  left_join(filter(grouped_oil_palm_bilateral_domestic, scenario == "F_EU_ban")) %>%
  mutate(value = if_else(is.na(value), 0, value),
         scenario = "F_EU_ban")

grouped_oil_palm_bilateral_domestic <- grouped_oil_palm_bilateral_domestic %>%
  filter(scenario != "F_EU_ban") %>%
  bind_rows(grouped_oil_palm_bilateral_domestic_ban_fillout)

grouped_soy_bilateral_domestic_ban_fillout <- grouped_soy_bilateral_domestic %>%
  filter(scenario == "A_REF") %>%
  ungroup() %>%
  select(-scenario, -value) %>%
  distinct() %>%
  left_join(filter(grouped_soy_bilateral_domestic, scenario == "F_EU_ban")) %>%
  mutate(value = if_else(is.na(value), 0, value),
         scenario = "F_EU_ban")

grouped_soy_bilateral_domestic <- grouped_soy_bilateral_domestic %>%
  filter(scenario != "F_EU_ban") %>%
  bind_rows(grouped_soy_bilateral_domestic_ban_fillout)


diff_grouped_oil_palm_bilateral_domestic <- diff_from_scen(grouped_oil_palm_bilateral_domestic,
                                                           diff_scenarios = diff_scenarios,
                                                           ref_scenario = REF_scenario,
                                                           join_var = c("grouped_region_export", "grouped_region_import", "input", "trade",  "year", "Units"),
                                                           grouped_region_export, input, year, Units) %>%
  mutate(scenario.diff = factor(scenario.diff), levels = diff_scenarios)

diff_grouped_soy_bilateral_domestic <- diff_from_scen(grouped_soy_bilateral_domestic,
                                                      diff_scenarios = diff_scenarios,
                                                      ref_scenario = REF_scenario,
                                                      join_var = c("grouped_region_export", "grouped_region_import", "input", "trade",  "year", "Units"),
                                                      grouped_region_export, input, year, Units) %>%
  mutate(scenario.diff = factor(scenario.diff), levels = diff_scenarios)

# gross vs. net reduction
diff_gross_grouped_palm_bilateral_domestic <- diff_grouped_oil_palm_bilateral_domestic %>%
  filter(value < 0) %>%
  group_by(scenario.diff, grouped_region_export, year) %>%
  dplyr::summarise(value = sum(value))

diff_gross_grouped_soy_bilateral_domestic <- diff_grouped_soy_bilateral_domestic %>%
  filter(value < 0) %>%
  group_by(scenario.diff, grouped_region_export, year) %>%
  dplyr::summarise(value = sum(value))

  # PALM AND SOY PRODUCTION AND CONSUMPTION [$] -----------------------------

# Multiply $/kg * 10^3 kg/ton * 10^3 ton/Mt * Mt
ag_domestic_USD <- ag_domestic %>%
  left_join(ag_domestic_prices,
            by = c("scenario", "region","trade", "subsector", "year"),
            suffix = c(".volume",".price")) %>%
  mutate(value = value.volume * value.price * 10^9,
         Units = "2020$",
         import_region = "domestic") %>%
  select(-sector.volume, -sector.price)

# ag producer revenue (ag export production * domestic price)
# (the price that producers sell at, which is lower than the price consumers pay, due to tax causing DWL in restricting regions)
ag_export_producer_USD <- ag_export %>%
  select(-sector, -subsector, -trade) %>%
  mutate(input = tolower(input)) %>%
  rename(subsector = input) %>%
  left_join(ag_domestic_prices,
            by = c("scenario","region", "subsector", "year"),
            suffix = c(".volume",".price")) %>%
  select(-sector, -trade)  %>%
  na.omit() %>%
  mutate(value = value.volume * value.price * 10^9,
         Units = "2020$",
         trade = "exported")

ag_import_USD <- ag_import %>%
  left_join(ag_import_prices,
            by = c("scenario", "region", "sector","trade", "subsector", "year"),
            suffix = c(".volume",".price")) %>%
  mutate(value = value.volume * value.price * 10^9,
         Units = "2020$") %>%
  select(-sector)
ag_export_USD <- ag_export %>%
  left_join(ag_export_prices,
            by = c("scenario", "import_region", "region", "sector","trade", "subsector", "year"),
            suffix = c(".volume",".price")) %>%
  mutate(value = value.volume * value.price * 10^9,
         Units = "2020$")


ag_production <- bind_rows(ag_domestic_USD, ag_export_producer_USD)

ag_consumption <- bind_rows(ag_import_USD, ag_domestic_USD)

  # AG PRODUCTION REVENUE  [$]------------------------------------------------------

grouped_ag_production_revenue <-  ag_production %>%
  left_join(grouped_region_mapping, by = "region") %>%
  group_by(scenario, grouped_region, subsector, year, Units) %>%
  dplyr::summarise(value = sum(value))

total_oil_crop_ag_revenue <- grouped_ag_production_revenue %>%
  filter(subsector %in% c("soybean","oilpalm", "oilcrop","fibercrop")) %>%
  group_by(scenario, grouped_region, year, Units) %>%
  dplyr::summarise(value = sum(value))

total_ag_revenue <- grouped_ag_production_revenue %>%
  group_by(scenario, grouped_region, year, Units) %>%
  dplyr::summarise(value = sum(value))

  # DIFF AG PRODUCTION REVENUE [$] ------------------------------------------

diff_grouped_ag_production_revenue <- diff_from_scen(grouped_ag_production_revenue,
                                                     ref_scenario = REF_scenario,
                                                     diff_scenarios = c(diff_scenarios, forest_scenarios),
                                                     join_var = c("grouped_region","subsector", "year", "Units"))

diff_total_oil_crop_ag_revenue <- diff_from_scen(total_oil_crop_ag_revenue,
                                                 ref_scenario = REF_scenario,
                                                 diff_scenarios = c(diff_scenarios, forest_scenarios),
                                                 join_var = c("grouped_region", "year", "Units")) %>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels))

diff_total_ag_revenue <- diff_from_scen(total_ag_revenue,
                                        ref_scenario = REF_scenario,
                                        diff_scenarios = c(diff_scenarios, forest_scenarios),
                                        join_var = c("grouped_region", "year", "Units")) %>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels))

  # CUMULATIVE DIFF AG PRODUCTION REVENUE [$] --------------------------------------

cum_diff_ag_revenue <- diff_grouped_ag_production_revenue %>%
  group_by(scenario.diff, grouped_region, subsector, Units) %>%
  dplyr::mutate(value = cumsum(value)) %>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels))

cum_diff_total_oil_crop_revenue <- diff_total_oil_crop_ag_revenue %>%
  group_by(scenario.diff, grouped_region, Units) %>%
  dplyr::mutate(value = cumsum(value)) %>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels))

cum_diff_total_ag_revenue <- diff_total_ag_revenue %>%
  group_by(scenario.diff, grouped_region, Units) %>%
  dplyr::mutate(value = cumsum(value) )%>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels))

  # AG CONSUMER EXPENDITURE [$] ---------------------------------------------------

grouped_ag_expend <- ag_consumption %>%
  filter(subsector != "sawnwood") %>%
  left_join(grouped_region_mapping, by = "region") %>%
  group_by(scenario, grouped_region, subsector, year, Units) %>%
  dplyr::summarise(value = sum(value))

total_grouped_ag_expend <- grouped_ag_expend %>%
  group_by(scenario, grouped_region, year, Units) %>%
  dplyr::summarise(value = sum(value))

  # DIFF AG CONSUMER EXPENDITURE [$]--------------------------------------------


diff_total_grouped_ag_expend <- diff_from_scen(total_grouped_ag_expend,
                                               ref_scenario = REF_scenario,
                                               diff_scenarios = c(diff_scenarios, forest_scenarios),
                                               join_var = c("grouped_region", "year", "Units")) %>%
  mutate(grouped_region = factor(grouped_region,levels = agg_region_levels))

  # CUM DIFF CONSUMER AG EXPENDITURE [$]----------------------------------------


cum_diff_total_grouped_ag_expend <- diff_total_grouped_ag_expend %>%
  group_by(scenario.diff, grouped_region, Units) %>%
  mutate(value = cumsum(value)) %>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels))

cum_diff_total_grouped_ag_expend_w_global <- cum_diff_total_grouped_ag_expend  %>%
  group_by(scenario.diff, year, Units, scenario.ref) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(region = "Global") %>%
  bind_rows(cum_diff_total_grouped_ag_expend ) %>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels_w_global))

  # LAND ALLOCATION [Ha] ---------------------------------------------------------

agg_crop_land_alloc <- crop_land_alloc %>%
  left_join(land_alloc_mapping, by = c("landleaf")) %>%
  filter(landleaf %!in% c("Tundra", "UrbanLand", "RockIceDesert")) %>%
  group_by(Units, scenario, region, agg_land, year) %>%
  dplyr::summarise(value = sum(value))

grouped_agg_crop_land_alloc <- agg_crop_land_alloc %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(Units, scenario, grouped_region, agg_land, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(agg_land = factor(agg_land, levels = land_levels))


diff_grouped_agg_crop_land_alloc <- diff_from_scen(grouped_agg_crop_land_alloc,
                                                       diff_scenarios = c(diff_scenarios, forest_scenarios),
                                                       ref_scenario = REF_scenario,
                                                       join_var = c("Units", "grouped_region", "agg_land", "year"),
                                                       Units, grouped_region, year) %>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels))

diff_global_agg_crop_land_alloc <- diff_grouped_agg_crop_land_alloc %>%
  ungroup() %>%
  group_by(Units, scenario.diff, agg_land, year, scenario.ref) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(grouped_region = "Global") %>%
  bind_rows(diff_grouped_agg_crop_land_alloc_NDC) %>%
  mutate(grouped_region = factor(grouped_region, levels = agg_region_levels_w_global))

  # LUC EMISSIONS [MtCO2] -----------------------------------------------

reg_LUC_emissions <- LUC_emissions %>%
  group_by(Units, scenario, region, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(sector = "LUC CO2")

grouped_reg_LUC_emissions <- reg_LUC_emissions %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(Units, scenario, grouped_region, year) %>%
  dplyr::summarise(value = sum(value))


global_LUC_emissions <- reg_LUC_emissions %>%
  group_by(Units, scenario, year) %>%
  dplyr::summarise(value = sum(value)) %>%
  mutate(grouped_region = "Global")

grouped_reg_LUC_emissions_w_global <- bind_rows(grouped_reg_LUC_emissions,
                                                global_LUC_emissions)

  # CUMULATIVE LUC EMISSIONS [MtCO2] ----------------------------------------

cum_global_LUC_emissions <- global_LUC_emissions %>%
  filter(year >= 2015) %>%
  group_by(Units, scenario) %>%
  arrange(Units, scenario) %>%
  dplyr::mutate(cum_value = cumsum(value))

cum_reg_LUC_emissions <- reg_LUC_emissions %>%
  filter(year >= 2015) %>%
  group_by(Units, scenario, region) %>%
  arrange(Units, scenario, region) %>%
  dplyr::mutate(cum_value = cumsum(value))

grouped_cum_reg_LUC_emissions <- cum_reg_LUC_emissions %>%
  left_join(grouped_region_mapping, by = c("region")) %>%
  group_by(Units, scenario, grouped_region, year) %>%
  dplyr::summarise(cum_value = sum(cum_value)) %>%
  dplyr::mutate(value = cum_value)


# PAPER FIGURES -----------------------------------------------------------


  # 1A ----------------------------------------------------------------------

for (scen in REF_scenario) {
  grouped_oil_palm_bilateral_domestic_circle %>% filter(scenario == REF_scenario, year == 2015) %>%
    ungroup() %>%
    select(REG_im = grouped_region_import, REG_ex = grouped_region_export, flow = value) %>%
    arrange(factor(REG_im, levels = agg_region_levels_circle)) ->
    dat_circular
  
  dat_circular %>% spread(REG_im, flow) -> A
  
  dat_circular_flip <- dat_circular %>%
    select(REG_ex, REG_im, flow) %>%
    arrange(factor(REG_ex, levels = agg_region_levels_circle))
  
  file.path(PLOT_FOLDER, paste0("Fig_1A.pdf")) -> plotname
  
  #save figure to default wd
  pdf(file = plotname, width = 10, height = 10)
  
  #base plot
  chordDiagram(as.data.frame(dat_circular),
               transparency = 0.5,
               directional = -1,
               direction.type = c("diffHeight", "arrows"),
               diffHeight = -uh(2, "mm")
               ,link.arr.type = "big.arrow"
               ,annotationTrack = c("grid")
               , grid.col = agg_region_colors
               ,preAllocateTracks = list(list(track.height = c(0.3))
                                         ,list(track.height = c(0.035))
               ))
  
  #title(main = "test figure")
  
  circos.track(track.index = 3, panel.fun = function(x, y) {
    circos.axis(h = 1, labels.cex = 1.2)
  }, bg.border = NA)
  
  circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    
    #make text label vertical when space is too small; cex to adjust font size
    
    if(abs(xplot[2] - xplot[1]) < 20 | abs(xplot[2] - xplot[1]) > 340) {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                  niceFacing = TRUE, adj = c(0, 0.5), col = "black",
                  cex = 1.5)
    } else {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                  niceFacing = TRUE, adj = c(0.5, 0), col= "black",
                  cex = 1.5)
    }  }, bg.border = NA)
  
  dev.off() #dump
  
  image_write(image_crop(image_read(plotname),
                         geometry_area(width = 4300, height = 3900, x_off = 350, y_off = 550),
                         repage = FALSE), plotname)
  
  
}

  # 1B ----------------------------------------------------------------------

for (scen in REF_scenario) {
  grouped_soy_bilateral_domestic_circle %>% filter(scenario == REF_scenario, year == 2015) %>%
    ungroup() %>%
    select(REG_im = grouped_region_import, REG_ex = grouped_region_export, flow = value) %>%
    arrange(factor(REG_im, levels = agg_region_levels_circle)) ->
    dat_circular
  
  dat_circular %>% spread(REG_im, flow) -> A
  
  dat_circular_flip <- dat_circular %>%
    select(REG_ex, REG_im, flow) %>%
    arrange(factor(REG_ex, levels = agg_region_levels_circle))
  
  file.path(PLOT_FOLDER, paste0("Fig_1B.pdf")) -> plotname
  
  #save figure to default wd
  pdf(file = plotname, width = 10, height = 10)
  
  #base plot
  chordDiagram(as.data.frame(dat_circular),
               transparency = 0.5,
               directional = -1,
               direction.type = c("diffHeight", "arrows"),
               diffHeight = -uh(2, "mm")
               ,link.arr.type = "big.arrow"
               ,annotationTrack = c("grid")
               , grid.col = agg_region_colors
               ,preAllocateTracks = list(list(track.height = c(0.3))
                                         ,list(track.height = c(0.035))
               ))
  
  #title(main = "test figure")
  
  circos.track(track.index = 3, panel.fun = function(x, y) {
    circos.axis(h = 1, labels.cex = 1.2)
  }, bg.border = NA)
  
  circos.track(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    xplot = get.cell.meta.data("xplot")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    
    #make text label vertical when space is too small; cex to adjust font size
    
    if(abs(xplot[2] - xplot[1]) < 20 | abs(xplot[2] - xplot[1]) > 340) {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                  niceFacing = TRUE, adj = c(0, 0.5), col = "black",
                  cex = 1.5)
    } else {
      circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                  niceFacing = TRUE, adj = c(0.5, 0), col= "black",
                  cex = 1.5)
    }  }, bg.border = NA)
  
  dev.off() #dump
  
  image_write(image_crop(image_read(plotname),
                         geometry_area(width = 4300, height = 3900, x_off = 350, y_off = 550),
                         repage = FALSE), plotname)
  
  
}

  # 1C - REF ----------------------------------------------------------------------

ggplot(data = filter(grouped_oil_palm_bilateral_domestic,
                     year == 2050 || (scenario == REF_scenario & year == 2015),
                     scenario %in% c(REF_scenario)),
       aes(x = interaction(scenario, year), y = value, fill = grouped_region_import, group = grouped_region_import))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Oil palm production in 2050", x = "", y = "Mt") +
  facet_wrap(~grouped_region_export, nrow = 1, scales = "free") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = agg_region_colors, breaks = agg_region_levels)+
  scale_x_discrete(labels = REF_scenario_bar_labels_2050,
                   limits = REF_scenario_bar_limits_2050)+
  ggsave(paste0(PLOT_FOLDER,"Fig_1C_REF.pdf", sep = ""),width=4.5, height=3.5, units="in")


  # 1C - DIFF SCENARIOS -----------------------------------------------------

ggplot(data = filter(diff_grouped_oil_palm_bilateral_domestic,
                     year == 2050,
                     scenario.diff %in% c(diff_scenarios)),
       aes(x = interaction(scenario.diff, year), y = value, fill = grouped_region_import, group = grouped_region_import))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_oil_palm_bilateral_domestic, (year == 2050),
                              scenario.diff %in% c(diff_scenarios)),
                aes(x = interaction(scenario.diff, year), ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Diff. oil palm production in 2050", x = "", y = "Mt") +
  facet_wrap(~grouped_region_export, nrow = 1, scales = "free") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = agg_region_colors, breaks = agg_region_levels)+
  scale_x_discrete(labels = scenario_bar_labels_2050_diff)+
  ggsave(paste0(PLOT_FOLDER,"FIG_1C_SCEN_DIFF.pdf", sep = ""),width=5.5, height=4.25, units="in")


  # 1D - REF ----------------------------------------------------------------

ggplot(data = filter(grouped_soy_bilateral_domestic,
                     year == 2050 || (scenario == REF_scenario & year == 2015),
                     scenario %in% c(REF_scenario)),
       aes(x = interaction(scenario, year), y = value, fill = grouped_region_import, group = grouped_region_import))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Soybean production in 2050", x = "", y = "Mt") +
  facet_wrap(~grouped_region_export, nrow = 1, scales = "free") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = agg_region_colors, breaks = agg_region_levels)+
  scale_x_discrete(labels = REF_scenario_bar_labels_2050,
                   limits = REF_scenario_bar_limits_2050)+
  ggsave(paste0(PLOT_FOLDER,"FIG_1D_REF.pdf", sep = ""),width=5.2, height=3.5, units="in")


  # 1D - DIFF SCENARIOS -----------------------------------------------------

ggplot(data = filter(diff_grouped_soy_bilateral_domestic,
                     year == 2050,
                     scenario.diff %in% c(diff_scenarios)),
       aes(x = interaction(scenario.diff, year), y = value, fill = grouped_region_import, group = grouped_region_import))+
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity")+
  geom_errorbar(data = filter(diff_grouped_soy_bilateral_domestic, (year == 2050),
                              scenario.diff %in% c(diff_scenarios)),
                aes(x = interaction(scenario.diff, year), ymin = value.net, ymax = value.net), lty = "32", width = 1, size = 0.4)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Diff. soy production in 2050", x = "", y = "Mt") +
  facet_wrap(~grouped_region_export, nrow = 1, scales = "free") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_fill_manual(values = agg_region_colors, breaks = agg_region_levels)+
  scale_x_discrete(labels = scenario_bar_labels_2050_diff)+
  ggsave(paste0(PLOT_FOLDER,"FIG_1D_SCEN_DIFF.pdf", sep = ""),width=6.7, height=4.25, units="in")


  # 2 -----------------------------------------------------------------------

ggplot(data = filter(diff_global_agg_crop_land_alloc, scenario.diff %in% c(diff_scenarios, forest_scenarios),
                     year == 2050),
       aes(x = grouped_region, y = value/10, fill = agg_land, group = agg_land))+
  geom_bar(position = position_stack(reverse = FALSE), stat = "identity")+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, linewidth = 1)+
  labs(title = "Land use change from REF in 2050", x = "", y = "diff. million Ha") +
  facet_wrap(~interaction(scenario.diff, year), nrow = 2, scales = "fixed", labeller = as_labeller(scenario_forest_bar_labels_2050_diff)) +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 18)) +
  scale_fill_manual(values = land_colors, name = "Land use")+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"Fig_2.pdf", sep = ""),width=20, height=10, units="in")



  # 3 -----------------------------------------------------------------------

plot_cum_reg_LUC_emissions <- cum_reg_LUC_emissions %>%
  filter(year == 2050,
         scenario %in% c("A_REF",
                         "B_EU_freeze",
                         "D_HighInc_freeze",
                         "E_EUTop5_freeze",
                         "F_EU_ban",
                         "G_LUCpa")) %>%
  mutate(subRegion = region,
         param = "param1",
         value = cum_value,
         subRegion = if_else(subRegion == "EU-12", "EU_12", subRegion),
         subRegion = if_else(subRegion == "EU-15", "EU_15", subRegion)) %>%
  select(scenario, subRegion, value)

plot_map <- rmap::map(plot_cum_reg_LUC_emissions,
                      folder = paste(getwd(), "/", PLOT_FOLDER, sep = ""),
                      title = paste("Cumulative LUC emissions 2015-2050"),
                      scenRef = "A_REF",
                      scenDiff = c("B_EU_freeze",
                                   "D_HighInc_freeze",
                                   "E_EUTop5_freeze",
                                   "F_EU_ban",
                                   "G_LUCpa"),
                      nameAppend = paste("cum_LUC_em_", sep = ""),
                      pdfpng = "pdf")

  # 4A ----------------------------------------------------------------------
ggplot(data = filter(cum_diff_total_oil_crop_revenue,
                     year == 2050, scenario.diff %in% c(paper_scenarios)),
       aes(x = grouped_region, y = value/(10^9), color = scenario.diff, group = scenario.diff))+
  geom_point(size = 2, alpha = 0.65)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "Change in cum. oil crop production revenue in 2050", x = "", y = "$ (billions)") +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_color_manual(labels = scenario_labels,
                     values = scenario_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"Fig_4A.pdf", sep = ""),width=5, height=3, units="in")


  # 4B ----------------------------------------------------------------------

ggplot(data = filter(cum_diff_total_ag_revenue,
                     year == 2050, scenario.diff %in% c(paper_scenarios)),
       aes(x = grouped_region, y = value/(10^9), color = scenario.diff,group = scenario.diff))+
  geom_point(size = 2, alpha = 0.65)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "Change in cum. total production revenue in 2050", x = "", y = "$ (billions)") +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_color_manual(labels = scenario_labels,
                     values = scenario_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"Fig_4B.pdf", sep = ""),width=5, height=3, units="in")


  # 4C ----------------------------------------------------------------------

ggplot(data = filter(cum_diff_total_grouped_ag_expend,
                     year == 2050, scenario.diff %in% c(paper_scenarios, forest_scenarios)),
       aes(x = grouped_region, y = value/(10^9), color = scenario.diff,group = scenario.diff))+
  geom_point(size = 2, alpha = 0.65)+
  stat_identity(yintercept=0, geom='hline', inherit.aes=TRUE, size = 1, color = "black")+
  labs(title = "Change in cum. total ag. expenditures in 2050", x = "", y = "$ (billions)") +
  theme_bw()+
  theme(text = element_text(size = 12))+
  theme(legend.position = "right", text = element_text(size = 12)) +
  scale_color_manual(labels = scenario_labels,
                     values = scenario_colors)+
  coord_flip()+
  ggsave(paste0(PLOT_FOLDER,"Fig_4C.pdf", sep = ""),width=5, height=3, units="in")






