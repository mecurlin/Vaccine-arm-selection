require(dplyr)
require(tidyverse)
require(tidyr)
require(readxl)
require(ggplot2)
require(lubridate)
require(rstatix)
library(ggplot2)
library(stringr)
library(purrr)

########################### DATA CLEANING ##########################

# read in data pull data
rm(list = ls())
data1 <- read.csv("data_pull/main_all_data_marcel_23Sept23.csv")

#Function to calculate actual arm group based on actual arm given at v1 and v2
calculate_group <- function(var1, var2){
  ifelse({{var1}} == "right" & {{var2}} == "right", "same",
         ifelse({{var1}} == "left" & {{var2}} == "left", "same",
                ifelse({{var1}} == "right" & {{var2}} == "left", "opp",
                       ifelse({{var1}} == "left" & {{var2}} == "right", "opp",
                              NA))))
}

calculate_metagroup <- function(var1, var2, var3){
  ifelse({{var1}} == "right" & {{var2}} == "right" & {{var3}} == "right", "SS",
         ifelse({{var1}} == "left" & {{var2}} == "left" & {{var3}} == "left", "SS",
                ifelse({{var1}} == "right" & {{var2}} == "left" & {{var3}} == "right", "OO",
                       ifelse({{var1}} == "left" & {{var2}} == "right" & {{var3}} == "left", "OO",
                              ifelse({{var1}} == "right" & {{var2}} == "right" & {{var3}} == "left", "SO",
                                     ifelse({{var1}} == "left" & {{var2}} == "left" & {{var3}} == "right", "SO",
                                            ifelse({{var1}} == "right" & {{var2}} == "left" & {{var3}} == "left", "OS",
                                                   ifelse({{var1}} == "left" & {{var2}} == "right" & {{var3}} == "right", "OS",
                                                          NA))))))))
}

# data cleanup
data1 <- data1 %>% 
  mutate(across('arm_randomization_assignment', str_replace, 'Opposite', 'opposite')) %>% 
  mutate(across('arm_randomization_assignment', str_replace, 'opposite', 'opp')) %>% 
  mutate(across(c("v1_arm_delivered", "v2_arm_delivered"), as.character)) %>% 
  mutate(v1_arm_delivered = gsub(" ", "", v1_arm_delivered)) %>% 
  mutate(v2_arm_delivered = gsub(" ", "", v2_arm_delivered)) %>%
  mutate(v3_arm_delivered = gsub(" ", "", v3_arm_delivered)) %>% 
  mutate(actual_arm_group = calculate_group(v1_arm_delivered, v2_arm_delivered)) %>% 
  mutate(meta_arm_group = calculate_metagroup(v1_arm_delivered, v2_arm_delivered, v3_arm_delivered))

# change data types
data1 <- data1 %>% mutate("sex" = as.factor(sex), 
 #                               "dob" = ymd(dob),
                                "arm_randomization_assignment" = as.factor(arm_randomization_assignment),
                                "arm_randomization_enrollment" = as.factor(arm_randomization_enrollment),
                                "v1_arm_delivered" = as.factor(v1_arm_delivered),
                                "v2_arm_delivered" = as.factor(v2_arm_delivered),
                                "v3_arm_delivered" = as.factor(v3_arm_delivered),
                                "v1_vaccine_date" = ymd(v1_vaccine_date),
                                "v2_vaccine_date" = ymd(v2_vaccine_date),
                                "v3_vaccine_date" = ymd(v3_vaccine_date),
                                "w2" = ymd(w2),                               
                                "w3" = ymd(w3),
                                "w4" = ymd(w4))

data1 <- data1 %>% mutate("v1_v2" = as.numeric(v2_vaccine_date - v1_vaccine_date), 
                                "v2_v3" = as.numeric(v3_vaccine_date - v2_vaccine_date),
                                "v2_w2" = as.numeric(w3 - v2_vaccine_date),
                                "v2_w3" = as.numeric(w3 - v2_vaccine_date),
                                "w3_v3" = as.numeric(v3_vaccine_date - w3),
                                "v3_w4" = as.numeric(w4 - v3_vaccine_date), 
                                "arm_randomization_assignment" = factor(arm_randomization_assignment, levels = c("same", "opp")),
                                "actual_arm_group" = factor(actual_arm_group, levels = c("same", "opp")),
                                "meta_arm_group" = factor(meta_arm_group, levels = c("SS", "SO", "OS", "OO")))

data1 <- data1 %>% 
  rename("arm_rand_assign" = "arm_randomization_assignment") %>% 
  rename("arm_rand_enroll" = "arm_randomization_enrollment") %>% 
  rename("v1_date" = "v1_vaccine_date") %>% 
  rename("v2_date" = "v2_vaccine_date") %>% 
  rename("v3_date" = "v3_vaccine_date") %>% 
  rename("v1_arm" = "v1_arm_delivered") %>% 
  rename("v2_arm" = "v2_arm_delivered") %>% 
  rename("v3_arm" = "v3_arm_delivered") %>% 
  rename("w1_lumit" = "w1_titer_ab_promega_assay_result_string") %>% 
  rename("w2_lumit" = "w2_titer_ab_promega_assay_result_string") %>% 
  rename("w2_elisa" = "w2_titer_elisa_igg_assay_result_string") %>% 
  rename("w3_lumit" = "w3_titer_ab_promega_assay_result_string") %>% 
  rename("w3_elisa" = "w3_anti_Wuhan_Hu_1_RBD_BAB_titer_assay_result_string") %>% 
  rename("w4_lumit" = "w4_titer_ab_promega_assay_result_string") %>% 
  rename("w4_elisa" = "w4_anti_Wuhan_Hu_1_RBD_BAB_titer_assay_result_string") %>% 
  rename("w3_nc" = "w3_Antinucleocapsid_ELISA_assay_result_string") %>% 
  rename("w4_nc" = "w4_Antinucleocapsid_ELISA_assay_result_string") %>% 
  rename("w3_d614g_50" = "w3_montefiori_nAb_D614G_id50_assay_result_string") %>% 
  rename("w3_d614g_80" = "w3_montefiori_nAb_D614G_id80_assay_result_string") %>% 
  rename("w4_d614g_50" = "w4_montefiori_nAb_D614G_id50_assay_result_string") %>% 
  rename("w4_d614g_80" = "w4_montefiori_nAb_D614G_id80_assay_result_string") %>% 
  rename("w4_b11529_50" = "w4_montefiori_nAb_B.1.1.529_id50_assay_result_string") %>% 
  rename("w4_b11529_80" = "w4_montefiori_nAb_B.1.1.529_id80_assay_result_string") 

# reorder variables
data1 <- data1 %>% relocate(actual_arm_group, .after = arm_rand_enroll) %>% 
  relocate(meta_arm_group, .after = actual_arm_group) %>% 
  relocate(v1_arm, .after = meta_arm_group) %>% 
  relocate(v2_arm, .after = v1_arm) %>% 
  relocate(v3_arm, .after = v2_arm) %>% 
  relocate(w2, .after = v3_date) %>% 
  relocate(w3, .after = w2) %>% 
  relocate(w4, .after = w3) %>% 
  relocate(v1_v2, .after = w4) %>% 
  relocate(v2_w2, .after = v1_v2) %>% 
  relocate(v2_v3, .after = v2_w2) %>% 
  relocate(v2_w3, .after = v2_v3) %>% 
  relocate(w3_v3, .after = v2_w3) %>% 
  relocate(v3_w4, .after = w3_v3) %>% 
  relocate(w3_nc, .after = v3_w4) %>% 
  relocate(w4_nc, .after = w3_nc) %>% 
  relocate(w1_lumit, .after = w4_nc) %>% 
  relocate(w2_lumit, .after = w1_lumit) %>% 
  relocate(w2_elisa, .after = w2_lumit) %>% 
  relocate(w3_lumit, .after = w2_elisa) %>% 
  relocate(w3_elisa, .after = w3_lumit) %>% 
  relocate(w4_lumit, .after = w3_elisa) %>% 
  relocate(w4_elisa, .after = w4_lumit)

# replace "Below LOD" and values =< 0

data1 <- data.frame(lapply(data1, function(x) {
  gsub("Below LOD", "0.06", x)
}))

data1 <- data.frame(lapply(data1, function(x) {
  gsub("<30", 30/sqrt(30), x)
}))

# change data types
data1 <- data1 %>% mutate(
  "sex" = factor(sex, levels = c("M", "F")),
  "age" = as.numeric(age),
  "arm_rand_assign" = factor(arm_rand_assign, levels = c("same", "opp")),
  "arm_rand_enroll" = factor(arm_rand_enroll, levels = c("enrolled", "declined", "lost to followup")),
  "actual_arm_group" = factor(actual_arm_group, levels = c("same", "opp")),
  "meta_arm_group" = factor(meta_arm_group, levels = c("SO", "SS", "OS", "OO")),
  "v1_arm" = factor(v1_arm, levels = c("left", "right")),
  "v2_arm" = factor(v2_arm, levels = c("left", "right")),
  "v3_arm" = factor(v3_arm, levels = c("left", "right")),
  "v1_date" = ymd(v1_date),
  "v2_date" = ymd(v2_date),
  "v3_date" = ymd(v3_date),
  "w2" = ymd(w2),
  "w3" = ymd(w3),
  "w4" = ymd(w4),
  "v1_v2" = as.numeric(v2_date - v1_date), 
  "v2_w2" = as.numeric(w2 - v2_date), 
  "v2_v3" = as.numeric(v3_date - v2_date),
  "v2_w3" = as.numeric(w3 - v2_date),
  "w3_v3" = as.numeric(v3_date - w3),
  "v3_w4" = as.numeric(w4 - v3_date),
  "w3_nc" = as.numeric(w3_nc),
  "w4_nc" = as.numeric(w4_nc),
  "w2_lumit" = as.numeric(w2_lumit),
  "w2_elisa" = as.numeric(w2_elisa),
  "w3_lumit" = as.numeric(w3_lumit),
  "w3_elisa" = as.numeric(w3_elisa),
  "w4_lumit" = as.numeric(w4_lumit),
  "w4_elisa" = as.numeric(w4_elisa),
  "w3_d614g_50" = as.numeric(w3_d614g_50),
  "w3_d614g_80" = as.numeric(w3_d614g_80),
  "w4_d614g_50" = as.numeric(w4_d614g_50),
  "w4_d614g_80" = as.numeric(w4_d614g_80),
  "w4_b11529_50" = as.numeric(w4_b11529_50),
  "w4_b11529_80" = as.numeric(w4_b11529_80)) %>% 
  mutate(w3_nc = replace(w3_nc, w3_nc == 0, 1/sqrt(2))) %>%  
  mutate(w4_nc = replace(w4_nc, w4_nc == 0, 1/sqrt(2))) %>% 
  mutate(w2_lumit = replace(w2_lumit, w2_lumit == 0, 1/sqrt(2))) %>% 
  mutate(w2_elisa = replace(w2_elisa, w2_elisa == 0, 1/sqrt(2)))

# convert antibody results to log values 
data1 <- data1 %>% mutate_at(assay_results, log10) 

# save data
saveRDS(data1, file = "all_data_cleaned.rds")
remove(data1)
remove(calculate_group)
remove(calculate_metagroup)

######################### ANALYSIS ####################################

# read data
all_data <- readRDS(file = "all_data_cleaned.rds")

# make variable names
comparison_variables <- c("age", "v1_v2", "v2_w2", "v2_v3", "v2_w3", "w3_v3", "v3_w4")
assay_results <- c("w2_lumit", "w2_elisa", "w3_lumit", "w3_elisa", "w4_lumit", "w4_elisa", "w3_nc", "w4_nc", "w3_d614g_50", "w3_d614g_80", "w4_d614g_50", "w4_d614g_80", "w4_b11529_50", "w4_b11529_80")
neut_assays <- c("w3_d614g_50", "w3_d614g_80", "w4_d614g_50", "w4_d614g_80", "w4_b11529_50", "w4_b11529_80")
binding_assays <- c("w2_lumit", "w2_elisa", "w3_lumit", "w3_elisa", "w4_lumit", "w4_elisa", "w3_nc", "w4_nc")
ratios <- c("w3DGIg", "w3DGIgG", "w4DGIg", "w4DGIgG", "w4BIg", "w4BIgG")
vars <- c(comparison_variables, assay_results, ratios)

# diagnostic subgroups
wrong_arm_given <- all_data %>% filter(arm_rand_enroll == "enrolled") %>% filter(arm_rand_assign != actual_arm_group)
natural_infection_w3 <- all_data %>% filter(w3_nc >= log10(200))
natural_infection_w4 <- all_data %>% filter(w4_nc >= log10(200))
natural_infection <- all_data %>% filter(w3_nc >= log10(200) | w4_nc >= log10(200))
incomplete_arm_info <- all_data %>% filter(is.na(v1_arm) | is.na(v2_arm))
no_followup <- all_data %>% filter(is.na(w3) & is.na(w4))
all_followup <- all_data %>% filter(!is.na(w3) & !is.na(w4))
no_v3 <- all_data %>% filter(is.na(v3_date))
no_assay <- all_data %>% filter(is.na(w3_lumit) & is.na(w3_elisa) & is.na(w4_lumit) & is.na(w4_elisa))
declined_enrollment <- all_data %>% filter(arm_rand_enroll == "declined")
enrolled <- all_data %>% filter(arm_rand_enroll == "enrolled")
not_approached <- all_data %>% filter(arm_rand_enroll != "enrolled") %>% filter(arm_rand_enroll != "declined")

########################### ANALYSIS SUBSETS ############################
# create subgroup datasets
# as treated - ignore NC, randomization enrollment
# clean - randomized-enrolled, NC-negative, has V3 (for W4 analysis)
# natural infection (NC positive) group
as_treated <-  all_data %>% 
  anti_join(incomplete_arm_info, by = "qr_code") %>% 
  anti_join(no_assay, by = "qr_code") %>% 
  anti_join(no_followup, by = "qr_code")

w3_infected <- natural_infection_w3 %>% 
  anti_join(incomplete_arm_info, by = "qr_code") %>% 
  anti_join(no_assay, by = "qr_code") %>% 
  anti_join(no_followup, by = "qr_code") %>% 
  select(-neut_assays)

all_infected <- natural_infection %>% 
  anti_join(incomplete_arm_info, by = "qr_code") %>% 
  anti_join(no_assay, by = "qr_code") %>% 
  anti_join(no_followup, by = "qr_code") %>% 
  filter(!is.na(w3_lumit)) %>% 
  unique()

clean_w3 <-  all_data %>% 
  anti_join(incomplete_arm_info, by = "qr_code") %>% 
  anti_join(no_assay, by = "qr_code") %>% 
  anti_join(no_followup, by = "qr_code") %>% 
  anti_join(natural_infection_w3, by = "qr_code") %>% 
  filter(arm_rand_enroll == "enrolled")

clean_w4 <- all_data %>%
  anti_join(incomplete_arm_info, by = "qr_code") %>% 
  anti_join(no_assay, by = "qr_code") %>% 
  anti_join(no_followup, by = "qr_code") %>% 
  anti_join(natural_infection_w3, by = "qr_code") %>% 
  anti_join(natural_infection_w4, by = "qr_code") %>% 
  filter(arm_rand_enroll == "enrolled") %>% 
  filter(!is.na(v3_date)) %>% 
  filter(!is.na(w4))

# matched data with only 54 pairs
pair_qr_codes <- read.csv("analysis_matched/duke_subanalysis_neg_nucleocapsid_group_from_sedi.csv")
pair_qr_codes <- pair_qr_codes %>% select(qr_code) %>% mutate(qr_code = as.character(qr_code))
matched <- all_data %>% 
  semi_join(pair_qr_codes, by = "qr_code") %>% 
  mutate(arm_rand_assign = replace(arm_rand_assign, qr_code == "14748", "same")) %>% 
  mutate(w3DGIg = log10((10^(w3_d614g_50)) / (10^(w3_lumit)))) %>% 
  mutate(w3DGIgG = log10((10^(w3_d614g_50)) / (10^(w3_elisa)))) %>% 
  mutate(w4DGIg = log10((10^(w4_d614g_50)) / (10^(w4_lumit)))) %>% 
  mutate(w4DGIgG = log10((10^(w4_d614g_50)) / (10^(w4_elisa)))) %>% 
  mutate(w4BIg = log10((10^(w4_b11529_50)) / (10^(w4_lumit)))) %>% 
  mutate(w4BIgG = log10((10^(w4_b11529_50)) / (10^(w4_elisa))))

####################### DATA ANALYSIS #################################

# overall summary stats
fullouter_clean_summary_stats <- fullouter_clean %>% summarise_at(vars, list(mean = mean, sd = sd), na.rm = TRUE)
write.csv(t(fullouter_clean_summary_stats), "fullouter_clean_analysis/clean_w3_w4_summary_stats.csv", row.names=TRUE)

as_treated_summary_stats <- as_treated %>% summarise_at(vars, list(mean = mean, sd = sd), na.rm = TRUE)
write.csv(t(as_treated_summary_stats), "as_treated_analysis/as_treated_summary_stats.csv", row.names=TRUE)

matched_summary_stats <- matched %>% summarise_at(vars, list(mean = mean, sd = sd), na.rm = TRUE)
write.csv(t(matched_summary_stats), "matched_analysis/matched_summary_stats.csv", row.names=TRUE)

# summary statistics by group
fullouter_clean_by_group_summary_stats <- fullouter_clean %>% group_by(actual_arm_group) %>% summarise_at(vars, list(mean = mean, sd = sd), na.rm = TRUE)
write.csv(t(fullouter_clean_by_group_summary_stats), "fullouter_clean_analysis/fullouter_clean_by_group_summary.csv", row.names=TRUE)
table(fullouter_clean$actual_arm_group)

as_treated_by_group_summary_stats <- as_treated %>% group_by(actual_arm_group) %>% summarise_at(vars, list(mean = mean, sd = sd), na.rm = TRUE)
write.csv(t(as_treated_by_group_summary_stats), "as_treated_analysis/as_treated_by_group_summary_stats.csv", row.names=TRUE)

matched_by_group_summary_stats <- matched %>% group_by(actual_arm_group) %>% summarise_at(vars, list(mean = mean, sd = sd), na.rm = TRUE)
write.csv(t(matched_by_group_summary_stats), "matched_analysis/matched_by_group_summary_stats.csv", row.names=TRUE)

infected_by_group_summary_stats <- w3_infected %>% group_by(actual_arm_group) %>% summarise_at(vars, list(mean = mean, sd = sd), na.rm = TRUE)
write.csv(t(infected_by_group_summary_stats), "infected_analysis/infectedi_by_group_summary_stats.csv", row.names=TRUE)

# T-tests
# define function
my_t_test <- function(dataset, parameter, grouping_variable) {
  formula <- do.call("~", list(rlang::enexpr(parameter), rlang::enexpr(grouping_variable)))
  parameter <- dataset %>% t_test(formula, paired = FALSE, detailed = TRUE) %>% add_significance()
  return(parameter)
}

fullouter_clean_data <- lapply(syms(vars), my_t_test, dataset = fullouter_clean, arm_rand_assign)
fullouter_clean_t_test <- fullouter_clean_data %>% bind_rows() # %>% mutate(parameter = assay_results) %>% relocate(parameter)
write.csv(fullouter_clean_t_test, "fullouter_clean_analysis/full_outer_clean_t_test.csv", row.names=TRUE)

fullouter_clean_data <- lapply(syms(assay_results), my_t_test, dataset = fullouter_clean, meta_arm_group)
fullouter_clean_t_test <- fullouter_clean_data %>% bind_rows() 
write.csv(fullouter_clean_t_test, "analysis_supp/full_outer_clean_meta_t_test.csv", row.names=TRUE)

as_treated_data <- lapply(syms(assay_results), my_t_test, dataset = as_treated, actual_arm_group)
as_treated_t_test <- as_treated_data %>% bind_rows() %>% mutate(parameter = assay_results) %>% relocate(parameter)
write.csv(as_treated_t_test, "as_treated_analysis/as_treated_actual_t_test.csv", row.names=TRUE)

as_treated_data <- lapply(syms(assay_results), my_t_test, dataset = as_treated, meta_arm_group)
as_treated_t_test <- as_treated_data %>% bind_rows() %>% mutate(parameter = assay_results) %>% relocate(parameter)
write.csv(as_treated_t_test, "as_treated_analysis/as_treated_meta_t_test.csv", row.names=TRUE)

matched_data <- lapply(syms(vars), my_t_test, dataset = matched, actual_arm_group)
matched_t_test <- matched_data %>% bind_rows() 
write.csv(matched_t_test, "analysis_matched/matched_actual_t_test.csv", row.names=TRUE)

infected_data <- lapply(syms(binding_assays), my_t_test, dataset = w3_infected, actual_arm_group)
infected_t_test <- infected_data %>% bind_rows() 
write.csv(infected_t_test, "analysis_supp/infected_actual_t_test2.csv", row.names=TRUE)


all_infected_data <- lapply(syms(binding_assays), my_t_test, dataset = all_infected, actual_arm_group)
all_infected_t_test <- all_infected_data %>% bind_rows() 
write.csv(all_infected_t_test, "analysis_supp/all_infected_t_test.csv", row.names=TRUE)


############################# PLOTS ##############################

####################### PLOT DATASETS ############################
# Lumit data
# shape data with group and assay result
lumit_foc <- fullouter_clean %>% select(qr_code, actual_arm_group, w2_lumit, w3_lumit, w4_lumit) %>% 
  mutate(across(actual_arm_group, as.character)) %>% 
  rename(arm.group = actual_arm_group) %>% 
  pivot_longer(cols = c("w2_lumit", "w3_lumit", "w4_lumit"), names_to = "assay", values_to = "RLU") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_lumit", "opp_w2_lumit", "same_w3_lumit", "opp_w3_lumit", "same_w4_lumit", "opp_w4_lumit" )))

# Elisa data
# shape data with group and assay result
elisa_foc <- fullouter_clean %>% select(qr_code, actual_arm_group, w2_elisa, w3_elisa, w4_elisa) %>% 
  mutate(across(actual_arm_group, as.character)) %>% 
  rename(arm.group = actual_arm_group) %>% 
  pivot_longer(cols = c("w2_elisa", "w3_elisa", "w4_elisa"), names_to = "assay", values_to = "ug_ml") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_elisa", "opp_w2_elisa", "same_w3_elisa", "opp_w3_elisa", "same_w4_elisa", "opp_w4_elisa" )))

# Lumit data meta groups
lumit_meta_foc <- fullouter_clean %>% 
  filter(!is.na(w4_lumit)) %>% 
  filter(!is.na(meta_arm_group)) %>% 
  select(qr_code, meta_arm_group, w4_lumit) %>% 
  mutate(across(meta_arm_group, as.character)) %>% 
  rename(arm.group = meta_arm_group) %>% 
  pivot_longer(cols = c("w4_lumit"), names_to = "assay", values_to = "RLU") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("SS_w4_lumit", "SO_w4_lumit", "OS_w4_lumit", "OO_w4_lumit")))

elisa_meta_foc <- fullouter_clean %>% 
  filter(!is.na(w4_elisa)) %>% 
  filter(!is.na(meta_arm_group)) %>% 
  select(qr_code, meta_arm_group, w4_elisa) %>% 
  mutate(across(meta_arm_group, as.character)) %>% 
  rename(arm.group = meta_arm_group) %>% 
  pivot_longer(cols = c("w4_elisa"), names_to = "assay", values_to = "ug_ml") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("SS_w4_elisa", "SO_w4_elisa", "OS_w4_elisa", "OO_w4_elisa")))

# Elisa data
# shape data with group and assay result
elisa_foc <- fullouter_clean %>% select(qr_code, actual_arm_group, w2_elisa, w3_elisa, w4_elisa) %>% 
  mutate(across(actual_arm_group, as.character)) %>% 
  rename(arm.group = actual_arm_group) %>% 
  pivot_longer(cols = c("w2_elisa", "w3_elisa", "w4_elisa"), names_to = "assay", values_to = "ug_ml") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_elisa", "opp_w2_elisa", "same_w3_elisa", "opp_w3_elisa", "same_w4_elisa", "opp_w4_elisa" )))

################## matched ratios ########################

lumit_matched_r <- matched %>% select(qr_code, arm_rand_assign, w3DGIg, w4DGIg, w4BIg) %>% 
  mutate(across(arm_rand_assign, as.character)) %>% 
  rename(arm.group = arm_rand_assign) %>% 
  pivot_longer(cols = c("w3DGIg", "w4DGIg", "w4BIg"), names_to = "assay", values_to = "RLU") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w3DGIg", "opp_w3DGIg", "same_w4DGIg", "opp_w4DGIg", "same_w4BIg", "opp_w4BIg" )))

# Elisa data
elisa_matched_r <- matched %>% select(qr_code, arm_rand_assign, w3DGIgG, w4DGIgG, w4BIgG) %>% 
  mutate(across(arm_rand_assign, as.character)) %>% 
  rename(arm.group = arm_rand_assign) %>% 
  pivot_longer(cols = c("w3DGIgG", "w4DGIgG", "w4BIgG"), names_to = "assay", values_to = "ug_ml") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w3DGIgG", "opp_w3DGIgG", "same_w4DGIgG", "opp_w4DGIgG", "same_w4BIgG", "opp_w4BIgG" )))

################# as treated dataset #########################
# Lumit data
# shape data with group and assay result
lumit_as_tr <- as_treated %>% select(qr_code, actual_arm_group, w2_lumit, w3_lumit, w4_lumit) %>% 
  mutate(across(actual_arm_group, as.character)) %>% 
  rename(arm.group = actual_arm_group) %>% 
  pivot_longer(cols = c("w2_lumit", "w3_lumit", "w4_lumit"), names_to = "assay", values_to = "RLU") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_lumit", "opp_w2_lumit", "same_w3_lumit", "opp_w3_lumit", "same_w4_lumit", "opp_w4_lumit" )))

# Elisa data
# shape data with group and assay result
elisa_as_tr <- as_treated %>% select(qr_code, actual_arm_group, w2_elisa, w3_elisa, w4_elisa) %>% 
  mutate(across(actual_arm_group, as.character)) %>% 
  rename(arm.group = actual_arm_group) %>% 
  pivot_longer(cols = c("w2_elisa", "w3_elisa", "w4_elisa"), names_to = "assay", values_to = "ug_ml") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_elisa", "opp_w2_elisa", "same_w3_elisa", "opp_w3_elisa", "same_w4_elisa", "opp_w4_elisa" )))

################# matched dataset #########################
# Lumit data
# shape data with group and assay result
lumit_match <- matched %>% select(qr_code, arm_rand_assign, w2_lumit, w3_lumit, w4_lumit) %>% 
  mutate(across(arm_rand_assign, as.character)) %>% 
  rename(arm.group = arm_rand_assign) %>% 
  pivot_longer(cols = c("w2_lumit", "w3_lumit", "w4_lumit"), names_to = "assay", values_to = "RLU") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_lumit", "opp_w2_lumit", "same_w3_lumit", "opp_w3_lumit", "same_w4_lumit", "opp_w4_lumit" )))

# Elisa data
# shape data with group and assay result
elisa_match <- matched %>% select(qr_code, arm_rand_assign, w2_elisa, w3_elisa, w4_elisa) %>% 
  mutate(across(arm_rand_assign, as.character)) %>% 
  rename(arm.group = arm_rand_assign) %>% 
  pivot_longer(cols = c("w2_elisa", "w3_elisa", "w4_elisa"), names_to = "assay", values_to = "ug_ml") %>%
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_elisa", "opp_w2_elisa", "same_w3_elisa", "opp_w3_elisa", "same_w4_elisa", "opp_w4_elisa" )))

################# infected dataset #########################
# Lumit data
# shape data with group and assay result
lumit_inf <- all_infected %>% select(qr_code, actual_arm_group, w2_lumit, w3_lumit, w4_lumit) %>% 
  mutate(across(actual_arm_group, as.character)) %>% 
  rename(arm.group = actual_arm_group) %>% 
  pivot_longer(cols = c("w2_lumit", "w3_lumit", "w4_lumit"), names_to = "assay", values_to = "RLU") %>% 
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_lumit", "opp_w2_lumit", "same_w3_lumit", "opp_w3_lumit", "same_w4_lumit", "opp_w4_lumit" ))) %>% 
  filter(!is.na(RLU))

# Elisa data
# shape data with group and assay result
elisa_inf <- w3_infected %>% select(qr_code, actual_arm_group, w2_elisa, w3_elisa, w4_elisa) %>% 
  mutate(across(actual_arm_group, as.character)) %>% 
  rename(arm.group = actual_arm_group) %>% 
  pivot_longer(cols = c("w2_elisa", "w3_elisa", "w4_elisa"), names_to = "assay", values_to = "ug_ml") %>%
  unite(group, c(arm.group, assay), sep = "_", remove = TRUE) %>% 
  mutate(across(group, as.factor)) %>% 
  mutate(group = factor(group, levels = c("same_w2_elisa", "opp_w2_elisa", "same_w3_elisa", "opp_w3_elisa", "same_w4_elisa", "opp_w4_elisa" ))) %>% 
  filter(!is.na(ug_ml))

######################### PLOTS ##########################
#################### violin plots ########################

# plot function
make_plot <- function(mydata, assay, group, title)  {
  n_values <- mydata %>% group_by({{group}}) %>% summarise(no_rows = length({{group}})) %>% select(no_rows)
  y_u_limit <- mydata %>% select({{assay}}) %>% summarise(max(., na.rm=TRUE)) %>% floor + 1.2 %>% unlist()
  y_l_limit <- mydata %>% select({{assay}}) %>% summarise(min(., na.rm=TRUE)) %>% floor + 0.3 %>% unlist()
  annotation_y_position <- mydata %>% select({{assay}}) %>% summarise(max(., na.rm=TRUE)) %>% + 0.2 %>% unlist()
  mydata %>%  filter(!is.na({{group}})) %>% 
    ggplot() +
# colors for regular groups
# scale_color_manual(values = c("#90CAFA", "#FFB300", "#1D88E5", "#F67C01", "#1465C1", "#A21819")) +
# colors for metagroups
    scale_color_manual(values = c("#90CAFA", "#1D88E5", "#FFB300", "#F67C01")) +
    aes(x = {{group}}, y = {{assay}}, color = {{group}}) + 
    geom_jitter(color="black", size = 3, alpha = 0.5, width = 0.15, show.legend = FALSE) +
    geom_boxplot(width = 0.2, notch = FALSE, position = position_dodge(1), lwd = 3, outlier.shape = NA, fill = "black", alpha = 0.01, show.legend = FALSE) +
    # ylim(y_l_limit[1,1], y_u_limit[1,1]) +
    ylim(y_l_limit[1,1], y_u_limit[1,1]) +
    geom_violin(alpha = 0, linetype = "solid", linewidth = 1, show.legend = FALSE) +
    theme(panel.grid.major=element_line(colour="#cccccc", size = 0.25),
          panel.grid.minor=element_line(colour="#cccccc"),
          panel.background = element_rect(fill='transparent', color = "#cccccc"),
          plot.background = element_rect(fill='transparent'), 
          legend.position = c(0.92, 0.15), 
          legend.background = element_rect(fill = 'transparent'), 
          plot.title = element_text(hjust = 1)) +
    ggtitle(title) +
    # annotate("text", label = "LoD", x = 0.47, y = 1.54, size = 4, colour = "black") + 
    annotate("text", label = "n =", x = 0.6, y = annotation_y_position, size = 6, colour = "grey40") + 
    annotate("text", label = n_values[1,1], x = 1, y = annotation_y_position, size = 6, colour = "grey40") +
    annotate("text", label = n_values[2,1], x = 2, y = annotation_y_position, size = 6, colour = "grey40") +
    annotate("text", label = n_values[3,1], x = 3, y = annotation_y_position, size = 6, colour = "grey40") +
    annotate("text", label = n_values[4,1], x = 4, y = annotation_y_position, size = 6, colour = "grey40") 
#    annotate("text", label = n_values[5,1], x = 5, y = annotation_y_position, size = 6, colour = "grey40") +
#    annotate("text", label = n_values[6,1], x = 6, y = annotation_y_position, size = 6, colour = "grey40")
}

# make plot
# code presented for FOC group
# other groups done similarly
title <- "FOC, total Ig, W4 by metagroup"
make_plot(lumit_meta_foc, RLU, group, title)
plot <- make_plot(lumit_meta_foc, RLU, group, title)
ggsave("Plots_supplementary_FOC_metagroups_Ig.pdf", plot)


########################## histograms ############################

# prepare data
# code presented for FOC group
# other groups done similarly

fullouter_clean <- fullouter_clean %>% mutate("v2_w4" = as.numeric(w4 - v2_date))
fullouter_clean_long <- gather(fullouter_clean, key = "interval", value = "value", c("v1_v2", "v2_w2", "v2_v3", "v2_w3", "v3_w4", "v2_w4", "age"))

# set colors
col1 <- c("#999999", "#E69F00", "#56B4E9")
col2 <- c("#1565C0", "#B71C1C")

plot <- ggplot(fullouter_clean_long, aes(value, fill = actual_arm_group)) +
  geom_histogram(data = subset(fullouter_clean_long, interval == "v1_v2"), aes(y = stat(count)), position = "identity", binwidth = 2, color = "white", alpha = 0.5) +
  geom_histogram(data = subset(fullouter_clean_long, interval == "v2_w2"), aes(y = stat(count)), position = "identity", binwidth = 2, color = "white", alpha = 0.5) +
  geom_histogram(data = subset(fullouter_clean_long, interval == "v2_v3"), aes(y = stat(count)), position = "identity", binwidth = 10, color = "white", alpha = 0.5) +
  geom_histogram(data = subset(fullouter_clean_long, interval == "v2_w3"), aes(y = stat(count)), position = "identity", binwidth = 10, color = "white", alpha = 0.5) +
  geom_histogram(data = subset(fullouter_clean_long, interval == "v3_w4"), aes(y = stat(count)), position = "identity", binwidth = 15, color = "white", alpha = 0.5) +
  geom_histogram(data = subset(fullouter_clean_long, interval == "v2_w4"), aes(y = stat(count)), position = "identity", binwidth = 15, color = "white", alpha = 0.5) +
  geom_histogram(data = subset(fullouter_clean_long, interval == "age"), aes(y = stat(count)), position = "identity", binwidth = 3, color = "white", alpha = 0.5) +
  scale_color_manual(values = col1) +
  scale_fill_manual(values = col2) +
  theme(panel.border = element_rect(fill = NA, colour = "gray", linewidth = 0.2), 
        axis.line = element_line(colour = "gray", linewidth = 0.2), 
        panel.grid.major = element_line(linewidth = 0.2, linetype = 'solid', colour ="gray"), 
        panel.grid.minor = element_blank(), panel.background = element_blank()) +
  facet_wrap(~interval, scales = "free", ncol = 3)
plot

#c ggsave("test.1.pdf", plot)
aspect_ratio <- 2.5
ggsave("test.2.pdf", plot, height = 4, width = 4 * aspect_ratio, limitsize = FALSE, units="in")

# Full outer clean age histogram
col1 <- c("#999999", "#E69F00", "#56B4E9")
col2 <- c("#1565C0", "#B71C1C")
plot <- ggplot(fullouter_clean, aes(age, fill = actual_arm_group)) +
  geom_histogram(aes(y = stat(count)), position = "identity", binwidth = 2, color = "white", alpha = 0.5) +
  scale_color_manual(values = col1) +
  scale_fill_manual(values = col2) +
  theme(legend.position = "none") +                     
  theme(panel.border = element_rect(fill = NA, colour = "gray", linewidth = 0.2), 
        axis.line = element_line(colour = "gray", linewidth = 0.2), 
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour ="gray"), 
        panel.grid.minor = element_blank(), panel.background = element_blank())
ggsave("full_outer_clean_age_histogram.pdf", plot)


########################## loess plots ############################

loess1 <- as_treated %>% 
  drop_na(v2_w2) %>% 
  drop_na(w2_lumit) %>% 
  drop_na(w2_elisa) %>% 
  arrange(v2_w2)%>% 
  mutate(index = 1:nrow(.))

loess2 <- loess1 %>% 
  mutate(w2_lumit = replace(w2_lumit, actual_arm_group == "opp", NA))

loess3 <- loess1 %>% 
  mutate(w2_lumit = replace(w2_lumit, actual_arm_group == "same", NA))

plot <- ggplot(loess1, aes(x = v2_w2, y = w2_lumit)) +
         geom_jitter(color="black", size = 3, alpha=0.3, width = 0.3, show.legend = FALSE) +
         stat_smooth(aes(x = loess2$v2_w2, y = loess2$w2_lumit), color = "#90CAFA", linewidth = 2, method = "loess", se = FALSE, span = 1) +
         stat_smooth(aes(x = loess3$v2_w2, y = loess3$w2_lumit), color = "#FFB300", linewidth = 2, method = "loess", se = FALSE, span = 1) +
         theme(panel.border = element_rect(fill = NA, colour = "gray", linewidth = 0.2), 
                axis.line = element_line(colour = "gray", linewidth = 0.2), 
                panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour ="gray"), 
                panel.grid.minor = element_blank(), panel.background = element_blank()) 
ggsave("lumit_loess.pdf", plot)   


loess2 <- loess1 %>% 
  mutate(w2_elisa = replace(w2_elisa, actual_arm_group == "opp", NA))

loess3 <- loess1 %>% 
  mutate(w2_elisa = replace(w2_elisa, actual_arm_group == "same", NA))
plot <- ggplot(loess1, aes(x = v2_w2, y = w2_elisa)) +
        geom_jitter(color="black", size = 3, alpha=0.3, width = 0.3, show.legend = FALSE) +
        stat_smooth(aes(x = loess2$v2_w2, y = loess2$w2_elisa), color = "#90CAFA", linewidth = 2, method = "loess", se = FALSE, span = 1) +
        stat_smooth(aes(x = loess3$v2_w2, y = loess3$w2_elisa), color = "#FFB300", linewidth = 2, method = "loess", se = FALSE, span = 1) +
        theme(panel.border = element_rect(fill = NA, colour = "gray", linewidth = 0.2), 
                axis.line = element_line(colour = "gray", linewidth = 0.2), 
                panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour ="gray"), 
                panel.grid.minor = element_blank(), panel.background = element_blank()) 
ggsave("elisa_loess.pdf", plot) 
