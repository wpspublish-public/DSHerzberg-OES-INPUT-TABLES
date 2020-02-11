suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

# form <- c('interview', 'parent', 'teacher', 'clinician')
form <- c('interview', 'parent', 'teacher')
scale_acr <- c('PHY', 'ADP', 'SOC', 'COG', 'COM')

# Create char vec holding names of input .xlsx containing scale lookups.
# `purrr::map_chr()` returns a char vec. Mapping `paste0()` allows you to paste
# the names of the three forms into the file name stem, creating charvec with
# three file names.
scale_file_name <- map_chr(form, ~ paste0('scale_lookup_', .x))

# read in percentile lookup column
perc_lookup <- suppressMessages(read_csv(here('INPUT-FILES/Percentile-Lookup-SS.csv')))

# read in age labels for OES output lookup table
age_labels <- suppressMessages(read_csv(here('INPUT-FILES/OES-TABLES/OES-age-labels.csv'))) %>%
  mutate(agestrat = str_sub(agestrat, 4))

scale_readin <- function(x) {
  # express the directory path to the input file as a string.
  here(
    paste0('INPUT-FILES/OES-TABLES/', x, '.xlsx')) %>%
    assign('path', ., envir = .GlobalEnv)
  
  
  # input file is multi-tabbed .xlsx. Tabs contain lookup tables for each
  # agestrat. read input file into a df, stacking tabs on top of one another, and
  # creating a new column 'agestrat' to identify the origin tab of each set of rows.
  path %>% 
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = path,
           .id = 'agestrat') %>% 
    # recode agestrat so that it will sort properly
    mutate(agestrat = str_sub(agestrat, 4) %>% 
             str_pad(3, side = 'left', '0')) %>% 
    assign('input_lookup', ., envir = .GlobalEnv)
}

scale_lookup_pre <- scale_file_name %>% 
  map(scale_readin) %>% 
  setNames(form) %>% 
  bind_rows(.id = 'form')

# add rows for clinician form, with NA for all SS vars
scale_lookup <- 
  scale_lookup_pre %>% 
  filter(form == 'interview') %>% 
  mutate(form = 'clinician') %>% 
  mutate_at(vars(PHY:COM), ~ as.numeric(NA)) %>% 
  bind_rows(scale_lookup_pre, .)

# Read in growth score .xlsx
growth_lookup <- here('INPUT-FILES/OES-TABLES/growth-score-lookup.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/OES-TABLES/growth-score-lookup.xlsx'),
         .id = 'form') %>% 
  rename_at(vars(PHY:COM), ~ paste0(.x,"_G")) 

# Read in age equiv .xlsx
ageEquiv_lookup <- here('INPUT-FILES/OES-TABLES/ageEquiv-form-lookup.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/OES-TABLES/ageEquiv-form-lookup.xlsx'),
         .id = 'form') %>%
  # For some reason, R is appending garbage chars to the end of value '<2:0' on
  # read-in. Next mutate_at subsets the string to get rid of the garbage chars.
  mutate_at(vars(PHY:COM), ~ case_when(
    str_detect(.x, '<') ~ str_sub(.x, 1, 4),
    TRUE ~ .x
  )) %>% 
  rename_at(vars(PHY:COM), ~ str_c(.x, '_AE'))

# Read in CV .xlsx
CV_lookup <- here('INPUT-FILES/OES-TABLES/Form-Agestrat-CV.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/OES-TABLES/Form-Agestrat-CV.xlsx'),
         .id = 'form') %>% 
  mutate(agestrat = str_sub(agestrat, 4) %>% 
           str_pad(3, side = 'left', '0'), 
         CV_type = str_sub(form, -4),
         form = tolower(form),
         form = str_sub(form, 1, -6)) 
CV_90_lookup <- CV_lookup %>% 
  filter(CV_type == "CV90") %>% 
  rename(
    ADP_CV90 = ADP,
    COG_CV90 = COG,
    COM_CV90 = COM,
    PHY_CV90 = PHY,
    SOC_CV90 = SOC
  ) %>%
  select(-CV_type)
CV_95_lookup <- CV_lookup %>% 
  filter(CV_type == "CV95") %>% 
  rename(
    ADP_CV95 = ADP,
    COG_CV95 = COG,
    COM_CV95 = COM,
    PHY_CV95 = PHY,
    SOC_CV95 = SOC
  ) %>%
  select(-CV_type)

scale_CV_growth_AE_lookup <- list(scale_lookup, CV_90_lookup, CV_95_lookup) %>% 
  reduce(left_join, by = c('form', 'agestrat')) %>% 
  left_join(growth_lookup, by = c('form', 'rawscore')) %>% 
  left_join(ageEquiv_lookup, by = c('form', 'rawscore')) 

scale_CI_growth_AE_lookup <- scale_acr %>%
  map_dfc(~ scale_CV_growth_AE_lookup %>% 
            # dplyr::transmute() is similar to mutate(), but it drops the input
            # columns after creating the new var
            transmute(
              # Next four operations lines get upper, lower bounds of CIs as numbers
              !!str_c(.x, '_CI90_LB_pre') := !!sym(.x) - !!sym(str_c(.x, '_CV90')),
              !!str_c(.x, '_CI90_UB_pre') := !!sym(.x) + !!sym(str_c(.x, '_CV90')), 
              !!str_c(.x, '_CI95_LB_pre') := !!sym(.x) - !!sym(str_c(.x, '_CV95')),
              !!str_c(.x, '_CI95_UB_pre') := !!sym(.x) + !!sym(str_c(.x, '_CV95')), 
              # Next four operations truncate UB at 160, LB at 40, and coerce
              # both to character
              !!str_c(.x, '_CI90_LB') := as.character(case_when(
                !!sym(str_c(.x, '_CI90_LB_pre')) < 40 ~ 40,
                TRUE ~ !!sym(str_c(.x, '_CI90_LB_pre'))
              )),
              !!str_c(.x, '_CI90_UB') := as.character(case_when(
                !!sym(str_c(.x, '_CI90_UB_pre')) > 160 ~ 160,
                TRUE ~ !!sym(str_c(.x, '_CI90_UB_pre'))
              )),
              !!str_c(.x, '_CI95_LB') := as.character(case_when(
                !!sym(str_c(.x, '_CI95_LB_pre')) < 40 ~ 40,
                TRUE ~ !!sym(str_c(.x, '_CI95_LB_pre'))
              )),
              !!str_c(.x, '_CI95_UB') := as.character(case_when(
                !!sym(str_c(.x, '_CI95_UB_pre')) > 160 ~ 160,
                TRUE ~ !!sym(str_c(.x, '_CI95_UB_pre'))
              )),
              # Next two operations yield the formatted, truncated CIs as strings
              !!str_c(.x, '_CI90') :=
                str_c(!!sym(str_c(.x, '_CI90_LB')), !!sym(str_c(.x, '_CI90_UB')), sep = ' - '),
              !!str_c(.x, '_CI95') :=
                str_c(!!sym(str_c(.x, '_CI95_LB')), !!sym(str_c(.x, '_CI95_UB')), sep = ' - ')
            )
  ) %>%
  # At this point the object has only the new columns; all input columns have
  # been dropped by transmute(). Now bind_cols joins the new cols with the
  # original input set. select() then pares to only those columsn needed in the
  # final OES output.
  bind_cols(scale_CV_growth_AE_lookup, .) %>% 
  select(form:COM, ends_with('CI90'), ends_with('CI95'), ends_with('_G'), ends_with('_AE')) %>% 
  # rename SS cols so all cols to be gathered are named with the format
  # "scaleName_scoreType"
  rename_at(vars(PHY:COM), ~ paste0(.x,"_SS")) %>% 
  # gather "scaleName_scoreType" cols into key column, SS and CI values into val
  # col
  gather(key, val, 4:ncol(.)) %>%
  # Now split "scaleName_scoreType" in key col into two cols: scale and type
  extract(key, into = c("scale", "type"), "([:alpha:]{3})?\\_?(.*)") %>%
  # spread so that type yields cols of SS, G, CI90, CI95, and that quad remains
  # paired with correct form, agestrat, rawscore, and scale.
  spread(type, val) %>% 
  select(scale, form, agestrat, rawscore, SS, CI90, CI95, G, AE) %>% 
  rename(growth = G, AgeEquiv = AE) %>% 
  arrange(scale) %>% 
  mutate(
    SS = as.numeric(SS),
    growth = as.numeric(growth)
  )

# Read in GDS .xlsx, using same general method as multi-tab .xlsx, but without
# requiring a function
GDS_lookup <- here('INPUT-FILES/OES-TABLES/GDS_lookup.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/OES-TABLES/GDS_lookup.xlsx'),
         .id = 'form') %>% 
  # to nest form-rawscore-GDS triplets within values of agestrat, tidyr::crossing is used
  # because inputs have no common vars
  crossing(age_labels, .) %>% 
  filter(!(form == 'teacher' & agestrat %in% c("000", "002", "004", "006", "008", 
                                               "010", "012", "014", "016", "018",
                                               "020", "022"))) %>% 
  select(form, rawscore, GDS, agestrat) %>% 
  # add CIs - the CVs are constant across all forms and agestrats
  mutate(GDS_CI90_LB = case_when(
    GDS - 9 < 40 ~ 40,
    TRUE ~ GDS - 9),
    GDS_CI90_UB = case_when(
      GDS + 9 > 160 ~ 160,
      TRUE ~ GDS + 9),
    GDS_CI95_LB = case_when(
      GDS - 12 < 40 ~ 40,
      TRUE ~ GDS - 12),
    GDS_CI95_UB = case_when(
      GDS + 12 > 160 ~ 160,
      TRUE ~ GDS + 12),
    GDS_CI90 = str_c(as.character(GDS_CI90_LB), as.character(GDS_CI90_UB), sep = ' - '),
    GDS_CI95 = str_c(as.character(GDS_CI95_LB), as.character(GDS_CI95_UB), sep = ' - ')
  ) %>% 
  rename(SS = GDS, CI90 = GDS_CI90, CI95 = GDS_CI95) %>% 
  mutate(
    scale = 'GDS',
    growth = as.numeric(NA)
  ) %>% 
  select(scale, form, agestrat, rawscore, SS, growth, CI90, CI95)

# Assemble OES output table: first stack scale and GDS tables
OES_lookup <- bind_rows(scale_CI_growth_AE_lookup, GDS_lookup) %>% 
  # This drops rows that are NA on SS and growth, which shouldn't exist on final output table.
  filter(!(is.na(SS) & is.na(growth))) %>% 
  left_join(perc_lookup, by = 'SS') %>% 
  mutate(descrange = case_when(
    SS >= 131 ~ 'Well above average',
    between(SS, 116, 130) ~ 'Above average',
    between(SS, 85, 115) ~ 'Average',
    between(SS, 70, 84) ~ 'Below average',
    SS <= 69 ~ 'Delayed',
    TRUE ~ NA_character_
  )) %>% 
  arrange(match(scale, c('PHY', 'ADP', 'SOC', 'COG', 'COM', 'GDS')), form, agestrat) %>% 
  left_join(age_labels, by = 'agestrat') %>% 
  rename(agerange = OES_label) %>% 
  select(scale, form, agerange, rawscore, SS, CI90, CI95, growth, descrange, Percentile, AgeEquiv)

# Write OES lookup table to .csv
write_csv(OES_lookup, here(
  'OUTPUT-FILES/DP4-OES-lookup.csv'
))





