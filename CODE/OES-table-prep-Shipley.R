suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

form <- c('child', 'adult')
scale_acr <- c('VOC', 'ABS', 'BLO', 'CMA', 'CMB')
CV <- c('68', '90', '95')

# read in percentile lookup column
perc_lookup <- suppressMessages(read_csv(here('INPUT-FILES/SHIPLEY/Percentile-Lookup-SS.csv')))

scale_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('INPUT-FILES/SHIPLEY/', x, '-rawToSS.xlsx')) %>%
    assign('path', ., envir = .GlobalEnv)


  # input file is multi-tabbed .xlsx. Tabs contain lookup tables for each
  # agestrat. read input file into a df, stacking tabs on top of one another, and
  # creating a new column 'agestrat' to identify the origin tab of each set of rows.
  path %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = path,
           .id = 'agestrat')
}

scale_lookup_pre <- form %>% 
  map(scale_readin) %>% 
  setNames(form) %>% 
  bind_rows(.id = 'form')

# Read in age equiv .cvs
age_equiv_pre <- suppressMessages(read_csv(here('INPUT-FILES/SHIPLEY/age-equiv.csv')))

# Read in CV .xlsx
CV_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('INPUT-FILES/SHIPLEY/CV', x, '.xlsx')) %>%
    assign('path', ., envir = .GlobalEnv)
  path %>% 
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = path,
           .id = 'form',
           col_types = c('text', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
           )
}

CV_lookup_pre <- CV %>% 
  map(CV_readin) %>% 
  setNames(CV) %>% 
  reduce(full_join, by = c('form', 'agestrat'))

# join scale, AE, CV
all_lookup <- scale_lookup_pre %>% 
  left_join(age_equiv_pre, by = 'rawscore') %>% 
  left_join(CV_lookup_pre, by = c('form', 'agestrat'))

# Create CIs and transform into multilevel format required for final OES output.
all_lookup_tall <- scale_acr %>%
  map_dfc(~ all_lookup %>% 
            # dplyr::transmute() is similar to mutate(), but it drops the input
            # columns after creating the new var
            transmute(
              # Next four operations lines get upper, lower bounds of CIs as numbers
              !!str_c(.x, '_CI68_LB_pre') := !!sym(.x) - !!sym(str_c(.x, '_CV68')),
              !!str_c(.x, '_CI68_UB_pre') := !!sym(.x) + !!sym(str_c(.x, '_CV68')), 
              !!str_c(.x, '_CI90_LB_pre') := !!sym(.x) - !!sym(str_c(.x, '_CV90')),
              !!str_c(.x, '_CI90_UB_pre') := !!sym(.x) + !!sym(str_c(.x, '_CV90')), 
              !!str_c(.x, '_CI95_LB_pre') := !!sym(.x) - !!sym(str_c(.x, '_CV95')),
              !!str_c(.x, '_CI95_UB_pre') := !!sym(.x) + !!sym(str_c(.x, '_CV95')), 
              # Next four operations truncate UB at 160, LB at 40, and coerce
              # both to character
              !!str_c(.x, '_CI68_LB') := as.character(case_when(
                !!sym(str_c(.x, '_CI68_LB_pre')) < 40 ~ 40,
                TRUE ~ !!sym(str_c(.x, '_CI68_LB_pre'))
              )),
              !!str_c(.x, '_CI68_UB') := as.character(case_when(
                !!sym(str_c(.x, '_CI68_UB_pre')) > 160 ~ 160,
                TRUE ~ !!sym(str_c(.x, '_CI68_UB_pre'))
              )),
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
              !!str_c(.x, '_CI68') :=
                str_c(!!sym(str_c(.x, '_CI68_LB')), !!sym(str_c(.x, '_CI68_UB')), sep = ' - '),
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
  bind_cols(all_lookup, .) %>% 
  select(form:last(scale_acr), ends_with('CI68'), ends_with('CI90'), ends_with('CI95'), ends_with('_AE')) %>% 
  # rename SS cols so all cols to be gathered are named with the format
  # "scaleName_scoreType"
  rename_at(vars(scale_acr), ~ paste0(.x,"_SS")) %>% 
  # gather "scaleName_scoreType" cols into key column, SS and CI values into val
  # col
  gather(key, val, 4:ncol(.)) %>%
  # Now split "scaleName_scoreType" in key col into two cols: scale and type
  extract(key, into = c("scale", "type"), "([:alpha:]{3})?\\_?(.*)") %>%
  # spread so that type yields cols of SS, G, CI90, CI95, and that quad remains
  # paired with correct form, agestrat, rawscore, and scale.
  spread(type, val) %>% 
  select(scale, form, agestrat, rawscore, SS, CI90, CI95, AE) %>% 
  rename(AgeEquiv = AE) %>% 
  arrange(scale) %>% 
  mutate(
    SS = as.numeric(SS)
  )

# Assemble OES output table: first stack scale and GDS tables
OES_lookup <- all_lookup_tall %>% 
  # This drops rows that are NA on SS, which shouldn't exist on final output table.
  filter(!is.na(SS)) %>% 
  left_join(perc_lookup, by = 'SS') %>% 
  mutate(descrange = case_when(
    SS >= 130 ~ 'Superior',
    between(SS, 120, 129) ~ 'Well Above average',
    between(SS, 110, 119) ~ 'Above average',
    between(SS, 90, 109) ~ 'Average',
    between(SS, 80, 89) ~ 'Below average',
    between(SS, 70, 79) ~ 'Well Below average',
    SS <= 69 ~ 'Low',
    TRUE ~ NA_character_
  )) %>% 
  arrange(match(scale, c('VOC', 'ABS', 'BLO', 'CMA', 'CMB')), match(form, c('child', 'adult')), agestrat) %>% 

  #### CODE BELOW NOT YET ADAPTED FOR SHIPLEY
  
  
  left_join(age_labels, by = 'agestrat') %>% 
  rename(agerange = OES_label) %>% 
  select(scale, form, agerange, rawscore, SS, CI90, CI95, growth, descrange, Percentile, AgeEquiv)

# Write OES lookup table to .csv
write_csv(OES_lookup, here(
  'OUTPUT-FILES/DP4-OES-lookup.csv'
))


