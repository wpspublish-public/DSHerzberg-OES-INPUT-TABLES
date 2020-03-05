suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

file_strat_var <- 'form'
file_strat_val <- c('parent', 'teacher', 'self')
tab_strat_var <- 'agestrat'
key_var <- 'raw'
col_var <- 'SS'

scale_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('INPUT-FILES/', x, '-', tab_strat_var, '-', key_var, 'To', col_var, '.xlsx')) %>%
    assign('path', ., envir = .GlobalEnv)


  # input file is multi-tabbed .xlsx. Tabs contain lookup tables for each
  # agestrat. read input file into a df, stacking tabs on top of one another, and
  # creating a new column 'agestrat' to identify the origin tab of each set of rows.
  path %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = path,
          .id = tab_strat_var)
}

file_strat_val %>% 
  map(scale_readin) %>% 
  setNames(file_strat_val) %>% 
  bind_rows(.id = file_strat_var) %>% 
  assign(
    paste0(file_strat_var, '_', tab_strat_var, '_', key_var, 'To', col_var, '_lookup'), 
    ., envir = .GlobalEnv)

# Read in CV .xlsx
CV_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('INPUT-FILES/CV', x, '.xlsx')) %>%
    assign(paste0(file_strat_var, '_', tab_strat_var, '_', key_var, 'To', col_var), ., envir = .GlobalEnv)
  path %>% 
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = path,
           .id = 'form')
}

CV_lookup_pre <- CV %>% 
  map(CV_readin) %>% 
  setNames(CV) %>% 
  reduce(full_join, by = c('form', 'agestrat'))

# Read in growth score .xlsx
growth_lookup_pre <- here('INPUT-FILES/growth.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/growth.xlsx'),
         .id = 'form')

# Read in age-equiv .xlsx
age_equiv_lookup_pre <- here('INPUT-FILES/age-equiv.xlsx') %>% 
  excel_sheets() %>%
  set_names() %>%
  map_df(read_excel,
         path = here('INPUT-FILES/age-equiv.xlsx'),
         .id = 'form')

# join scale, CV, growth, AE
all_lookup <- scale_lookup_pre %>% 
  left_join(CV_lookup_pre, by = c('form', 'agestrat')) %>% 
  left_join(growth_lookup_pre, by = c('form', 'rawscore')) %>% 
  left_join(age_equiv_lookup_pre, by = c('form', 'rawscore')) 


# Create CIs and transform into multilevel format required for final OES output.
all_lookup_tall <- scale_acr %>%
  map_dfc(~ all_lookup %>% 
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
  bind_cols(all_lookup, .) %>% 
  select(form:last(scale_acr), ends_with('CI90'), ends_with('CI95'), ends_with('_G'), ends_with('_AE')) %>% 
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
  select(scale, form, agestrat, rawscore, SS, CI90, CI95, G, AE) %>% 
  rename(growth = G, AgeEquiv = AE) %>% 
  arrange(scale) %>% 
  mutate(
    SS = as.numeric(SS),
    growth = as.numeric(growth)
  )
