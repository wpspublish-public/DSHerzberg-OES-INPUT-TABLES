suppressMessages(library(here))
suppressMessages(library(tidyverse))
suppressMessages(library(readxl))

form <- c('parent', 'teacher', 'self')
scale_acr <- c('COG', 'EMO')
CV <- c('90', '95')

scale_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('INPUT-FILES/', x, '-rawToSS.xlsx')) %>%
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

# Read in CV .xlsx
CV_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('INPUT-FILES/CV', x, '.xlsx')) %>%
    assign('path', ., envir = .GlobalEnv)
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


# join scale, CV, growth
all_lookup <- scale_lookup_pre %>% 
  left_join(CV_lookup_pre, by = c('form', 'agestrat')) %>% 
  left_join(growth_lookup_pre, by = c('form', 'rawscore'))



