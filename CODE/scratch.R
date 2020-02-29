CV_readin <- function(x) {
  # express the directory paths to the input files as a char vec.
  here(
    paste0('INPUT-FILES/', x, '-CV.xlsx')) %>%
    assign('path', ., envir = .GlobalEnv)
  
  
  # input file is multi-tabbed .xlsx. Tabs contain lookup tables for each
  # agestrat. read input file into a df, stacking tabs on top of one another, and
  # creating a new column 'agestrat' to identify the origin tab of each set of rows.
  path %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel,
           path = path
           # .id = 'agestrat',
           # col_types = c('text', 'numeric', 'numeric', 'numeric', 'numeric')
  )
}

CV_lookup_pre <- form %>% 
  map(CV_readin) %>% 
  setNames(form) %>% 
  bind_rows(.id = 'form')
