test <- scale_acr %>%
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
  
  ######### START HERE: RUN OES-table-prep-generic first
  
  
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
