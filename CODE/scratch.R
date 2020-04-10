input_gathered <- form_agestrat_rawToSS_lookup %>%
  gather('scale','SS',-form, -agestrat, -rawscore) %>% 
  select(scale, everything())
