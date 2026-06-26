news <- news_districts %>%
  dplyr::select(District_Clean, State_Clean, District_Match, Match_Method) %>%
  dplyr::filter(District_Clean !="") %>%
  dplyr::distinct()

news_update <- news_update %>%
  dplyr::filter(!is.na(District_Match),
                District_Match !="-") %>%
  dplyr::select(-Match_Method)

news <- rows_update(news, news_update, by=c("District_Clean", "State_Clean"), unmatched = "ignore")


news_key <- news %>%
  dplyr::filter(District_Match != "") %>%
  dplyr::select(-Match_Method)


write.xlsx(news_key, "news_key.xlsx")


news_districts <- news_districts %>%
  dplyr::select(District_Clean, State_Clean, District_Match, Match_Method) %>%
  dplyr::filter(District_Clean != "",
                !Match_Method %in% c("lake_or_reservoir", "river", "canal", "sea",
                                     "mountain_or_pass", "waterfall", "desert", 
                                     "state_name", "region")) %>%
  dplyr::distinct()

news_manual <- read.xlsx("news_data_manual.xlsx")

news_districts <- news_districts %>%
  dplyr::filter(!Match_Method %in% c("unsure", "mistagged_state", "ambiguous", 
                                     "not_a_place", "ambiguous_state")) %>%
  dplyr::select(-Match_Method) %>%
  dplyr::distinct()





dists <- as.data.frame(unique(news$District_Clean))
dists_new_census <- as.data.frame(unique(setdiff(str_to_lower(dists$`unique(news$District_Clean)`), str_to_lower(covars$district_census11))))
dists_new_shape <- as.data.frame(unique(setdiff(str_to_lower(dists$`unique(news$District_Clean)`), str_to_lower(covars$district_shape23))))
dists_unmatched_census <- as.data.frame(unique(setdiff(str_to_lower(covars$district_census11), str_to_lower(dists$`unique(news$District_Clean)`))))
dists_unmatched_shape <- as.data.frame(unique(setdiff(str_to_lower(covars$district_shape23), str_to_lower(dists$`unique(news$District_Clean)`))))

setdiff(dists_unmatched_shape$`unique(setdiff(str_to_lower(covars$district_shape23), str_to_lower(dists$\`unique(news$District_Clean)\`)))`, dists_unmatched_census$`unique(setdiff(str_to_lower(covars$district_census11), str_to_lower(dists$\`unique(news$District_Clean)\`)))`)
setdiff(dists_unmatched_census$`unique(setdiff(str_to_lower(covars$district_census11), str_to_lower(dists$\`unique(news$District_Clean)\`)))`, dists_unmatched_shape$`unique(setdiff(str_to_lower(covars$district_shape23), str_to_lower(dists$\`unique(news$District_Clean)\`)))`)
