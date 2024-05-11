get_tidy_demog <- function(d){
  
  # non dropdown 
  
  non_dropdown_df <- d %>% 
    filter(grepl("demog", trial_type) | grepl("survey-likert", trial_type)) %>% 
    select(subject, variable_type, trial_type, responses) %>% 
    filter(!variable_type %in% c("demog_birht_year_month", "demog_country_born_current_grew_up", 
                                 "demog_parent_grewup", "demog_gender_ethnic", "demog_age_gender_ethnic", "adult_help", 
                                 "finalfeedback")) %>% 

    group_by(subject) %>%
    mutate(responses = map(responses, ~ fromJSON(.) %>% as.data.frame())) %>%
    unnest(responses) %>%
    select(-variable_type) %>%
    group_by(subject) %>%
    mutate_at(vars(-group_cols()), function(x) {
      lapply((list(as.character(x))), function(x) unique(x[!is.na(x)]))
    }) %>%
    distinct() %>% 
    mutate_at(vars(-group_cols()), as.character) 
  
  # check if this is actually a file that includes demog 
  if(ncol(non_dropdown_df) > 4){
    
    
    
    non_dropdown_df <- non_dropdown_df %>% 
      pivot_longer(
        cols = 4:ncol(.), 
        names_to = "demog_question", 
        values_to = "demog_response"
      ) 
    
  
 
  
  if("id" %in% colnames(non_dropdown_df)){
    non_dropdown_df <- non_dropdown_df %>% 
      select(subject, id, demog_question, demog_response) %>% 
      mutate(id = as.character(id))
  }else{
    non_dropdown_df <- non_dropdown_df %>% 
      select(subject, demog_question, demog_response) %>% 
      mutate(id = "NA")
  }
  
  
  

  
  # dropdown 
  dropdown_df <- d %>% 
    filter(grepl("demog", trial_type) | grepl("survey-likert", trial_type)) %>% 
    select(subject, variable_type, trial_type, responses) %>% 
    filter(variable_type %in% c("demog_birht_year_month", "demog_country_born_current_grew_up", 
                                "demog_parent_grewup", "demog_gender_ethnic", "demog_age_gender_ethnic")) %>% 
    group_by(subject) %>%
    mutate(responses = map(responses, ~ fromJSON(.))) %>%
    unnest(responses) %>% 
    group_by(subject, variable_type) %>% 
    #add row number to question type to seperate question from answer
    mutate(q_id = row_number()) %>% 
    # since answer often occurs second, it must be an even number 
    filter(q_id %% 2 == 0) %>% 
    mutate(q_id = q_id / 2) %>% 
    unnest(responses)
  
  # demog_birht_year_month
  birth_year_month_df <- dropdown_df %>% 
    ungroup() %>% 
    filter(variable_type == "demog_birht_year_month") %>% 
    mutate(demog_question = case_when(
      q_id == 1 ~ "birth_year", 
      q_id == 2 ~ "birth_month"
    )) %>% 
    rename(demog_response = responses) %>% 
    mutate(id = unique(non_dropdown_df$id)) %>% 
    select(subject, id, demog_question, demog_response)
  
  if (nrow(birth_year_month_df) == 0){
    
    birth_year_month_df <- dropdown_df %>% 
      ungroup() %>% 
      filter(variable_type == "demog_age_gender_ethnic") %>% 
      mutate(demog_question = case_when(
        q_id == 1 ~ "age",
        q_id == 2 ~ "gender", 
        q_id == 3 ~ "ethnic"
      )) %>% 
      rename(demog_response = responses) %>% 
      mutate(id = unique(non_dropdown_df$id)) %>% 
      select(subject, id, demog_question, demog_response)
    
  }
  
  
  
  
  # demog_country_born_current_grew_up
  
  #  (d %>% filter(variable_type == "demog_country_born_current_grew_up") %>% select(responses))$responses
  
  kid_country_df <- dropdown_df %>% 
    ungroup() %>% 
    filter(variable_type == "demog_country_born_current_grew_up") %>% 
    mutate(demog_question = case_when(
      q_id == 1 ~ "kid_born_in", 
      q_id == 2 ~ "kid_now_in", 
      q_id == 3 ~ "kid_now_state"
    )) %>% 
    rename(demog_response = responses) %>% 
    mutate(id = unique(non_dropdown_df$id)) %>% 
    select(subject, id, demog_question, demog_response)
  
  
  # demog_parent_grewup
  
  parent_country_df <- dropdown_df %>% 
    ungroup() %>% 
    filter(variable_type == "demog_parent_grewup") %>% 
    mutate(demog_question = case_when(
      q_id == 1 ~ "parent_live_in", 
      q_id == 2 ~ "parent_grow_up_in", 
    )) %>% 
    rename(demog_response = responses) %>% 
    mutate(id = unique(non_dropdown_df$id)) %>% 
    select(subject, id, demog_question, demog_response)
  
  
  # demog_gender_ethnic
  gender_df <- dropdown_df %>% 
    ungroup() %>% 
    filter(variable_type == "demog_gender_ethnic") %>% 
    mutate(demog_question = case_when(
      q_id == 1 ~ "gender", 
      q_id == 2 ~ "ethnic"
    )) %>%  
    rename(demog_response = responses) %>% 
    mutate(id = unique(non_dropdown_df$id)) %>% 
    select(subject, id, demog_question, demog_response)
  
  
  clean_demog_df <- bind_rows(
    non_dropdown_df, 
    birth_year_month_df, 
    kid_country_df, 
    parent_country_df, 
    gender_df
  ) %>% 
    filter(!demog_question == "finalfeedback")
  
  return (clean_demog_df)
  
  
  }
  
  
}