# bIPF allows for NAs, so instead take the average of the answered items
# "The BIPF is scored by summing the scored items to create a total score, 
# dividing the total score by the maximum possible score based on the 
# number of items scored, and multiplying by 100. 
# Thus, the B-IPF represents an overall index of functioning, 
# with higher scores indicating greater functional impairment."
# Kleiman, S. E., Bovin, M. J., Black, S. K., Rodriguez, P., Brown, L. G., Brown, M. E., Lunney, C. A., Weathers, F. W., Schnurr, P. P., Spira, J., Keane, T. M., & Marx, B. P. (2020). Psychometric properties of a brief measure of posttraumatic stress disorder–related impairment: The Brief Inventory of Psychosocial Functioning. Psychological Services, 17(2), 187–194. https://doi.org/10.1037/ser0000306



# SCORE THE TOTAL BIPF ----------------------------------------------------
## Creates a function to score the bIPF
score_bipf <-
  function(data_x){
  
  ## Calculate the number of NAs and number of answered Items
  data_x <-
    data_x %>% 
    select(bipf_children,
           bipf_daily,
           bipf_family,
           bipf_friends,
           bipf_education,
           bipf_spouse,
           bipf_work) %>%
    transmute(
      bipf_NAs = rowSums(is.na(.)), 
      bipf_answered = 7 - bipf_NAs 
    ) %>% 
    bind_cols(data_x)
  
  ## Sum together the item scores without regard for NAs
  data_x <-
    data_x %>% 
    rowwise() %>% 
    mutate(bipf_total = 
             sum(bipf_children,
                 bipf_daily,
                 bipf_family,
                 bipf_friends,
                 bipf_education,
                 bipf_spouse,
                 bipf_work, na.rm = TRUE)
           ) %>% 
    ungroup()
    
  ## Score the scale as the mean per answered item x 100
  data_x <-
    data_x %>% 
    mutate(
      bipf_score = bipf_total / (bipf_answered * 6) * 100
    )
  
  return(data_x)
}



# Categories --------------------------------------------------------------
## "B-IPF scores in the 0-10 range, no impairment; 
## 11-30, mild impairment; 
## 31-50, moderate impairment; 
## 51-80, severe impairment; 
## 81-100, extreme impairment."

## Creates a function to categorize the bIPF scores
categorize_bipf <-
  function(data_x){
    data_x <-
      data_x %>% 
      mutate(
          bipf_category =
            factor(
              case_when(
                bipf_score <= 10 ~ 0, 
                bipf_score > 10 & bipf_score <= 30 ~ 1,
                bipf_score > 30 & bipf_score <= 50 ~ 2,
                bipf_score > 50 & bipf_score <= 80 ~ 3,
                bipf_score > 80 ~ 4
              ),
              levels = c(0, 1, 2, 3, 4),
              labels = c('No Impairment', 'Mild Impairment', 'Moderate Impairment', 'Severe Impairment', 'Extreme Impairment'),
              ordered = TRUE
            )
        )
    
    return(data_x)
}



# Now apply the functions to the 3 data sets ------------------------------

## Dataset 1
data_1 <- data_1 %>% categorize_bipf() # calculate the categories for the outcome variable
data_1 %>% count(bipf_category) # print to show scoring/categorization worked
data_1 %>% count(bipf_NAs, bipf_answered) # Data 1 has no NAs

## Dataset 2
data_2 <- data_2 %>% score_bipf() # calculate the scores for the outcome variable
data_2 <- data_2 %>% categorize_bipf() # calculate the categories for the outcome variable
data_2 %>% count(bipf_category) # print to show scoring/categorization worked
data_2 %>% count(bipf_NAs, bipf_answered) # Data 2 has various numbers of NAs. About 15 did not answer 4 or more.
data_2 <- data_2 %>% filter(bipf_answered >= 4) # remove the 15 results that did not answer at least 4 bIPF items 

## Dataset 3
data_3 <- data_3 %>% score_bipf() # calculate the scores for the outcome variable
data_3 <- data_3 %>% categorize_bipf() # calculate the categories for the outcome variable
data_3 %>% count(bipf_category) # print to show scoring/categorization worked
data_3 %>% count(bipf_NAs, bipf_answered) # Data 3 has various numbers of NAs. About 15 did not answer 4 or more.
data_3 <- data_3 %>% filter(bipf_answered >= 4) # remove the 15 results that did not answer at least 4 bIPF items 



