
library(tidyverse)
data <- read_csv(here::here('data/data_clean.csv'))


# bIPF allows for NAs, so instead take the average of the answered items
# "The BIPF is scored by summing the scored items to create a total score, 
# dividing the total score by the maximum possible score based on the 
# number of items scored, and multiplying by 100. 
# Thus, the B-IPF represents an overall index of functioning, 
# with higher scores indicating greater functional impairment."
# Kleiman, S. E., Bovin, M. J., Black, S. K., Rodriguez, P., Brown, L. G., Brown, M. E., Lunney, C. A., Weathers, F. W., Schnurr, P. P., Spira, J., Keane, T. M., & Marx, B. P. (2020). Psychometric properties of a brief measure of posttraumatic stress disorder–related impairment: The Brief Inventory of Psychosocial Functioning. Psychological Services, 17(2), 187–194. https://doi.org/10.1037/ser0000306
data <-
  data %>%
  rowwise() %>% 
  mutate(
    bipf_mean = 
      mean(
        c(bipf_spouse, 
          bipf_children, 
          bipf_family,
          bipf_education,
          bipf_friends,
          bipf_work),
        na.rm = T
      )
  ) %>% ungroup()

data <-
  data %>% 
  select(bipf_spouse, 
         bipf_children, 
         bipf_family,
         bipf_education,
         bipf_friends,
         bipf_work) %>%
  transmute(
    bipf_NAs = rowSums(is.na(.)), 
    bipf_answered = 7 - bipf_NAs 
  ) %>% 
  bind_cols(data)


data <-
  data %>% 
  mutate(
    bipf_score = bipf_total / (bipf_answered * 6) * 100
  )



# Categories --------------------------------------------------------------
## "B-IPF scores in the 0-10 range, no impairment; 
## 11-30, mild impairment; 
## 31-50, moderate impairment; 
## 51-80, severe impairment; 
## 81-100, extreme impairment."
data <-
  data %>% 
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
          ordered = T
        )
    )

data %>% count(bipf_category, bipf_score) %>% print(n = 300)

# Missing Data ------------------------------------------------------------
data %>% count(bipf_NAs)
## About 5% of results had 4 or more NAs for the scale
## Another 10% had 3 or more. Lets drop any with 3 or more NAs. 
data <-
  data %>% 
  filter(bipf_NAs < 3)


# Create a dummy variable for 0s and non-zeros ----------------------------
data <-
  data %>% 
  mutate(bipf_gt_zero = if_else(bipf_mean == 0, 0, 1)
  )

# Create a dummy variable for 0s and non-zeros ----------------------------
data <-
  data %>% 
  mutate(bipf_zero = if_else(bipf_mean == 0, 1, 0)
  )

data %>% count(bipf_category)
data %>% count(bipf_score) %>% print(n = 340)
