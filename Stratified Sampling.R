########## ------------- ########## ------------- ########## -------------

# Columns we have created: gender_grouped, insurance, income, education_level, education_group, age_binary
library(dplyr)

# for reproducibility of random sampling
set.seed(215)

# Total desired sample size
total_n <- 11000

## Target counts per field - Doing currently for gender, race, age, education, income ; leaving insurance for now
target_counts <- list(
  gender_grouped = c("Male" = 4400, "Female" = 6600),
  race_ethnicity = c("white/non latino" = 8100, "all latinos" = 1390, "black/african american" = 1510),
  age_binary = c("Under 50" = 2420, "50 or older" = 8580),
  education_group = c("High school graduate or less" = 1900,
                      "Some college/Associate's" = 3370,
                      "College graduate or higher" = 5730),
  income = c("Low income (<$25k)" = 2310,
             "Middle income ($25k-$75k)" = 3410,
             "Upper middle income ($75k-$150k)" = 2552,
             "High income (>$150k)" = 1210, 
             "Missing" = 1518)
  ## Since medicare, medicaid and government is split in this version
  # Insurance = c("private insurance" = 4730,
  #               "medicare/medicaid/government" = 4730,
  #               "other health insurance" = 220,
  #               "no insurance" = 11)
)



#############  Sampling: Experiments ############## 

library(dplyr)
library(tidyr)

set.seed(215)
total_n <- 11000

# Define target proportions (normalized from counts)
target_props <- list(
  gender_grouped = c("Male" = 0.4, "Female" = 0.6), 
  race_ethnicity = c("white/non latino" = 0.7, "all latinos" = 0.1, "black/african american" = 0.11, "Other" = 0.08), 
  age_binary = c("Under 50" = 0.22, "50 or older" = 0.78), 
  education_group = c("High school graduate or less" = 0.19,
                      "Some college/Associate's" = 0.30,
                      "College graduate or higher" = 0.51), 
  income = c("Low income (<$25k)" = 0.21,
             "Middle income ($25k-$75k)" = 0.31,
             "Upper middle income ($75k-$150k)" = 0.232,
             "High income (>$150k)" = 0.11,
             "Missing" = 0.128), 
  insurance = c("Private insurance" = 0.43,
                "Medicare" = 0.123,
                "Medicaid" = 0.089,
                "Government" = 0.215,
                "Other health plans" = 0.02,
                "no insurance" = 0.001,
                "Missing" = 0.119)
)


# Create a unique group key in FULLDATA
FULLDATA <- FULLDATA %>%
  mutate(group_key = paste(gender_grouped, sep = "::"))

# Generate all possible combinations of values based on your target proportions
prop_df <- expand.grid(target_props, stringsAsFactors = FALSE) %>%
  mutate(
    group_key = paste(gender_grouped, sep = "::"),
    joint_prop = gender_grouped
  ) %>%
  mutate(sample_n = round(joint_prop * total_n)) %>%
  filter(sample_n > 0)

# Sample from FULLDATA according to each group_key
sampled_df <- lapply(seq_len(nrow(prop_df)), function(i) {
  row <- prop_df[i, ]
  subset <- FULLDATA %>% filter(group_key == row$group_key)
  
  if (nrow(subset) >= row$sample_n) {
    sample_n(subset, row$sample_n)
  } else if (nrow(subset) > 0) {
    warning(paste("Sampling with replacement for:", row$group_key))
    sample_n(subset, row$sample_n, replace = TRUE)
  } else {
    message(paste("No data available for:", row$group_key))
    NULL
  }
}) %>% bind_rows()

# Final check
cat("Total rows in final sampled dataset:", nrow(sampled_df), "\n")




### ### ### ### ### ### ### ### One by One method ### ### ### ### ### ### ### ### ### ### 

### Experiment 1
library(dplyr)
library(tidyr)

set.seed(123)
total_n <- 24000

# targets you decided on ─ named vector  level = how many rows you want
target_prop_gender_grouped <- c(Male = 0.4, Female = 0.6)

# sample size
sample_size <- 24000

# translate fractions → exact integers
target_n <- round(target_prop_gender_grouped * sample_size)

# make sure rounding still adds up to sample_size
diff <- sample_size - sum(target_n)
if (diff != 0) {
  # give any leftover rows to the group with the largest share
  target_n[which.max(target_prop_gender_grouped)] <- target_n[which.max(target_prop_gender_grouped)] + diff
}

# draw the sample
sampled_1 <- FULLDATA %>%                     # your data
  inner_join( tibble(gender_grouped = names(target_n), target_n = target_n), by = "gender_grouped") %>%        # attach the target sizes
  group_by(gender_grouped) %>%        # one stratum per gender
  slice_sample(n = first(target_n), replace = FALSE) %>%   # draw
  ungroup()


library(dplyr)

### Experiment 1.1
# Define your target sample size and proportions
total_sample_size <- 24000
target_prop_gender_grouped <- c(Male = 0.4, Female = 0.6)

# Calculate sample sizes for each group
sample_sizes <- round(total_sample_size * target_prop_gender_grouped)

# Sample from each group
stratified_sample_1 <- FULLDATA %>%
  group_by(gender_grouped) %>%
  slice_sample(n = sample_sizes[cur_group()]) %>%
  ungroup()




### Experiment 1.2 -> Kinda works -> Now do some engineering -> Make this into a function
library(dplyr)

# Define your proportions
total_sample_size <- 1000
target_prop_gender_grouped <- c(Male = 0.4, Female = 0.6)
sample_sizes <- round(total_sample_size * target_prop_gender_grouped)

# Get available counts per group
group_counts <- FULLDATA %>% 
  count(gender_grouped) %>%
  deframe()

# Check if sample sizes are feasible
if(any( sample_sizes > group_counts[names(sample_sizes)] )) {
  stop("Sample size larger than available data for some groups")
}

# Sample row indices for each group
set.seed(123)  # for reproducibility
sampled_indices <- c()

for(group in names(sample_sizes)) {
  group_indices <- which(FULLDATA$gender_grouped == group)
  sampled_group_indices <- sample(group_indices, sample_sizes[group], replace = FALSE)
  sampled_indices <- c(sampled_indices, sampled_group_indices)
}

# Get the sample
stratified_sample <- FULLDATA[sampled_indices, ]

######NULL MODEL#####

### See later
# kidney_model_null <- glm(genetic_utilization ~ kidney_red_flag_count, 
#                          family = binomial(link = "logit"), 
#                          data = subset(FULLDATA, diagnosis == "kidney disease"))
# 
# # View the summary of the model
# summary(kidney_model_null)  ###OR: 1.6
# 
# library(broom)
# odds_ratios <- exp(coef(kidney_model_null))
# conf_int <- exp(confint(kidney_model_null))
# results_null_table <- cbind(OR = odds_ratios, Lower = conf_int[,1], Upper = conf_int[,2])
# print(results_null_table)


############# ############# ############ Now: Randomly Sampling and Matching ~ 11,000 ############## ############### ################## ################## 

# Percentages we need from table 1:

## Gender	%
# Male	39.4%
# Female	59.0%
# Gender Diverse/Not specified/Prefer not to answer  	1.6%

## Race/Ethnicity	
# White/Non Hispanic	69.0%
# All Latino	10.3%
# Black	11.0%
# Other	7.8%
# Missing	1.9%

## Age	
# Under 50:	21.4%
# 50 or older:	78.6%

## Education	
# High school graduate or less	17.1%
# Some college/associates	30.0%
# College grad or higher	51.7%
# Not specified	1.2%

## Income	
# low income (<$25k)	20.6%
# middle income ($25k-$75k)	30.5%
# upper middle income ($75k-$150k)	23.2%
# high income (>$150k)	10.9%
# Missing	14.8%

## Insurance	
# private insurance	43.2%
# medicare/medicaid/government	43.5%
# other health insurance	2.3%
# no insurance	0.1%
# Missing	11.0%



#############  Sampling: Main method ############## 

### Inference 1.3 Function -> sample_func
sample_func <- function(total_sample_size, target_props, col_name, data){
  
  # Calculate sample_sizes
  sample_sizes <- round(total_sample_size * target_props)
  
  
  # Get available counts per group
  df_counts <- as.data.frame(table(data[[col_name]]))
  names(df_counts) <- c(col_name, "n")
  group_counts <- df_counts %>% deframe()
  
  view( group_counts[names(sample_sizes)] )
  view(sample_sizes)
  # Check if sample sizes are feasible
  if(any(sample_sizes > group_counts[names(sample_sizes)])) {
    stop("Sample size larger than available data for some groups")
  }
  
  # Sample row indices for each group
  set.seed(123)  # for reproducibility
  sampled_indices <- c()
  
  for(group in names(sample_sizes)) {
    group_indices <- which(data[[col_name]] == group)
    sampled_group_indices <- sample(group_indices, sample_sizes[group], replace = FALSE)
    sampled_indices <- c(sampled_indices, sampled_group_indices)
  }
  
  # Get the sample
  stratified_sample <- data[sampled_indices, ]
  
  # Check if there are no repetitions in the sample - Prints TRUE if there are no repititions
  print( nrow(stratified_sample) == nrow(unique(stratified_sample)) )
  
  return(stratified_sample)
  
}


### Inference 1.4 Function -> print variable ratios - for checking the proportions at every step
print_val_ratios <- function(data, col_name){
  tab <- table( data[[col_name]] )
  ratios <- new.env()
  for(value in unique(data[[col_name]])) {
    ratios[[value]] = tab[value] / sum(tab)
  }
  
  for (key in names(ratios)) {
    cat(key, ":", ratios[[key]], "\n")
  }
  
}

########## Samples

### 1
set.seed(213456)
total_sample_size <- 24000
target_prop_race_ethnicity = c("white/non latino" = 0.74, "all latinos" = 0.085, "black/african american" = 0.068, "other" = 0.078, "missing" = 0.020)
sample_1_ <- sample_func(total_sample_size, target_prop_race_ethnicity, "race_ethnicity", FULLDATA)

# Check the previous variables' spread
print_val_ratios(sample_1_, "race_ethnicity")


### 2
set.seed(213456)
# Use sample_1 and use the proportions for the next field
total_sample_size <- 21000 # Shortend the sample size from the previous
target_prop_gender_grouped <- c(Male = 0.4, Female = 0.6)
sample_2_ <- sample_func(total_sample_size, target_prop_gender_grouped, "gender_grouped", sample_1_)

# Check the previous variables' spread
print_val_ratios(sample_2_, "race_ethnicity")
print_val_ratios(sample_2_, "gender_grouped")

### 3
set.seed(213456)
# Use sample_1 and use the proportions for the next field
total_sample_size <- 19000 # Shortend the sample size from the previous
target_prop_age_binary = c("Under 50" = 0.22, "50 or older" = 0.78)
sample_3_ <- sample_func(total_sample_size, target_prop_age_binary, "age_binary", sample_2_)

# Check the previous variables' spread
print_val_ratios(sample_3_, "race_ethnicity")
print_val_ratios(sample_3_, "gender_grouped")
print_val_ratios(sample_3_, "age_binary")


### 4
set.seed(213456)
# Use sample_1 and use the proportions for the next field
total_sample_size <- 17000 # Shortend the sample size from the previous
target_prop_education_group = c("High school graduate or less" = 0.137,
                                "Some college/Associate's" = 0.30,
                                "College graduate or higher" = 0.563)
sample_4_ <- sample_func(total_sample_size, target_prop_education_group, "education_group", sample_3_)

# Check the previous variables' spread
print_val_ratios(sample_4_, "race_ethnicity")
print_val_ratios(sample_4_, "gender_grouped")
print_val_ratios(sample_4_, "age_binary")
print_val_ratios(sample_4_, "education_group")


### 5
set.seed(213456)
# Use sample_1 and use the proportions for the next field
total_sample_size <- 14000 # Shortend the sample size from the previous
target_prop_income = c("Low income (<$25k)" = 0.172,
                       "Middle income ($25k-$75k)" = 0.320,
                       "Upper middle income ($75k-$150k)" = 0.247,
                       "High income (>$150k)" = 0.128,
                       "Missing" = 0.133)
sample_5_ <- sample_func(total_sample_size, target_prop_income, "income", sample_4_)

# Check the previous variables' spread
print_val_ratios(sample_5_, "race_ethnicity")
print_val_ratios(sample_5_, "gender_grouped")
print_val_ratios(sample_5_, "age_binary")
print_val_ratios(sample_5_, "education_group")
print_val_ratios(sample_5_, "income")

## Insurance not included for now:
## Final Word - Only race_ethnicity is a bit wayward, otherwise everything else matches

### 6
set.seed(213456)
# Sampling 14,000 from this 11,000 -> no constraints
total_sample_size <- 11000
## Below not needed currently since we are not doting on insurance too much
# target_prop_insurance = c(
#   "private insurance" = 0.432,
#   "medicare/medicaid/government" = 0.435,
#   "other health insurance" = 0.023,
#   "no insurance" = 0.001,
#   "Missing" = 0.11,
#   )

sample_6_ <- sample_5_ %>% slice_sample(n = total_sample_size)


# Check the previous variables' spread
print_val_ratios(sample_6_, "race_ethnicity")
print_val_ratios(sample_6_, "gender_grouped")
print_val_ratios(sample_6_, "age_binary")
print_val_ratios(sample_6_, "education_group")
print_val_ratios(sample_6_, "income")
print_val_ratios(sample_6_, "insurance")