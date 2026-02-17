library(tidyverse)

# Read data
df <- read_csv("data/IER_support_responses.csv")

# Create response data with unique IDs
data_for_qualtrics <- df %>%
  ungroup() %>%
  mutate(response_id = row_number()) %>%
  select(response_id, pID, gender, Condition, support_response)

# Check total responses
n_responses <- nrow(data_for_qualtrics)
cat("Total responses:", n_responses, "\n")

# Set parameters
responses_per_set <- 20 # bump to 15
n_sets <- ceiling(n_responses / responses_per_set)

cat("Responses per set:", responses_per_set, "\n")
cat("Number of sets:", n_sets, "\n")

# Assign to sets - balanced by Condition
set.seed(123)
data_with_sets <- data_for_qualtrics %>%
  group_by(Condition) %>%
  mutate(set_number = rep(0:(n_sets-1), length.out = n())) %>%
  ungroup() %>%
  arrange(set_number, response_id)

# Check balance
cat("\nResponses per set:\n")
data_with_sets %>%
  count(set_number) %>%
  print(n = Inf)

cat("\nCondition balance per set:\n")
data_with_sets %>%
  count(set_number, Condition) %>%
  pivot_wider(names_from = Condition, values_from = n) %>%
  print(n = Inf)

# Master lookup
master_lookup <- df %>%
  ungroup() %>%
  mutate(response_id = row_number()) %>%
  select(
    response_id,
    pID,
    gender,
    Condition,
    everything()
  )

write_csv(master_lookup, "data/response_id_lookup.csv")

# Create CSV for Qualtrics
loop_merge_file <- data_with_sets %>%
  select(
    response_id, 
    set_number, 
    response_text = support_response,
    provider_gender = gender,  # ADD: Gender of person who wrote response
    condition = Condition       # ADD: Same vs other condition
  ) %>%
  mutate(
    response_text = str_replace_all(response_text, '"', "'"),
    response_text = str_replace_all(response_text, "\n", " "),
    response_text = str_replace_all(response_text, "\r", " "),
    response_text = str_trim(response_text)
  )

write_csv(loop_merge_file, "data/responses_with_sets.csv")
cat("Saved: data/responses_with_sets.csv\n")

# Reference file
set_reference <- data_with_sets %>%
  group_by(set_number) %>%
  summarise(
    n_responses = n(),
    response_ids = paste(response_id, collapse = ", "),
    n_same = sum(Condition == "same", na.rm = TRUE),
    n_other = sum(Condition == "other", na.rm = TRUE),
    first_response_id = min(response_id),
    last_response_id = max(response_id)
  )

write_csv(set_reference, "data/set_reference.csv")

cat("\n=== FILES CREATED ===\n")
cat("1. data/responses_with_sets.csv     <- Upload to Qualtrics\n")
cat("2. data/response_id_lookup.csv      <- SAVE THIS! For merging later\n")
cat("3. data/set_reference.csv           <- Optional reference\n")