
library(tidyverse)
library(vroom)

#load all exception files and deduplicate
files <- list.files(pattern = "*_sti-exceptions.csv")
exceptions <- vroom(files) %>%
  distinct()

#identify cases script unable to close (regardless of time delay)
manual_closure_required <- exceptions %>%
  mutate(address_flag = grepl("County must be specified|County cannot be 'Out of State'|County must be 'Out of State'", Reason),
         sex_flag = grepl("Sex/gender is blank|Sex/gender is entered as Unknown", Reason),
         age_flag = grepl("age cannot be blank", Reason),
         invalid_flag = grepl("has invalid|is not valid|should contain a minimum of two", Reason),
         deceased_flag = grepl("Deceased", Reason),
         rx_flag = grepl("Cannot select both an adequate treatment and inadequate", Reason),
         any_flag = address_flag | sex_flag | invalid_flag | deceased_flag | age_flag | rx_flag) %>%
  filter(any_flag)

#save file for manual closure
write_csv(manual_closure_required, paste0(Sys.Date(),'_manual-closure-required.csv'))
