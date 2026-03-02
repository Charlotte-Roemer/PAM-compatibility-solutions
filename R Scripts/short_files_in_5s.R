
Recorder = "Audiomoth"
round_to = "5 seconds"
Sp_select = "Pippip"

test = Metadata_All %>% 
  select(Site, Recorder, TriggerLevel, idparticipation) %>% 
  right_join(Obs, by = join_by(idparticipation == participation)) %>% 
  mutate(Recorder = Recorder_rename(Recorder)) 

# Consider Batlogger A and Batlogger A+ the same machine
test$Recorder = gsub("^Batlogger.*", "Batlogger", test$Recorder)

test_aggreg = test %>% 
  filter(Recorder == .env$Recorder) %>%
  filter(espece == Sp_select) %>% 
  mutate(datetime = as.POSIXct(DateTimefun(donnee), format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(datetime = floor_date(datetime, unit = round_to)) 

test_aggreg2 = test_aggreg %>% 
  group_by(datetime, idparticipation, espece) %>%
  slice_max(tadarida_probabilite, with_ties = FALSE) %>% # keep highest probability
  ungroup()

# Percentage of files that are dupplicated by 5s intervals
A = nrow(test_aggreg)
B = nrow(test_aggreg2)
(A-B) / (A+B) * 100
