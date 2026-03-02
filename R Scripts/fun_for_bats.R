#### DATE NIGHT FROM FILE NAME ####
DateNightFun <- function(Filename) { 
  DateNightResult= as.Date(ifelse(str_sub(Filename,-10,-10)==0, #Attention si ".wav" ou pas =+/-3 characteres
                                  as.character(as.Date(paste(str_sub(Filename,-19,-16),
                                                             str_sub(Filename,-15,-14),
                                                             str_sub(Filename,-13,-12),
                                                             sep="-"))-1),
                                  as.character(as.Date(paste(str_sub(Filename,-19,-16),
                                                             str_sub(Filename,-15,-14),
                                                             str_sub(Filename,-13,-12),
                                                             sep="-")))))
}

#### DATE TIME FROM FILE NAME ####
DateTimefun <- function(Filename) {
  nchar = ifelse(grepl(".wav", Filename), 3, 0)
  DateTemp = as.character(paste(str_sub(Filename,-(19+nchar),-(16+nchar)),
                                str_sub(Filename,-(15+nchar),-(14+nchar)),
                                str_sub(Filename,-(13+nchar),-(12+nchar)),
                                sep="-"))
  TimeTemp = as.character(paste(str_sub(Filename,-(10+nchar),-(9+nchar)),
                                str_sub(Filename,-(8+nchar),-(7+nchar)),
                                str_sub(Filename,-(6+nchar),-(5+nchar)),
                                sep=":"))
  paste0(DateTemp, " ", TimeTemp)
}

####  PUT RECORDER FULL NAME #### 
Recorder_rename <- function(Recorder_column) {
  Recorder_column = as.character(Recorder_column) 
  case_when(
    Recorder_column == 1 ~ "Anabat_Scout",
    Recorder_column == 2 ~ "Anabat_Swift",
    Recorder_column == 3 ~ "Audiomoth",
    Recorder_column == 4 ~ "Avisoft_UltraSoundGate",
    Recorder_column == 7 ~ "Batcorder",
    Recorder_column == 8 ~ "Batlogger_A",
    Recorder_column == 9 ~ "Batlogger_A+",
    Recorder_column == 10 ~ "Batlogger_C",
    Recorder_column == 11 ~ "Batmode_S+",
    Recorder_column == 12 ~ "GSM_Batcorder",
    Recorder_column == 13 ~ "Mini_Batcorder",
    Recorder_column == 14 ~ "Passive_recorder",
    Recorder_column == 15 ~ "Peersonic_RPA3",
    Recorder_column == 16 ~ "Petterson_D500X",
    Recorder_column == 17 ~ "SM2BAT",
    Recorder_column == 18 ~ "SM2BAT+",
    Recorder_column == 19 ~ "SM3BAT",
    Recorder_column == 20 ~ "SM4BAT",
    Recorder_column == 21 ~ "SongMeter_MiniBat",
    Recorder_column == 23 ~ "Batlogger_WE_X",
    Recorder_column == 24 ~ "other",
    .default = Recorder_column  # Keep other values unchanged
  )
}

#### AGGREGATE DETECTIONS ####
aggregate_detections <- function(df, round_to) {
  if(Time_interval == "day"){
    df %>%
      select(-frequence_mediane, -temps_debut, -temps_fin) %>% 
      mutate(datetime = as.POSIXct(DateNight, format = "%Y-%m-%d")) %>% 
      mutate(datetime = floor_date(datetime, unit = round_to)) %>%
      group_by(datetime, participation, espece) %>%
      slice_max(tadarida_probabilite, with_ties = FALSE) %>% # keep highest probability
      ungroup()
  }else{
    df %>%
      select(-frequence_mediane, -temps_debut, -temps_fin) %>% 
      mutate(datetime = as.POSIXct(DateTimefun(donnee), format = "%Y-%m-%d %H:%M:%S")) %>% 
      mutate(datetime = floor_date(datetime, unit = round_to)) %>%
      group_by(datetime, participation, espece) %>%
      slice_max(tadarida_probabilite, with_ties = FALSE) %>% # keep highest probability
      ungroup()
  }
  
}

####  SUMMARISE ACTIVITY PER NIGHT #### 
Activity_summary <- function(file, proba) {
  Result = file %>% 
    select(tadarida_probabilite, donnee, participation, espece) %>% 
    filter(tadarida_probabilite > proba) %>% 
    group_by(participation, DateNight, espece) %>% 
    summarise(nb_triggered_files = length(donnee)) %>% 
    mutate(probability_filter = proba)
}

#### CALCULATE ACTIVITY FOR SPECIES WHICH WAS RECORDED AT 0 S #### 
Species_activity_at_0 <- function(file) {
  Obs_0s = file %>% 
    filter(temps_debut == 0)
  
  Activity_0s = data.frame()
  for(i in 1:length(Filter_proba)){
    Activity_0_temp = Activity_summary(Obs_0s, Filter_proba[i])
    Activity_0s = rbind(Activity_0s, Activity_0_temp)
  }
  
  Activity_0s = Activity_0s %>% 
    mutate(espece = paste0(espece, "_0s"))
}

#### SCALE TRIGGER LEVEL ####

scale_trigger <- function(trig, table, rec) {
  
  # select recorder
  tab <- table[table$Recorder == rec, ]
  
  # Sort by decreasing trigger level
  tab <- tab[order(tab$TriggerLevel_adjusted, decreasing = TRUE), ]
  
  x <- tab$TriggerLevel_adjusted
  y <- tab$TriggerLevel_scaled
  
  # Exact case
  if (trig %in% x) {
    return(y[x == trig][1])
  }
  
  # Framing
  x1 <- max(x[x > trig])
  x2 <- min(x[x < trig])
  
  y1 <- y[x == x1][1]
  y2 <- y[x == x2][1]
  
  # Linear interpolation
  y1 + (trig - x1) * (y2 - y1) / (x2 - x1)
}
