rm(list = ls())
library(haven)
library(dplyr)
#All files read in this study are country-specific .sav files obtained from the DHS-8 datasets. The original file names were not changed.

need_index <- c("V001", "V002", "V005", "V021", "V023", "V000", "HW70", "B19", "V106", "V190", "V012", "V701", "V040", "V171B", "V170", "V151", "HW57")
need_index_2 <- c("HV001", "HV002", "HV243A", "HV216", "HV009")

dhs_data_1 <- read_sav("AOKR81FL.sav")
dhs_data_1_subset <- dhs_data_1[ ,colnames(dhs_data_1) %in% need_index]
dhs_data_1_2 <- read_sav("AOHR81FL.sav")
dhs_data_1_2_subset <- dhs_data_1_2[ ,colnames(dhs_data_1_2) %in% need_index_2]
dhs_data_1_2_subset <- dhs_data_1_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_1_new <- dhs_data_1_subset %>%
  left_join(
    dhs_data_1_2_subset,
    by = c("V001", "V002")
  )

dhs_data_2 <- read_sav("CDKR81FL.sav")
dhs_data_2_subset <- dhs_data_2[ ,colnames(dhs_data_2) %in% need_index]
dhs_data_2_2 <- read_sav("CDHR81FL.sav")
dhs_data_2_2_subset <- dhs_data_2_2[ ,colnames(dhs_data_2_2) %in% need_index_2]
dhs_data_2_2_subset <- dhs_data_2_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_2_new <- dhs_data_2_subset %>%
  left_join(
    dhs_data_2_2_subset,
    by = c("V001", "V002")
  )

dhs_data_3 <- read_sav("KEKR8CFL.sav")
dhs_data_3_subset <- dhs_data_3[ ,colnames(dhs_data_3) %in% need_index]
dhs_data_3_2 <- read_sav("KEHR8CFL.sav")
dhs_data_3_2_subset <- dhs_data_3_2[ ,colnames(dhs_data_3_2) %in% need_index_2]
dhs_data_3_2_subset <- dhs_data_3_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_3_new <- dhs_data_3_subset %>%
  left_join(
    dhs_data_3_2_subset,
    by = c("V001", "V002")
  )

dhs_data_4 <- read_sav("LSKR81FL.sav")
dhs_data_4_subset <- dhs_data_4[ ,colnames(dhs_data_4) %in% need_index]
dhs_data_4_2 <- read_sav("LSHR81FL.sav")
dhs_data_4_2_subset <- dhs_data_4_2[ ,colnames(dhs_data_4_2) %in% need_index_2]
dhs_data_4_2_subset <- dhs_data_4_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_4_new <- dhs_data_4_subset %>%
  left_join(
    dhs_data_4_2_subset,
    by = c("V001", "V002")
  )

dhs_data_5 <- read_sav("MWKR81FL.sav")
dhs_data_5_subset <- dhs_data_5[ ,colnames(dhs_data_5) %in% need_index]
dhs_data_5_2 <- read_sav("MWHR81FL.sav")
dhs_data_5_2_subset <- dhs_data_5_2[ ,colnames(dhs_data_5_2) %in% need_index_2]
dhs_data_5_2_subset <- dhs_data_5_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_5_new <- dhs_data_5_subset %>%
  left_join(
    dhs_data_5_2_subset,
    by = c("V001", "V002")
  )

dhs_data_6 <- read_sav("MLKR8AFL.sav")
dhs_data_6_subset <- dhs_data_6[ ,colnames(dhs_data_6) %in% need_index]
dhs_data_6_2 <- read_sav("MLHR8AFL.sav")
dhs_data_6_2_subset <- dhs_data_6_2[ ,colnames(dhs_data_6_2) %in% need_index_2]
dhs_data_6_2_subset <- dhs_data_6_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_6_new <- dhs_data_6_subset %>%
  left_join(
    dhs_data_6_2_subset,
    by = c("V001", "V002")
  )

dhs_data_7 <- read_sav("NGKR8BFL.sav")
dhs_data_7_subset <- dhs_data_7[ ,colnames(dhs_data_7) %in% need_index]
dhs_data_7_2 <- read_sav("NGHR8BFL.sav")
dhs_data_7_2_subset <- dhs_data_7_2[ ,colnames(dhs_data_7_2) %in% need_index_2]
dhs_data_7_2_subset <- dhs_data_7_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_7_new <- dhs_data_7_subset %>%
  left_join(
    dhs_data_7_2_subset,
    by = c("V001", "V002")
  )


dhs_data_8 <- read_sav("SNKR8SFL.sav")
dhs_data_8_subset <- dhs_data_8[ ,colnames(dhs_data_8) %in% need_index]
dhs_data_8_2 <- read_sav("SNHR8SFL.sav")
dhs_data_8_2_subset <- dhs_data_8_2[ ,colnames(dhs_data_8_2) %in% need_index_2]
dhs_data_8_2_subset <- dhs_data_8_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_8_new <- dhs_data_8_subset %>%
  left_join(
    dhs_data_8_2_subset,
    by = c("V001", "V002")
  )

dhs_data_9 <- read_sav("TZKR82FL.sav")
dhs_data_9_subset <- dhs_data_9[ ,colnames(dhs_data_9) %in% need_index]
dhs_data_9_2 <- read_sav("TZHR82FL.sav")
dhs_data_9_2_subset <- dhs_data_9_2[ ,colnames(dhs_data_9_2) %in% need_index_2]
dhs_data_9_2_subset <- dhs_data_9_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_9_new <- dhs_data_9_subset %>%
  left_join(
    dhs_data_9_2_subset,
    by = c("V001", "V002")
  )

dhs_data_10 <- read_sav("ZMKR81FL.sav")
dhs_data_10_subset <- dhs_data_10[ ,colnames(dhs_data_10) %in% need_index]
dhs_data_10_2 <- read_sav("ZMHR81FL.sav")
dhs_data_10_2_subset <- dhs_data_10_2[ ,colnames(dhs_data_10_2) %in% need_index_2]
dhs_data_10_2_subset <- dhs_data_10_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_10_new <- dhs_data_10_subset %>%
  left_join(
    dhs_data_10_2_subset,
    by = c("V001", "V002")
  )

dhs_data_11 <- read_sav("BFKR81FL.sav")
dhs_data_11_subset <- dhs_data_11[ ,colnames(dhs_data_11) %in% need_index]
dhs_data_11_2 <- read_sav("BFHR81FL.sav")
dhs_data_11_2_subset <- dhs_data_11_2[ ,colnames(dhs_data_11_2) %in% need_index_2]
dhs_data_11_2_subset <- dhs_data_11_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_11_new <- dhs_data_11_subset %>%
  left_join(
    dhs_data_11_2_subset,
    by = c("V001", "V002")
  )

dhs_data_12 <- read_sav("CIKR81FL.sav")
dhs_data_12_subset <- dhs_data_12[ ,colnames(dhs_data_12) %in% need_index]
dhs_data_12_2 <- read_sav("CIHR81FL.sav")
dhs_data_12_2_subset <- dhs_data_12_2[ ,colnames(dhs_data_12_2) %in% need_index_2]
dhs_data_12_2_subset <- dhs_data_12_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_12_new <- dhs_data_12_subset %>%
  left_join(
    dhs_data_12_2_subset,
    by = c("V001", "V002")
  )

dhs_data_13 <- read_sav("GHKR8CFL.sav")
dhs_data_13_subset <- dhs_data_13[ ,colnames(dhs_data_13) %in% need_index]
dhs_data_13_2 <- read_sav("GHHR8CFL.sav")
dhs_data_13_2_subset <- dhs_data_13_2[ ,colnames(dhs_data_13_2) %in% need_index_2]
dhs_data_13_2_subset <- dhs_data_13_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_13_new <- dhs_data_13_subset %>%
  left_join(
    dhs_data_13_2_subset,
    by = c("V001", "V002")
  )

dhs_data_14 <- read_sav("MZKR81FL.sav")
dhs_data_14_subset <- dhs_data_14[ ,colnames(dhs_data_14) %in% need_index]
dhs_data_14_2 <- read_sav("MZHR81FL.sav")
dhs_data_14_2_subset <- dhs_data_14_2[ ,colnames(dhs_data_14_2) %in% need_index_2]
dhs_data_14_2_subset <- dhs_data_14_2_subset %>%
  rename(
    V001 = HV001,
    V002 = HV002
  )
dhs_data_14_new <- dhs_data_14_subset %>%
  left_join(
    dhs_data_14_2_subset,
    by = c("V001", "V002")
  )

dhs_data_1_new <- dhs_data_1_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_2_new <- dhs_data_2_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_3_new <- dhs_data_3_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_4_new <- dhs_data_4_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_5_new <- dhs_data_5_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_6_new <- dhs_data_6_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_7_new <- dhs_data_7_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_8_new <- dhs_data_8_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_9_new <- dhs_data_9_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_10_new <- dhs_data_10_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_11_new <- dhs_data_11_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_12_new <- dhs_data_12_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_13_new <- dhs_data_13_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_14_new <- dhs_data_14_new |>
  mutate(across(where(haven::is.labelled), haven::zap_labels))

dhs_data_summary <- rbind(dhs_data_1_new, dhs_data_2_new, dhs_data_3_new, 
                          dhs_data_4_new, dhs_data_5_new, dhs_data_6_new,
                          dhs_data_7_new, dhs_data_8_new, dhs_data_9_new,
                          dhs_data_10_new, dhs_data_11_new, dhs_data_12_new,
                          dhs_data_13_new, dhs_data_14_new)

dhs_data_summary$HW70 <- ifelse(
  dhs_data_summary$HW70 < 9990,
  dhs_data_summary$HW70 / 100,
  NA
)

dhs_data_summary$V106 <- ifelse(
  dhs_data_summary$V106 == 0,
  0,
  1
)

dhs_data_summary$V171B <- ifelse(
  dhs_data_summary$V171B == 0,
  0,
  1
)

dhs_data_summary$V701 <- ifelse(
  dhs_data_summary$V701 == 8,
  NA,
  ifelse(
    dhs_data_summary$V701 == 0,
    0,
    1
  )
)

dhs_data_summary$V190 <- ifelse(
  dhs_data_summary$V190 < 3,
  0,
  1
)
#dhs_data_summary$HV216 <- (dhs_data_summary$HV216 > 2)*1
dhs_data_summary$V151 <- (dhs_data_summary$V151 == 2)*1
dhs_data_summary$housing_density <- dhs_data_summary$HV009 / dhs_data_summary$HV216
dhs_data_summary$HV216 <- ifelse(
  is.na(dhs_data_summary$HV009) |
    is.na(dhs_data_summary$HV216) |
    dhs_data_summary$HV216 <= 0,
  NA,
  ifelse(
    dhs_data_summary$HV009 /
      dhs_data_summary$HV216 >= 3,
    1,
    0
  )
)

mean(is.na(dhs_data_summary$HW70))
mean(is.na(dhs_data_summary$V106))
mean(is.na(dhs_data_summary$V190))
mean(is.na(dhs_data_summary$HW57))
mean(is.na(dhs_data_summary$V005))
mean(is.na(dhs_data_summary$V701))
write.csv(dhs_data_summary, "Data_collection.csv")

