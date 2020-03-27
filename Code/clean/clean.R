### Clean ############################################################

# This file cleans various datasets

##########################################################################################

### 1. Universal credit claims
# load raw Universal credit claims counts (Source = https://stat-xplore.dwp.gov.uk/, accessed 27 March 2020)

uc_raw <- read_csv(paste(rawdatdir,"/uc_daily.csv", sep = ""), skip = 9)

# clean
uc <- uc_raw %>% filter(`Time Period (III)` == "Total") # keep only totals
uc <- gather(uc, key = "date", value = "uc_claims") # reshape to long
for (i in c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")) {
  uc$date <- gsub(uc$date, pattern = paste(i,", ", sep = " "), replacement = "")  
}
uc <- uc %>% mutate(uc_claims = as.integer(uc_claims)) %>% # put claims into integer form
  mutate(date = mdy(date)) %>% # make sure dates correctly coded
  filter(is.na(uc_claims) == F) %>%
  filter(is.na(uc_claims) == F,
         is.na(date) == F)
  
# Add in 105,000 claims on Tuesday 24th March 2020
uc <- add_row(uc, date = "2020-03-24", uc_claims = 105000)

# add in more date info
uc <- uc %>%  mutate(day = wday(date, label = TRUE),
                     year = year(date))

# Export daily data
saveRDS(uc, paste(datdir,"/uc_daily_clean.Rds", sep = ""))

### 2. Verify signups

v_raw <- read_csv(paste(rawdatdir,"/verify_signups.csv", sep = ""))

v <- v_raw %>% mutate(week_ending = dmy(week_ending))

# Export
saveRDS(v, paste(datdir,"/verify_signups_clean.Rds", sep = ""))

### 3. Google trends

gtrends_raw <- read_csv(paste(rawdatdir,"/googletrends.csv", sep = ""), skip = 1)
gtrends <- gtrends_raw %>% mutate(week = ymd(Week)) %>%
  rename(hits = `universal credit: (United Kingdom)`) %>%
  mutate(hits = as.integer(hits)) %>%
  filter(is.na(hits) == F) %>%
  select(week, hits)

# add in more date info
gtrends <- gtrends %>%  mutate(day = wday(week, label = TRUE),
                     year = year(week))

# Export
saveRDS(gtrends, paste(datdir,"/googletrends_clean.Rds", sep = ""))