### Clean ############################################################

# This file cleans various datasets

##########################################################################################

### 1. Universal credit claims
# load raw Universal credit claims counts (Source = https://stat-xplore.dwp.gov.uk/, accessed 27 March 2020)

uc_raw <- read_csv(paste(rawdatdir,"/uc_daily.csv", sep = ""), skip = 9)

### 1.1 Daily

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

# work out what proportion of claims in a 7-day period would typically fall on the Monday and Tuesday
uc_calcs <- uc
uc_calcs <- uc_calcs %>% filter(year >= 2016)

# now keep from first Monday
uc_calcs <- uc_calcs[4:dim(uc_calcs)[1],1:dim(uc_calcs)[2]]
nums <- c(0,seq(length = (dim(uc_calcs)[1]-1)))
nums <- floor(nums/7)
uc_calcs$nums <- nums
uc_calcs <- uc_calcs %>% group_by(nums) %>%
  mutate(week_end = max(date))
uc_calcs$montue = 0
uc_calcs$montue[which(uc_calcs$day == "Mon" | uc_calcs$day == "Tue")] = 1
uc_calcs <- uc_calcs %>% group_by(montue) %>%
  summarise(uc_claims = sum(uc_claims)) %>%
  mutate(uc_claims_pct = uc_claims/sum(uc_claims))
uc_calcs

# this looks like around 40% of weekly claims are on monday / tuesday
# so in a 9 day period starting monday, 2/7 claims are made on one monday / tuesday.
# so in general, a 7 day period starting on a wednesday is 5/7 of the 9 day period two days earlier.

### 1.2 Weekly

# Construct weekly data
# get this on a Wed-Tuesday basis
# First keep anything after 2016
uc <- uc %>% filter(date != "2020-03-24")
uc_weekly <- uc %>% filter(year >= 2016)

# now keep from first Wednesday
uc_weekly <- uc_weekly[6:dim(uc_weekly)[1],1:dim(uc_weekly)[2]]

# need to get previous year claims on similar dates.
# minimum estimate will assume that 16th and 17th had same rates as equivalent mon / tues from previous year
prevyearmon <- uc_weekly$uc_claims[which(uc_weekly$date == "2019-03-18")]
prevyeartue <- uc_weekly$uc_claims[which(uc_weekly$date == "2019-03-19")]

# assign week ID
nums <- c(0,seq(length = (dim(uc_weekly)[1]-1)))
nums <- floor(nums/7)
uc_weekly$nums <- nums
uc_weekly <- uc_weekly %>% group_by(nums) %>%
  mutate(week_end = max(date))
uc_weekly <- uc_weekly %>% group_by(nums) %>%
  summarise(uc_claims = sum(uc_claims),
            week_end = max(week_end)) %>% 
  select(-nums)

# find range of potential weekly claims based on statement of 477000 over 9 day period
min_claims <- 477000*(5/7) # this uses previous calculations to take off what we think the first Mon / Tues would have been, if spread is as in previous periods.
max_claims <- 477000 - prevyearmon - prevyeartue # this assumes the first 2 days are equal to previous year equivalent dates

# add in indicator for real / estimated data
uc_weekly <- uc_weekly %>% mutate(type = "real")

# add in data point for week ending Tuesday 24th
uc_weekly <- add_row(uc_weekly, week_end = "2020-03-24", uc_claims = min_claims, type = "min")
uc_weekly <- add_row(uc_weekly, week_end = "2020-03-24", uc_claims = max_claims, type = "max")

saveRDS(uc_weekly, paste(datdir,"/uc_weekly_clean.Rds", sep = ""))

### 1.3 Fortnightly


# First keep anything after 2016
uc_2weekly <- uc %>% filter(date != "2020-03-24", year >= 2016, year < 2020)

# assign fortnight ID
uc_2weekly <- uc_2weekly %>% mutate(month_section = if_else(day(date) <= 15,1,2)) %>%
  mutate(month = month(date)) %>%
  group_by(year,month, month_section) %>%
  summarise(date = max(date),
            uc_claims = sum(uc_claims)) %>%
  select(date, uc_claims) %>%
  ungroup()

# add 950,000 data
uc_2weekly <- add_row(uc_2weekly, date = "2020-03-31", uc_claims = 950000)

saveRDS(uc_2weekly, paste(datdir,"/uc_2weekly_clean.Rds", sep = ""))


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

### 4. News mentions

### 4.1 corona

news_raw <- read_csv(paste(rawdatdir,"/newsbank/31-Mar-2020 (11_03).csv", sep = ""))

news_clean <- news_raw %>% spread(Search, Hits) %>%
  mutate(date = mdy(Date),
         covid_pct = 100*`Coronavirus/Corona/Covid`/All) %>%
  select(-Date, - `Coronavirus/Corona/Covid`)

### 4.2 Brexit

news_brexit_raw <- read_csv(paste(rawdatdir,"/newsbank/01-Apr-2020 (08_17).csv", sep = ""))

news_brexit_clean <- news_brexit_raw %>% spread(Search, Hits) %>%
  mutate(date = mdy(Date))

# combine with previous searches
news_clean <- left_join(news_clean, news_brexit_clean)
news_clean <- news_clean %>% mutate(brexit_pct = 100*`Brexit`/All) %>%
  select(-All, - Brexit, -Date)

# Export
saveRDS(news_clean, paste(datdir,"/news_clean.Rds", sep = ""))


,
         bre_pct = 100*`Coronavirus/Corona/Covid`/All) %>%
  select(-Date, -All, - `Coronavirus/Corona/Covid`)

# Export
saveRDS(news_clean, paste(datdir,"/news_clean.Rds", sep = ""))

