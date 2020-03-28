### Analysis ############################################################

# This file produces analysis

##########################################################################################

### 1. Universal credit claims

# Load cleaned data
uc <- readRDS(paste(datdir,"/uc_daily_clean.Rds", sep = ""))

# Check distribution of claims

ggplot(data = uc, mapping = aes(x = uc_claims)) +
  geom_histogram()

# Make time series plot

ggplot(data = uc %>% filter(year >= 2016, day == "Tue"), mapping = aes(x = date, y = uc_claims)) + 
  geom_col(width = 7, fill = "darkorange2", alpha = 0.9) +
  theme_light() + xlab("") + ylab("Universal Credit claims (daily)") +
  theme(axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        plot.caption = element_text(hjust = 0, size = 10)) +
  annotate(geom = "text", x = dmy("01/10/2019"), y = 102000, label = c("105,000 claims"),
           color = "darkorange2", size = 5, fontface = "bold") +
  annotate(geom = "text", x = dmy("01/10/2019"), y = 97000, label = c("Tuesday 24th March"),
           color = "darkorange2", size = 5) + 
  labs(caption = "Notes: Daily Universal Credit claims. Data refers to claims made on Tuesdays to allow comparison with most recent data point.
Sources: DWP website (accessed 27/3/20), statement of Peter Schofield to House of Commons work and pensions select committee 25/3/20.
Jack Blundell (Stanford / CEP(LSE))")
ggsave("uc_claims.png", width = 12, height = 6.8)

# find out maximum daily claims

uc %>% arrange(-uc_claims)

# now create same for weekly, including uncertainty

uc_weekly <- readRDS(paste(datdir,"/uc_weekly_clean.Rds", sep = ""))

# Check distribution of claims

ggplot(data = uc_weekly, mapping = aes(x = uc_claims)) +
  geom_histogram()

# Make time series plot

ggplot() + 
  geom_col(data = uc_weekly %>% filter(type == "max"), mapping = aes(x = week_end, y = uc_claims), 
           width = 7, fill = "darkorange1", alpha = 0.5) +
  geom_col(data = uc_weekly %>% filter(type != "max"), mapping = aes(x = week_end, y = uc_claims), 
           width = 7, fill = "darkorange2", alpha = 0.9) +
  theme_light() + xlab("") + ylab("Universal Credit claims (weekly)") +
  theme(axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10, angle = 45),
        plot.caption = element_text(hjust = 0, size = 10)) +
  scale_y_continuous(labels = comma) +
  annotate(geom = "text", x = dmy("01/06/2019"), y = 410000, label = c("Between 341,000 and 453,000 claims"),
           color = "darkorange2", size = 5, fontface = "bold") +
  annotate(geom = "text", x = dmy("01/06/2019"), y = 390000, label = c("Week ending Tuesday 24th March"),
           color = "darkorange2", size = 5) + 
  labs(caption = "Notes: Weekly Universal Credit claims. Estimate for week ending Tuesday 24th March based on 9-day report of 477,000, which included counts from Monday 16th and Tuesday 17th.
Sources: DWP website (accessed 27/3/20), statement of Peter Schofield to House of Commons work and pensions select committee 25/3/20.
Jack Blundell (Stanford / CEP(LSE))") + 
  geom_segment(aes(x = dmy("10/03/2020"), y = 340714, xend =  dmy("10/03/2020"), yend = 452987), 
               arrow = arrow(length = unit(0.01, "npc"))) + 
  geom_segment(aes(x = dmy("10/03/2020"), y = 452987, xend =  dmy("10/03/2020"), yend = 340714), 
               arrow = arrow(length = unit(0.01, "npc")))
ggsave("uc_claims_weekly.png", width = 12, height = 6.8)

# find out maximum daily claims

uc %>% arrange(-uc_claims)

### 2. Signups for verify service

# Load cleaned data
v <- readRDS(paste(datdir,"/verify_signups_clean.Rds", sep = ""))

# Check distribution of claims

ggplot(data = v, mapping = aes(x = signups)) +
  geom_histogram()

# Make time series plot

ggplot(data = v, mapping = aes(x = week_ending, y = signups)) + 
  geom_col(width = 7, fill = "dodgerblue3", col = "dodgerblue4", alpha = 0.8) +
  theme_light() + xlab("") + ylab("New sign-ups") +
  theme(axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.caption = element_text(hjust = 0, size = 11)) +
  annotate(geom = "text", x = dmy("8/3/2020"), y = 118416, label = c("118,416 new sign-ups"),
           color = "dodgerblue3", size = 5, fontface = "bold") +
  annotate(geom = "text", x = dmy("8/3/2020"), y = 113000, label = c("Week ending 22nd March"),
           color = "dodgerblue3", size = 5) + 
  labs(caption = "Notes: Number of new signups for Gov.uk Verify.
Source: https://www.gov.uk/performance/govuk-verify (accessed 27/3/2020).
Jack Blundell (Stanford / CEP(LSE))")
ggsave("verify_signups.png", width = 12, height = 6.8)

### 3. Google trends

# Load cleaned data
gtrends <- readRDS(paste(datdir,"/googletrends_clean.Rds", sep = ""))

# Check distribution of claims

ggplot(data = gtrends, mapping = aes(x = hits)) +
  geom_histogram()

# Make time series plot

ggplot(data = gtrends %>% filter(year >= 2018), mapping = aes(x = week, y = hits)) + 
  geom_col(width = 7, fill = "coral3", col = "coral4", alpha = 0.8) +
  theme_light() + xlab("") + ylab("Google search index for -Universal Credit-") +
  theme(axis.text.x = element_text(color = "black", size = 11),
        axis.text.y = element_text(color = "black", size = 11),
        plot.caption = element_text(hjust = 0, size = 11)) + 
  labs(caption = "Notes: UK Google searches for -Universal Credit-. Maximum normalized to 100.
Source: Google (accessed 27/3/2020)
Jack Blundell (Stanford / CEP(LSE))")
ggsave("googletrends.png", width = 12, height = 6.8)