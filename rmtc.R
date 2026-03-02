setwd("C:/Users/Emma/MTF")
topcauseloss <- read.csv("C:/Users/Emma/MTF/top3causes.csv")
droughtdata <- topcauseloss[topcauseloss$Cause.of.Loss == "Drought", ]
avgpayout <- mean(droughtdata$Avg.amout.paid.out.per.policy)



# Create a simple linear regression model
freqmodel <- lm( X..of.Policies.Paid.Out ~ Cause.1+Cause.2+Cause.3, data = topcauseloss)

summary(freqmodel)

ggplot(topcauseloss, aes(x = Year, y = Frequency, color = Cause)) + geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Frequency of Loss Claims by Cause",
       x = "Year",
       y = "Frequency")

/*fit linear regression model for each of the top three causes*/
claimbymonth <- read.csv("C:/Users/Emma/MTF/newclaimdata.csv")
names(claimbymonth)
modeldrought <- lm(DroughtClaim ~ Time1, data = claimbymonth)
modelprice <- lm(Price.Claim ~ Time1, data = claimbymonth)
modelmois <- lm(Moisture.Claim ~ Time1, data = claimbymonth)
summary(modeldrought)
summary(modelprice)
summary(modelmois)

/*variables correlation coefficents)
claimbymonthnew <- claimbymonth[,c(2,3,4,5)]
cor(claimbymonthnew)


###############################################################################################
setwd("C:/Users/Emma/MTF")
crashdata <- read.csv("C:/Users/Emma/MTF/Somerville new.csv")

crashaddress <- read.csv("C:/Users/Emma/MTF/Somerville by address year.csv")
names(crashaddress)
beforecovid <- mean(crashaddress$year2019+crashaddress$year2020)
aftercovid <- mean(crashaddress$year2021+crashaddress$year2022 +crashaddress$year2023 + crashaddress$year2024)jnk


###################################################################################################
For Boston traffic post-COVID:

Negative Binomial (crash counts) + Time Series (traffic trends) + Loss Model (financial risk)

That combo lets you:

Predict crashes

Track changes over time

Translate risk into money
##################################################################################################
install.packages("MASS")
library(MASS)
install.packages("dplyr")
library(dplyr)

pois_model <- glm(
  totalaccident ~ 
    Light.Conditions +
    Weather.Conditions +
    Road.Surface +
    Speed.Limit +
    Police.Shift +
    Hit.and.Run.Flag +
    Work.Zone +
    crashmonth +
    crashyear,
  family = poisson(link = "log"),
  data = crashdata
)

summary(pois_model)




crash_summary <- crashdata %>%
  group_by(Crash.Location, crashyear, crashmonth) %>%
  summarise(total_crashes = sum(totalaccident, na.rm = TRUE),
            avg_speed = mean(Speed.Limit, na.rm = TRUE),
            .groups = 'drop')

nb_model <- glm.nb(total_crashes ~ avg_speed + crashyear + crashmonth, data = crash_summary)
summary(nb_model)





pm25 <- read.csv("C:/Users/Emma/MTF/pm25data.csv")
weather <- read.csv("C:/Users/Emma/MTF/weatherdataboston.csv")


class(pm25$date)
pm25$date <- as.Date(pm25$date)
weather$date <- as.Date(weather$date)
merge1 <- merge(pm25, weather, by = "date", all = TRUE)
merge1 <- na.omit(merge1)
write.csv(merge1, "C:/Users/Emma/MTF/merge1.csv", row.names = FALSE)



setwd("C:/Users/Emma/MTF")

data20182019 <- read.csv("C:/Users/Emma/MTF/20182019.csv")
data20202021 <- read.csv("C:/Users/Emma/MTF/20202021.csv")
data20222023 <- read.csv("C:/Users/Emma/MTF/20222023.csv")
data20242025 <- read.csv("C:/Users/Emma/MTF/20242025.csv")
data2026 <- read.csv("C:/Users/Emma/MTF/2026.csv")




allcrashdata <- list(
  data20182019,
  data20202021,
  data20222023,
  data20242025,
  data2026
)

allcrashdata <- do.call(rbind, allcrashdata)
write.csv(allcrashdata, "C:/Users/Emma/MTF/allcrashdata.csv", row.names = FALSE)



---------------
Bayesian hierarchal negative binomial

library(brms)
library(dplyr)
library(lubridate)
library(readr)

df <- read.csv("C:/Users/Emma/MTF/bostoncrashsproad20182026.csv")

df <- df %>%
  mutate(
    crash_date = as.Date(crash_date),
    year = year(crash_date),
    road_surface = as.factor(road_surface)
  )

# Aggregate to daily counts by intersection + road surface
daily <- df %>%
  group_by(intersection, crash_date, year, road_surface) %>%
  summarise(crash_count = n(), .groups = "drop")

train <- daily %>% filter(crash_date >= as.Date("2023-05-01") & crash_date <= as.Date("2025-12-31"))

future <- daily %>% filter(year == 2026)

fit <- brm(
  crash_count ~ 1 + year + road_surface + (1 | intersection),
  data = train,
  family = negbinomial(),
  chains = 4, cores = 4, iter = 2000
)


pred_2026 <- posterior_predict(fit, newdata = future)

future$pred_mean <- colMeans(pred_2026)
future$pred_low  <- apply(pred_2026, 2, quantile, 0.05)
future$pred_high <- apply(pred_2026, 2, quantile, 0.95)

# Rank intersections by risk (average across road_surface categories)
risk_2026 <- future %>%
  group_by(intersection) %>%
  summarise(
    mean_crashes = mean(pred_mean),
    low = mean(pred_low),
    high = mean(pred_high),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_crashes))

write_csv(
  risk_2026,
  "/Users/yiyuanemmazhou/Documents/New project/crash_risk_2026_by_intersection.csv"
)

# Show top 20
head(risk_2026, 20)



---------------------------------------------------
prediction model for crash: charts



library(tidyverse)
library(lubridate)

# ---- INPUT FILES ----
city_file <- "C:/Users/Emma/MTF/boston_daily_crash_counts_2018_2030.csv"
top10_monthly_file <- "C:/Users/Emma/MTF/top10_intersections_monthly_2018_2030.csv"

# ---- CITYWIDE YEARLY BAR CHART ----
city <- read_csv(city_file)
city <- city %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(crashes = sum(crash_count, na.rm = TRUE))

p1 <- ggplot(city, aes(x = factor(year), y = crashes)) +
  geom_col(fill = "steelblue") +
  labs(title = "Boston Crashes by Year (2018–2030)",
       x = "Year", y = "Total Crashes") +
  theme_minimal()

ggsave(
  filename = "C:/Users/Emma/MTF/boston_crashes_by_year_2018_2030.png",
  plot = p1,
  width = 8, height = 4, dpi = 300
)

# ---- TOP 10 INTERSECTIONS OVER TIME ----
top10 <- read_csv(top10_monthly_file)

top10 <- top10 %>%
  mutate(month = as.Date(month))

p2 <- ggplot(top10, aes(x = month, y = crash_count, color = intersection)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Top 10 Intersections Monthly Crashes (2018–2030)",
    x = "Month",
    y = "Monthly Crashes"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(
  "C:/Users/Emma/MTF/top10_intersections_monthly_2018_2030.png",
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

------------------------------------
traffic volume manipulation

trafficvolumes <- "C:/Users/Emma/Downloads/Miovision_Traffic_Volume_Data_(All)_20260215.csv"
trafficvolumes <- read.csv(
  "C:\Users\Emma Zhou\Downloads\Miovision_Traffic_Volume_Data_(All)_20260215.csv",
  stringsAsFactors = FALSE
)

names(trafficvolumes)






---------------------------------------------------------
Anisotropic KDE model


#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
  library(sf)
  library(zoo)
})

# =========================
# Configuration
# =========================
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2025-12-31")

crash_file <- "C:/Users/Emma Zhou/Downloads/boston_daily_crashes_by_intersection_2018_2026.csv"
weather_file <- "C:/Users/Emma Zhou/Downloads/weatherdatapollutionmodel.csv"
pm25_file <- "C:/Users/Emma Zhou/Downloads/pm25data.csv"

output_dir <- "C:/Users/Emma Zhou/Downloads/"

# Anisotropic bandwidth params
h <- 250
c_scale <- 0.03

assert_exists <- function(path) {
  if (!file.exists(path)) stop(sprintf("Missing file: %s", path))
}

max_na_run <- function(x) {
  rr <- rle(is.na(x))
  if (!any(rr$values)) return(0L)
  max(rr$lengths[rr$values])
}

interpolate_linear <- function(x) {
  if (all(is.na(x))) return(x)
  as.numeric(zoo::na.approx(x, x = seq_along(x), na.rm = FALSE, rule = 2))
}

rotation_matrix <- function(theta_rad) {
  matrix(c(cos(theta_rad), -sin(theta_rad),
           sin(theta_rad),  cos(theta_rad)),
         nrow = 2, byrow = TRUE)
}

sigma_from_wind <- function(v, theta_rad, h, c_scale) {
  stretch <- 1 + c_scale * v
  stretch <- max(stretch, 1e-6)
  lambda1 <- h^2 * stretch
  lambda2 <- h^2 / stretch
  Lambda <- matrix(c(lambda1, 0, 0, lambda2), nrow = 2, byrow = TRUE)
  R <- rotation_matrix(theta_rad)
  Sigma <- R %*% Lambda %*% t(R)
  list(Sigma = Sigma, lambda1 = lambda1, lambda2 = lambda2)
}

kernel_density_targets <- function(points_xy, targets_xy, Sigma) {
  n <- nrow(points_xy)
  if (is.null(n) || n == 0) return(rep(0, nrow(targets_xy)))
  invS <- solve(Sigma)
  detS <- det(Sigma)
  norm_const <- 1 / (2 * pi * sqrt(detS))
  apply(targets_xy, 1, function(x0) {
    diffs <- sweep(points_xy, 2, x0, FUN = "-")
    q <- rowSums((diffs %*% invS) * diffs)
    mean(norm_const * exp(-0.5 * q))
  })
}

year_from_date <- function(x) lubridate::year(as.Date(x))



# filter the data
crash_raw <- read_csv(crash_file, show_col_types = FALSE) %>%
  transmute(
    date = as.Date(`Crash.Date`),
    intersection = `Near.Intersection.Roadway`,
    crash_count = as.numeric(crash_count)
  ) %>%
  filter(
    !is.na(date),
    date >= start_date,
    date <= end_date,
    !is.na(intersection),
    !is.na(crash_count)
  )


weather_raw <- read_csv(weather_file, show_col_types = FALSE) %>%
  transmute(
    date = as.Date(date),
    wspd = as.numeric(wspd),
    wdir = as.numeric(wdir),
    tavg = as.numeric(tavg),
    rhum = as.numeric(rhum)
  ) %>%
  filter(!is.na(date), date >= start_date, date <= end_date)


pm25_raw <- read_csv(pm25_file, show_col_types = FALSE) %>%
  transmute(
    date = as.Date(date),
    pm25 = as.numeric(pm25)
  ) %>%
  filter(!is.na(date), date >= start_date, date <= end_date)



# weather imputation

calendar <- tibble(date = seq.Date(start_date, end_date, by = "day"))

weather_daily <- calendar %>%
  left_join(weather_raw, by = "date") %>%
  arrange(date) %>%
  mutate(
    wspd_orig = wspd,
    wdir_orig = wdir,
    wspd = na.approx(wspd, na.rm = FALSE),
    wdir = na.approx(wdir, na.rm = FALSE),
    wspd_imputed = is.na(wspd_orig) & !is.na(wspd),
    wdir_imputed = is.na(wdir_orig) & !is.na(wdir)
  )

imputation_summary <- weather_daily %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    days = n(),
    wspd_missing = sum(is.na(wspd_orig)),
    wdir_missing = sum(is.na(wdir_orig)),
    wspd_imputed = sum(wspd_imputed),
    wdir_imputed = sum(wdir_imputed),
    .groups = "drop"
  )

weather_daily <- weather_daily %>%
  mutate(
    wdir_to_deg = (wdir + 180) %% 360,
    theta_rad = wdir_to_deg * pi / 180
  )

city_daily <- crash_raw %>%
  group_by(date) %>%
  summarise(
    total_crashes = sum(crash_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(pm25_raw, by = "date") %>%
  left_join(weather_daily, by = "date")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(
  weather_daily,
  file.path(output_dir, "weather_imputed_daily.csv")
)

write_csv(
  imputation_summary,
  file.path(output_dir, "wind_imputation_summary.csv")
)

write_csv(
  city_daily,
  file.path(output_dir, "daily_crashes_weather_pm25.csv")
)

message("Done.")