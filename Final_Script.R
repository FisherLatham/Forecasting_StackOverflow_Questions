# Load necessary packages
library(fpp3)
library(zoo)

# Read in R and Python StackOverflow data from project
py_df <- read.csv("py_so_traffic.csv")
r_df <- read.csv("r_so_traffic.csv")

# Merge data together based on year-month-day
df <- merge(py_df, r_df, by = c("year", "month", "day"), all = T, )

# Rename columns
df <- df %>%
  rename(
    "Python" = "question_count.x",
    "R" = "question_count.y"
  )

# Create new date
df$Date <- paste(df$year,df$month,df$day, sep="-")
df$Date <- as.Date(df$Date, format="%Y-%m-%d")

# Delete incomplete year 2008
df <- df[-which(df$year==2008), ]

# Fill all NAN with 0 (doesn't effect total)
df[is.na(df)] <- 0 

# Create total column
df$Total <- df$Python + df$R

# Create a year-month column
df$year_month <- floor_date(df$Date, "month")

# Group by year month, return total, python, r
df <- df %>% 
  group_by(year_month) %>% 
  summarize(
    Python = sum(Python),
    R = sum(R),
    Total = sum(Total)
  )

# Convert to tsible
df$year_month <- yearmonth(df$year_month)
df <- df %>%
  as_tsibble(index = year_month)

# Straight line through to make regular data
df <- df %>% 
  update_tsibble(regular = TRUE) %>% 
  fill_gaps()
df <- df %>% 
  mutate(Total = na.approx(Total))

# Define lambda for Box Cox
lambda <- df %>%
  features(Total, features = guerrero) %>%
  pull()

# Box Cox Total
df$Tot_bc <- box_cox(df$Total, lambda)

# Train and Split for results
set.seed(475);
holdout <- tail(df,24)
train <- df[!(df$year_month %in% holdout$year_month),]

# Cross validation
train_cv <- train %>%
  stretch_tsibble(.init = 36, .step = 12)

# Save train as tsibble
train_cv <- as_tsibble(train_cv)

# Cv of best models
fit <- train_cv %>%
  model(ets_auto = ETS(box_cox(Total, lambda) ~ error("A") + trend("Ad") + season("A")),
        ets_manual = ETS(box_cox(Total, lambda) ~ error("A") + trend("A") + season("A")),
        arima_auto = ARIMA(box_cox(Total, lambda) ~ pdq(0,1,1) + PDQ(0,1,1)),
        arima_manual = ARIMA(box_cox(Total, lambda) ~ pdq(1,1,0) + PDQ(1,1,0))
  ) 

# Check RMSE of best models with cv
fit %>%
  forecast(h=12) %>%
  accuracy(train)

# Define lambda for Box Cox
lambda <- df %>%
  features(Total, features = guerrero) %>%
  pull()

# Fit simple models
fit_simp <- train %>%
  model(
    mean = MEAN(Total),
    naive = NAIVE(Total),
    snaive = SNAIVE(Total),
    drift = NAIVE(Total ~ drift())
  )

# Fit tslm models
fit_tslm <- train %>% 
  model(
  tslm=TSLM(box_cox(Total, lambda)),
  tslm_trend= TSLM(box_cox(Total, lambda)~trend()),
  tslm_season= TSLM(box_cox(Total, lambda)~season()),
  tslm_both= TSLM(box_cox(Total, lambda)~trend()+season()),
  ) 
  
# Fit ets models
fit_ets <- train %>%
  model(
    ets = ETS(box_cox(Total, lambda)),
    ets2 = ETS(box_cox(Total, lambda) ~ error("A") + trend("A") + season("A")),
    ets3 = ETS(box_cox(Total, lambda) ~ error("A") + trend("M") + season("A"))
  )

# Fit arima models
fit_arima <- train %>%
  model(
    auto_arima = ARIMA(box_cox(Total, lambda)),
    my_arima = ARIMA(box_cox(Total, lambda) ~ pdq(1,1,0) + PDQ(1,1,0))
  )

# Check params of auto arima
fit_arima %>%
  select(auto_arima)

# Check accuracy  
fit_simp %>%
  forecast(h=24) %>%
  accuracy(df) 

# Individual plots for ppt
fit_arima %>%
  select(my_arima) %>%
  forecast(h=24) %>%
  autoplot(df) + theme_classic()

# Top 4 models based on RMSE and intuition
final_fit<- df %>%
  model(ets_auto = ETS(box_cox(Total, lambda) ~ error("A") + trend("Ad") + season("A")),
        ets_manual = ETS(box_cox(Total, lambda) ~ error("A") + trend("A") + season("A")),
        arima_auto = ARIMA(box_cox(Total, lambda) ~ pdq(0,1,1) + PDQ(0,1,1)),
        arima_manual = ARIMA(box_cox(Total, lambda) ~ pdq(1,1,0) + PDQ(1,1,0)),
        )

# Based on cv, ets auto preforms best 
final_fit %>%
  select(ets_auto) %>%
  forecast(h=12) %>%
  autoplot(df, level=NULL) + theme_classic()

# Save forecast
fc <- final_fit %>%
  select(ets_auto) %>%
  forecast(h=12)

# Predictions future
preds <- fc[, c(2,4)]

# read to csv
write.csv(preds,"Group1Forecast.csv", row.names = FALSE)

