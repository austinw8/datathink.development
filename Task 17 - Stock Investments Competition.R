library(tidyverse)
library(tidyquant)

tq_get_options()
prices <- tq_get(c("T", "VZ"), get = "stock.prices", from = "2023-10-01", to = "2024-07-09")
glimpse(prices)

prices |> 
  group_by(symbol) |> 
  summarise(max = max(adjusted),
            min = min(adjusted),
            mean = mean(adjusted))

ggplot(prices, aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  theme_bw() +
  labs(
    title = "AT&T vs. Verizon Adjusted Values",
    subtitle = "October 2023 - July 2024",
    x = "Date",
    y = "Adjusted"
  )

prices <- prices |> 
  mutate(movag = SMA(adjusted, n = 10))

ggplot(prices, aes(x = date, y = movag, color = symbol)) +
  geom_line(size = 1) +
  geom_smooth(se = FALSE) +
  theme_bw()

sp_500 <- tq_index("SP500") |> 
  tq_get(get = "stock.prices", from = "2023-10-01", to = "2024-07-09")

#Task 17 ---------------------------------------------------------

stocks <- tq_get(c("AMZN", "MSFT", "NFLX", "VZ", "UBER", "HSY"), get = "stock.prices", from = "2023-10-01", to = "2024-07-09") |> 
  mutate(movag = SMA(adjusted, n = 10),
         portfolio = case_when(
           symbol %in% c("AMZN", "MSFT", "NFLX") ~ "My Stocks",
           symbol %in% c("VZ", "UBER", "HSY") ~ "Friend Stocks",
         ))
         
stocks |> 
  group_by(symbol) |> 
  summarise(max = max(adjusted),
            min = min(adjusted),
            mean = mean(adjusted))

daily_returns <- stocks |> 
  group_by(symbol, portfolio, symbol) |> 
  tq_transmute(select = adjusted, mutate_fun = periodReturn, period = "daily", col_rename = "daily_return")

cum_returns <- daily_returns |> 
  group_by(portfolio, date, symbol) |> 
  summarise(cum_return = prod(1 + daily_return) -1, .groups = "drop")

ggplot(cum_returns, aes(x = date, y = cum_return, color = portfolio)) +
  geom_line(size = 1, alpha = 0.75) +
  labs(
    title = "Portfolio Performance Comparison", 
    x = "",
    y = "Cumulative Return"
  )

ggplot(stocks, aes(x = date, y = adjusted, color = portfolio)) +
  #geom_line(size = 1) +
  geom_smooth(se = FALSE) +
  theme_bw()

ggplot(stocks, aes(x = date, y = adjusted, color = portfolio)) +
  geom_path(size = 1) +
  theme_bw()


ggplot(stocks, aes(x = date, y = adjusted, color = symbol)) +
  geom_line(size = 1) +
  geom_smooth(se = FALSE) +
  theme_bw()
  

ggplot(cum_returns %>% filter(portfolio == "My Stocks"), aes(x = date, y = cum_return, color = symbol)) +
  geom_line(size = 1) +
  labs(title = "Cumulative Returns of My Stocks",
       x = "Date",
       y = "Cumulative Return",
       color = "Stock Symbol") +
  theme_minimal()

ggplot(cum_returns %>% filter(portfolio == "My Stocks"), aes(x = date, y = cum_return, color = symbol)) +
  geom_smooth(size = 1, se = FALSE) +
  labs(title = "Cumulative Returns of My Stocks",
       x = "Date",
       y = "Cumulative Return",
       color = "Stock Symbol") +
  theme_minimal()
