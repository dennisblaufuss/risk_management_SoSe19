#--------------------------------------------------------------------------------------------------------------------------------------------
#LOAD PACKAGES
#--------------------------------------------------------------------------------------------------------------------------------------------

library(quantmod)
library(ggplot2)
library(car)

#--------------------------------------------------------------------------------------------------------------------------------------------
#DATA
#--------------------------------------------------------------------------------------------------------------------------------------------

adj_close <- getSymbols.yahoo("AAPL", from = "2010-01-01" , to = "2019-12-31",  auto.assign = F)[,6] 
returns <- na.spline(ROC(adj_close, type = "discrete"))
ann_sd <- sqrt(252) * (lag(rollapply(returns[,1], 252, sd, align = "right")))
impl_vola <- getSymbols.yahoo("^VIX", from = "2010-01-01" , to = "2019-12-31",  auto.assign = F)[,6] * 0.01
rf <- 0.01
rf_xts <- xts(c(rep(rf,length(index(adj_close)))), order.by = index(adj_close))

#--------------------------------------------------------------------------------------------------------------------------------------------
#FUNCTIONS
#--------------------------------------------------------------------------------------------------------------------------------------------

BS <- function(s, k, r, t, sigma, type = c("call", "put"))   
{
  d1 <- (log(s / k)+(r + sigma ^ 2/2) * t) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  if(type == 'call')
  {
    BS <- s * pnorm(d1) - k * exp(-r * t) * pnorm(d2)
  }
  else
  {
    BS <- k * exp(-r * t) * pnorm(-d2) - s * pnorm(-d1)
  }
}

BS_price <- function (interval, type = c("call", "put"), estimation = c("historical", "implied")) 
{
  idx <- index(adj_close[interval])
  time <- xts(seq(from = length(idx), to = 1) / (length(idx) / maturity), order.by = idx)
  strike <- xts(rep(first(adj_close[interval]), length(idx)), order.by = idx)
  if(estimation == 'historical')
  {
    BS_price <- BS(adj_close[interval], strike, rf, time, ann_sd[interval], type)
  }
  else
  {
    BS_price <- BS(adj_close[interval], strike, rf, time, impl_vola[interval], type)
  }
}

parity_test <- function (interval, estimation = c("historical", "implied")) 
{
  idx <- index(adj_close[interval])
  time <- xts(seq(from = length(idx), to = 1)/(length(idx) / maturity), order.by = idx)
  strike <- xts(rep(first(adj_close[interval]), length(idx)), order.by = idx)
  
  if(estimation == 'historical')
  {
    BS_put <- BS(adj_close[interval], strike, rf_xts, time, ann_sd[interval], 'put')
    BS_call <- BS(adj_close[interval], strike, rf_xts, time, ann_sd[interval], 'call')
  }
  else
  {
    BS_put <- BS(adj_close[interval], strike, rf_xts, time, impl_vola[interval], 'put')
    BS_call <- BS(adj_close[interval], strike, rf_xts, time, impl_vola[interval], 'call')
  }
  paritiy_test <- (BS_call + strike / (1 + rf) ^ time) - (BS_put + adj_close[interval])
}

Vega <- function(interval, estimation = c("historical", "implied"))
{
  if(estimation == 'historical')
  {
    sigma <- ann_sd[interval]
  }
  else
  {
    sigma <- impl_vola[interval]
  }
  s <- adj_close[interval]
  idx <- index(adj_close[interval])
  t <- xts(seq(from = length(idx), to = 1)/(length(idx) / maturity), order.by = idx)
  k <- xts(rep(first(adj_close[interval]), length(idx)), order.by = idx)
  d1 <- (log(s / k)+(rf_xts + sigma ^ 2/2) * t) / (sigma * sqrt(t))
  NP <- ( exp(-d1 ^ 2/2) / sqrt(2 * pi))
  Vega <- s * sqrt(t) * NP
}

#--------------------------------------------------------------------------------------------------------------------------------------------
#ANALYSIS
#--------------------------------------------------------------------------------------------------------------------------------------------

maturity <- 365/365
timespan <- "2019-01-01/2019-12-31"
#timespan <- "2019-01-01/2019-12-31"
call <- BS_price(timespan, 'call', 'historical')
names(call) <- "BS Price"
put <- BS_price(timespan, 'put', 'historical')
names(put) <- "BS Price"
call_impl <- BS_price(timespan, 'call', 'implied')
names(call_impl) <- "BS Price"
put_impl <- BS_price(timespan, 'put', 'implied')
names(put_impl) <- "BS Price"
vega <- Vega(timespan, 'historical')
names(vega) <- "Vega"
vega_impl <- Vega(timespan, 'implied')
names(vega_impl) <- "Vega"

parity_hist <- parity_test(timespan, 'historical')
parity_impl <- parity_test(timespan, 'implied')

#--------------------------------------------------------------------------------------------------------------------------------------------
#PLOTS
#--------------------------------------------------------------------------------------------------------------------------------------------

df_vola <-data.frame(time = index(ann_sd[timespan]), val1 = coredata(ann_sd[timespan]), val2 = coredata(impl_vola[timespan]))
names(df_vola)[names(df_vola) == "AAPL.Adjusted"] <- "Historical"
names(df_vola)[names(df_vola) == "VIX.Adjusted"] <- "Implied"
plot_vola<- ggplot(df_vola, aes(time)) +
  labs(x = "Date", y = "Annualized Volatility") +
  theme(panel.background = element_rect(fill = "#F7F6F0"), text = element_text(color = "#423C30")) +
  geom_line(aes(y = Historical, color = "Historical")) +
  geom_line(aes(y = Implied, color = "Implied")) +
  scale_colour_manual("", breaks = c("Historical", "Implied"), values = c("red", "blue")) +
  ylim(0, 0.35)
plot_vola

df_put <-data.frame(time = index(put), val1 = coredata(put), val2 = coredata(put_impl))
names(df_put)[names(df_put) == "BS.Price"] <- "Historical"
names(df_put)[names(df_put) == "BS.Price.1"] <- "Implied"
plot_put<- ggplot(df_put, aes(time)) +
  labs(x = "Date", y = "Black-Scholes Price") +
  theme(panel.background = element_rect(fill = "#F7F6F0"), text = element_text(color = "#423C30")) +
  geom_line(aes(y = Historical, color = "Historical")) +
  geom_line(aes(y = Implied, color = "Implied")) +
  scale_colour_manual("", breaks = c("Historical", "Implied"), values = c("red", "blue"))
plot_put

df_call <-data.frame(time = index(call), val1 = coredata(call), val2 = coredata(call_impl))
names(df_call)[names(df_call) == "BS.Price"] <- "Historical"
names(df_call)[names(df_call) == "BS.Price.1"] <- "Implied"
plot_call<- ggplot(df_call, aes(time)) +
  labs(x = "Date", y = "Black-Scholes Price") +
  theme(panel.background = element_rect(fill = "#F7F6F0"), text = element_text(color = "#423C30")) +
  geom_line(aes(y = Historical, color = "Historical")) +
  geom_line(aes(y = Implied, color = "Implied")) +
  scale_colour_manual("", breaks = c("Historical", "Implied"), values = c("red", "blue"))
plot_call

df_vega <-data.frame(time = index(vega), val1 = coredata(vega), val2 = coredata(vega_impl))
names(df_vega)[names(df_vega) == "Vega"] <- "Historical"
names(df_vega)[names(df_vega) == "Vega.1"] <- "Implied"
plot_vega <- ggplot(df_vega, aes(time)) +
  labs(x = "Date", y = "Vega") +
  theme(panel.background = element_rect(fill = "#F7F6F0"), text = element_text(color = "#423C30")) +
  geom_line(aes(y = Historical, color = "Historical")) +
  geom_line(aes(y = Implied, color = "Implied")) +
  scale_colour_manual("", breaks = c("Historical", "Implied"), values = c("red", "blue"))
plot_vega

#--------------------------------------------------------------------------------------------------------------------------------------------
#OUTPUTS GPOWER
#--------------------------------------------------------------------------------------------------------------------------------------------

mean(ann_sd[timespan])
sd(ann_sd[timespan])
mean(impl_vola[timespan])
sd(impl_vola[timespan])
levene_temp <- c(coredata(ann_sd[timespan]), coredata(impl_vola[timespan]))
levene_group_temp <- as.factor(c(rep(1, length(ann_sd[timespan])), rep(2, length(impl_vola[timespan]))))
leveneTest(levene_temp, levene_group_temp, mean)

mean(put)
sd(put)
mean(put_impl)
sd(put_impl)
levene_temp <- c(coredata(put), coredata(put_impl))
levene_group_temp <- as.factor(c(rep(1, length(put)), rep(2, length(put_impl))))
leveneTest(levene_temp, levene_group_temp, mean)

mean(call)
sd(call)
mean(call_impl)
sd(call_impl)
levene_temp <- c(coredata(call), coredata(call_impl))
levene_group_temp <- as.factor(c(rep(1, length(call)), rep(2, length(call_impl))))
leveneTest(levene_temp, levene_group_temp, mean)

mean(vega)
sd(vega)
mean(vega_impl)
sd(vega_impl)
levene_temp <- c(coredata(vega), coredata(vega_impl))
levene_group_temp <- as.factor(c(rep(1, length(vega)), rep(2, length(vega_impl))))
leveneTest(levene_temp, levene_group_temp, mean)