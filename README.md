# Sharpe-ratio
install.packages('DBI')
install.packages('RMySQL')
library(DBI)
library(RMySQL)

#提取上证指数
mydb= dbConnect(MySQL(),user='ktruc002', password='35442fed', dbname='cn_stock_quote', host='172.19.3.250') 
SQL_statement<- "SELECT  `trade_date`,  `index_code`, `last`
FROM `cn_stock_index`.`daily_quote`
WHERE index_code='000001' and trade_date>'2013-01-01 00:00:00'
ORDER BY `trade_date` DESC, `index_name` DESC "
dbGetQuery(mydb,SQL_statement)
Index_table <- dbGetQuery(mydb,SQL_statement)

install.packages("data.table")
install.packages("dplyr")
install.packages("tidyquant")
install.packages("tidyverse")
install.packages("xts")
install.packages("lubridate")
install.packages("readxl")
install.packages("highcharter")
install.packages("tidyquant")
install.packages("timetk")
install.packages("tibbletime")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("scales")

library(data.table)
library(dplyr)
library(tidyquant)
library(tidyverse)
library(xts)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)

#生成时间序列表
data<-as.data.table(Index_table)
date<-as.Date(data$trade)
data[,':='(date=date)]
table<-data[,!1]
setcolorder(table,c('date','index_code','last'))
xts_table<-as.xts.data.table(table)
symbols <- c("last")

# 7.1 xts world
rfr <- .0003
#将日价格转换为月价格
prices_monthly <- to.monthly(xts_table,
                             indexAt = "lastof",
                             OHLC = FALSE)

#计算收益率
asset_returns_xts <-
  Return.calculate(prices_monthly,
                   method = "log") %>%
  na.omit()
head(asset_returns_xts)

#计算夏普指数
sharpe_xts <-SharpeRatio(asset_returns_xts,
                         Rf = rfr,
                         FUN = "StdDev") %>%
  `colnames<-`("sharpe_xts")
sharpe_xts <-data.table(sharpe_xts)
sharpe_xts


#7.2 tidyverse world
asset_returns_dplyr_byhand <-
  xts_table %>%
#将日价格转换为月价格,计算收益率
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  data.frame(date = index(.)) %>%
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols)

asset_returns_dplyr_byhand <-
  asset_returns_dplyr_byhand %>%
  na.omit()#去除空值

head(asset_returns_dplyr_byhand)

#计算夏普比率
sharpe_tidyverse_byhand <-
  asset_returns_dplyr_byhand %>%
  summarise(sharpe_000001_tidyverse = mean(asset_returns_dplyr_byhand$last - rfr)/
              sd(asset_returns_dplyr_byhand$last - rfr))
sharpe_tidyverse_byhand

#7.3 tidyquant world  
 xts1 <- xts_table %>%
   data.frame(date = index(.))
#计算收益率
asset_returns_tq_builtin <-
  xts_table %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type = "log") %>%
  spread(asset, monthly.returns) %>%
  select(date, symbols) %>%
  slice(-1)
head(asset_returns_tq_builtin)

sharpe_tq <-
  asset_returns_tq_builtin  %>%
  tq_performance(Ra = last ,
                 performance_fun = SharpeRatio,
                 Rf = rfr,
                 FUN = "StdDev") %>%
  'colnames<-'("sharpe_000001_tq")
sharpe_tq

#比较三种方法计算的夏普指数
sharpe_tq %>%
  mutate(tidy_sharpe = sharpe_tidyverse_byhand$sharpe_000001_tidyverse,xts_sharpe = sharpe_xts$sharpe_xts)

#比较同一时间s&p500的夏普比率
market_returns_xts<-
  getSymbols("SPY",
             SRC='yahoo',
             from="2012-12-31",
             to="2017-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>% #提取数据
  map(~Ad(get(.))) %>% #隔离调整后的价格
  reduce(merge)  %>% #将列表合并为一个对象，使用索引去对齐数据
  'colnames<-'("SPY") %>% #给列命名
  to.monthly(indexAt = "lastof",#是否索引到月的最后一天
             OHLC=FALSE)
head(market_returns_xts)

market_sharpe <-
  market_returns_xts %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
   mutate(returns = 
            (log(SPY)-log(lag(SPY))))%>%
   na.omit() %>%
   summarise(ratio =
               mean(returns - rfr)/sd(returns - rfr))
market_sharpe$ratio

#7.4夏普指数可视化
#添加两列收益率，分别是高于无风险报酬率和低于无风险报酬率的
sharpe_byhand_with_return_columns <-
  asset_returns_dplyr_byhand %>%
  mutate(ratio = 
           mean(asset_returns_dplyr_byhand$last - rfr)/sd(asset_returns_dplyr_byhand$last - rfr))%>%
  mutate(returns_below_rfr = 
           if_else(asset_returns_dplyr_byhand$last < rfr, asset_returns_dplyr_byhand$last, as.numeric(NA))) %>%
  mutate(returns_above_rfr = 
           if_else(asset_returns_dplyr_byhand$last > rfr, asset_returns_dplyr_byhand$last, as.numeric(NA)))%>%
  mutate_if(is.numeric,funs(round(.,4)))

sharpe_byhand_with_return_columns %>%
  head(3)

#7.4.1 散点图
#蓝线代表2016年11月30日的垂直线，紫线表示无风险收益率
#绿点表示高于无风险收益率的上证指数月收益率，红点表示低于无风险收益率的上证指数月收益率
sharpe_byhand_with_return_columns %>%
  ggplot(aes(x = date))+
  geom_point(aes(y = returns_below_rfr),
             colour = "red")+
  geom_point(aes(y = returns_above_rfr),
             colour = "green")+
  geom_vline(xintercept = 
               as.numeric(as.Date("2016-11-30")),
             color = "blue")+
  geom_hline(yintercept = rfr,
             color = "purple",
             linetype = "dotted")+
  annotate(geom = "text",
           x = as.Date("2016-11-30"),
           y = -.04,
           label = "Election",
           fontface = "plain",
           angle = 90,
           alpha = .5,
           vjust = 1.5)+
           ylab("percent monthly returns")+
          scale_y_continuous(breaks = pretty_breaks(n=10))+
          scale_x_date(breaks = pretty_breaks(n=8))

#7.4.2柱状图
sharpe_byhand_with_return_columns %>%
  ggplot(aes(x=asset_returns_dplyr_byhand$last))+
  geom_histogram(alpha =0.45,
                 binwidth = .01,
                 fill = "cornflowerblue")+
  geom_vline(xintercept = rfr,
             color = "green")+
  annotate(geom = "text",
           x = rfr,
           y = 13,
           label = "rfr",
           fontface = "plain",
           angle = 90,
           alpha = .5,
           vjust = 1)

#夏普比率VS标准差
asset_returns_long <-
  asset_returns_dplyr_byhand %>%
  gather(asset, returns, -date) %>%
  group_by(asset)
head(asset_returns_long, 3)

#生成xts world格式的月收益率标准差
asset_sd_xts_builtin <-
  StdDev(asset_returns_xts)

asset_sd_xts_builtin_percent <-
  round(asset_sd_xts_builtin * 100, 2)
asset_sd_xts_builtin_percent[1,1]

stand_dev<-sd(asset_returns_dplyr_byhand$last)
sharpe<-mean(asset_returns_dplyr_byhand$last - rfr)/
           sd(asset_returns_dplyr_byhand$last - rfr)
df<-data.frame(stand_dev,sharp)

ggplot(df)+aes(x = stand_dev,
             y = sharpe,
             color = symbols)+
  geom_point(size = 2) +
  geom_text(
    aes(x =
          sd(asset_returns_dplyr_byhand$last),
        y =
          sharpe_tq$sharpe_000001_tq + .02,
        label = "SZZS_000001 ")) +
  ylab("Sharpe Ratio") +
  xlab("standard deviation") +
  ggtitle("Sharpe Ratio versus Standard Deviation") +
  theme_update(plot.title = element_text(hjust = 0.5))

##7.5 xts world格式的滚动收益率
window <- 24

rolling_sharpe_xts <- 
  rollapply(asset_returns_xts,
            window,
            function(x)
              SharpeRatio(x,
                          Rf = rfr,
                          FUN = "StdDev")) %>%
  na.omit() %>%
  `colnames<-`("xts")
head(rolling_sharpe_xts)

##7.6 tidyverse and tibbletime格式的滚动收益率
# 创建滚动函数
sharpe_roll_24 <-
  rollify(function(returns) {
    ratio = mean(returns - rfr)/sd(returns - rfr)
  },
  window = window)

rolling_sharpe_tidy_tibbletime <-
  asset_returns_dplyr_byhand %>%
  as_tbl_time(index = date) %>%
  mutate(tbltime_sharpe = sharpe_roll_24(last)) %>%
  na.omit() %>%
  select(-last)
head(rolling_sharpe_tidy_tibbletime)

##7.7tidyquant格式的滚动收益率
sharpe_tq_roll <- function(df){
  SharpeRatio(df,
              Rf = rfr,
              FUN = "StdDev")
}

rolling_sharpe_tq <-
  asset_returns_tq_builtin %>%
  tq_mutate(
    select = last,
    mutate_fun = rollapply,
    width = window,
    align = "right",
    FUN = sharpe_tq_roll,
    col_rename = "tq_sharpe"
  ) %>%
  na.omit()
head(rolling_sharpe_tq)

#比较三种方法计算的滚动夏普指数
rolling_sharpe_tidy_tibbletime %>%
  mutate(xts_sharpe = coredata(rolling_sharpe_xts),
         tq_sharpe = rolling_sharpe_tq$tq_sharpe ) %>%
  head(3)

##7.8 可视化滚动的夏普比率
highchart(type = "stock") %>%
  hc_title(text = "Rolling 24-Month Sharpe") %>%
  hc_add_series(rolling_sharpe_xts,
                name = "sharpe",
                color = "blue") %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)

rolling_sharpe_xts %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  rename(rolling_sharpe = xts) %>%
  ggplot(aes(x = date,
             y = rolling_sharpe)) +
  
  geom_line(color = "cornflowerblue") +
  ggtitle("Rolling 24-Month Sharpe Ratio") +
  labs(y = "rolling sharpe ratio") +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  theme(plot.title = element_text(hjust = 0.5))
