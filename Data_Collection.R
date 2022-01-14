library(tidyverse)
library(quantmod)
library(lubridate)
library(TTR)

#### Technical Stock Data ####
getSymbols("^GDAXI", from='2009-11-01',to='2021-12-31')
raw_DAX <- as_tibble(GDAXI,rownames = "date") %>% 
  set_names(c("date","open","high","low","close","volume","adjusted")) %>% 
  mutate(date = as_date(date)) %>% 
  mutate(moye = format(as.POSIXct(date,format='%Y-%m-%d'),format='%Y %m'))

#set alpha for exponential smoothing
a <- 0.3

DAX <- raw_DAX %>% na.omit %>%
  mutate( sm_adj =adjusted,
    sm_op = open,
    sm_hi = high,
    sm_cl = close,
    sm_lo = low)
  
for (i in 2:length(DAX$open)) {
  DAX$sm_op[i] <- DAX$open[i]* a + (1-a)*DAX$sm_op[i-1]
  DAX$sm_hi[i] <- DAX$high[i]* a + (1-a)*DAX$sm_hi[i-1]
  DAX$sm_cl[i] <- DAX$close[i]* a + (1-a)*DAX$sm_cl[i-1]
  DAX$sm_lo[i] <- DAX$low[i]* a + (1-a)*DAX$sm_lo[i-1]
  DAX$sm_adj[i] <- DAX$adjusted[i]* a + (1-a)*DAX$sm_adj[i-1]
}
  
DAX <- DAX %>% select(c(date,moye,open,sm_op,close,sm_cl,high, sm_hi,low,sm_lo,adjusted,sm_adj))

#### Calculing RSI ####
RSI_DAX <- DAX %>% mutate(gain = sm_adj -lag(sm_adj,1),
                      loss = (sm_adj -lag(sm_adj,1))*-1,
                      gain = ifelse(gain > 0,gain,NA),
                      loss = ifelse(loss > 0,loss,NA))

# set n for RSI (14)
n <- 14
RSI_DAX$upt <- 0
for (i in 1:length(RSI_DAX$date)) {
  ifelse(i <= 14,RSI_DAX$upt[i] <- NA,
         RSI_DAX$upt[i] <- sum(RSI_DAX$gain[(i-n+1):(i)],na.rm=TRUE)/n)
  
}

RSI_DAX$dwt <- 0
for (i in 1:length(RSI_DAX$date)) {
  ifelse(i <= 14,RSI_DAX$dwt[i] <- NA,
         RSI_DAX$dwt[i] <- sum(RSI_DAX$loss[(i-n+1):(i)],na.rm=TRUE)/n)
  
}

RSI_DAX <- RSI_DAX %>% mutate(rsi = 100 - (100/(1+upt/dwt)))

#### Stochastic Fast K ####
# set n (14)
n <- 14
RSI_DAX$llt <- 0
for (i in 1:length(RSI_DAX$date)) {
  ifelse(i <= 14,RSI_DAX$llt[i] <- NA,
         RSI_DAX$llt[i] <- min(RSI_DAX$sm_lo[(i-n+1):(i)],na.rm=TRUE))
  
}
# set n (14)
n <- 14
RSI_DAX$hht <- 0
for (i in 1:length(RSI_DAX$date)) {
  ifelse(i <= 14,RSI_DAX$hht[i] <- NA,
         RSI_DAX$hht[i] <- max(RSI_DAX$sm_hi[(i-n+1):(i)],na.rm=TRUE))
  
}
RSI_DAX <- RSI_DAX %>% mutate(sfk = 100*(sm_cl-llt)/(hht-llt))

#### Stochastic Fast D ####
# set n (3)
n <- 3
RSI_DAX$sfd <- 0
for (i in 1:length(RSI_DAX$date)) {
  ifelse(i <= 16,RSI_DAX$sfd[i] <- NA,
         RSI_DAX$sfd[i] <- sum(RSI_DAX$sfk[(i-n+1):(i)],na.rm=TRUE)/n)
  
}
#### Stochastic Slow D ####
n <- 3
RSI_DAX$ssd <- 0
for (i in 1:length(RSI_DAX$date)) {
  ifelse(i <= 18,RSI_DAX$ssd[i] <- NA,
         RSI_DAX$ssd[i] <- sum(RSI_DAX$sfd[(i-n+1):(i)],na.rm=TRUE)/n)
  
}

#### Larry Williams R % ####
RSI_DAX <- RSI_DAX %>% mutate(lwr = 100*(hht-sm_cl)/(hht-llt))

##### Moving Average Convergence Divergence MACD #### 
RSI_DAX <- RSI_DAX %>% mutate(ema_26 = EMA(sm_cl, n=26),
                              ema_12 = EMA(sm_cl, n=12),
                              diff = ema_12-ema_26,
                              macdt = lag(diff,1),
                              macd = macdt+ 2/10*(diff-macdt)
                              )

#### Rate of Change ROC #####
RSI_DAX <- RSI_DAX %>% mutate(roc = (sm_cl/lag(sm_cl,20))-1)

#### Commodity Channel Indec CCI ####
RSI_DAX <- RSI_DAX %>% mutate(mt = (sm_hi+sm_lo+sm_cl)/3)

n <- 20
RSI_DAX$smt <- 0
for (i in 1:length(RSI_DAX$date)) {
  ifelse(i <= 20,RSI_DAX$smt[i] <- NA,
         RSI_DAX$smt[i] <- sum(RSI_DAX$mt[(i-n+1):(i)],na.rm=TRUE)/n)
  
}

RSI_DAX$dt <- 0
for (i in 1:length(RSI_DAX$date)) {
  ifelse(i <= 20,RSI_DAX$dt[i] <- NA,
         RSI_DAX$dt[i] <- sum(abs(RSI_DAX$mt[(i-n+1):(i)]-RSI_DAX$smt[i]),na.rm=TRUE)/n)
  
}

RSI_DAX <- RSI_DAX %>% mutate(commodity_channel_index = (mt-smt)/(0.015*dt))

DAX <- RSI_DAX

#### Macroeconomic Factors ####
setwd("/Users/henryspecht/Documents/Uni/UM/Thesis/Data")
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
#smoothing for monthly observations
m <- 0.5
#1 year bond yield
yb1 <- read_csv("macro/1_year_bond.csv", name_repair = rlang::as_function(~tolower( .x ))) %>% 
  mutate(date= as_date(date, format='%b %d, %Y'),
         sm_price = price)  %>% 
  arrange(date)

## 1. Exponentially smoothing every Macro
for (i in 2:length(yb1$price)) {
  yb1$sm_price[i] <- yb1$price[i]* a + (1-a)*yb1$sm_price[i-1]
}

## 2. Growth Rate the last month: lag 1 for monthly, lag 20 for daily observations
yb1 <- yb1 %>% 
  mutate(rate = (sm_price/lag(sm_price,20)-1)) %>% 
  select(c(date,rate)) %>% set_names(c("date","yb1"))
  
DAX <- left_join(DAX,yb1)
DAX <- DAX %>% fill(yb1)
#10 year bond yield
yb10<- read_csv("macro/10_year_bond.csv", name_repair = rlang::as_function(~tolower( .x )))%>% 
  mutate(date= as_date(date, format='%b %d, %Y'),
         sm_price = price)  %>% 
  arrange(date)

for (i in 2:length(yb10$price)) {
  yb10$sm_price[i] <- yb10$price[i]* a + (1-a)*yb10$sm_price[i-1]
}

yb10 <- yb10 %>% 
  mutate(rate= ifelse(sm_price == 0, 0, (sm_price - lag(sm_price,20)))/lag(sm_price,20)) %>% 
  select(c(date,rate)) %>% set_names(c("date","yb10"))

DAX <- left_join(DAX,yb10)

#ecb interest rate
ecb <- read_csv("macro/ecb_interest.csv") %>% set_names(c("date","interest","comment")) %>% select(date,interest) %>% 
  .[-(1:7),] %>% na.omit()  %>%  
  mutate(date= ym(date), 
         interest = as.numeric(interest),
         moye = format(as.POSIXct(date,format='%Y-%m-%d'),format='%Y %m'),
         sm_interest = interest) %>% 
  arrange(date)

for (i in 2:length(ecb$interest)) {
  ecb$sm_interest[i] <- ecb$interest[i]* m + (1-m)*ecb$sm_interest[i-1]
}

ecb <- ecb %>% 
  mutate(rate = (sm_interest/lag(sm_interest,1)-1)) %>%   
  select(moye, rate) %>% set_names(c("moye","ecb_int"))

DAX <- left_join(DAX,ecb)

# 3 month libor
libor <- read_csv("macro/3_month_euribor.csv") %>% set_names(c("date","libor")) %>% 
   mutate(libor = ifelse(libor != ".", libor, lag(libor,1)),
          libor = ifelse(libor != ".", libor, lead(libor,1)),
          libor =as.numeric(libor),
          sm_libor = libor)%>% 
  arrange(date)

for (i in 2:length(libor$libor)) {
  libor$sm_libor[i] <- libor$libor[i]* a + (1-a)*libor$sm_libor[i-1]
}

libor <- libor %>% 
  mutate(rate = (sm_libor/lag(sm_libor,20)-1)) %>% 
  select(date, rate) %>% set_names(c("date","libor"))

DAX <- left_join(DAX,libor)

#Exchange rates
china <- read_csv2("macro/china_euro.csv")[-(1:7),] %>% set_names(c("date","china_euro","comment")) %>% 
  select(date, china_euro) %>% mutate(date= as_date(date),
                                     china_euro = str_replace(china_euro,",","."),
                                     china_euro = as.numeric(china_euro),
                                     sm_china_euro = china_euro) %>% na.omit()%>% 
  arrange(date)

for (i in 2:length(china$china_euro)) {
  china$sm_china_euro[i] <- china$china_euro[i]* a + (1-a)*china$sm_china_euro[i-1]
}

china <- china %>% 
  mutate(rate = (sm_china_euro/lag(sm_china_euro,20)-1)) %>%  
  select(date, rate) %>% set_names(c("date","china"))

DAX <- left_join(DAX,china)
  
#### Japan ####
japan <- read_csv2("macro/japan_euro.csv")[-(1:4),] %>% 
  set_names(c("date","japan_euro","comment")) %>% 
  select(date, japan_euro) %>% mutate(date= as_date(date),
                                      japan_euro = str_replace(japan_euro,",","."),
                                      japan_euro = as.numeric(japan_euro),
                                      sm_japan_euro = japan_euro) %>% na.omit()%>% 
  arrange(date)

for (i in 2:length(japan$japan_euro)) {
  japan$sm_japan_euro[i] <- japan$japan_euro[i]* a + (1-a)*japan$sm_japan_euro[i-1]
}

japan <- japan %>% 
  mutate(rate = (sm_japan_euro/lag(sm_japan_euro,20)-1)) %>%    
  select(date, rate) %>% set_names(c("date","japan"))

DAX <- left_join(DAX,japan)

#### British Pound ####

gbp <- read_csv2("macro/gbp_euro.csv")[-(1:4),] %>% set_names(c("date","gbp_euro","comment")) %>% 
  select(date, gbp_euro) %>% mutate(date= as_date(date),
                                    gbp_euro = str_replace(gbp_euro,",","."),
                                    gbp_euro = as.numeric(gbp_euro),
                                    sm_gbp_euro = gbp_euro) %>% na.omit()%>% 
  arrange(date)
  
for (i in 2:length(gbp$gbp_euro)) {
  gbp$sm_gbp_euro[i] <- gbp$gbp_euro[i]* a + (1-a)*gbp$sm_gbp_euro[i-1]
}

gbp <- gbp %>% 
  mutate(rate = (sm_gbp_euro/lag(sm_gbp_euro,20)-1)) %>%   
  select(date, rate) %>% set_names(c("date","gbp"))

DAX <- left_join(DAX,gbp)

#### US Dollar ####
usd <- read_csv2("macro/usd_euro.csv")[-(1:4),] %>% 
  set_names(c("date","usd_euro","comment")) %>% 
  select(date, usd_euro) %>% mutate(date= as_date(date),
                                    usd_euro = str_replace(usd_euro,",","."),
                                    usd_euro = as.numeric(usd_euro),
                                    sm_usd_euro = usd_euro) %>% na.omit() %>% 
  arrange(date)

for (i in 2:length(usd$usd_euro)) {
  usd$sm_usd_euro[i] <- usd$usd_euro[i]* a + (1-a)*usd$sm_usd_euro[i-1]
}

usd <- usd %>% 
  mutate(rate = (sm_usd_euro/lag(sm_usd_euro,20)-1)) %>%    
  select(date, rate) %>% set_names(c("date","usd"))

DAX <- left_join(DAX,usd)

### Gold price ####
gold <- read_csv("macro/gold_price.csv")[-(1:8),] %>%
  set_names(c("date","gold_price","comment")) %>% 
  select(date, gold_price)  %>% mutate(date= as_date(date),
                                       gold_price = str_replace(gold_price,",","."),
                                       gold_price = as.numeric(gold_price),
                                       sm_gold_price = gold_price) %>% na.omit() %>% 
  arrange(date)

for (i in 2:length(gold$gold_price)) {
  gold$sm_gold_price[i] <- gold$gold_price[i]* a + (1-a)*gold$sm_gold_price[i-1]
}


gold <- gold %>% 
  mutate(rate = (sm_gold_price/lag(sm_gold_price,20)-1)) %>%   
  select(date, rate) %>% set_names(c("date","gold"))

DAX <- left_join(DAX,gold)
DAX <- DAX %>% fill(gold)

### consumer price index ####
cpi <- read_csv2("macro/consumer_price_index.csv")[-(1:4),] %>% dplyr::slice(-n()) %>% 
  set_names(c("date","cpi","comment")) %>% 
  select(date, cpi) %>% 
  mutate(date= as_date(date, format = '%Y-%m-%d')) %>% 
  mutate(date = format(as.POSIXct(date,format='%Y-%m-%d'),format='%Y %m'),
         cpi = str_replace(cpi,",","."),
         cpi = as.numeric(cpi),
         sm_cpi = cpi) %>% na.omit() %>% 
  arrange(date)

for (i in 2:length(cpi$cpi)) {
  cpi$sm_cpi[i] <- cpi$cpi[i]* m + (1-m)*cpi$sm_cpi[i-1]
}

cpi <- cpi %>% 
  mutate(rate = (sm_cpi/lag(sm_cpi,1)-1)) %>%    
  select(date, rate) %>% set_names(c("moye","cpi"))

DAX <- left_join(DAX,cpi)

### producer price index ####
ppi <- read_csv("macro/producer_price_index.csv")[-(1:8),] %>%
  set_names(c("date","ppi","comment")) %>% 
  select(date, ppi) %>% 
  mutate(date= as_date(ym(date))) %>% 
  mutate(date = format(as.POSIXct(date,format='%Y-%m-%d'),format='%Y %m'),
         ppi = as.numeric(ppi),
         sm_ppi = ppi) %>% na.omit() %>% 
arrange(date)

for (i in 2:length(ppi$ppi)) {
  ppi$sm_ppi[i] <- ppi$ppi[i]* m + (1-m)*ppi$sm_ppi[i-1]
}

ppi <- ppi %>% 
  mutate(rate = (sm_ppi/lag(sm_ppi,1)-1)) %>%    
  select(date, rate) %>% set_names(c("moye","ppi"))

DAX <- left_join(DAX,ppi)

### consumer confidence index ####
cci <- read_csv("macro/consumer_confidence_index.csv", name_repair = rlang::as_function(~tolower( .x ))) %>%
  filter(location == "DEU") %>% 
  select(time,value) %>% set_names(c("date","cci")) %>% mutate(date= ym(date)) %>%
  mutate(date = format(as.POSIXct(date,format='%Y-%m-%d'),format='%Y %m')) %>% 
  mutate(sm_cci = cci)%>% 
  arrange(date)

for (i in 2:length(cci$cci)) {
  cci$sm_cci[i] <- cci$cci[i]* m + (1-m)*cci$sm_cci[i-1]
}

cci <- cci %>% 
  mutate(rate = (sm_cci/lag(sm_cci,1)-1)) %>%    
  select(date, rate) %>% set_names(c("moye","cci"))

DAX <- left_join(DAX,cci)

### business confidence index ####
bci <- read_csv("macro/business_confidence_index.csv", name_repair = rlang::as_function(~tolower( .x ))) %>% 
  filter(location == "DEU") %>% 
  select(time,value) %>% set_names(c("date","bci")) %>% mutate(date= ym(date)) %>%
  mutate(date = format(as.POSIXct(date,format='%Y-%m-%d'),format='%Y %m'))%>% 
  mutate(sm_bci = bci)%>% 
  arrange(date)

for (i in 2:length(bci$bci)) {
  bci$sm_bci[i] <- bci$bci[i]* m + (1-m)*bci$sm_bci[i-1]
}

bci <- bci %>% 
  mutate(rate = (sm_bci/lag(sm_bci,1)-1)) %>%    
  select(date, rate) %>% set_names(c("moye","bci"))

DAX <- left_join(DAX,bci)

### Crude Oil ####
getSymbols("CL=F", from='2010-01-01',to='2021-11-30')
oil <- as_tibble(`CL=F`,rownames = "date") %>% 
  set_names(c("date","open","high","low","close","volume","adjusted")) %>% 
  mutate(date = as_date(date))  %>% 
  select(date,adjusted) %>% set_names(c("date","oil_usd")) %>% na.omit %>% 
  mutate(sm_oil_usd = oil_usd)%>% 
  arrange(date)

for (i in 2:length(oil$oil_usd)) {
  oil$sm_oil_usd[i] <- oil$oil_usd[i]* a + (1-a)*oil$sm_oil_usd[i-1]
}

oil <- oil %>% 
  mutate(rate = (sm_oil_usd/lag(sm_oil_usd,20)-1)) %>%    
  select(date, rate) %>% set_names(c("date","oil")) 

DAX <- left_join(DAX,oil)

DAX <- DAX %>% fill(oil)

### hhwi commodity price index ####
cmpi <- read_csv("macro/commodity_price_index.csv")[-(1:9),] %>% 
  set_names(c("date","cmpi","comment")) %>% 
  select(date, cmpi)  %>% mutate(date= as_date(ym(date))) %>% 
  mutate(date = format(as.POSIXct(date,format='%Y-%m-%d'),format='%Y %m'),
         cmpi = as.numeric(cmpi),
         sm_cmpi = cmpi) %>% na.omit() %>% 
  arrange(date)

for (i in 2:length(cmpi$cmpi)) {
  cmpi$sm_cmpi[i] <- cmpi$cmpi[i]* m + (1-m)*cmpi$sm_cmpi[i-1]
}

cmpi <- cmpi %>% 
  mutate(rate = (sm_cmpi/lag(sm_cmpi,1)-1)) %>%    
  select(date, rate) %>% set_names(c("moye","cmpi"))

DAX <- left_join(DAX,cmpi)
#### VDAX NEW ####
load("/Users/henryspecht/Documents/Uni/UM/Thesis/Data/macro/vdax.Rdata")

vdax <- vdax %>% set_names(c('date', 'close', 'open', 'high', 'low')) %>% 
  mutate(sm_vdax = open) %>% na.omit() %>%  arrange(date)

for (i in 2:length(vdax$open)) {
  vdax$sm_vdax[i] <- vdax$open[i]* a + (1-a)*vdax$sm_vdax[i-1]
}

vdax <- vdax %>% 
  mutate(rate = (sm_vdax/lag(sm_vdax,20)-1)) %>%    
  select(date, rate) %>% set_names(c("date","vdax")) 

DAX <- left_join(DAX,vdax)
DAX <- DAX %>% fill(vdax)

#### EER 19 Index ####
eer <- read_csv("macro/EER19_daily.csv", skip = 5)[,-3]  %>% 
  set_names(c("date","eer19")) %>% 
  mutate(date= as_date(date)) %>% 
  filter(!eer19== "-") %>% 
  mutate(eer19 = as.numeric(eer19),
         sm_eer = eer19) %>% 
  arrange(date)

for (i in 2:length(eer$eer19)) {
  eer$sm_eer[i] <- eer$eer19[i]* a + (1-a)*eer$sm_eer[i-1]
}

eer <- eer %>% 
  mutate(rate = (sm_eer/lag(sm_eer,20)-1)) %>%    
  select(date, rate) %>% set_names(c("date","eer"))

DAX <- left_join(DAX,eer)

#### Sentiment Values ####
load("/Users/henryspecht/Documents/Uni/UM/Thesis/Data/sentiment.Rdata")

sent <- sent %>% select(-.row) %>% mutate(sm_sent = estimate)

for (i in 2:length(sent$estimate)) {
  sent$sm_sent[i] <- sent$estimate[i]* a + (1-a)*sent$sm_sent[i-1]
}
  
sent$sm_sent <-  (sent$sm_sent/lag(sent$sm_sent,20)-1)


sent <- sent %>% 
  select(date,sm_sent)

DAX <- left_join(DAX,sent)

DAX <-  DAX %>% fill(sm_sent)

rm(ecb,china,japan,gbp,usd,cpi,ppi,cci,bci,gold,GDAXI, 'CL=F', oil, vdax, eer, yb1, yb10, libor)

#### Response Variable ####

DAX <-  DAX %>% mutate(response = lead(adjusted,20) - adjusted , 
                       growth = factor(if_else(response >= 0,"Yes","No" )),
                       response_week = (lead(adjusted,5) - adjusted),
                       growth_week = factor(if_else(response_week >= 0,"Yes","No" )),)

DAX <-  DAX %>% mutate(growth = fct_shift(growth,1),
                       growth_week = fct_shift(growth_week,1))
# pure numbers not exponentially smoothed

DAX <- DAX %>% select(date, sm_adj, gain, loss,response, growth, response_week, growth_week, rsi, sfk,sfd, ssd, lwr,  macd,roc,commodity_channel_index,
                     yb1, yb10,ecb_int, libor,china, japan, gbp, usd, gold, oil,cmpi, vdax, eer , cpi,ppi,cci, bci, sm_sent) %>% 
  set_names(c("date","closing_price","gain","loss","response","growth","response_week","growth_week","relative_strength_index","stochastic_fast_k","stochastic_fast_d",
              "stochastic_slow_d","larry_williams_r","moving_average_convergence_divergence","rate_of_change",
              "commodity_channel_index","treasury_yield1","treasury_yield10","ecb_interest_rate","libor","rmb","yen","gbp",
              "usd","gold","oil", "commodity_price_index","vdax", "effective_exchange_rate","consumer_price_index","producer_price_index","consumer_confidence_index","business_confidence_index", "sentiment")) 
  
DAX <- DAX %>% filter(date >= "2010-09-01" & date <= "2021-11-30") 

save(DAX, file = "DAX_prepared.RData")
library(corrr)
DAX %>% select(-c(date, gain,loss,growth, closing_price, growth_week)) %>% 
  correlate()%>% shave(upper = TRUE) %>% 
  rplot(print_cor = TRUE, shap =15)+theme(axis.text.x = element_text(angle = 60, hjust = 1))
