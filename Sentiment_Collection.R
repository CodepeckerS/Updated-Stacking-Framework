#### Packages #####
library(rvest)
library(tidyverse)
library(tidymodels)
library(tidytext)
library(lubridate)
library(textdata)
library(textrecipes)
library(stopwords)
library(quantmod)
library(SnowballC)
library(spacyr)

### Newslinks ####
links <- tibble()
#3084
for (i in 5:3086) {
  url <- paste("https://de.investing.com/indices/germany-30-news/",i,sep = "")
  temp <- try(read_html(url), silent = TRUE)
  link <- temp %>% html_element("#leftColumn") %>% html_elements("div.mediumTitle1") %>% html_elements("a.title") %>%  html_attr("href") %>% as_tibble()
  date <- temp %>% html_element("#leftColumn") %>% html_elements("div.mediumTitle1") %>% html_elements("span.date") %>% 
    html_text2() %>% as_tibble() %>% mutate(value = str_remove(value,"- ")) %>% transmute(date = dmy(value))
  temp <- cbind(date,link)
  links <- rbind(links,temp)
}

rm(i,temp,date,link)
newslinks <- links %>% mutate(link = url_absolute(value,"https://de.investing.com"))

i <- which(is.na(newslinks$link))
newslinks$link[i] <- paste("https://de.investing.com",links$value[i],sep="") 

newslinks %>% filter(!grepl("de.investing.com",link))

newslinks <- newslinks %>% na.omit()
save(newslinks, file= 'linksdecember.RData')

#load("newslinks.RData")
texts <- tibble()
for (i in 1:length(newslinks$link)) {
  url <- newslinks$link[i]
  temp <- try(read_html(url), silent = TRUE)
  ifelse(grepl("Error in open.connection", temp), raw <- "Dead link",
         raw <-  temp %>%  html_elements("div.WYSIWYG.articlePage")  %>%  html_text2() %>% str_c(sep = " ", collapse = " ")  %>% str_squish())
  ifelse(grepl("Error in open.connection", temp), headline <- "Dead link",
         headline <-  temp %>%  html_elements("h1")  %>%  html_text2() %>% str_c(sep = "", collapse = "")  %>% str_squish())
  complete <- tibble("date"=newslinks$date[i],"link"=newslinks$link[i], headline, raw)
  texts <- rbind(texts,complete) 
}

save(texts, file= 'textsdecember.RData')
texts <- texts %>% na.omit() 

tidy_texts <- texts %>%  
  filter(grepl("DAX", headline, ignore.case = TRUE)|
           grepl("Euro", headline, ignore.case = TRUE)|
           grepl("EZB", headline, ignore.case = TRUE)|
           grepl("Frankfurt",headline, ignore.case = TRUE)|
           grepl("Stuttgart",headline, ignore.case = TRUE)|
           grepl("Deutschland", headline, ignore.case = TRUE)|
           grepl("Überblick",headline, ignore.case = TRUE))

z <- anti_join(texts,tidy_texts, by= "link")

deads <- which(tidy_texts$raw == "Dead link")

tidy_texts <- tidy_texts %>% filter(date >= "2010-02-01" & date <= "2021-11-31") %>% distinct(link, .keep_all = TRUE)

tidy_texts <- tidy_texts %>% 
  mutate(
    raw = str_remove(raw,".*?(\\(dpa-AFX\\) -)"),
    raw = str_remove(raw,".*?(Investing\\.com\\s)"),
    raw = str_remove(raw,".*?(\\(Reuters\\) -)"),
  )

tidy_texts$text <-  str_glue_data(tidy_texts, "{headline} {raw}")

news <- tidy_texts %>% select(date, text, link) %>% mutate(text = str_to_lower(text))
setwd("/Users/henryspecht/Documents/Uni/UM/Thesis/Data")
save(news, file = "DE-NEWS-Investingdecember.RData")
#### Screening for Sentiment Charged Words ####
#load("DE-NEWS-Investingdecember.RData")
#Getting Stock Index prices
getSymbols("^GDAXI", from='2010-01-01',to='2021-12-03')
raw_DAX <- as_tibble(GDAXI,rownames = "date") %>% 
  set_names(c("date","open","high","low","close","volume","adjusted")) %>% 
  mutate(date = as_date(date)) %>% select(date, adjusted, open) %>% na.omit()

DAX <-  raw_DAX %>% mutate(response = lead(adjusted,1) -lag(open,1) , 
                       growth = factor(if_else(response >= 0,"Yes","No" )))

d_news <- left_join(news,DAX) %>% na.omit() %>% add_rowindex() %>%  mutate(doc_id = paste0("doc", row_number()))

teids <- d_news %>% filter(date > "2021-01-01") %>% select(.row)
d_newscomp <- d_news
d_news <- d_newscomp %>% anti_join(teids) %>% 
  mutate(p = rank(response,na.last = FALSE, ties.method = "min")/nrow(.))
d_newscomp <- d_newscomp %>% 
  mutate(p = rank(response,na.last = FALSE, ties.method = "min")/nrow(.))
d_newstest <- d_newscomp %>% right_join(teids)
spacy_initialize(model = "de_core_news_sm", entity = FALSE)

dax_tokens <- d_newscomp  %>%  
  select(doc_id, text) %>% 
  spacy_parse() %>% 
  filter(!(lemma %in% stopwords("de",source = "snowball"))) %>% 
  filter(!grepl("[[:digit:]]", lemma)) %>% 
  filter(!grepl("[[:punct:]]", lemma)) %>% 
  filter(!grepl("nn",lemma)) %>% 
    select(doc_id, lemma)

dax_tokens <- left_join(dax_tokens,d_newscomp) %>% select(date, .row, lemma) %>% setNames(c("date",".row", "word")) 

dax_tokensall <- dax_tokens
dax_tokenstest <- dax_tokensall %>% right_join(teids)
dax_tokens <- dax_tokensall %>% anti_join(teids)

tidy_tokens <- dax_tokens %>% 
  distinct(word) 

classified <- left_join(dax_tokens, DAX, by = "date") %>% distinct(word,.row, .keep_all = TRUE) %>% na.omit()

all <- classified %>% count(word) %>% inner_join(tidy_tokens) %>% arrange(-n)
positive <- classified %>% filter(growth == "Yes") %>% count(word) %>% inner_join(tidy_tokens) %>% arrange(-n)

f <- left_join(all,positive, by = "word") %>% set_names(c("word","n_all","n_positive")) %>% 
  mutate(f= n_positive / n_all) %>% arrange(-f) %>% mutate(across(everything(), ~ifelse(is.na(.),0,.))) 

## Aus loop entfernt für optimierung
#train
wordcount <- dax_tokens %>% group_by(.row) %>% count(word)
d <- wordcount %>% pivot_wider(id_cols = word, names_from = .row, values_from = n,names_prefix = "art", values_fill = 0)
#test
wordtest <- dax_tokenstest %>% group_by(.row) %>% count(word)
dtest <- wordtest %>% pivot_wider(id_cols = word, names_from = .row, values_from = n,names_prefix = "art", values_fill = 0)

W <-  d_news %>% mutate(p_1 = 1-p) %>% select(p, p_1) %>% mutate(across(everything(), ~ifelse(is.nan(.),0,.))) %>% 
  as.matrix(.) %>% t(.)
w <- d_newstest %>% mutate(p_1 = 1-p) %>% select(p, p_1) %>% mutate(across(everything(), ~ifelse(is.nan(.),0,.))) %>% 
  as.matrix(.)
### Function to maximize
arg_max<-function(p){s[,i]*sum(ddach[,i]*log(p*O[,1]+(1-p)*O[,2])+lamb*log(p*(1-p)))}

fl <- c(200,250,300)
ap <- c(0.06,0.075,0.08)
am <- c (0.05,0.06,0.075)
lm <- c(0.0025,0.001,0.0005)
lambda <- as.numeric((d_news %>% count(growth) %>% .[2,2])/(d_news %>% count(growth) %>% .[,2] %>% sum(.)))
end <- tibble()
for (alpha in fl) {
  fm <- f %>% filter(n_all >= alpha)

for (beta in ap){ 
alphaplus <- beta
for (gamma in am) {
alphaminus <- gamma
upper <- lambda+alphaplus
lower <- lambda-alphaminus

wordlist <- fm %>% filter(f > upper | f < lower)
list <- wordlist %>% select(word)
di <- left_join(list,d)
### Algorithm 2
s <- di %>% summarise(across(where(is.numeric), ~sum(.)))
s <- s^-1

ddach <- di %>% select(-word)
ddachi <- ddach
for (i in 1:ncol(ddach)) {
  ddachi[,i] <- as.vector(ddach[,i])*as.numeric(s[1,i])
}
ddachi <- ddachi %>%  mutate(across(everything(), ~ifelse(is.nan(.),0,.)))

D <- as.matrix(ddachi)

O <- D %*% t(W) %*% solve(W %*% t(W))
O <- as_tibble(O) %>% mutate(across(everything(), ~ifelse(. < 0,0,.))) %>% 
  as.matrix()

### re-normalization to unit l1 form. Stimmt das?
for (i in 1:ncol(O)) {
  O[,i] <-  O[,i]/sum(abs(O[,i])) 
}


#test set
di <- left_join(list,dtest) %>% mutate(across(everything(), ~ifelse(is.na(.),0,.))) 
s <- di %>% summarise(across(where(is.numeric), ~sum(.)))
s <- s^-1
ddach <- di %>% select(-word)

for (delta in lm) {

lamb <- delta 

estimates <- tibble()
for (i in 1:ncol(ddach)){
  res <- optimize(arg_max, interval = c(0,1), maximum = TRUE ) %>% .$maximum
  estimates <- estimates %>% rbind(res)
}

result <- cbind(w, estimates) %>% setNames(c("p","p_minus","estimate"))
perfind <-result %>% rmse(truth = p, estimate) %>% select(.metric, .estimate) %>% 
  bind_rows(result %>% rsq(truth = p, estimate) %>% select(.metric, .estimate) ) %>% 
  bind_rows(result %>% mae(truth = p, estimate) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(lambda = lamb,
                                                                        a_plus = alphaplus,
                                                                        a_minus = alphaminus,
                                                                        min_coun = alpha)
end <- end %>% rbind(perfind)
}
}
}
}

#### Create Estimates ####
param <- end %>% filter(rsq == max(rsq))
lamb <- as.numeric(param[,4])
alphaplus <- as.numeric(param[,5])
alphaminus <- as.numeric(param[,6])
minc <- as.numeric(param[,7])

upper <- lambda+alphaplus
lower <- lambda-alphaminus

wordlist <- f %>% filter(f > upper | f < lower) %>% filter(n_all >= minc)
list <- wordlist %>% select(word)

di <- left_join(list,d) %>% mutate(across(everything(), ~ifelse(is.na(.),0,.))) 
### Algorithm 2
s <- di %>% summarise(across(where(is.numeric), ~sum(.)))
s <- s^-1

ddach <- di %>% select(-word)
ddachi <- ddach
for (i in 1:ncol(ddach)) {
  ddachi[,i] <- as.vector(ddach[,i])*as.numeric(s[1,i])
}
ddachi <- ddachi %>%  mutate(across(everything(), ~ifelse(is.nan(.),0,.)))

D <- as.matrix(ddachi)

O <- D %*% t(W) %*% solve(W %*% t(W))
O <- as_tibble(O) %>% mutate(across(everything(), ~ifelse(. < 0,0,.))) %>% 
  as.matrix()

### re-normalization to unit l1 form. Stimmt das?
for (i in 1:ncol(O)) {
  O[,i] <-  O[,i]/sum(abs(O[,i])) 
}

#for estimation
wordcount <- dax_tokensall %>% group_by(.row) %>% count(word)
d <- wordcount %>% pivot_wider(id_cols = word, names_from = .row, values_from = n,names_prefix = "art", values_fill = 0)
W <-  d_newscomp %>% mutate(p_1 = 1-p) %>% select(p, p_1) %>% mutate(across(everything(), ~ifelse(is.nan(.),0,.))) %>% 
  as.matrix(.) %>% t(.)

di <- left_join(list,d) %>% mutate(across(everything(), ~ifelse(is.na(.),0,.))) 
### Algorithm 2
s <- di %>% summarise(across(where(is.numeric), ~sum(.)))
s <- s^-1

ddach <- di %>% select(-word)

est <- tibble()
for (i in 1:ncol(ddach)){
  res <- optimize(arg_max, interval = c(0,1), maximum = TRUE) %>% .$maximum
  est <- est %>% rbind(res)
}

sent <- cbind(t(W), est) %>% setNames(c("p","p_minus","estimate"))

perfo <-sent %>% rmse(truth = p, estimate) %>% select(.metric, .estimate) %>% 
  bind_rows(sent %>% rsq(truth = p, estimate) %>% select(.metric, .estimate) ) %>% 
  bind_rows(sent %>% mae(truth = p, estimate) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(lambda = lamb,
                                                                        a_plus = alphaplus,
                                                                        a_minus = alphaminus,
                                                                        min_coun = alpha)

sent <- sent %>% add_rowindex() %>% left_join(d_newscomp, by='.row')  %>% select(estimate, .row, p.x, date)
sent <- sent %>% group_by(date) %>% summarise(across(where(is.numeric), ~mean(.)))

perfo <-sent %>% rmse(truth = p.x, estimate) %>% select(.metric, .estimate) %>% 
  bind_rows(sent %>% rsq(truth = p.x, estimate) %>% select(.metric, .estimate) ) %>% 
  bind_rows(sent %>% mae(truth = p.x, estimate) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(lambda = lamb,
                                                                        a_plus = alphaplus,
                                                                        a_minus = alphaminus,
                                                                        min_coun = alpha)
sent <- sent %>% select(-p.x)
save(sent, file = 'sentiment.RData')
rm(GDAXI,DAX,raw_DAX,d_news,d_newstest, dax_tokens,dax_tokenstest,tidy_tokens, classified, all, positive, wordcount)
