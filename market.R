# Представьте, что перед командой Яндекс.Маркета стоит задача по увеличению LTV 
# за счет уменьшения оттока пользователей и увеличения частоты контакта. 
# К вам приходит менеджер и просит провести исследование и ответить на вопрос, 
# каким способом можно достичь указанной цели. 
# 
# Вам необходимо сделать RFM-анализ и подготовить рекомендации команде сервиса. 
# Предложите хотя бы один проект по увеличению LTV и оцените потенциальную пользу от этого проекта. 
# Рекомендации по увеличению монетизируемости без увеличения частоты контакта так же приветствуются.
# 
# В приложенном файле  собраны данные об активности пользователей основного сайта Яндекс.Маркета.
# Поля: 
#   timestamp хита 
# user_id 
# url - url хита или "click", если пользователь перешел в интернет-магазин 
# geo_id - идентификатор региона пользователя 
# category - идентификатор категории 
# price - стоимость клика, если пользователь перешел в интернет-магазин
# 
# Результаты анализа оформите, пожалуйста, в виде презентации и рассказа команде о результатах.  

library(data.table)
library(dplyr)
library(anytime)
library(stringr)

#rfm <- fread('~/Downloads/market_task/rfm.tsv', sep = '\t', stringsAsFactors = FALSE, fill = TRUE)
#names(rfm) <- c('timest', 'user_id', 'url', 'geo_id', 'category', 'price')
#rfm$timest <- anytime(rfm$timest)
#rfm$date <- anydate(rfm$timest)

geo <- fread('~/Downloads/market_task/geo_table', stringsAsFactors = FALSE)
categ <- fread('~/Downloads/market_task/categories_tree', stringsAsFactors = FALSE)

#save(rfm, file='rfm.RData')
#save(geo, file='geo_m.RData')
#save(categ, file='categ_m.RData')
#load(file = 'rfm.RData')

names(geo)[names(geo)=='id']='geo_id'
names(geo)[names(geo)=='name']='geo_name'

# тээк чет есть одно бракованное значение
which(rfm$geo_id=='$s')
# просто поменяю его на самое распространенное - 10000
head(as.data.frame(table(rfm$geo_id)) %>% arrange(desc(Freq)))
rfm[which(rfm$geo_id=='$s'), 4] <- '10000'

geo$geo_id <- as.character(geo$geo_id)
geo$TZ_OFFSET <- geo$TZ_OFFSET/3600
geo_help <- geo[, c(1,2)]
names(geo_help) <- c('parent_region_id', 'parent_geo_name')
geo$parent_region_id <- as.character(geo$parent_region_id)
geo <- left_join(geo, geo_help)

# выделим чисто Россию (так-то не понадобится, но пусть будет)
ids <- c(225)
ids_while <- ids
ids_test <- ids
while (ids){
  ids_while <- as.numeric(unique(filter(geo, parent_region_id %in% ids_while)$geo_id))
  if (length(ids_while)==0){
    break
  } else{
    ids_test <- c(ids_test, ids_while)
  }
}

rus <- data.frame('geo_id' = ids_test, 'country' <- 'russia')
names(rus) <- c('geo_id', 'country')
rus$geo_id <- as.character(rus$geo_id)

# присобачим россию к главной выборке, выделим землю как самое частое значение, остальное - отхер
rfm <- left_join(rfm, rus)
rfm$country <- as.character(rfm$country)
rfm$country[which(is.na(rfm$country) & rfm$geo_id==10000)] <- 'earth'
rfm$country[which(is.na(rfm$country) & rfm$geo_id!=10000)] <- 'other'
#98% кликов - россия
round(table(filter(rfm, url=='click')$country)/nrow(filter(rfm, url=='click')), 2)
table(filter(rfm, geo_id!=10000)$country)

# посмотрим, получится ли что-то, если выделить москву и мо
ids <- c(1)
ids_while <- ids
ids_test <- ids
while (ids){
  ids_while <- as.numeric(unique(filter(geo, parent_region_id %in% ids_while)$geo_id))
  if (length(ids_while)==0){
    break
  } else{
    ids_test <- c(ids_test, ids_while)
  }
}

mos <- data.frame('geo_id' = ids_test, 'city' <- 'moscow')
names(mos) <- c('geo_id', 'city')
mos$geo_id <- as.character(mos$geo_id)
rfm <- left_join(rfm, mos)
rfm$city <- as.character(rfm$city)
rfm$city[which(is.na(rfm$city)==T)] <- 'other'
table(rfm$city)
#54% кликов - москва и мо
round(table(filter(rfm, url=='click')$city)/nrow(filter(rfm, url=='click')), 2)

# у каждой категории выделим базовую категорию. мож пригодится (нет)
k <- (sapply(categ$category_pass, function(x) substr(x, 3, str_locate(substr(x, 3, nchar(x)), '\\\\t')[1]+1)))
categ$cat_1 <- k
cat <- categ[,c(3,4,6)]
cat$hyper_cat_id <- as.character(cat$hyper_cat_id)
names(cat) <- c('category', 'cat', 'cat_base')

# каких-то категорий в справочнике не оказалось, поэтому ставим статус хз
rfm <- left_join(rfm, cat)
head(filter(rfm, is.na(cat)==F))
rfm$cat[which(is.na(rfm$cat)==T & rfm$category!='')] <- 'Неизвестно'
rfm$cat_base[which(rfm$cat=='Неизвестно' & rfm$category!='')] <- 'Неизвестно'
table(rfm$cat_base)

# делим на 5 групп давность
rfm$recency <- 5
rfm$recency[which(month(rfm$date)==12)] <- 1
rfm$recency[which(month(rfm$date)==11)] <- 2
rfm$recency[which(month(rfm$date)==10)] <- 3
rfm$recency[which(month(rfm$date) %in% c(9,8,7))] <- 4

# анализируем клики и смотрим, как их поделить на 5 групп
clicks <- as.data.frame(table(filter(rfm, url=='click')$user_id))
#clicks <- as.data.frame(table(rfm$user_id))
names(clicks) <- c('user_id', 'clicks')
clicks$user_id <- as.character(clicks$user_id)
ggplot(filter(clicks, clicks>=0), aes(clicks)) + geom_histogram() + xlim(0,30)

rfm <- left_join(rfm, clicks)

rfm$frequency <- 5
rfm$frequency[which(rfm$clicks>=16)] <- 1
rfm$frequency[which(rfm$clicks %in% c(11,12,13,14,15))] <- 2
rfm$frequency[which(rfm$clicks %in% c(5,6,7,8,9,10))] <- 3
rfm$frequency[which(rfm$clicks %in% c(2,3,4))] <- 4

# ну и теперь все то же самое для бабоса
monetary <- rfm %>% group_by(user_id) %>% summarise(monetary = sum(ifelse(is.na(price)==T, 0, price)))
names(monetary) <- c('user_id', 'money')
ggplot(filter(monetary, money>0), aes(money)) + geom_density() + xlim(0,8000)

rfm <- left_join(rfm, monetary)

rfm$monetary <- 5
rfm$monetary[which(rfm$money>=1000)] <- 1
rfm$monetary[which(rfm$money>=750 & rfm$money<1000)] <- 2
rfm$monetary[which(rfm$money>=500 & rfm$money<750)] <- 3
rfm$monetary[which(rfm$money>=250 & rfm$money<500)] <- 4

rfm$rfm <- as.numeric(paste0(rfm$recency, rfm$frequency, rfm$monetary))
rfm$rf <- as.numeric(paste0(rfm$recency, rfm$frequency))

clients_urfm <- rfm %>% group_by(user_id) %>% summarise(money=sum(money), rfm=min(rfm), rf=min(rf))

View(clients_urfm %>% group_by(rf) %>% summarise(Part=round(100*n()/nrow(clients_urfm), 2), n=n(), money=sum(money),
                                                 money_part=(round(100*sum(money)/sum(clients_urfm$money), 2))) %>% 
       arrange(desc(money)) %>% mutate_if(is.numeric, funs(as.character(signif(., 10)))))
# recent = 1 содержит 94.31% всей прибыли, несмотря на то, что это лишь 1.58% всех клиентов выборки

#для москвы
clients_urfm_mos <- filter(rfm, city=='moscow') %>% group_by(user_id) %>% summarise(money=sum(money), rfm=min(rfm), rf=min(rf))

View(clients_urfm_mos %>% group_by(rf) %>% summarise(Part=round(100*n()/nrow(clients_urfm), 2), n=n(), money=sum(money),
                                                 money_part=(round(100*sum(money)/sum(clients_urfm$money), 2))) %>% 
       arrange(desc(money)) %>% mutate_if(is.numeric, funs(as.character(signif(., 10)))))





#расчет потенциальной прибыли 
sum25 <- 0
for (i in 2:5){
  n <- round(nrow(filter(clients_urfm, rf==i*10+1))/10, 0)
  s <- sum(head(filter(clients_urfm, rf==i*10+1) %>% arrange(money), n)$money)
  sum25 <- sum25 + s
}
as.integer(sum25)

sum1 <- 0
for (i in 1){
  n <- round(nrow(filter(clients_urfm, rf==i*10+1))/10, 0)
  s <- sum(head(filter(clients_urfm, rf==i*10+1) %>% arrange(money), n)$money)
  sum1 <- sum1 + s
}
as.integer(sum1)

rmarkdown::render('knitr_demo.Rmd')

write.csv(clients_urfm %>% group_by(rf) %>% summarise(Part=round(100*n()/nrow(clients_urfm), 2), n=n(), money=sum(money),
                                                      money_part=(round(100*sum(money)/sum(clients_urfm$money), 2))) %>% 
            arrange(desc(money)) %>% mutate_if(is.numeric, funs(as.character(signif(., 10)))), file='clients_urfm.csv')
clients_urfm %>% group_by(rf) %>% summarise(Part=round(100*n()/nrow(clients_urfm), 2), n=n(), money=sum(money),
                                            money_part=(round(100*sum(money)/sum(clients_urfm$money), 2))) %>% 
  arrange(desc(money)) %>% mutate_if(is.numeric, funs(as.character(signif(., 10))))