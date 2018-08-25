######################### Pre-Analysis #################################

### looking at our data

impalacon %>% src_tbls()


impalacon %>% tbl("airways") %>% 
  glimpse()

impalacon %>% tbl("airways") %>% 
  head()


## delete tables
#db_drop_table(impalacon$con, "airlines") # drops a table


####################### DATA ANALYSIS ##################################
library(tidyverse)

airlines2 = impalacon %>% tbl("airways")
airlines2 %>% glimpse()

# plot how many flights per uniquecarrier
airlines2 %>%
  count(uniquecarrier) %>%
  as.data.frame() %>%
  mutate(uniquecarrier=as.factor(uniquecarrier),
         n=as.integer(n)) %>% 
  ggplot() + aes(x=factor(uniquecarrier), y=n) +
  geom_bar(stat="identity")


# plot how many flights per uniquecarrier fill color by DaysOfWeek
airlines2 %>%
  count(uniquecarrier, dayofweek) %>%
  as.data.frame() %>%
  mutate(uniquecarrier=as.factor(uniquecarrier),
         dayofweek=as.integer(dayofweek),
         n=as.integer(n)) %>%
  ggplot()+aes(x=factor(uniquecarrier),
               y=n, 
               fill=factor(dayofweek))+
  geom_bar(stat="identity")


# plot mean Distance only with valid cancellation code, by DayOfWeek
airlines2 %>% 
  select(dayofweek,distance,cancellationcode) %>%
  filter(cancellationcode %in% c("A","B","C","D")) %>%
  group_by(dayofweek, cancellationcode)%>%
  summarize(count=n(),
            meanD=mean(distance, na.rm=TRUE)) %>%
  as.data.frame() %>%
  mutate(cancellationcode=as.factor(cancellationcode),
         dayofweek=as.integer(dayofweek),
         count=as.integer(count),
         meand=as.integer(meanD)) %>%
  ggplot() +
  aes(x=factor(dayofweek), y=meand) +
  geom_boxplot()


# plot arrival delays by month

airlines2 %>% select(month,arrdelay) %>%
  ggplot() + aes(month,arrdelay) + geom_violin(aes(group=month)) +
  labs(y="Arrival Delay (in minutes)") +
  ggtitle("Arrival flight Arrival delay by month")


################ Challenge ###############

#plot how many flights with valid cancellation code by months and  
#fill color with cancellation code

