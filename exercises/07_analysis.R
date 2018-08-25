
######################### Pre-Analysis #################################

### looking at our data

impalacon %>% src_tbls()


impalaconn %>% tbl("airways") %>% 
  glimpse()

impalaconn %>% tbl("airways") %>% 
  head()


## delete tables
# db_drop_table(impalacon$con, "airlines") # drops a table


############################# Query ###################################

airlines2 =impalacon %>% tbl("airways") 
airlines2 %>% glimpse()


# Total Number of Flights Cancelled Each Month

airlines2 %>% select(month, cancelled) %>%
              filter(cancelled == 1) %>%
              group_by(month) %>%
              summarize(count=n()) %>%
  as.data.frame() %>%
  mutate(month=as.integer(month),
         count=as.integer(count)) %>%
   arrange(month)                   
 

# top 5 airports and airline based on activity

airlines2 %>% select(uniquecarrier) %>%
              group_by(uniquecarrier)%>%
              summarize(cnt=n()) %>%
  as.data.frame() %>%
  mutate(uniquecarrier=as.factor(uniquecarrier),
         cnt=as.integer(cnt)) %>%
         arrange(desc(cnt))%>%
         slice(1:5)


airlines2 %>% select(origin) %>%
              group_by(origin) %>%
            summarize(cnt=n()) %>%
  as.data.frame() %>%
  mutate(origin=as.factor(origin),
         cnt=as.integer(cnt)) %>%
  arrange(desc(cnt))%>%
  slice(1:5)

# mean delay for top 5 airports per year

airlines2 %>% select(uniquecarrier,arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay) %>%
  mutate(totDelay=arrdelay + depdelay + carrierdelay + weatherdelay + nasdelay + securitydelay + lateaircraftdelay) %>%
  group_by(uniquecarrier)%>%
  summarize(cnt=n(),
            meandelay=mean(totDelay)) %>%
  as.data.frame() %>%
  mutate(uniquecarrier=as.factor(uniquecarrier),
         meandelay=as.integer(meandelay),
         cnt=as.integer(cnt)) %>%
  arrange(desc(cnt))%>%
  slice(1:5)


# Top 10 route(origin and dest) that has seen maximum diversions?

airlines2 %>% select(origin, dest, diverted) %>%
  filter(diverted==1) %>%
  group_by(origin, dest) %>%
  summarize(div=n()) %>%
  as.data.frame() %>%
  mutate(origin=as.factor(origin),
         dest=as.factor(dest),
         div=as.integer(div)) %>%
  arrange(desc(div)) %>% slice(1:10)
 
############# Challenge #####################

# using dplyr find the following :>
# Total Number of Flights Diverted Each Month
# Which month have seen the most number of cancellation due to bad weather? (cancellationcode=B)
# Top 5 most visited destination.


