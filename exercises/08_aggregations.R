######################################################
# -------------- AGGREGATION --------------------------
######################################################

airlines2 =impalacon %>% tbl("airways") 
airlines2 %>% glimpse()


######### grouping

airlines2 %>%
  select(dayofweek, distance) %>%
  filter(!is.na(dayofweek)) %>%
  arrange(dayofweek) %>%
  group_by(dayofweek) %>% groups()   # see the groups


airlines2 %>%
  select(dayofweek, distance) %>%
  filter(!is.na(dayofweek)) %>%
  arrange(dayofweek) %>%
  group_by(dayofweek) %>% group_size() # see members in group


######### Cumulative

# cumulative sum

airlines2 %>%
  select(dayofweek, distance) %>%
  filter(!is.na(dayofweek)) %>%
  arrange(dayofweek) %>%
  group_by(dayofweek) %>%
  mutate(running = cumsum(distance))
  
# cumulative min

airlines2 %>%
  select(dayofweek, distance) %>%
  filter(!is.na(dayofweek)) %>%
  arrange(dayofweek) %>%
  group_by(dayofweek) %>%
  mutate(running = cummin(distance))

# cumulative max

airlines2 %>%
  select(dayofweek, distance) %>%
  filter(!is.na(dayofweek)) %>%
  arrange(dayofweek) %>%
  group_by(dayofweek) %>%
  mutate(running = cummax(distance))


########### Summarize

airlines2 %>%
  select(dayofweek, distance, weatherdelay) %>%
  filter(!is.na(dayofweek)) %>%
  filter(!is.na(weatherdelay)) %>%
  arrange(dayofweek) %>%
  group_by(dayofweek) %>% 
  summarise_all(funs(mean))   # for all the columns


airlines2 %>%
  select(dayofweek, distance, weatherdelay) %>%
  filter(!is.na(dayofweek)) %>%
  filter(!is.na(weatherdelay)) %>%
  arrange(dayofweek) %>%
  group_by(dayofweek) %>% 
  summarise_at(
    vars(weatherdelay), funs(mean)) # for selected columns only


airlines2 %>%
  select(dayofweek, distance, weatherdelay, origin) %>%
  filter(!is.na(dayofweek)) %>%
  filter(!is.na(weatherdelay)) %>%
  arrange(dayofweek) %>%
  group_by(dayofweek) %>% 
  summarise_if(
    function(x) is.numeric(x), 
    funs(mean))                 # for numeric columns only


############# Ranking

airlines2 %>%
  select(distance) %>%
  collect() %>%
  as.data.frame() %>%     # arrange lowest to highest
  ntile(3) %>% table()    # break your data into 3 parts
                          

airlines2 %>%
  select(distance) %>%
  collect() %>%
  as.data.frame() %>%     # arrange lowest to highest
  min_rank() %>% head()   # order data from lowest to highest


airlines2 %>%
  select(distance) %>%
  collect() %>%
  as.data.frame() %>%     # arrange lowest to highest
  dense_rank() %>% head()   # order data from lowest to highest
# Rank values are not skipped in the event of ties.


airlines2 %>%
  select(distance) %>%
  collect() %>%
  as.data.frame() %>%     # arrange lowest to highest
  percent_rank() %>% head()   # order data from lowest to highest
# where 0.75 is the 75% percentile rank.


airlines2 %>%
  select(distance) %>%
  collect() %>%
  as.data.frame() %>%     # arrange lowest to highest
  cume_dist()   # order data from lowest to highest
#CUME_DIST calculates the cumulative distribution of a value in a group of values


############ Challenge

Using dplyr find the following :>

# find the cumulative max of distance grouped by dayofweek and month arranged in order of month

# find the summary table for mean and max of weatherdelay and distance