library(dbplot)
library(ggplot2)

airlines2 = impalacon %>% tbl("airways")
airlines2 %>% glimpse()


### histogram
airlines2 %>% 
  select(distance) %>% collect() %>%
 # mutate(distance=as.integer(distance)) %>%
  dbplot_histogram(distance)
  gc()

## histogram with bin width  
airlines2 %>% 
    select(distance) %>% collect() %>%
    mutate(distance=as.integer(distance)) %>%
    dbplot_histogram(distance, binwidth=50)
  gc()


## scatterplot    
airlines2 %>%
  filter(!is.na(arrdelay)) %>%
  filter(!is.na(depdelay)) %>%
  select(arrdelay, depdelay) %>%
  collect() %>%
  mutate(arrdelay=as.integer(arrdelay), 
         depdelay=as.integer(depdelay)) %>%
  dbplot_raster(arrdelay, depdelay)  
gc()

## barplot
airlines2 %>%
  select(cancellationcode) %>%
  filter(!is.na(cancellationcode)) %>%
  filter(cancellationcode !="") %>%
  collect() %>%
  mutate(cancellationcode=as.character(cancellationcode)) %>%
  dbplot_bar(cancellationcode)

## line graph
airlines2 %>%
  select(month) %>%
  collect() %>%
  mutate(month=as.numeric(month)) %>%
  dbplot_line(month)


## line graph with mean
airlines2 %>%
  select(month, depdelay) %>%
  collect() %>%
  mutate(month=as.numeric(month),
         depdelay=as.numeric(depdelay)) %>%
  dbplot_line(month, mean(depdelay, na.rm=T))


## boxplot (doesn't work with impala)
# airlines2 %>%
#   select(cancellationcode, distance) %>%
#   filter(!is.na(cancellationcode)) %>%
#   filter(cancellationcode !="") %>%
#   collect() %>%
#   mutate(cancellationcode=as.character(cancellationcode),
#          distance=as.numeric(distance)) %>%
#   dbplot_boxplot(cancellationcode, distance)




