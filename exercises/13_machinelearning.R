
######################### Pre-Analysis #################################

### looking at our data

impalaconn %>% src_tbls()


impalacon %>% tbl("airways") %>% 
  glimpse()

impalacon %>% tbl("airways") %>% 
  head()


###################### MACHINE LEARNING ###################################

library(purrr)
library(randomForest)
library(broom)
#devtools::install_github("njtierney/broomstick")
library(broomstick)


airlines2 =impalacon %>% tbl("airways") 
airlines2 %>% glimpse()


#### linear model

library(tidypredict)

airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance) %>%
                     collect() %>%
                    do(tidy(lm(distance ~ .,
                        data=.))) %>%
                      View()

airlines2 %>% select(arrdelay,depdelay,
                            carrierdelay,weatherdelay,nasdelay,
                            securitydelay,lateaircraftdelay,
                            distance) %>%
                      collect() %>%
                      do(glance(lm(distance ~.,
                          data=.))) %>% View()

airlines2 %>% select(arrdelay,depdelay,
                            carrierdelay,weatherdelay,nasdelay,
                            securitydelay,lateaircraftdelay,
                            distance) %>%
                     collect() %>%
                      do(augment(lm(distance ~ .,
                      data=.))) %>% ggplot() + aes(distance, .fitted) +
                      geom_point() +geom_abline(slope=1, intercept=0)


# with grouping


airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     dayofweek, distance) %>%
  collect() %>%
  nest(-dayofweek) %>%
  mutate(test=map(data, ~lm(.x$distance ~ ., data=. )),
         tidied=map(test, tidy)
  )%>%
  unnest(tidied, .drop=TRUE) %>%
  arrange(dayofweek) %>%
  View()   
  

airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance, dayofweek) %>%
  collect() %>%
  nest(-dayofweek) %>%
  mutate(test=map(data, ~lm(.x$distance ~ ., data=. )),
         aug=map(test, augment)
  )%>%
  unnest(aug, .drop=TRUE) %>%
  arrange(dayofweek) %>% ggplot() + aes(.x.distance, .fitted) +
  geom_point() +geom_abline(slope=1, intercept=0) +
  facet_wrap(.~dayofweek)



### logistic regression


airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance, cancellationcode) %>%
              filter(cancellationcode %in% c("A","B")) %>%
                     collect() %>%
  mutate(arrdelay=ifelse(is.na(arrdelay),0,arrdelay),
         depdelay=ifelse(is.na(depdelay),0,depdelay),
         carrierdelay=ifelse(is.na(carrierdelay),0,carrierdelay),
         weatherdelay=ifelse(is.na(weatherdelay),0,weatherdelay),
         securitydelay=ifelse(is.na(securitydelay),0,securitydelay),
         lateaircraftdelay=ifelse(is.na(lateaircraftdelay),0,lateaircraftdelay),
         distance=ifelse(is.na(distance),0,distance),
         cancellationcode=ifelse(cancellationcode=="A",1,0)) %>%
  do(tidy(glm(cancellationcode ~ .,family=binomial(link="logit"),
             data=.))) %>%
  View()


#### decision tree

airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance, cancellationcode) %>%
  filter(cancellationcode %in% c("A","B","C","D")) %>%
  collect() %>%
  mutate(arrdelay=ifelse(is.na(arrdelay),0,arrdelay),
         depdelay=ifelse(is.na(depdelay),0,depdelay),
         carrierdelay=ifelse(is.na(carrierdelay),0,carrierdelay),
         weatherdelay=ifelse(is.na(weatherdelay),0,weatherdelay),
         securitydelay=ifelse(is.na(securitydelay),0,securitydelay),
         lateaircraftdelay=ifelse(is.na(lateaircraftdelay),0,lateaircraftdelay),
         distance=ifelse(is.na(distance),0,distance),
         cancellationcode=factor(as.character(cancellationcode)))%>%
  do(tidy(rpart(cancellationcode ~ .,
              data=.))) %>%
  View()

airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance, cancellationcode) %>%
  filter(cancellationcode %in% c("A","B","C","D")) %>%
  collect() %>%
  mutate(arrdelay=ifelse(is.na(arrdelay),0,arrdelay),
         depdelay=ifelse(is.na(depdelay),0,depdelay),
         carrierdelay=ifelse(is.na(carrierdelay),0,carrierdelay),
         weatherdelay=ifelse(is.na(weatherdelay),0,weatherdelay),
         securitydelay=ifelse(is.na(securitydelay),0,securitydelay),
         lateaircraftdelay=ifelse(is.na(lateaircraftdelay),0,lateaircraftdelay),
         distance=ifelse(is.na(distance),0,distance),
         cancellationcode=factor(as.character(cancellationcode)))%>%
  do(rpart.plot(rpart(cancellationcode ~ .,
                data=.)))


## random forest
airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance, cancellationcode) %>%
  filter(cancellationcode %in% c("A","B","C","D")) %>%
  collect() %>%
  mutate(arrdelay=ifelse(is.na(arrdelay),0,arrdelay),
         depdelay=ifelse(is.na(depdelay),0,depdelay),
         carrierdelay=ifelse(is.na(carrierdelay),0,carrierdelay),
         weatherdelay=ifelse(is.na(weatherdelay),0,weatherdelay),
         securitydelay=ifelse(is.na(securitydelay),0,securitydelay),
         lateaircraftdelay=ifelse(is.na(lateaircraftdelay),0,lateaircraftdelay),
         distance=ifelse(is.na(distance),0,distance),
         cancellationcode=factor(as.character(cancellationcode)))%>%
  do(tidy(randomForest(cancellationcode ~ .,
                data=.))) %>%
  View()



### kmeans

airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance) %>%
                     collect() %>%
  mutate(arrdelay=ifelse(is.na(arrdelay),0,arrdelay),
         depdelay=ifelse(is.na(depdelay),0,depdelay),
         carrierdelay=ifelse(is.na(carrierdelay),0,carrierdelay),
         weatherdelay=ifelse(is.na(weatherdelay),0,weatherdelay),
         securitydelay=ifelse(is.na(securitydelay),0,securitydelay),
         lateaircraftdelay=ifelse(is.na(lateaircraftdelay),0,lateaircraftdelay),
         distance=ifelse(is.na(distance),0,distance)) %>%
  do(glance(kmeans(.,3),as.matrix(.))) # kmeans with 3 clusters



airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance) %>%
                     collect() %>%
               mutate(arrdelay=ifelse(is.na(arrdelay),0,arrdelay),
                      depdelay=ifelse(is.na(depdelay),0,depdelay),
                      carrierdelay=ifelse(is.na(carrierdelay),0,carrierdelay),
                      weatherdelay=ifelse(is.na(weatherdelay),0,weatherdelay),
                      securitydelay=ifelse(is.na(securitydelay),0,securitydelay),
                      lateaircraftdelay=ifelse(is.na(lateaircraftdelay),0,lateaircraftdelay),
                      distance=ifelse(is.na(distance),0,distance)) %>%
                     do(augment(kmeans(.,3),as.matrix(.))) %>% # kmeans with 3 clusters
                     ggplot() + aes(weatherdelay, distance, 
                                    col=.cluster) + geom_point(size=2)

### with grouping

airlines2 %>% select(arrdelay,depdelay,
                     carrierdelay,weatherdelay,nasdelay,
                     securitydelay,lateaircraftdelay,
                     distance, dayofweek) %>%
  collect() %>%
  mutate(arrdelay=ifelse(is.na(arrdelay),0,arrdelay),
         depdelay=ifelse(is.na(depdelay),0,depdelay),
         carrierdelay=ifelse(is.na(carrierdelay),0,carrierdelay),
         weatherdelay=ifelse(is.na(weatherdelay),0,weatherdelay),
         securitydelay=ifelse(is.na(securitydelay),0,securitydelay),
         lateaircraftdelay=ifelse(is.na(lateaircraftdelay),0,lateaircraftdelay),
         distance=ifelse(is.na(distance),0,distance),
         dayofweek=factor(dayofweek)) %>%
         group_by(dayofweek) %>%
         do(augment(kmeans(.,3),as.matrix(.)))%>% ggplot() + 
         aes(weatherdelay, distance, col=.cluster) + 
         geom_point(size=2) +
         facet_wrap(.~dayofweek)
