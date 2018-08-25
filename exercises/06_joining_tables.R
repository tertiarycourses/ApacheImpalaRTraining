carr=read.csv(file.choose())  #choose carriers.csv

names(carr)

  
# import your carriers table into impala
# join your airways table with carriers table
# see if you have all the data from carriers table joined with airways table
# save the first 1000 rows into a new table "table1"

dbCreateTable(conn=impalaconn,
                     name="carriers",
                     fields=carr,
                     temporary=F)

dbWriteTable(conn=impalaconn,
             name="carriers",
             value=carr,
             temporary=F,
             overwrite=T)


impalaconn %>% src_tbls()
impalaconn %>% tbl("carriers") %>% glimpse()

airways = impalaconn %>% tbl("airways")
carriers = impalaconn %>% tbl("carriers")
    
airways %>% left_join(carriers, by=c("uniquecarrier"="code")) %>% 
                      head(1000000) %>% 
                      compute(name="table1", temporary=FALSE)


table1 = impalaconn %>% tbl("table1")

table1 %>% head()


table1%>% group_by(description) %>% 
  summarize(count=n()) %>% head()


dbRemoveTable(impalaconn, "table1")

########## Challenge ###############

# import your airports table into impala
# join your airways table with airports from origin column
# save the first 1000 rows into a new table "table2"









