library(tabplotd3)


diamonds=read.csv("C://Users//user//Desktop//DataBases//datasets//BigData//csv_files//diamonds.csv")

# five columns, where the data is sorted on price
tableplot(diamonds, select = c(carat, price, cut, color, clarity), sortCol = price)

# We can focus our attention to the 5% most expensive diamond
tableplot(diamonds, select = c(carat, price, cut, color, clarity), sortCol = price, 
          from = 0, to = 5)

# filtering data of premium cut diamonds that cost less than 5000$.
tableplot(diamonds, subset = price < 5000 & cut == "Premium")


#### layout options

tableplot(diamonds, 
          select = 1:7, 
          fontsize = 14, 
          legend.lines = 8, 
          title = "Shine on you crazy Diamond", 
          fontsize.title = 18)

tableSave(tab, 
          filename = "diamonds.png", 
          width = 5, 
          height = 3, 
          fontsize = 6, 
          legend.lines = 6)


############### d3 layout

itabplot(diamonds)

####################################################################################3
#                             Tabplot Objects
#####################################################################################

tab=tableplot(diamonds, plot=F)
summary(tab)

