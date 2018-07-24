#Created by Andreas Stroehlein
#24.07.2018
#RNA-Seq Workshop Federation University, Ballarat, 25.07.2018

#Where to find help:
#https://community.rstudio.com/
#https://stackoverflow.com
#Try to google your problem/error as descriptive as possible first before posting a new question
#Chances are someone else ran into the same problem before you

#A simple tutorial going over most of the stuff we will cover this morning:
#https://rpubs.com/bradleyboehmke/data_wrangling

#If you don't know what a function does, simply type ?<function name> or ??<function name> into your console (bottom left) and
#you will find information about it in your help panel (bottom right)

#Cheatsheets:
#https://www.rstudio.com/resources/cheatsheets/

#For today's session the following four "cheatsheets" will be useful:
#RStudio IDE, if you are having trouble navigating RStudio
#https://github.com/rstudio/cheatsheets/raw/master/rstudio-ide.pdf

#Data wrangling with dplyr and tidyr
#https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

#Data transformation (dplyr)
#https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf

#Data visualisation (ggplot)
#https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf

#Let's start by loading the tidyverse package, if you haven't done so already:
library(tidyverse)

#You can check which packages are currently loaded in your R session by running:
sessionInfo()
#You can check which packages are installed by running:
installed.packages()
#You can also check the "Packages" tab on the right hand side


#Recap: Concept of tidy data
#Every variable has its own column and every observations or value has its own row
#Looking at the dataset mtcars
mtcars
#What is "untidy" about this data set?
#How can we fix this?

#Using the tidyverse to clean up data
#The principle of the tidyverse is that you start with a dataset (a "tibble" or a "tidy" form of data table) and then transform
#this data set step by step using the pipe symbol " %>% "
#Pipe shortcut (%>%): CTRL/CMD + Shift + M
#functions are usually verbs (or type conversion functions starting with "as_")
#that describe quite intuitively what is done with the data set coming into the function from the left-hand side
# "." is a placeholder for "dataset coming in from the left-hand side"
#These are all equivalent
mtcars %>% View()
mtcars %>% View(.)
View(mtcars)

#In this case we start with an untidy (mtcars) data set and clean it up using three functions
#Remember, if we want to save our transformation to the same data set or to a new data set
#we can do so by using the "<-" assignment operator
#In this case we will store the tidied data into a new dataset called mt_cars
mt_cars <- mtcars %>%
  mutate(make.model=rownames(mtcars)) %>%
  separate(make.model, c("make","model"), extra = "merge", fill = "right") %>%
  as_tibble()

#To be able to monitor each step of a tidyverse pipeline I recommend installing the following add-in for RStudio
#https://github.com/daranzolin/ViewPipeSteps
devtools::install_github("daranzolin/ViewPipeSteps")
#To see what each step in a tidyverse pipeline does, simply select the command and select "View Pipe Chain Steps" in the "Addins"
#drop-down menu at the top of your RStudio window
#Will show you a tibble (as a new tab in your top window via View()) for each "pipe"-step in your tidyverse "pipe"-line

mt_cars

#The result of a tidy transformation will (almost) always be a tidy data frame (tibble), even if it is a single value
mt_cars %>% count()

#To get the value itself (as a vector) we can use pull()
mt_cars %>% count() %>% pull()
#[1] 32
#this is a vector, not a dataframe

#subset dataset to only include particular VARIABLES (columns) using select
mt_cars %>% select(mpg,cyl) %>%
  #order dataset according to values in a particular column:
  arrange(mpg)

#inverse ordering
mt_cars %>% select(mpg,cyl) %>% arrange(desc(mpg))
#easy to see that the cars with less cylinders have a better fuel efficieny (i.e. higher mpg value)

#filter a data set for VALUES with certain properties
mt_cars %>% filter(is.na(model))
#Omit any data records (rows) for which there is no information on any of the variables
mt_cars %>% na.omit() %>% count()

#Task: Calculate the average consumption in mpg, grouped by number of cylinders
mt_cars %>% group_by(cyl)
# Groups:   cyl [3]
#How many different cylinder types are there?
mt_cars %>% distinct(cyl)
mt_cars %>% count(cyl)

avg_mpg_cyls <- mt_cars %>% group_by(cyl) %>% summarise(avg_mpg = mean(mpg))

#average engine size (displacement in cubic inches) grouped by cylinders
avg_displ_cyls <- mt_cars %>% group_by(cyl) %>% summarise(avg_displ = mean(disp))
  
#Join those two tables together using join
full_join(avg_mpg_cyls, avg_displ_cyls, by = "cyl")

#we could have done this directly within the second calculation using left_join()
mt_cars %>% group_by(cyl) %>% summarise(avg_displ = mean(disp)) %>% left_join(., avg_mpg_cyls, by = "cyl")
#or using right_join()
mt_cars %>% group_by(cyl) %>% summarise(avg_displ = mean(disp)) %>% right_join(avg_mpg_cyls, ., by = "cyl")

#########################################################################################


#Because the mtcars data set is small and not tidy, we will use another, slightly tidier and larger dataset called mpg (miles per gallon)
#that contains information of cars and their fuel consumption from two years, 1999 and 2008
#Let's see what it looks like:
#Show first ten entries and some additional information in the console
mpg
#Show data table
mpg %>% View()

#Information about the data and its variables:
?mpg
#manufacturer
#model
#displ - displacement
#year
#cyl - number of cylinders
#trans - type of transmission
#drv - drive train (front, rear, 4WD)
#fl - fuel type
#class
#cty - consumption in the city in mpg
#hwy - consumption on the highway in mpg

#Let's check if this data set has any duplicates
mpg %>% count()
mpg %>%  distinct() %>%  count()
#225

#Let's remove the duplicates by writing the filtered data back into the same data frame "mpg"
mpg <- mpg %>% distinct()
#Note that we are overwriting the original mpg dataset here
#calling rm(mpg) will remove this dataset and subsequently, calling mpg will be reset to the original dataset we started off with

#Now let's check if there are multiple cty and hwy values reported for the same car, model, engine type, year etc.
#select everything but the "cty" and "hwy" column and count.
mpg %>% select(-cty, -hwy) %>% distinct %>%  count()
#223
#this means that there are some data records that are exact duplicates but have two hwy or cty values reported for it (thus making the row unique as long as we include all columns)
#Let's identify these records

############Use "View Pipe Chains" here to see what the individual steps do

mpg %>%
  #group the data set by all variables except hwy and cty
  group_by_at(vars(-hwy, -cty)) %>%
  #count the number of elements in each group and assign it to the new variable (column) n
  summarise(n = n()) %>%
  #now filter for any groups that have more than 1 elements
  filter(n > 1) %>% 
  
#   # A tibble: 2 x 10
#   # Groups:   manufacturer, model, displ, year, cyl, trans, drv, fl [2]
#   manufacturer model       displ  year   cyl trans      drv   fl    class          n
# <chr>        <chr>       <dbl> <int> <int> <chr>      <chr> <chr> <chr>      <int>
#   1 dodge        caravan 2wd   3.8  1999     6 auto(l4)   f     r     minivan        2
# 2 honda        civic         1.6  1999     4 manual(m5) f     r     subcompact     2
  
  #now remove the n columm (as it doesn't exist in the original data set)
  select(-n) %>% 
  #join this data set with the original mpg table to see what these two values for hwy of the dodge caravan are:
  #left_join means match all variable values from the table coming from the left with the table supplied in the parentheses, in our case mpg
  left_join(mpg)

# # A tibble: 4 x 11
# # Groups:   manufacturer, model, displ, year, cyl, trans, drv, fl [?]
# manufacturer model       displ  year   cyl trans      drv   fl    class        cty   hwy
# <chr>        <chr>       <dbl> <int> <int> <chr>      <chr> <chr> <chr>      <int> <int>
#   1 dodge        caravan 2wd   3.8  1999     6 auto(l4)   f     r     minivan       15    22
# 2 dodge        caravan 2wd   3.8  1999     6 auto(l4)   f     r     minivan       15    21
# 3 honda        civic         1.6  1999     4 manual(m5) f     r     subcompact    28    33
# 4 honda        civic         1.6  1999     4 manual(m5) f     r     subcompact    25    32


#To make this data set really tidy, we will just take the average of the hwy and cty values for those cars with more than one entry:
mpg %>% group_by_at(vars(-hwy, -cty)) %>% summarise(hwy = mean(hwy), cty = mean(cty)) %>%  ungroup() %>%
# and then filter for the duplicate cars to check everything was done correctly
#Note that you can use filter() with as many conditions as you like in one function call, and even supply multiple possible values for each variable using the %in% operator like so:
filter(manufacturer %in% c("dodge","honda"), model %in% c("caravan 2wd", "civic"), displ %in% c(3.8, 1.6), trans %in% c("auto(l4)", "manual(m5)"))
#Warning: his filtering steps returns two other car entries due to the way we filtered here, but let's not worry about these
#Important is here that our transformation created a single entry for the two cars of interest

# manufacturer model       displ  year   cyl trans      drv   fl    class        hwy   cty
# <chr>        <chr>       <dbl> <int> <int> <chr>      <chr> <chr> <chr>      <dbl> <dbl>
#1 dodge        caravan 2wd   3.8  1999     6 auto(l4)   f     r     minivan     21.5  15  
#and
#4 honda        civic         1.6  1999     4 manual(m5) f     r     subcompact  32.5  26.5
# with the averaged of the original values

#Let's overwrite mpg to get a final clean data set
#again we group data by everything but hwy, apply our summary function, then ungroup
mpg <- mpg %>% group_by_at(vars(-hwy, -cty)) %>% summarise(hwy = mean(hwy), cty = mean(cty)) %>%  ungroup()


#Using this data set we will go through some of the main functionalities of the dpolyr and tidyr packages again
#as we try to visualise some of the data using ggplot

#Every graph usually has four main components:
#1. a data set - check

#2. a coordinate system (created by the ggplot() call)
#3. a so-called geom, which represent visual marks/points in that coordinate system that represent the data points
#Every geom has one or more visual properties, for example x value, y value, size of a point, color of a point etc.
#4.Your data can be mapped to visual properties by using the aes() (for "aesthetics") function.

#A ggplot command always has a ggplot() function at the start and is then extended by one or more layers of geom_ functions
#by means of a "+". Be careful not to use "%>%" here (The resulting error message will suggest that you made this mistake if you do)
#you can define aes() in your ggplot() call and these aesthetics then are applied to all following geom_ calls
#First, we use the geom_bar function which is a simple function that counts the number of data records based on a discrete variable
#Such a discrete variable could for example be the fuel type
mpg %>% distinct(fl)
mpg %>% ggplot(aes(fl)) + geom_bar()
#will do the same as
mpg %>% ggplot() + geom_bar(aes(fl))
#whereas the aes() defined in the geom_ function  will always overwrite the one defined in the ggplot() function
mpg %>% ggplot(aes(fl)) + geom_bar(aes(x = manufacturer))
#What we can see here is that although the discrete variable "manufacturer" is plotted,
#the name of the axis is defined by the ggplot() call,
#as this call creates the coordinate system
#The following will create the right label:
mpg %>% ggplot() + geom_bar(aes(x = manufacturer))
#press Tab to see what the value in your aes() function maps to. You can leave this out but for beginners it is nice to see
#that aes(manufacturer) means aes(x = manufacturer) means "I am mapping the counts of the discrete variable "manufacturer" to the
# x axis in my coordinate system
#As a general tip, hitting Tab once you have typed the openeing parenthesis of a function will show you the parameter options

#Let's do something a bit more involved now.
#Suppose we want to know whether the average city fuel consumption has increased, decreased or stayed the same between the 1999 and 2008 data sets
#Which variables do we want to map? And how? So we want to compare 1999 and 2008 which is stored in the variable "year"
mpg %>% select(year)

#two ways of exploring what the year variable looks like
mpg %>% group_by(year) %>% count()
mpg %>% count(year)

#some other uses of the summary functions, finding minima and maxima for variables
mpg %>% group_by(year) %>% summarise(min_cty = min(cty))
mpg %>% group_by(year) %>% summarise(max_cty = max(cty))

#if we have a look at the data type of "year" we can see that it is of type integer
#let's see what problem this causes when we try to plot this data
mpg %>% ggplot(aes(x = year, y = cty, group = year)) + geom_boxplot()

#to get rid of this issue, we can create a new column which encodes the year as a discrete variable (1999 or 2008), not an integer
mpg <- mpg %>% mutate(year.fact = as.factor(year))

#calling the same plot then gives us:
mpg %>% ggplot(aes(x = year.fact, y = cty, group = year.fact)) + geom_boxplot()

#if we want to what these boxplots are made up of we can use a different function, geom_dotplot():
mpg %>% ggplot(aes(x = year.fact, y = hwy, group = year.fact)) + geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.5)
#check ?geom_dotplot for parameter options

#Now let's have a look at city mpg vs. highway mpg
#Similar to what we did for the two years before, we need a variable (column) that contains either "city" or "highway" as a discrete value
#To get this column we can use the gather() function from the dplyr package to transform our data from wide to long format
mpg <- mpg %>% gather(key = "cty_hwy", value = "consumption", c(cty, hwy))
#We create a new column called "cty_hwy"
#which will be filled with the column names of the columns we selected (here we select cty and hwy using c(cty, hwy))
#the values originally present in cty and hwy are now put into a new column "consumption"
#Note that our entire table is now twice the size (446 instead of 223) as we have put the values for cty and hwy into a row each (long format) compared to
#one row in our original table

#To revert this (convert back from long format to wide), or in a case where the opposite tranformation is required, we can use the spread() function
mpg %>% spread(key = "cty_hwy", value = "consumption")

#But for now we leave our data in the long format, and get back to plotting. Now that we have our data in the format we want, we can apply the same function as before
# our x aesthetic will be mapped to cty_hwy instead of year and we will group our data by the same variable
# the y variable will become the "consumption"
mpg %>% ggplot(aes(x = cty_hwy, y = consumption, group = cty_hwy)) + geom_boxplot()
#Suppose we now also want to additionally visualise this data to reflect the two different years as we did before
#We can then create two plots next to each other using a function called facet_grid() and define by which variable we want to group the data
mpg %>% ggplot(aes(x = cty_hwy, y = consumption, group = cty_hwy)) + geom_boxplot() + facet_grid("year.fact")
#showing that the average consumption for both cty and highway has stayed almost the same between the two years

#Let's ask another question:
#Do certain manufacturers have higher mpg than others, on average?
#Let's see how we can easily add another variable (the manufacturer) to the plot using the facet_grid() function
#We simply plot the same plot as before but add another element in the last call, plotting the manufacturer vs. year
mpg %>% ggplot(aes(x = cty_hwy, y = consumption, group = cty_hwy)) + geom_boxplot() + facet_grid(manufacturer ~ year.fact)

#In our small Plot preview on the right, this is not a very nice representation of our data, but if we write this to a file with the right dimensions, it will automatically be correctly scaled
#and can be viewed perfectly. For this we use the function ggsave(), which saves the last plot that was created into a file in the current working directory
ggsave("mpg_cty_hwy_by_manufacturer_and_year.pdf", device = "pdf", width = 5, height = 30)

#you can always change the "cosmetics" of your plot at the end of a plotting command
#I like to use the + theme_minimal() command at the end of my plots to get rid of unnecessary background color and lines
#here we have also added two different colors for the years to distinguish them more easily
mpg %>% ggplot(aes(x = cty_hwy, y = consumption, group = cty_hwy, colour = cty_hwy)) + geom_boxplot() + facet_grid(manufacturer ~ year.fact) + theme_minimal()

#Refer to your ggplot cheatsheet for additional preset themes to choose from and other options to make your plot publication-ready
#See the "Position Adjustments", "Themes", "Labels", "Legends" and "Zooming" sections

#Some of us might find the mpg (miles per gallon) misleading because the lower the value, the worse the fuel efficiency

#So let's write a function to convert mpg to lphk (liters per 100 km)
mpgToLphk <- function(mpg_val)
{
#1 mile = 1.60934 km
#1 gallon = 3.78541 ltr

#kilometers per litre
kpl <- (mpg_val * 1.60934) / 3.78541

#litres per 100km
lphk <- 100 / kpl

return(lphk)
}

# To see what a function does just call it in your console with out the parentheses so for example to see what
# mpgToLphk() does call mpgToLphk which will show you the code that this function calls upon execution
mpgToLphk

mpg <- mpg %>% mutate(lphk = mpgToLphk(consumption))
mpg

mpg %>% ggplot(aes(x = cty_hwy, y = lphk, group = cty_hwy)) + geom_boxplot() + facet_grid(manufacturer ~ year.fact)
ggsave("lphk_cty_hwy_by_manufacturer_and_year.pdf", device = "pdf", width = 5, height = 30)


#In the case of two continous variables (a number on a continous axis) we might want to see if they are somehow correlated so
#we might want to plot one of them on the x axis and one on the y axis
#Let's transfer our cty_hwy variable back into two separate columns:
#and then plot 
mpg <- mpg %>% select(-consumption) %>% spread(key = "cty_hwy", value = "lphk")
mpg %>% ggplot(aes(x = cty, y = hwy, colour = year.fact)) + geom_point()
#this doesn't show us a lot as all the data points are on top of each other
#let's use a different function, geom_smooth(). Running
?geom_smooth
#tells us:
#Smoothed conditional means
#Aids the eye in seeing patterns in the presence of overplotting. 
#Nice, that's exactly what we want
mpg %>% ggplot(aes(x = cty, y = hwy, colour = year.fact)) + geom_smooth()
#what does this tell us?
#There seems to be a near-linear relationship between city and hwy fuel consumption
#The steep line is a bit misleading here, suggesting that hwy consumption is > than city consumption (note, we are now plotting lphk, so the city value should be higher than the hwy value)
#The problem is that the length of the y axis (5-20) is almost twice that of the x axis (5-25)
#Let's fix this by setting the aspect ratio of x and y axis to 1
mpg %>% ggplot(aes(x = cty, y = hwy, colour = year.fact)) + geom_smooth() + coord_fixed(1)
#Now we have a flatter line, as expected, and have provided a more honest representation of the data

#Looks like in 1999 there were actually no cars with really high fuel consumptions (> 21ltr in the city and > 15ltr on the hwy)
#Quick check
mpg %>%  filter(year == 1999) %>% summarise(cty = max(cty), hwy = max(hwy))
#whereas for 2008
mpg %>%  filter(year == 2008) %>% summarise(cty = max(cty), hwy = max(hwy))

#Remember:
#before you think about plotting think about how many different variables do you want to represent and how they are going to be encoded
#in your plot, i.e. as a color, as a certain dot size, in a certain position (e.g. x or y axis). These are your aesthetics that you have to define and it might require reorganising your data

##############################################################################################

#For the afternoon we need to install the following packages from Bioconductor:
source("https://bioconductor.org/biocLite.R")
biocLite("edgeR")
biocLite("org.Hs.eg.db")
#say "yes" or "a" (all) when prompted during installation

#Next we need to install Brendan's package ("tidyGE_0.0.0.9000.tar.gz")
install.packages("tidyGE_0.0.0.9000.tar.gz", repos = NULL, type="source")