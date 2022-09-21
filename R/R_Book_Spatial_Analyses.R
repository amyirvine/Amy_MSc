library(knitr)
library(rmarkdown)
install.packages("tmap", dependencies = T)
install.packages("GISTools", depend = T)

R_object <- value

# examples of simple assignment
x <- 5 
y <- 4 
# the variables can be used in other operations
x+y
# including defining new variables
z <- x + y
z
# which can then be passed to other functions
sqrt(z)

# example of Vector assignment
tree.heights <- c(4.3,7.1,6.3,5.2,3.2,2.1)
tree.heights

tree.heights**2

sum(tree.heights)
mean(tree.heights)
max.height <- max(tree.heights)
max.height

#Sub-setting
tree.heights [1]    # first element
tree.heights[1:3]   # a subset of elements 1 to 3
sqrt(tree.heights[1:3]) #square roots of the subset
tree.heights[c(5,3,2)]  # a subset of elements 5,3,2: note the ordering
tree.heights 

# examples of Character Variable assignment
name <- "Lex Comber" 
name

# these can be assigned to a vector of character variables
cities <- c("Leicester","Newcastle","London","Leeds","Exeter")
cities
length(cities)
# an example of a Logical Variable
northern <- c(FALSE, TRUE, FALSE, TRUE, FALSE)
northern

# this can be used to subset other variables
cities[northern]

#DATA TYPES
character(8) #these are empty quotes

# conversion to actual characrer
as.character("8") 

# tests
is.character(8) #False because just quotes
is.character("8") #true because a value is now a character
#character can be anything 


#NUMERICS
#similar to as.double and as.real

numeric(8)
# conversions
as.numeric(c("1980","-8","Geography"))
as.numeric(c(FALSE,TRUE))
# tests
is.numeric(c(8, 8))
is.numeric(c(8, 8, 8, "8")) #the 8 in quotes is now a character

#LOGICAL FUNCTIONS
#vector of specified length
#default = elements set to equal false
#any TRUE, True, true or number other than 0 = set to TRUE
#any FALSE, false, False and zero = set to false
#any words set to NA
#is.logical returns TRUE or FALSE
logical(7)
# conversion
as.logical(c(7, 5, 0, -4,5))
# TRUE and FALSE can be converted to 1 and 0
as.logical(c(7,5,0,-4,5)) * 1
as.logical(c(7,5,0,-4,5)) + 0
    ##different ways to declare T or F

as.logical(c("True","T","FALSE","Raspberry","9","0", 0))

#Consider for spatial data
data <- c(3, 6, 9, 99, 54, 32, -102)
# a logical test 
index <- (data > 10)
index
# used to subset data
data[index]
sum(data)
sum(data[index])

#DEFINING VECTORS
vector(mode = "numeric", length = 8)
vector(length = 8)
# testing and conversion
tmp <- data.frame(a=10:15, b=15:20) #a and b are columns
is.vector(tmp)
as.vector(tmp)

#MATRICES
#must include info for the number of columns and number of rows
# defining matrices
matrix(ncol = 2, nrow = 0)
matrix(1:6)
matrix(1:6, ncol = 2)
# conversion and test
as.matrix(6:3)
is.matrix(as.matrix(6:3))

flow <- matrix(c(2000, 1243, 543, 1243, 212, 545, 
                 654, 168, 109), c(3,3), byrow=TRUE) #c is 3 across and 3 down
# Rows and columns can have names, not just 1,2,3,...
colnames(flow) <- c("Leeds", "Maynooth"," Elsewhere")
rownames(flow) <- c("Leeds", "Maynooth", "Elsewhere")
# examine the matrix
flow 
# and functions exist to summarise
outflows <- rowSums(flow) 
outflows

#If data class is not a matrix, then use names rather than rownames or colnames
z <- c(6,7,8)
names(z) <- c("Newcastle","London","Manchester")
z

#FACTORS
#levels of the data are defined
#could be as.factor or as.ordered
# a vector assignment
house.type <- c("Bungalow", "Flat", "Flat", 
                "Detached", "Flat", "Terrace", "Terrace")
# a factor assignment
house.type <- factor(c("Bungalow", "Flat", "Flat", 
                       "Detached", "Flat", "Terrace", "Terrace"), 
                     levels=c("Bungalow","Flat","Detached","Semi","Terrace"))
house.type
# table can be used to summarise
table(house.type)
# 'levels' control what can be assigned 
house.type <- factor(c("People Carrier", "Flat", 
                       "Flat", "Hatchback", "Flat", "Terrace", "Terrace"), 
                     levels=c("Bungalow","Flat","Detached","Semi","Terrace"))
house.type
#good for when data must fit into discrete classes

#ORDERING DATA
#good for organizing from bad to good or lower to higher
income <-factor(c("High", "High", "Low", "Low", 
                  "Low", "Medium", "Low", "Medium"), 
                levels=c("Low", "Medium", "High"))
income >  "Low"

# 'levels' in 'ordered' defines a relative order 
income <-ordered(c("High", "High", "Low", "Low", 
                   "Low", "Medium", "Low", "Medium"), 
                 levels=c("Low", "Medium", "High"))
income > "Low" #income greater than low

sort(income) #tells you the order and can be used to verify the order

#2.3.2.5 Lists
#can be used to gather all different data types together
tmp.list <- list("Lex Comber",c(2015, 2018), 
                 "Lecturer", matrix(c(6,3,1,2), c(2,2)))
tmp.list

tmp.list[[4]] #this selects a feature in the list

#give the list & items within a real name
employee <- list(name="Lex Comber", start.year = 2015, 
                 position="Professor")
employee

#Can also join lists
append(tmp.list, list(c(7,6,9,1)))

# use "lappy" with different functions to assign them
lapply(tmp.list[[2]], is.numeric)
lapply(tmp.list, length)

#Example for defining my own classes
employee <- list(name="Lex Comber", start.year = 2015, 
                 position="Professor")
#assign this to a new class
class(employee) <- "staff"

print.staff <- function(x) {
  cat("Name: ",x$name,"\n") #where x is the "Name"
  cat("Start Year: ",x$start.year,"\n") #Where x is the Start Year
  cat("Job Title: ",x$position,"\n")}

# an example of the print class 
print(employee)

#Can unclass variables 
print(unclass(employee))

#2.3.2.7 Classes in Lists
#Variables can be defined to new or user-defined class objets
#this below function defines a new "staff" object
new.staff <- function(name,year,post) {
  result <- list(name=name, start.year=year, position=post)
  class(result) <- "staff"
  return(result)}
  #then define the list

leeds.uni <- vector(mode='list',3)
# assign values to elements in the list
leeds.uni[[1]] <- new.staff("Heppenstall, Alison", 2017,"Professor")
leeds.uni[[2]] <- new.staff("Comber, Lex", 2015,"Professor")
leeds.uni[[3]] <- new.staff("Langlands, Alan", 2014,"VC")

leeds.uni

###2.3.2.8 Data.frame vs tibble
#data table:
# rows = some real world feature i.e. person, transaction, date
# columns = some attribute associated with that feature
#Data can also be a matrix - but these can only hold one type of data (integer OR logical Or character)
#data.frame and tibble can hold different data types in different columns/fields
#tibbles can be used to hold spatial data from sf, sp
#In spatial data tables, each record typically represents some real world geological feature i.e. place, route, region 
  #and the attributes associated with that feature i.e. population, length, area

#DATAFRAME
#2D, the name of vectors = col names
df <- data.frame(dist = seq(0,400, 100),  #the seq means count by 100 between 0-400
                 city = c("Leeds", "Nottingham", "Leicester", "Durham", "Newcastle"))
str(df)

df$city # it has now put in that these are factors instead of characters

#If you don't want the data frame to automatically change the characters strings to factors
df <- data.frame(dist = seq(0,400, 100),  
                 city = c("Leeds", "Nottingham", "Leicester", "Durham", "Newcastle"),
                 stringsAsFactors = FALSE)
str(df)

#TIBBLE
tb <- tibble(dist = seq(0,400, 100),  
             city = c("Leeds", "Nottingham", "Leicester", "Durham", "Newcastle"))

#biggest criticism of data.frame is the partial matching issue
df$ci
tb$ci
#a tibble always returns a tibble but a dataframe can return a vector or data frame
#EXAMPLE BELOW
# 1 column
df[,2]
tb[,2]
class(df[,2])
class(tb[,2])
# 2 columns
df[,1:2]
tb[,1:2]
class(df[,1:2])
class(tb[,1:2])
#tibbles are lazy and do not revert variables to data classes that work
#Tibbles are better for finding errors early in the code

#convert between tibbles and data frames
data.frame(tb) 
as_tibble(df) 

names(tb)
colnames(tb)
rownames(tb)
length(tb)
ncol(tb)
nrow(tb)

#these can be subsetted using the square brackets
#can be combined using cbind or rbind
cbind(df, Pop = c(700,250,230,150,1200))
cbind(tb, Pop = c(700,250,230,150,1200))

vignette("tibble")#the info about tibbles

#2.3.3 Self-Test
colours <- factor(c("red","blue","red","white",
                    "silver","red","white","silver",
                    "red","red","white","silver","silver"),
                  levels=c("red","blue","white","silver","black"))
colours[4] <- "orange"
colours

table(colours)

colours2 <-c("red","blue","red","white",
             "silver","red","white","silver",
             "red","red","white","silver")
# Now, make the table
table(colours2)

#Practicing with factors
car.type <- factor(c("saloon","saloon","hatchback",
                     "saloon","convertible","hatchback","convertible",
                     "saloon", "hatchback","saloon", "saloon",
                     "saloon","hatchback"),
                   levels=c("saloon","hatchback","convertible"))
table(car.type, colours) #these are based on the order you wrote the characters in the list
crosstab <- table(car.type,colours)

#Question 3
engine <- ordered(c("1.1litre","1.3litre","1.1litre",
                    "1.3litre","1.6litre","1.3litre","1.6litre",
                    "1.1litre","1.3litre","1.1litre", "1.1litre",
                    "1.3litre","1.3litre"),
                  levels=c("1.1litre","1.3litre","1.6litre"))

#if the variables are ordered, can look to see which ones are greater than/less than
engine > "1.1litre"

#2.3.3.2 Matrices Self Question
dim(crosstab) # Matrix dimensions
rowSums(crosstab) # Row sums
colnames(crosstab) # Column names
apply(crosstab,1,max) #1 = row (2 would mean col), max = find max value in each row

#now 2 for columns
apply(crosstab,2,max)

#helpful feature is to know which place holds the largest number
example <- c(1.4,2.6,1.1,1.5,1.2)
which.max(example)

#Question 6
levels(engine)
#now of the colour levels, which one is the most observed in the columns and please give the name, not the placement
levels(colours)[which.max(crosstab[,1])]

#gives the same output
colnames(crosstab)[which.max(crosstab[,1])]
colnames(crosstab)

crosstab[,1] #of the first column, give the values for each row

which.max(crosstab[,1]) #of the first column, which has the max value

# Define the function
which.max.name <- function(x) {
  return(names(x)[which.max(x)])} 
# Next, give the variable 'example' names for the values
names(example) <- c("Bradford","Leeds","York",
                    "Harrogate","Thirsk") 
example

which.max.name(example) 

#Question 7
crosstab
which.max.name(apply(crosstab,1,max)) #which row has the most sold cars
which.max.name(apply(crosstab,2,max)) #which column has the most sold cars

#Question 8
new.sales.data <- function(colours, car.type) {
  xtab <- table(car.type,colours)
  result <- list(colour=apply(xtab,1,which.max.name),
                 type=apply(xtab,2,which.max.name),
                 total=sum(xtab))
  class(result) <- "sales.data"
  return(result)}

this.week <- new.sales.data(colours,car.type)
this.week

#this.week is a new variable of class sales.data

#Question 9
print.sales.data <- function(sales.data) {
  xtab <- table(car.type,colours)
  result <- list(colour=apply(xtab,1,which.max.name),
                 type=apply(xtab,2,which.max.name),
                 total=sum(xtab))
  class(result) <- "sales.data"
  return(result)}

print.sales.data <- function(x) {
  cat("Car Type: ",x$car.type,"\n") 
  cat("Colours: ",x$colours,"\n")
  }
print.sales.data(car.type, colours)#****

#2.4 PLOTS
x1 <- rnorm(100)
y1 <- rnorm(100)
plot(x1,y1)

#plot is default labelling axis by variable names
plot(x1,y1,pch=16, col='red') #pch = plot character, col = colour

plot(x1,y1,pch=16, col='red', type = 'l', lwd = 2) #type = line, lwd = plot line width

#Other examples
x2 <- seq(0,2*pi,len=100)
y2 <- sin(x2)
plot(x2,y2,type='l')
plot(x2,y2,type='l', lwd=3, col='darkgreen') 

plot(x2,y2,type='l', col='darkgreen', lwd=3, ylim=c(-1.2,1.2))
y2r <- y2 + rnorm(100,0,0.1)
points(x2,y2r, pch=16, col='darkred')

y4 <- cos(x2)
plot(x2, y2, type='l', lwd=3, col='darkgreen')
lines(x2, y4, lwd=3, lty=2, col='darkblue') #lty = specifies type of line
#lines can be used to add to previously made plot

#TIP: if you want two plots side by side, use mfrow
par(mfrow = c(1,2))
plot(x2, y2, type = 'l', lwd=3, col='darkgreen')
plot(x2, y2, type = 'l', col='darkgreen', lwd=3, ylim=c(-1.2,1.2))
points(x2, y2r, pch=16, col='darkred')
par(mfrow=c(1,1)) #resets par

#can also add polygons to the figure
#default, there are borders around the polygon but can get rid of these with 
x2 <- seq(0,2*pi,len=100)
y2 <- sin(x2)
y4 <- cos(x2)
# specify the plot layout and order
par(mfrow = c(1,2))
# plot #1
plot(y2,y4)
polygon(y2,y4,col='lightgreen') #adds to the first plot
# plot #2: this time with 'asp' to set the aspect ratio of the axes
plot(y2,y4, asp=1, type='n') #asp refers to making x and y axes to scale; type = n refers to drawing the axes to scale
polygon(y2,y4,col='lightgreen')


library(remotes)
install_version("GISTools", "0.7.1", depend = T)
library(GISTools)

#dataset georgia already exists
data(georgia)
  #does not load georgia.polys

# select the first element 
georgia.outline <- unionSpatialPolygons(georgia, rep(1,159))
plot(georgia.outline, asp=1, type='n', xlab="Easting", ylab="Northing")
  #georgia.polys does not work
  appling <- georgia.polys[[1]]
  # set the plot extent
  plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
# plot the selected features with hatching
polygon(georgia.outline, density=14, angle=135) 

#PLOT COLOURS
colours() # all R colours available

# set the plot extent
plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
# plot the points
points(x = runif(500,126,132)*10000, 
       y = runif(500,103,108)*10000, pch=16, col='red') 
# plot the polygon with a transparency factor
polygon(appling, col=rgb(0,0.5,0.7,0.4))

#Can add text, define the text size using cex (character expansion),and define the placement of text
plot(appling, asp=1, type='n', xlab="Easting", ylab="Northing")
polygon(appling, col="#B3B333")
# add text, sepcifying its placement, colour and size 
text(1287000,1053000,"Appling County",cex=1.5) 
text(1287000,1049000,"Georgia",col='darkred')
locator() #allows you to click in the plot and have the coordinates come up once you hit finish!!

#Can also use rect to plot rectangles for maps
plot(c(-1.5,1.5),c(-1.5,1.5),asp=1, type='n') 
# plot the green/blue rectangle
rect(-0.5,-0.5,0.5,0.5, border=NA, col=rgb(0,0.5,0.5,0.7)) #0 means transparent?
# then the second one
rect(0,0,1,1, col=rgb(1,0.5,0.5,0.7))

#Can also plot raster and tabular data
# load some grid data
data(meuse.grid)
# define a SpatialPixelsDataFrame from the data
mat = SpatialPixelsDataFrame(points = meuse.grid[c("x", "y")], 
                             data = meuse.grid)
# set some plot parameters (1 row, 2 columns)
par(mfrow = c(1,2))
# set the plot margins
par(mar = c(0,0,0,0))
# plot the points using the default shading
image(mat, "dist")
# load the package
library(RColorBrewer)
# select and examine a colour palette with 7 classes
greenpal <- brewer.pal(7,'Greens') 
# and now use this to plot the data
image(mat, "dist", col=greenpal)

#Gives a contour map of this same dataset
contour(mat, "dist")

#2.5 GGPLOT!!!!
##### DATA USED HERE IS OUTDATED
library(ggplot2) #is inside tidyverse anyways
library(tidyverse)

#qplot used for quick, simple plots
#elements in "theme" control the way the plot looks
#plot must be specified, then the type of graph i.e. line
qplot(x2,y2r,col=I('darkred'), ylim=c(-1.2, 1.2)) + 
  geom_line(aes(x2,y2), col=I("darkgreen"), size = I(1.5)) + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"))+
  theme_bw() 
 # theme_dark()

#to map appling, convert from a matrix to a data-frame whos elements need to be labelled
appling <- data.frame(georgia2)
colnames(georgia2) <- c("X", "Y")

# create the first plot with qplot
p1 <- qplot(X, Y, data = georgia2, geom = "polygon", asp = 1, 
            colour = I("black"),
            fill=I(rgb(0,0.5,0.7,0.4))) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20))

# create a data.frame to hold the points 
df <- data.frame(x = runif(500,126,132)*10000, 
                 y = runif(500,103,108)*10000)

# now use ggplot to contruct the layers of the plot
p2 <- ggplot(georgia2, aes(x = X, y= Y)) +
  geom_polygon(fill = I(rgb(0,0.5,0.7,0.4))) +
  geom_point(data = df, aes(x, y),col=I('red')) +
  coord_fixed() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20))
# finally combine these in a single plot 
# using the grid.arrange function
# NB you may have to install the gridExtra package
library(gridExtra) #contains the gridarrange package
grid.arrange(p1, p2, ncol = 2)

#Taking data from the georgia info and converting to a tibble
# data.frame
df <- data.frame(georgia)
# tibble
tb <- as_tibble(df)
tb
tb$rural <- as.factor((tb$PctRural > 50) +  0) #+0 is used to convert the values to either 1 or 0
levels(tb$rural) <- list("Non-Rural" = 0, "Rural"=1)

#then we use the MedInc variable (median county income) & create an income category variable
tb$IncClass <- rep("Average", nrow(tb)) #names all the rows "Average"
tb$IncClass[tb$MedInc >=  41204] = "Rich"
tb$IncClass[tb$MedInc <=  29773] = "Poor" #now renames the rows as either rich or poor

#check the number of rows in each column
table(tb$IncClass)

#scatterplots used to show two variables together 
#Examine PCTBach (bachelor's degrees) and PCTEld (elderly) which are the percentage of populations with each
ggplot(data = tb, mapping=aes(x=PctBach, y=PctEld)) + 
  geom_point()

#can then pass a grouping variable to the colour parameter in aes
ggplot(data = tb, mapping=aes(x=PctBach, y=PctEld, colour="rural")) + 
  geom_point()

#now add a trendline
ggplot(data = tb, mapping = aes(x = PctBach, y = PctEld)) + 
  geom_point() +
  geom_smooth(method = "lm")

#can also change style templates and colours
ggplot(data = tb, mapping = aes(x = PctBach, y = PctEld)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "red", fill = "lightsalmon") + 
  theme_bw() +
  xlab("% of population with bachelor degree") +
  ylab("% of population that are elderly") 

 #histograms are good for examining distributions of income across 159 countries of Georgia
ggplot(tb, aes(x=MedInc)) + 
  geom_histogram(, binwidth = 5000, colour = "red", fill = "grey")

#add a distribution curve
ggplot(tb, aes(x=MedInc)) + 
  geom_histogram(aes(y=..density..),
                 binwidth=5000,colour="white") +
  geom_density(alpha=.4, fill="darksalmon") +
  # Ignore NA values for mean
  geom_vline(aes(xintercept=median(MedInc, na.rm=T)),
             color="orangered1", linetype="dashed", size=1)

#Can also generate multiple plots using the facet() option
ggplot(tb, aes(x=PctBach, fill=IncClass)) +
  geom_histogram(color="grey30",
                 binwidth = 1) +
  scale_fill_manual("Income Class", 
                    values = c("orange", "palegoldenrod","firebrick3")) +
  facet_grid(IncClass~.) +
  xlab("% Bachelor degrees") +
  ggtitle("Bachelors degree % in different income classes")

#or can use boxplots
ggplot(tb, aes(x = "",PctBach)) + 
  geom_boxplot()

#and can be extended using grouping 
ggplot(tb, aes(IncClass, PctBach, fill = factor("rural"))) + 
  geom_boxplot() +
  scale_fill_manual(name = "Rural",
                    values = c("orange", "firebrick3"),
                    labels = c("Non-Rural"="Not Rural","Rural"="Rural")) +
  xlab("Income Class") +
  ylab("% Bachelors")

#2.6.1 reading in text files
# display the first six rows
head(appling)
# display the variable dimensions 
dim(appling)

#To assign names to the data columns
colnames(appling) <- c("X", "Y")

#Can also write the data into a CSV and read it back in in a different file type
write.csv(appling, file = "test.csv")
write.csv(appling, file = "test.csv", row.names = F)

#Can also read a csv into R
tmp.appling <- read.csv(file = "test.csv")
read.csv(file = "test.csv")

#Now reading in data files but first... 
#Saving data

# this will save everything in the workspace
save(list = ls(), file = "MyData.RData")
# this will save just appling
save(list = "appling", file = "MyData.RData")
# this will save appling and georgia.polys
save(list = c("appling", "georgia.polys"), file = "MyData.RData")

load("MyData.RData")

#LOADING SPATIAL DATA FILES
library(rgdal)

#write the geogia data into a shape file
writeOGR(obj=georgia, dsn=".", layer="georgia", 
         driver="ESRI Shapefile", overwrite_layer=T) 

#it can now be read back into R as a shape file using readOGR
new.georgia <- readOGR("georgia.shp") 

#Alternatively, can use sf package
library(sf)
g2 <- st_read("georgia.shp")
st_write(g2, "georgia.shp", delete_layer = T)

#CHAPTER 3
#3.1 OVERVIEW
#Rows = person/place/thing
#Columns = some attribute associated with that thing
#IN geographical data, the row = geographical location 
  #& column = point, line or area
#All data is spatio-temporal (collected at some place and some time)

#To check if a package is already installed:
is.element("raster", installed.packages()) 
library(raster)
help("raster")

data(newhaven)
ls()
class(breach) #SpatialPoints = without attributes
class(blocks) #SpatialPolygonsDataframe = polygon with attributes

head(data.frame(blocks)) #look at the first few rows of data for the blocks attribute
head(blocks@data) #this does the same as above

plot(blocks)

#This adds roads in red and blocks in black on same plot
par(mar = c(0,0,0,0))
plot(roads, col="red")
plot(blocks, add = T)

#In R, there are Points, Linestrings (sequence of points connected by straight line), polygon (2d), multipoint, multilinestrings, multipolygon

library(sf)
vignette(package = "sf") #vignette provide additional info to the help page
vignette("sf1", package = "sf")

# load the georgia data
data(georgia)
# conversion to sf
georgia_sf = st_as_sf(georgia)
class(georgia_sf)
georgia_sf #first 10 rows are shown
georgia

# all attributes
plot(georgia_sf) #shades the first few objects within the dataset
# selected attribute
plot(georgia_sf[, 6]) #choosing the sixth column attribute
# selected attributes
plot(georgia_sf[,c(4,5)]) #plots two attributes bc concatenate

#Compare the headings of sp vs sf
## sp SpatialPolygonDataFrame object 
head(data.frame(georgia))
## sf polygon object
head(data.frame(georgia_sf))
  #dataframes of the sf objects have geometry attributes
  #can convert sp by using the as function
g2 <- as(georgia_sf, "Spatial") #changes it from sf to sp
class(g2)

roads_sf <- st_as_sf(roads)
class(roads_sf)
r2 <- as(roads_sf, "Spatial")
class(r2)
r2 <- as(roads, "sf")

#3.3 reading to and writing from sp format
library(rgdal)

writeOGR(obj=georgia, dsn=".", layer="georgia", 
         driver="ESRI Shapefile", overwrite_layer=T) 
#this can now be used in ArcGIS
new.georgia <- readOGR("georgia.shp") 
class(new.georgia)

writeOGR(new.georgia, dsn = ".", layer = "georgia", 
         driver="ESRI Shapefile", overwrite_layer = T) #overwrite meaning it will write over existing similarly named file
#dsn = working directory it will be saved to - where "." is the current working directory

td <- getwd()
writeOGR(new.georgia, dsn = td, layer = "georgia", 
         driver="ESRI Shapefile", overwrite_layer = T)

#Can also use readGDAL and writeGDAL for raster files

#READING TO AND WRITING FROM SF FORMAT
g2 <- st_read("georgia.shp")
#To write a sf feature to a file, it needs the object and the filename

st_write(g2, "georgia.shp", delete_layer = T)
#need to delete the layer because the geogia shape file exists in the working directory

#Below command is same as above with the one below writes out the defaults
st_write(g2, dsn = "georgia.shp", layer = "georgia.shp", 
         driver = "ESRI Shapefile", delete_layer = T)

#The sf2 vignette has the prefix options (e.g., ".shp")
vignette("sf2", package = "sf")

#3.4 MAPPING: AN INTRODUCTION TO tmap
#tmap can take sf or sp objects
rm(list=ls())

#qtm = quick tmap
data(georgia)
georgia_sf <- st_as_sf(georgia) 

install.packages("tmap")
library(tmap)
qtm(georgia, fill = "red", style = "natural")
#fill can be a colour or variable

qtm(georgia_sf, fill="MedInc", text="Name", text.size=0.5, 
    format="World_wide", style="classic", 
    text.root=5, fill.title="Median Income")

#3.4.3 Full tmap
#can add a series of layers to the map
#tm_shape = data plotted
#tm_aes = what is plotted

# do a merge
g <- st_union(georgia_sf)
# for sp
# g <- gUnaryUnion(georgia, id = NULL)

# plot the spatial layers
tm_shape(georgia_sf) +
  tm_fill("tomato")

tm_shape(georgia_sf) +
  tm_fill("tomato") +
  tm_borders(lty = "dashed", col = "gold") #added gold borders

tm_shape(georgia_sf) +
  tm_fill("tomato") +
  tm_borders(lty = "dashed", col = "gold") +
  tm_style("natural", bg.color = "grey90") #background colour

tm_shape(georgia_sf) +
  tm_fill("tomato") +
  tm_borders(lty = "dashed", col = "gold") +
  tm_style("natural", bg.color = "grey90") +
  # now add the outline
  tm_shape(g) +
  tm_borders(lwd = 2) #now adding an outline

#Now the title
tm_shape(georgia_sf) +
  tm_fill("tomato") +
  tm_borders(lty = "dashed", col = "gold") +
  tm_style("natural", bg.color = "grey90") +
  # now add the outline
  tm_shape(g) +
  tm_borders(lwd = 2) +
  tm_layout(title = "The State of Georgia", 
            title.size = 1, title.position = c(0.55, "top"))
is.element("grid", installed.packages())
library(grid)

#So now want to plot two plots side by side using different data
# 1st plot of georgia
t1 <- tm_shape(georgia_sf) +
  tm_fill("coral") +
  tm_borders() +
  tm_layout(bg.color = "grey85") 
# 2nd plot of georgia2
t2 <- tm_shape(georgia2) +
  tm_fill("orange") +
  tm_borders() +
  # the asp paramter controls aspect
  # this is makes the 2nd plot align
  tm_layout(asp = 0.86,bg.color = "grey95")

library(grid)
# open a new plot page
grid.newpage()
# set up the layout
pushViewport(viewport(layout=grid.layout(1,2))) # set it up with 1 row, 2 columns
# plot using the print command
print(t1, vp=viewport(layout.pos.col = 1, height = 5))
print(t2, vp=viewport(layout.pos.col = 2, height = 5))

data.frame(georgia_sf)[,13]
#these are the attributes of the 13th column
#to display the text on the map:

tm_shape(georgia_sf) +
  tm_fill("white") +
  tm_borders() +
  tm_text("Name", size = 0.3) + #where name is a column
  tm_layout(frame = FALSE)

#To subset the data, pull out the names you want
index <- c(81, 82, 83, 150, 62, 53, 21, 16, 124, 121, 17)
georgia_sf.sub <- georgia_sf[index,]

tm_shape(georgia_sf.sub) +
  tm_fill("gold1") +
  tm_borders("grey") +
  tm_text("Name", size = 1) +
  # add the outline
  tm_shape(g) +
  tm_borders(lwd = 2) +
  # specify some layout parameters
  tm_layout(frame = FALSE, title = "A subset of Georgia", 
            title.size = 1.5, title.position = c(0., "bottom"))

#Notice how tmap subgroups into the layers
# the 1st layer
tm_shape(georgia_sf) +
  tm_fill("white") +
  tm_borders("grey", lwd = 0.5) +
  # the 2nd layer
  tm_shape(g) +
  tm_borders(lwd = 2) +
  # the 3rd layer
  tm_shape(georgia_sf.sub) +
  tm_fill("lightblue") +
  tm_borders() +
  # specify some layout parameters
  tm_layout(frame = T, title = "Georgia with a subset of counties", 
            title.size = 1, title.position = c(0.02, "bottom"))

#3.4.4 Adding context
#Usually a map with more info is better
#Google Maps, OpenStreetMap, Leaflet
install.packages(c("OpenStreetMap"),depend=T) 

library(OpenStreetMap)
# define upper left, lower right corners 
georgia.sub <- georgia[index,]
ul <- as.vector(cbind(bbox(georgia.sub)[2,2], 
                      bbox(georgia.sub)[1,1]))
lr <- as.vector(cbind(bbox(georgia.sub)[2,1], 
                      bbox(georgia.sub)[1,2]))
# download the map tile
MyMap <- openmap(ul,lr)
# now plot the layer and the backdrop
par(mar = c(0,0,0,0))
plot(MyMap, removeMargin=FALSE)
plot(spTransform(georgia.sub, osm()), add = TRUE, lwd = 2)
#this spTransform command transforms the subset into spatial data
  #this makes it easier to work with

install.packages(c("RgoogleMaps"),depend=T) 

# load the package
library(RgoogleMaps)
# convert the subset
shp <- SpatialPolygons2PolySet(georgia.sub) #converts the subset to a polygon
# determine the extent of the subset
bb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"])
# download map data and store it
MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.jpg")
# now plot the layer and the backdrop
par(mar = c(0,0,0,0))
PlotPolysOnStaticMap(MyMap, shp, lwd=2, 
                     col = rgb(0.25,0.25,0.25,0.025), add = F) #defines the polygon over Google Maps

#Leadlet is good for when you want to embed interactive maps in an html file
tmap_mode('view') #interactive viewing needing internet connection
tm_shape(georgia_sf.sub) +
  tm_polygons(col = "#C6DBEF80" )
tmap_mode("plot") #viewing without interactive/internet
#remember to reset the tmap mode to plot

#3.4.5 Saving your map
#Fastest & quickest way is using the export button to save or copy to clipboar

#in a new document I did this
# load package and data
#library(GISTools)
#data(newhaven)
#proj4string(roads) <- proj4string(blocks)
# plot spatial data
#tm_shape(blocks) +
#  tm_borders() +
#  tm_shape(roads) +
#  tm_lines(col = "red") +
  # embellish the map
#  tm_scale_bar(width = 0.22) +
#  tm_compass(position = c(0.8, 0.07)) +
#  tm_layout(frame = F, title = "New Haven, CT", 
#            title.size = 1.5, 
#            title.position = c(0.55, "top"), 
#            legend.outside = T) 

#png(file='title', other setting)
source("newhavenmap.R") 
dev.off()

pts_sf <- st_centroid(georgia_sf)
setwd('~/Desktop/')
# open the file
png(filename = "Figure1.png", w = 5, h = 7, units = "in", res = 150)
# make the map
tm_shape(georgia_sf) +
  tm_fill("olivedrab4") +
  tm_borders("grey", lwd = 1) +
  # the points layer
  tm_shape(pts_sf) +
  tm_bubbles("PctBlack", title.size = "% Black", col = "gold")
# close the png file
dev.off()

#3.5 Mapping Spatial Data Attributes
rm(list=ls())
# load & list the data
data(newhaven)
ls()
# convert to sf 
blocks_sf <- st_as_sf(blocks)
breach_sf <- st_as_sf(breach)
tracts_sf <- st_as_sf(tracts)
# have a look at the attributes and object class
summary(blocks_sf) #spatial
class(blocks_sf)
summary(breach_sf) #point object
class(breach_sf)
summary(tracts_sf) #spatial
class(tracts_sf)
#all have data.frame
#breach has geometry attributes

data.frame(blocks_sf) #prints out all rows for blocks until limit is reached
head(data.frame(blocks_sf)) #prints out top 6 rows
colnames(data.frame(blocks_sf))
# or 
names(blocks_sf) #just the col names

#For spatial objects, can also use @data to access the data
colnames(blocks@data)
head(blocks@data)

#To access a particualr column: 
data.frame(blocks_sf$P_VACANT) #lists them all in a col
blocks$P_VACANT # this lists them all back to back
#or
attach(data.frame(blocks_sf)) #data frame attached to blocks now?
hist(P_VACANT)
#make sure you detach the data if you do this though as otherwise it can get messy with multiple dataframes on one variable
detach(data.frame(blocks_sf))

# use kde.points to create a kernel density surface
breach.dens = st_as_sf(kde.points(breach,lims=tracts))
summary(breach.dens)
#breach.dens = raster/pixels dataset with attributes in the data frame
breach.dens #has latitude and longitude
plot(breach.dens)

#can also add new attributes to variables
blocks_sf$RandVar <- rnorm(nrow(blocks_sf))
#this adds a new column called random variable

#3.5.3 Mapping polygons and attributes
#a chloropleth = thematic map in which areas are shaded in proportion to their attributes
tmap_mode('plot') #this is set to a non-interactive map
tm_shape(blocks_sf) +
  tm_polygons("P_VACANT") #automatically shades by attributes
#tmap is similar to ggplot in that you can pass it multiple functions
#tmap gives an automatic legend for this mapping

tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", breaks=seq(0, 100, by=25)) #every 25 is a shade

tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", breaks=c(10, 40, 60, 90))

tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", title = "Owner Occ") +
  tm_layout(legend.title.size = 1, #layout changes the legend size & placement
            legend.text.size = 1,
            legend.position = c(0.1, 0.1)) #could also say "centre" and "bottom"

#To view colour options:
display.brewer.all()

brewer.pal(5,'Blues')

tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "Reds") + #this pallette specifies reds
  tm_layout(legend.title.size = 1)
#if you use tm_fill, this will colour the polygons but the outlines will not be plotted 
  #unless also using tm_borders

tm_shape(blocks_sf) +
  tm_fill("P_OWNEROCC", title = "Owner Occ", palette = "Blues") +
  tm_layout(legend.title.size = 1)

# with equal intervals: the tmap default
p1 <- tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "Blues") +
  tm_layout(legend.title.size = 0.7)
# with style = kmeans
p2 <- tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "Oranges",
              style = "kmeans") +
  tm_layout(legend.title.size = 0.7)
# with quantiles
p3 <- tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "Greens", 
              breaks = c(0, round(quantileCuts(blocks$P_OWNEROCC, 6), 1))) +
  tm_layout(legend.title.size = 0.7)
# Multiple plots using the grid package
library(grid)
grid.newpage()
# set up the layout
pushViewport(viewport(layout=grid.layout(1,3)))
# plot using the print command
print(p1, vp=viewport(layout.pos.col = 1, height = 5))
print(p2, vp=viewport(layout.pos.col = 2, height = 5))
print(p3, vp=viewport(layout.pos.col = 3, height = 5))

#Can also display a histogram on the plot
tm_shape(blocks_sf) +
  tm_polygons("P_OWNEROCC", title = "Owner Occ", palette = "-GnBu", #the negative here reverses the pallete colour scheme
              breaks = c(0, round(quantileCuts(blocks$P_OWNEROCC, 6), 1)), 
              legend.hist = T) +
  tm_scale_bar(width = 0.22) +
  tm_compass(position = c(0.8, 0.07)) +
  tm_layout(frame = F, title = "New Haven", 
            title.size = 2, title.position = c(0.55, "top"), 
            legend.hist.size = 0.5)

# add a projection to tracts data and convert tracts data to sf
proj4string(tracts) <- proj4string(blocks)
tracts_sf <- st_as_sf(tracts)
tracts_sf <- st_transform(tracts_sf, "+proj=longlat +ellps=WGS84")
# plot 
tm_shape(blocks_sf) +
  tm_fill(col="POP1990", convert2density=TRUE, #the convert2density command = converts projection units (in this case latitude and longitude) to project in metres
            #then it determines the areal density in square kilometres
          style="kmeans", title=expression("Population (per " * km^2 * ")"), 
          legend.hist=F, id="name") +
  tm_borders("grey25", alpha=.5) + 
  # add tracts context
  tm_shape(tracts_sf) +
  tm_borders("grey40", lwd=2) 
#+ 
  #tm_format(bg.color="white", frame = FALSE, 
  #              legend.hist.bg.color="grey90")

#Create the population density summary
# add an area in km^2 to blocks
blocks_sf$area = st_area(blocks_sf) / (1000*1000)
# calculate population density manually
summary(blocks_sf$POP1990/blocks_sf$area)

tm_shape(blocks_sf) +
  tm_fill(c("P_RENTROCC", "P_BLACK")) +
  tm_borders() +
  tm_layout(legend.format = list(digits = 0), 
            legend.position = c("left", "bottom"),
            legend.text.size = 0.5,
            legend.title.size = 0.8)

tm_polygons

#3.5.4 Mapping points and attributes
#breach data = public disordination
#plotting lines is similar to plotting polygons
tm_shape(blocks_sf) +
  tm_polygons("white") + #tells it you want the polygon first
  tm_shape(breach_sf) +
  tm_dots(size = 0.5, shape = 19, col = "red", alpha = 0.5) #gives the dots

#New dataset with points
# load the data
data(quakes)
# look at the first 6 records
head(quakes)

#Difficult to manually create sf data so better to create sp data then convert
# define the coordinates
coords.tmp <- cbind(quakes$long, quakes$lat)
# create the SpatialPointsDataFrame
quakes.sp <- SpatialPointsDataFrame(coords.tmp, 
                                    data = data.frame(quakes), 
                                    proj4string = CRS("+proj=longlat "))
# convert to sf
quakes_sf <- st_as_sf(quakes.sp)

# map the quakes
tm_shape(quakes_sf) +
  tm_dots(size = 0.5, alpha = 0.3)

#Steps for creating spatial data
#Step 1. Define the coordinates
#Step 2. Assign them to an sp class of object 
#Step 3. Convert to sf if needed
help("SpatialPoints-class") #points just need a pair of coordinates
help("sf") #polygons/lines need lists of coordinates

#The quake data also has magnitude data that can be visualized in multiple ways
#could use size of dot or colour
library(grid)
# by size
p1 <- tm_shape(quakes_sf) +
  tm_bubbles("depth", scale = 1, shape = 19, alpha = 0.3, 
             title.size="Quake Depths")
# by colour
p2 <- tm_shape(quakes_sf) +
  tm_dots("depth", shape = 19, alpha = 0.5, size = 0.6, 
          palette = "PuBuGn", 
          title="Quake Depths")
# Multiple plots using the grid package
grid.newpage()
# set up the layout
pushViewport(viewport(layout=grid.layout(1,2)))
# plot using he print command
print(p1, vp=viewport(layout.pos.col = 1, height = 5))
print(p2, vp=viewport(layout.pos.col = 2, height = 5))

#Can also subset the data for points over magnitude 5.5
# create the index
index <- quakes_sf$mag > 5.5
summary(index)
# select the subset assign to tmp
tmp <- quakes_sf[index,]
# plot the subset
tm_shape(tmp) +
  tm_dots(col = brewer.pal(5, "Reds")[4], shape = 19, 
          alpha = 0.5, size = 1) + 
  tm_layout(title="Quakes > 5.5",
            title.position = c("centre", "top"))

#can use a google map backdrop
library(RgoogleMaps)
# define Lat and Lon
Lat <- as.vector(quakes$lat)
Long <- as.vector(quakes$long)
# get the map tiles 
# you will need to be online
MyMap <- MapBackground(lat=Lat, lon=Long)
# define a size vector
tmp <- 1+(quakes$mag - min(quakes$mag))/max(quakes$mag)
PlotOnStaticMap(MyMap,Lat,Long,cex=tmp,pch=1,col='#FB6A4A30')

MyMap <- MapBackground(lat=Lat, lon=Long, zoom = 10, 
                       maptype = "satellite")
PlotOnStaticMap(MyMap,Lat,Long,cex=tmp,pch=1,
                col='#FB6A4A50')

#3.5.5 Mapping lines and attributes
#lines can be roads 
#need to assign a coordinate system to roads
data(newhaven)
proj4string(roads) <- proj4string(blocks)
# 1. create a clip area
xmin <- bbox(roads)[1,1]
ymin <- bbox(roads)[2,1]
xmax <- xmin + diff(bbox(roads)[1,]) / 2
ymax <- ymin + diff(bbox(roads)[2,]) / 2
xx = as.vector(c(xmin, xmin, xmax, xmax, xmin))
yy = as.vector(c(ymin, ymax, ymax, ymin, ymin))
# 2. create a spatial polygon from this
crds <- cbind(xx,yy)
Pl <- Polygon(crds)
ID <- "clip"
Pls <- Polygons(list(Pl), ID=ID)
SPls <- SpatialPolygons(list(Pls))
df <- data.frame(value=1, row.names=ID)
clip.bb <- SpatialPolygonsDataFrame(SPls, df)
proj4string(clip.bb) <- proj4string(blocks)
# 3. convert to sf
# convert the data to sf
clip_sf <- st_as_sf(clip.bb)
roads_sf <- st_as_sf(roads)
# 4. clip out the roads and the data frame
roads_tmp <- st_intersection(st_cast(clip_sf), roads_sf)

#to avoid the warning error
roads_tmp <- st_intersection(st_geometry(st_cast(clip_sf)), st_geometry(roads_sf))
  #where x = st_cast(clip_sf) and y = roads_sf

#3.5.6 Mapping Raster Attributes
# you may have to install the raster package
# install.packages("raster", dep = T)
library(raster)
library(sp)
data(meuse.grid)
class(meuse.grid)
summary(meuse.grid)

#can then plot the x and y to examine the dataset
plot(meuse.grid$x, meuse.grid$y, asp = 1)
#can then convert to Spatial Pixel Dataframe object then converted to raster format
meuse.sp = SpatialPixelsDataFrame(points = 
                                    meuse.grid[c("x", "y")], data = meuse.grid, 
                                  proj4string = CRS("+init=epsg:28992"))
meuse.r <- as(meuse.sp, "RasterStack") #converts to raster

#Then try plotting sf using all of the attributes vs sp it will plot the specified layer of the meuse grid
plot(meuse.r)
plot(meuse.sp[,5]) #column 
spplot(meuse.sp[, 3:4])
image(meuse.sp[, "dist"], col = rainbow(7))
spplot(meuse.sp, c("part.a", "part.b", "soil", "ffreq"),
       col.regions=topo.colors(20))

#If you want to control the mappping of the attributes more, use tmap
# set the tmap mode to plot
tmap_mode('plot')
# map dist and ffreq attributes
tm_shape(meuse.r) +
  tm_raster(col = c("dist", "ffreq"), title = c("Distance", "Flood Freq"),
            palette = "Reds", style = c("kmeans", "cat"))

# set the tmap mode to view
tmap_mode('view')
# map the dist attribute 
tm_shape(meuse.r) +
  tm_raster(col = "dist", title = "Distance", style = "kmeans") +
  tm_layout(legend.format = list(digits = 1))

#You could also experiment with some of the refinements as with tm_polygons examples
tm_shape(meuse.r) +
  tm_raster(col = "soil", title = "Soil",
            palette = "Spectral", style = "cat") +
  tm_scale_bar(width = 0.3) +
  tm_compass(position = c(0.74, 0.05)) +
  tm_layout(frame = F, title = "Meuse flood plain", 
            title.size = 2, title.position = c("0.2", "top"), 
            legend.hist.size = 0.5)

#3.6 Simple descriptive statistical analyses
library(tidyverse)
install.packages("reshape2", dep = T) #doesnt work with Mac again

#3.6.1 Histograms and Boxplots
data(newhaven)
# the tidyverse package loads the ggplot2 package
library(tidyverse)
pushViewport(viewport(layout=grid.layout(2,1))) # set it up with 1 row, 2 columns

# standard approach with hist
hist(blocks$P_VACANT, breaks = 40, col = "cyan", 
     border = "salmon", 
     main = "The distribution of vacant property percentages", 
     xlab = "percentage vacant", xlim = c(0,40))
# ggplot approach
ggplot(blocks@data, aes(P_VACANT)) +
  geom_histogram(col = "salmon", fill = "cyan", bins = 40) +
  xlab("percentage vacant") +
  labs(title = "The distribution of vacant property percentages")

library(reshape2)
# a logical test
index <- blocks$P_VACANT > 10
# assigned to 2 high, 1 low
blocks$vac <- index + 1
blocks$vac <- factor(blocks$vac, labels = c("Low", "High"))

library(ggplot2)
ggplot(melt(blocks@data[, c("P_OWNEROCC", "P_WHITE", "P_BLACK", "vac")]), 
       aes(variable, value)) +
  geom_boxplot() +
  facet_wrap(~vac)

#manipulate it using melt from reshape2
ggplot(melt(blocks@data[, c("P_OWNEROCC", "P_WHITE", "P_BLACK", "vac")]), 
       aes(variable, value)) +
  geom_boxplot(colour = "yellow", fill = "wheat", alpha = 0.7) +
  facet_wrap(~vac) +
  xlab("") +
  ylab("Percentage") +
  theme_dark() +
  ggtitle("Boxplot of High and Low property vacancies")

#3.6.2 Scatter Plots and Regressions
#to see if we can visualize any trends in vacant properties and proportions of different ethnic groups:
plot(blocks$P_VACANT/100, blocks$P_WHITE/100) #might be more white people in a census block
plot(blocks$P_VACANT/100, blocks$P_BLACK/100) #might be a positive association with black people and vacant property due to socio-economic inequalities

#Let's look into this:
# assign some variables
p.vac <- blocks$P_VACANT/100
p.w <- blocks$P_WHITE/100
p.b <- blocks$P_BLACK/100
# bind these together
df <- data.frame(p.vac, p.w, p.b)
# fit regressions
mod.1 <- lm(p.vac ~ p.w, data = df) #describes the extent to which changes in p.vac are associated with change in p.w
mod.2 <- lm(p.vac ~ p.b, data = df) #describes the extent to which changes in p.vac are associated with changes in p.b
#use function lm to fit a linear regression model

#found that proportion of white people is a weak negative predictor of the proportion of vacant property in a census block
#and that the proportion of black people is a weak positive predictor of the proportion of vacant property in a census block
#Specifically, vacant property decreases by 1% for each 3.5% increase in the proportion of white people
#vacant property increases by 1% for each 3.7% increase in proportion of black people in census block

p1 <- ggplot(df,aes(p.vac, p.w))+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm') +
  geom_point() +
  xlab("Proportion of Vacant Properties") +
  ylab("Proporion White") +
  labs(title = "Regression of Vacant Properties against Proportion White")
p2 <- ggplot(df,aes(p.vac, p.b))+
  #stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm') +
  geom_point() +
  xlab("Proportion of Vacant Properties") +
  ylab("Proporion Black") +
  labs(title = "Regression of Vacant Properties against Proportion Black")

grid.newpage()
# set up the layout
pushViewport(viewport(layout=grid.layout(2,1)))
# plot using he print command
print(p1, vp=viewport(layout.pos.row = 1, height = 5))
print(p2, vp=viewport(layout.pos.row = 2, height = 5))

#3.6.3 Mosaic Plots
#mosaic plots good for true or false statements

# install the package
install.packages("ggmosaic", dep = T) #does not work in Mac again

# create the dataset
pops <- data.frame(blocks[,14:18]) * data.frame(blocks)[,11]
pops <- as.matrix(pops/100)
colnames(pops) <- c("White", "Black", "Ameri", "Asian", "Other")
# a true / false for vacant properties 
vac.10 <- (blocks$P_VACANT > 10) 
# create a cross tabulation
mat.tab <- xtabs(pops ~vac.10)
# melt the data
df <- melt(mat.tab)

# load the packages
library(ggmosaic)
# call ggplot and stat_mosaic
ggplot(data = df) +
  stat_mosaic(aes(weight = value, x = product(Var2), 
                  fill=factor(vac.10)), na.rm=TRUE) +
  theme(axis.text.x=element_text(angle=-90, hjust= .1)) + 
  labs(y='Proportion of Vacant Properties', x = 'Ethnic group',
       title="Mosaic Plot of Vacant Properties with ethnicty") + 
  guides(fill=guide_legend(title = "> 10 percent", reverse = TRUE))

# standard mosiac plot
ttext = sprintf("Mosaic Plot of Vacant Properties 
  with ethnicty")
mosaicplot(t(mat.tab),xlab='',
           ylab='Vacant Properties > 10 percent',
           main=ttext,shade=TRUE,las=3,cex=0.8)

#3.7 Self-Test Questions

