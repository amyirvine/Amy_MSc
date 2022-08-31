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
