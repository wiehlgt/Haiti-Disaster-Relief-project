# Gavin Wiehl
# gtw4vx
# SYS 6018
# to begin let's import our data by copying and pasting the beginning of Haiti project pt 1.

haiti <- read.table('HaitiPixels.csv',sep=",",header = TRUE) # read in the haiti.csv data
attach(haiti) # attach the haiti dataset
fix(haiti) # a quick look into the data
summary(haiti) # summarizing the data
names(haiti) # column names: Class, Red, Green, and Blue
str(haiti) # a look into the datatypes of the variables

plot(Class,las="2")
par(mfrow=(c(1,3)))
boxplot(Red~Class,las="2") # Soil is mostly red, rooftop and various non-tarp have a lot of red. 
boxplot(Green~Class,las="2") # Soil has a lot of green, as well as various non-tarp and blue tarp
boxplot(Blue~Class,las="2") # Blue tarp has the most blue, vegetation the least

# Lets create a binary variable, where blue tent equals 1 and not blue tent equals 0
items = rep(0,dim(haiti)[1])
items[Class=="Blue Tarp"] = 1
items = as.factor(items) # Blue tent should be classified as 1, other/not blue tent should be classified as 0
haiti$is_blu_trp = items
attach(haiti)























