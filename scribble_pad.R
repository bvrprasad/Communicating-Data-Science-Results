setwd("../datasci_course_materials/assignment6/")
pwd
dir()
sea <- read.csv("seattle_incidents_summer_2014.csv")
summary(sea)
?head
head(sea, 2)
class(sea)
str(sea)
san <- read.csv("sanfrancisco_incidents_summer_2014.csv")
str(san)
dim(san)
dim(sea)
?levels
levels(san$Category)
summar(san$Time)
summary(san$Time)
table(san$Category)
?table
summary(table(san$Category))
install.packages(c('rzmq','repr','IRkernel','IRdisplay'),
repos = c('http://irkernel.github.io/', getOption('repos')))
IRkernel::installspec()
IRkernel::installspec()
library("IRKernel")
library("IRkernel")
IRkernel::installspec()
objects()
rm(sea)
?objects
ls()
help("ls")
??size
Cstack_info()
object.size(san)
for(i in objects()) {print(paste(i, object.size(i)))}
allObj <- sapply(ls("package:base"),
function(x)
object.size(get(x, envir = baseenv()))
)
( bigObj <- as.object_sizes(rev(sort(allObj))[1:10] ) )
print(bigObj, humanReadable=TRUE)
str(san)
??time
?lubridate
san$Time[1]
san$Time[2]
hour(san$Time[2])
library("lubridate")
hour(san$Time[2])
hour(as.POSIXlt(san$Time[2]))
hour(as.POSIXlt(strptime(san$Time[2], format = "%H:%M")))
table(hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))))
hist(hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))))
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M")))), y=Category), factor = Category)
library("ggplot2")
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M")))), y=Category), factor = Category)
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M")))), y=Category), factor = Category)
help(ggplot)
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M")))), y=Category), color = Category)
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))), y=Category), color = Category)
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))), y=Category), color = Category) + geom_point
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))), y=Category), color = Category) + geom_point()
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))), y=Category), facet = Category) + geom_point()
str(san$Category)
levels(san$Category)
robbery <- san[san$Category == "ROBBERY",]
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))))) + geom_histogram()
robbery <- san[san$Category == "ROBBERY" or san$Category == "BURGLARY",]
robbery <- san[san$Category == "ROBBERY" || san$Category == "BURGLARY",]
dim(robbery)
robbery <- san[san$Category == "ROBBERY" | san$Category == "BURGLARY",]
dim(robbery)
robbery <- san[san$Category == "ROBBERY" | san$Category == "BURGLARY" | san$Category == "VEHICLE THEFT" | san$Category == "BURGLARY",]
dim(robbery)
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))))) + geom_histogram()
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M")))), binwidth =x) + geom_histogram()
ggplot(san, aes(x=hour(as.POSIXlt(strptime(san$Time, format = "%H:%M")))), binwidth =25) + geom_histogram()
ggplot(robbery, aes(x=hour(as.POSIXlt(strptime(robbery$Time, format = "%H:%M")))), binwidth =25) + geom_histogram()
robbery1 <- san[san$Category == "ROBBERY",]
ggplot(robbery1, aes(x=hour(as.POSIXlt(strptime(robbery1$Time, format = "%H:%M")))), binwidth =25) + geom_histogram()
robbery <- san[san$Category == "ROBBERY" | san$Category == "BURGLARY" | san$Category == "VEHICLE THEFT" | san$Category == "LARCENY/THEFT",]
dim(robbery)
ggplot(robbery, aes(x=hour(as.POSIXlt(strptime(robbery$Time, format = "%H:%M")))), binwidth =25) + geom_histogram()
str(san)
tapply(san$Category, hour(as.POSIXlt(strptime(robbery$Time, format = "%H:%M"))), length)
tapply(san$Category, hour(as.POSIXlt(strptime(san$Time, format = "%H:%M"))), length)
counts <- tapply(san$Category, [,san$Category, san$Time], length)
counts <- tapply(san$Category, san[,Category, Time], length)
str(san)
counts <- tapply(san$Category, san[,2,6], length)
dim(counts)
counts
counts <- tapply(san$Time, san[,2,6], length)
dim(counts)
str(san)
summary(counts)
san$Count <- c(1)
counts <- tapply(san$Count, san[,2,6], sum)
dim(counts)
san$Hour <- hour(as.POSIXlt(strptime(san$Time, format = "%H:%M")))
dim(san$Hour)
length(san$Hour)
?ggplot
?table
table(san$Hour, san$Count)
?tapply
counts <- aggregate(Count ~ Category + Hour, data = san, sum)
dim(counts)
ggplot(counts, aes(Hour, Count, fill = ..density..)) + + theme(aspect.ratio = 1) + facet_wrap(~ Category)
ggplot(counts, aes(Hour, Count, fill = ..density..)) + theme(aspect.ratio = 1) + facet_wrap(~ Category)
ggplot(counts, aes(Hour, Count, fill = ..density..)) + theme(aspect.ratio = 1) + facet_wrap(~ Category) + geom_point()
ggplot(counts, aes(Hour, Count)) + theme(aspect.ratio = 1) + facet_wrap(~ Category) + geom_point()
areawise <- aggregate(Count ~ Category + PdDistrict, data = san, sum)
ggplot(areawise, aes(PdDistrict, Count)) + theme(aspect.ratio = 1) + facet_wrap(~ Category) + geom_point()
ggplot(areawise, aes(PdDistrict, Count)) + theme(aspect.ratio = 1) + facet_wrap(~ Category) + opts(axis.text.x=theme_text(angle=-90)) + geom_point()
ggplot(areawise, aes(PdDistrict, Count)) + theme(aspect.ratio = 1) + facet_wrap(~ Category) + + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point()
ggplot(areawise, aes(PdDistrict, Count)) + theme(aspect.ratio = 1) + facet_wrap(~ Category) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point()
str(areawise$PdDistrict)
levels(areawise$PdDistrict)
central <- areawise[areawise$PdDistrict == "CENTRAL", ]
ggplot(central, aes(Category, Count)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point()
common <- areawise[areawise$Category == "ROBBERY" | areawise$Category == "THEFT", ]
ggplot(common, aes(PdDistrict, Count)) + theme(aspect.ratio = 1) + facet_wrap(~ Category) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point()
ggplot(common, aes(PdDistrict, Count)) + facet_wrap(~ Category) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point()
levels(areawise$Category)
common <- areawise[areawise$Category == "ROBBERY" | areawise$Category == "VEHICLE THEFT" | areawise$Category == "LARCENY/THEFT", areawise$Category == "BURGLARY", ]
common <- areawise[areawise$Category == "ROBBERY" | areawise$Category == "VEHICLE THEFT" | areawise$Category == "LARCENY/THEFT" | areawise$Category == "BURGLARY", ]
ggplot(common, aes(PdDistrict, Count)) + facet_wrap(~ Category) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point()
?month
head(san$Date)
san$Month <- month(as.POSIXlt(strptime(san$Date, format = "%m/%d/%Y")))
head(san$Month)
levels(san$Month)
levels(as.factor(san$Month))
months <- aggregate(Count ~ Month, data = san, sum)
ggplot(months, aes(Month, Count)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point()
?cor
daily <- aggregate(Count ~ Date + Category, data = san, sum)
dim(daily)
daily <- aggregate( ~ Date + Category, data = san, count)
daily <- aggregate(PdDistrict ~ Date + Category, data = san, count)
daily <- aggregate(PdDistrict ~ Date + Category, data = san, length)
dim(daily)
head(daily)
daily <- aggregate(Count ~ Date + Category, data = san, sum)
head(daily)
daily$Date <- as.POSIXlt(strptime(daily$Date, format = "%m/%d/%Y"))
head(daily)
ggplot(daily, aes(Date, Count)) + facet_wrap(~ Category) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_point()
?cor.test
