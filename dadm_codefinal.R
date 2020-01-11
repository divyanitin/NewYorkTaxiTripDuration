####DATA ACQUISITION

TAXIDATA <- read.csv("C:/Users/divya/OneDrive/Documents/SEM1/DADM/project/taxi.csv", stringsAsFactors = FALSE)
str(TAXIDATA)

####DATA CLEANING/DATA VISUALIZATION
#Converting categorical variable into a factor variable and data cleaning

library('dplyr') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
#install.packages("lubridate")
library('lubridate') # date and time

head(TAXIDATA)
#mdy_hm - month day year-hours minutes
TAXIDATA <- TAXIDATA %>% mutate(pickup_datetime = mdy_hm(pickup_datetime),
                                dropoff_datetime = mdy_hm(dropoff_datetime),
                                vendor_id = factor(vendor_id),
                                passenger_count = factor(passenger_count))
#factor levels
TAXIDATA$store_and_fwd_flag <- factor(TAXIDATA$store_and_fwd_flag)
levels(TAXIDATA$store_and_fwd_flag) <- c("N","Y")
str(TAXIDATA) #New structure


#all zero values are made NA
TAXIDATA$pickup_longitude[TAXIDATA$pickup_longitude==0] <- NA
TAXIDATA$pickup_latitude[TAXIDATA$pickup_latitude==0] <- NA
TAXIDATA$dropoff_longitude[TAXIDATA$dropoff_longitude==0] <- NA
TAXIDATA$dropoff_latitude[TAXIDATA$dropoff_latitude==0] <- NA
TAXIDATA$trip_duration[TAXIDATA$trip_duration==0] <- NA

#install.packages("Amelia")
library(Amelia)
missmap(TAXIDATA, main= "Missing vs Observed")

summary(TAXIDATA)
#pickup_datetime - 1500 NA's
#dropoff_datetime - 1500 NA's
#trip_duration - 8 NA's
#id, vendor_id, passenger_count, store_and_fwd_flag - categorical variables

library(mice)

#Inspect the missing data pattern
md.pattern(TAXIDATA)

#Impute the missing data m times, resulting in m completed data sets
imp <- mice(TAXIDATA, method="cart")

#complete function takes input an object of class mids as created by the function mice() and 
#fills in the missing data, and returns the completed data in a specified format.
TAXIDATANEW <- complete(imp)
summary(TAXIDATANEW)

#Graphical representations of the distribution of a variable
library(moments)
skewness(TAXIDATA$trip_duration, na.rm=TRUE)

## From the summary, we can see that other than trip duration, 
#the rest of the variables look approximately normally distributed. 
#To confirm the distribution of trip duration, lets check it graphically

library(ggplot2)
ggplot(data = TAXIDATANEW, mapping = aes(x = trip_duration, y = ..density..))+
  geom_histogram(bins = 80, color = "blue", alpha = 0.5)+
  scale_x_log10()+
  scale_y_sqrt() + geom_density(color="Maroon", lwd=0.8)

#box-whisker plot of Trip Duration with respect to Store_and_fwd_Flag
ggplot(TAXIDATANEW) + 
  aes(y = trip_duration, fill=store_and_fwd_flag) + theme_classic()+
  geom_boxplot(outlier.color = "Blue", outlier.shape = 8,outlier.size=.8) + 
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_log10() +
  labs(title = "Box Plot of Trip Duration") 

#Normal QQ Plot of Trip Duration
plot3 <- ggplot(TAXIDATANEW) + 
  aes(sample = log(trip_duration)) +
  geom_qq() +
  geom_qq_line(color="purple")+
  labs(title="Normal QQ-Plot", y="Trip_Duration (s)")
plot3

#box-whisker plots of all variables
boxplot(scale(TAXIDATANEW[,c(6,7,8,9,11)]),
        names=c("Pickup_longitude", "Pickup_latitude","Dropoff_Longitude", "Dropoff_Latitude","Trip_Duration"), varwidth=TRUE,
        main="Scaled Box-Whisker Plots for all (continuous) Variables", las = 1,cex.axis = 0.7)

#Bar of vendor Id with count
ven_count = TAXIDATANEW %>% 
  group_by(vendor_id) %>% 
  count() %>% 
  ggplot(aes(x=vendor_id,y=n, fill=vendor_id, las = 1,cex.axis = 0.7))+
  geom_col()+
  theme(legend.position = "none")

#pickup date time
pick_time <- TAXIDATANEW %>%
  ggplot(aes(x = pickup_datetime, las = 1,cex.axis = 0.7)) +
  geom_histogram(fill = "red", bins = 120) +
  labs(x = "Pickup dates")

#dropoff date time
drop_time <- TAXIDATANEW %>%
  ggplot(aes(x = dropoff_datetime, las = 1,cex.axis = 0.7)) +
  geom_histogram(fill = "blue", bins = 120) +
  labs(x = "Dropoff dates")

#Bar of passenger count with n
pss_count = TAXIDATANEW %>% 
  group_by(passenger_count) %>% 
  count() %>% 
  ggplot(aes(x=passenger_count,y=n, fill=passenger_count, las = 0.8,cex.axis = 0.5))+
  geom_col()+
  theme(legend.position = "none")

pick_lon <- TAXIDATANEW %>%
  filter(pickup_longitude > -74.05 & pickup_longitude < -73.7) %>%
  ggplot(aes(pickup_longitude, las = .8,cex.axis = 0.5)) +
  geom_histogram(fill = "red", bins = 40)

drop_long <- TAXIDATANEW %>%
  filter(dropoff_longitude > -74.05 & dropoff_longitude < -73.7) %>%
  ggplot(aes(dropoff_longitude, las = .8,cex.axis = 0.5)) +
  geom_histogram(fill = "blue", bins = 40)

pick_lat <- TAXIDATANEW %>%
  filter(pickup_latitude > 40.6 & pickup_latitude < 40.9) %>%
  ggplot(aes(pickup_latitude, las = .8,cex.axis = 0.5)) +
  geom_histogram(fill = "red", bins = 40)

drop_lat <- TAXIDATANEW %>%
  filter(dropoff_latitude > 40.6 & dropoff_latitude < 40.9) %>%
  ggplot(aes(dropoff_latitude, las = 1,cex.axis = 0.5)) +
  geom_histogram(fill = "blue", bins = 40)

store <- TAXIDATANEW %>%
  ggplot(aes(store_and_fwd_flag, las = 1,cex.axis = 0.7)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_y_log10()

trip_duration <- TAXIDATANEW %>% 
  ggplot(aes(trip_duration, las = 1,cex.axis = 0.7)) +
  geom_histogram(fill="Purple", bins=10) +  scale_x_log10()+
  scale_y_sqrt() 

#combining all the graphs plotted for each individual column in the dataset
layout <- matrix(c(1,2,3,4,5,6,7,8,9,10),5,4,byrow=TRUE)
library(ggplot2)
library(gridExtra)
grid.arrange(ven_count, pick_time,drop_time,pss_count, pick_lon,drop_long,pick_lat,drop_lat,store, trip_duration, ncol=5)

#Dot plot of days of week with respect to vendor
p4 <- TAXIDATANEW %>%
  mutate(wday = wday(pickup_datetime, label = TRUE)) %>%
  group_by(wday, vendor_id) %>%
  count() %>%
  ggplot(aes(wday, n, colour = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Day of the week", y = "Total number of pickups") +
  theme(legend.position = "bottom")

#Dot plot of hours of day with respect to vendor
p5 <- TAXIDATANEW %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups") +
  theme(legend.position = "bottom")

grid.arrange(p4,p5,ncol=1)

#line chart of hours, days and count with respect to month
p6 <- TAXIDATANEW %>%
  mutate(hpick = hour(pickup_datetime),
         wday = factor(wday(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, color = wday)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")
p6

#Trip duration by vendor ID and number of passenger
p10 = TAXIDATANEW %>%
  ggplot(aes(passenger_count, trip_duration, color = as.factor(passenger_count))) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none")+
  facet_wrap(~ vendor_id) +
  labs(y = "Trip duration [s]", x = "Number of passengers")
p10

#install.packages("rgdal")
library(rgdal)
setwd("C:/Users/divya/OneDrive/Documents/SEM1/DADM/project")
ny.map <- readOGR(".","ZillowNeighborhoods-NY")
neighborhoods <- ny.map[ny.map$City == "New York", ]
neighborhood_names <- levels(neighborhoods$Name)
(head(neighborhood_names, n = 12))

find_neighborhoods <- function(df, long_feature, lat_feature, neighborhood_feature) {
  
  dat <- data.frame(long = df[long_feature][[1]], lat = df[lat_feature][[1]])
  coordinates(dat) <- ~ long + lat
  proj4string(dat) <- proj4string(neighborhoods)
  df[neighborhood_feature] <- over(dat, neighborhoods)$Name
  
  levels(df[[neighborhood_feature]]) <- c(levels(df[[neighborhood_feature]]), "Unknown")
  df[[neighborhood_feature]][is.na(df[[neighborhood_feature]])] <- "Unknown"
  
  return(df)
}

library(data.table)
TAXIDATANEW = as.data.frame(TAXIDATANEW)
TAXIDATANEW = find_neighborhoods(TAXIDATANEW, "pickup_longitude", "pickup_latitude", "pickup_neighborhood")
TAXIDATANEW = find_neighborhoods(TAXIDATANEW, "dropoff_longitude", "dropoff_latitude", "dropoff_neighborhood")
TAXIDATANEW = as.data.table(TAXIDATANEW)

TAXIDATANEW[, same_neighborhood := as.factor(ifelse(pickup_neighborhood == dropoff_neighborhood, 1, 0))]

#temp = dtrain[,.(mean = .N),by=pickup_neighborhood][order(-mean)]
TAXIDATANEW = TAXIDATANEW[pickup_neighborhood %in% c("Upper East Side", "Upper West Side", "Midtown", "Flatiron District", "Garment District")]

TAXIDATANEW = TAXIDATANEW[dropoff_neighborhood %in% c("Upper East Side", "Upper West Side", "Midtown", "Flatiron District", "Garment District")]
library(dplyr)
library(ggplot2)
library(gridExtra)
plot1 <- TAXIDATANEW %>%
  group_by(dropoff_neighborhood, pickup_neighborhood) %>%
  summarise(mean = mean(trip_duration)) %>%
  ggplot(aes(dropoff_neighborhood, pickup_neighborhood, fill = mean)) +
  geom_tile() +
  labs(x = "dropoff_neighborhood", y = "pickup_neighborhood") +
  scale_fill_distiller(palette = "Spectral")
grid.arrange(plot1, ncol=1)

summary(TAXIDATANEW)

#Distance Graph
#install.packages("geosphere")
library(geosphere)
distance_km =  distHaversine(matrix(c(TAXIDATANEW$pickup_longitude,TAXIDATANEW$pickup_latitude), ncol = 2),
                             matrix(c(TAXIDATANEW$dropoff_longitude,TAXIDATANEW$dropoff_latitude), ncol = 2))/1000

TAXIDATANEW$distance_miles=distance_km/1609

ggplot(TAXIDATANEW,aes(distance_miles, trip_duration,color='red')) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]")

summary(TAXIDATANEW)
names(TAXIDATANEW)
str(TAXIDATANEW)

library("GGally")
ggpairs(TAXIDATANEW, columns = c(5:9,15,11), corSize=10)
#There are no two predictors which are highly correlated
#There are no two at time redundancy
#From the model we can see strong predictors are trip distance and neighborhood
#Correlation Plot
library("corrplot")
myMatrix <- cor(TAXIDATANEW[,c(6,7,8,9,15,11)])
corrplot(myMatrix, method = "circle")

#Model Fitting

g1 <- lm(trip_duration ~ passenger_count+pickup_longitude+pickup_latitude+
           dropoff_longitude+dropoff_latitude+distance_miles+pickup_neighborhood+dropoff_neighborhood+
           same_neighborhood, data=TAXIDATANEW)
summary(g1)

#FitPlot
library(ggplot2)
qplot(g1$fitted.values, trip_duration, data=TAXIDATANEW) +
  geom_abline(intercept=0, slope=1,color="hot pink")+
  ggtitle("Fit Plot")

#Residual Plot
ggplot(g1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept=0, color="hot pink") +
  ggtitle("Residual Plot")

g2 <- step(g1)
summary(g2)

library(car)
influencePlot(g1,id.method=cooks.distance(g1),id=TRUE,xlab="Hat-Values", ylab="Studentized Residuals")


#----Transformed outcome variable

g6 <- lm(log(trip_duration) ~ pickup_longitude+pickup_latitude+dropoff_latitude+
           pickup_neighborhood+dropoff_neighborhood+ distance_miles, 
         data = TAXIDATANEW)

g6 <- step(g6)
summary(g6)


g7 <- lm(log(trip_duration) ~ passenger_count+pickup_longitude+pickup_latitude+
           dropoff_longitude+dropoff_latitude+distance_miles+pickup_neighborhood+dropoff_neighborhood+
           same_neighborhood, data=TAXIDATANEW)
summary(g7)

anova(g6,g7)
#supports big model

#anova(g5,g1)


#-------------------------------------------#

#compareCoefs(g1,g2,g5,g6,g7,se=FALSE)
#----------------------------------------------#
#-------------------------------------------#

#----------------------------------------#

mod <- fortify(g1)
p11 <- qplot(.fitted, .resid, data = mod) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                   se = F)
p12 <- qplot(.fitted, abs(.resid), data = mod) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + geom_smooth(method = "lm", 
                                                                                color = "red", se = F)
grid.arrange(p11, p12, nrow = 2)

#As the values on xaxis  are increasing the variance in the points are increasing

summary(lm(abs(residuals(g1)) ~ fitted(g1)))

modgg <- fortify(g1)
modgs <- fortify(g7)
p22 <- qplot(.fitted, .resid, data = modgg) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                   se = F)
p23 <- qplot(.fitted, abs(.resid), data = modgg) + geom_hline(yintercept = 0, 
                                                              linetype = "dashed") + labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + 
  geom_smooth(method = "lm", color = "red", se = F)
p34 <- qplot(.fitted, .resid, data = modgs) + geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(title = "Residuals vs Fitted", x = "Fitted", y = "Residuals") + geom_smooth(color = "red", 
                                                                                   se = F)
p45 <- qplot(.fitted, abs(.resid), data = modgs) + geom_hline(yintercept = 0, 
                                                              linetype = "dashed") + labs(title = "Scale-Location", x = "Fitted", y = "|Residuals|") + 
  geom_smooth(method = "lm", color = "red", se = F)
grid.arrange(p22, p23, p34, p45, nrow = 2)

summary(lm(abs(residuals(g7)) ~ fitted(g7)))

#NON normal errors

p101 <- qplot(sample = scale(.resid), data = modgg) + geom_abline(intercept = 0, 
                                                                  slope = 1, color = "red") + labs(title = "Untransformed y", y = "Residuals")
p102 <- qplot(sample = scale(.resid), data = modgs) + geom_abline(intercept = 0, 
                                                                  slope = 1, color = "red") + labs(title = "Log-Tranformed y", y = "Residuals")
grid.arrange(p101, p102, nrow = 2)

#Shapiro test

shapiro.test(residuals(g1))
shapiro.test(residuals(g7))


#BOX COX

library(car)
(lambda <- powerTransform(g1))
lam <- lambda$lambda
glam <- lm(trip_duration^lam ~ passenger_count+pickup_longitude+pickup_latitude+
             dropoff_longitude+dropoff_latitude+distance_miles+pickup_neighborhood+dropoff_neighborhood+
             same_neighborhood, data=TAXIDATANEW)
modlam <- fortify(glam)
p200 <- qplot(sample = scale(.resid), data = modgs) + geom_abline(intercept = 0, 
                                                                  slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals log-transformed")
p202 <- qplot(sample = scale(.resid), data = modlam) + geom_abline(intercept = 0, 
                                                                   slope = 1, color = "red") + labs(title = "Normal QQ-Plot", y = "Residuals Box-Cox-Transform")
grid.arrange(p200, p202, nrow = 1)

shapiro.test(residuals(glam))

#Checking for influential outliers
influencePlot(g71)

library(faraway)
islands <- row.names(TAXIDATANEW)
halfnorm(lm.influence(g7)$hat, labs = islands, ylab = "Leverages")

cook <- cooks.distance(g7)
halfnorm(cook, 3, labs = islands, ylab = "Cook's distance")

g71 <- lm(log(trip_duration) ~ passenger_count+pickup_longitude+pickup_latitude+
            dropoff_longitude+dropoff_latitude+distance_miles+pickup_neighborhood+dropoff_neighborhood+
            same_neighborhood, data=TAXIDATANEW,
          subset = (cook < max(cook)))
compareCoefs(g7, g71)

#omnibus
oldpar <- par(mfrow = c(2, 2))
plot(g7, main = "Taxi Data")
#----------------------------------#

#Checking for correct model specification

g8 <- lm(log(trip_duration) ~ passenger_count+pickup_longitude+pickup_latitude+
           dropoff_longitude+dropoff_latitude+distance_miles+pickup_neighborhood+dropoff_neighborhood+
           same_neighborhood, data=TAXIDATANEW,
         subset = (cook < max(cook)))

library(car)
ceresPlots(g1, terms = ~.)

ceresPlots(g8, terms = ~.)
#after log transformation the model looks normal

#model fit is correct

#-----------------------------#
#model Validation
library(DAAG)

df <- data.frame(mse.g1=NULL, 
                 mse.g2=NULL, 
                 mse.g5=NULL,
                 mse.g6=NULL,
                 mse.g7=NULL)

for (i in 1:10) {
  seed <- round(runif(1, min=0, max=100))
  oldpar <- par(mar=c(1,0,5,0))
  mse.g1 <- CVlm(data = TAXIDATANEW, 
                 form.lm=g1, 
                 m=3, 
                 seed=seed,
                 printit=F,
                 main = "g1")
  mse.g2 <- CVlm(data = TAXIDATANEW, 
                 form.lm=g2, 
                 m=3, 
                 seed=seed, 
                 printit=F,
                 main = "g2")
  mse.g5 <- CVlm(data = TAXIDATANEW, 
                 form.lm=g5, 
                 m=3, 
                 seed=seed, 
                 printit=F,
                 main = "g5")
  mse.g6 <- CVlm(data = TAXIDATANEW, 
                 form.lm=g6, 
                 m=3, 
                 seed=seed, 
                 printit=F,
                 main = "g6")
  mse.g7 <- CVlm(data = TAXIDATANEW, 
                 form.lm=g7, 
                 m=3, 
                 seed=seed, 
                 printit=F,
                 main = "g7")
  par(oldpar)
  
  df.temp <- data.frame(mse.g1=attr(mse.g1, "ms"),
                        mse.g2=attr(mse.g2, "ms"),
                        mse.g5=attr(mse.g5, "ms"),
                        mse.g6=attr(mse.g6, "ms"),
                        mse.g7=attr(mse.g7, "ms")
  )
  df <- rbind(df,df.temp)
}

df
#Model 6 supported

confint(g1)

TAXIDATANEW$passenger_count <- as.factor(TAXIDATANEW$passenger_count)#---------------#
g9=lm(log(trip_duration) ~ pickup_longitude+pickup_latitude+dropoff_latitude+distance_miles, passenger_count, data=TAXIDATANEW)

g10=lm(log(trip_duration) ~ pickup_longitude+pickup_latitude+dropoff_latitude+distance_miles, as.factor(passenger_count), data=TAXIDATANEW)
confint(g7)

#bonfferoni 
m <- 5
#we need 1-alpha each trip wise confidence level
level.trip  <- .95
alpha  <- (1-level.trip)
level.ind  <- 1-alpha/(2*m)
#Bonferroni corrected confidence intervals

#comparing confidence intervals of coefficients without and with Bonferroni Correction Models
confint(g9, level =.95)
confint(g9, level  = 1-0.05/(2*5))
confint(g10, level=.95)
confint(g10, level = 1-0.05/(2*5))