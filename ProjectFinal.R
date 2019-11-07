#Clean the environment
rm(list = ls())

#rm(day2)

#Set working directory
setwd("G:/RPrac")

#Load the librarires
libraries = c("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#sapply(day, function(x) sum(length(which(is.na(x)))))  

#Fetch csv file data into R
day = read.csv(file = "day.csv", header = T, sep = ",")
#day2= read.csv(file = "day.csv", header = T, sep = ",")

#Converting the data to its required format for data exploration
day$dteday = as.character(day$dteday)
day$season = as.factor(day$season)
day$yr = as.factor(day$yr)
day$mnth = as.factor(day$mnth)
day$holiday = as.factor(day$holiday)
day$weekday = as.factor(as.character(day$weekday))
day$workingday = as.factor(as.character(day$workingday))
day$weathersit = as.factor(day$weathersit)


##DATA EXPLORATION##
#Check the distribution of categorical Data using bar graph
day$season_Factored = factor(x = day$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
day$yr_Factored = factor(x = day$yr, levels = c(0,1), labels = c("2011","2012"))
day$holiday_Factored = factor(x = day$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
day$weathersit_Factored = factor(x = day$weathersit, levels = c(1,2,3,4), 
                                 labels = c("Clear","Cloudy/Mist","Rain/Snow/Fog","Heavy Rain/Snow/Fog"))

Season_bar = ggplot(data = day, aes(x = season_Factored))+ geom_bar()+ ggtitle("Count of Season" )
Weather_bar = ggplot(data = day, aes(x = weathersit_Factored)) + geom_bar() + ggtitle("Count of Weather")
Holoday_bar = ggplot(data = day, aes(x = holiday_Factored)) + geom_bar() + ggtitle("Count of Holiday")
WDay_bar = ggplot(data = day, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")

gridExtra::grid.arrange(Season_bar,Weather_bar)
gridExtra::grid.arrange(Holoday_bar,WDay_bar)

#Data is normalised.Converting data to Check the distribution of numerical data using histogram.
day$temp_DeNormalized <- day$temp*39
day$feel_temp_DeNormalized <- day$atemp*50
day$windspeed_DeNormalized <- day$windspeed*67
day$hum_DeNormalized = day$hum * 100

Temp_hist = ggplot(data = day, aes(x =temp_DeNormalized)) + ggtitle("Temperature Distribution") + geom_histogram(bins = 50)
Hum_hist = ggplot(data = day, aes(x =hum_DeNormalized)) + ggtitle("Humidity Distribution") + geom_histogram(bins = 25)
Feel_hist = ggplot(data = day, aes(x =feel_temp_DeNormalized)) + ggtitle("Feel Temperature Distribution") + geom_histogram(bins = 25)
Wind_hist = ggplot(data = day, aes(x =windspeed_DeNormalized)) + ggtitle("Windspeed Distribution") + geom_histogram(bins = 25)

gridExtra::grid.arrange(Temp_hist,Hum_hist,Feel_hist,Wind_hist)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = day, aes(x =temp_DeNormalized, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = day, aes(x =hum_DeNormalized, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = day, aes(x =feel_temp_DeNormalized, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = day, aes(x =windspeed_DeNormalized, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike COunt")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)


#Check for outliers in data using boxplot
cnames = colnames(day[,c("temp_DeNormalized","feel_temp_DeNormalized","windspeed_DeNormalized","hum_DeNormalized")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = day)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}


gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

str(day)
#Remove outliers in Windspeed and humidity
val = day[,23][day[,23] %in% boxplot.stats(day[,23])$out]
day = day[which(!day[,23] %in% val),]

val = day[,24][day[,24] %in% boxplot.stats(day[,24])$out]
day = day[which(!day[,24] %in% val),]

#Check for multicollinearity using VIF
df = day[,c("instant","temp","atemp","hum","windspeed")]
vifcor(df)

#Check for collinearity using corelation graph
corrgram(day, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


#Remove the unwanted variables
df2=day
day <- subset(day, select = -c(holiday,instant,dteday,atemp,casual,registered,temp_DeNormalized,feel_temp_DeNormalized,windspeed_DeNormalized,
                               hum_DeNormalized,season_Factored,yr_Factored,holiday_Factored,weathersit_Factored))

rmExcept(keepers = "day")
########################################DECISION TREE########################################
#MAPE: 28.31%
#Accuracy: 71.69%

#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(day), 0.8 * nrow(day))
train = day[train_index,]
test = day[-train_index,]

#rpart for regression
dt_model = rpart(cnt ~ ., data = train, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model, test[,-10])

#Create dataframe for actual and predicted values
df = data.frame("actual"=test[,10], "pred"=dt_predictions)
head(df)


#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,10], dt_predictions)

## RANDOM FOREST ##
#MAPE: 22.39%
#Accuracy: 77.61%

#Train the data using random forest
rf_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-10])

#Create dataframe for actual and predicted values
df = cbind(df,rf_predictions)
head(df)

#Calculate MAPE
MAPE(test[,10], rf_predictions)

########################################LINEAR REGRESSION########################################
#MAPE: 12.17%
#MAE: 494
#Adjusted R squared: 0.8369
#F-statistic: 109.7

#Train the data using linear regression
lr_model = lm(formula = cnt~., data = train)

#Check the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test[,-10])

#Create dataframe for actual and predicted values
df = cbind(df,lr_predictions)
head(df)

#Calculate MAPE
MAPE(test[,10], lr_predictions)

#Plot a graph for actual vs predicted values
plot(test$cnt,type="b",lty=2,col="Red")
dots(lr_predictions,col="blue")

#Predict a sample data
predict(lr_model,test[31,])
