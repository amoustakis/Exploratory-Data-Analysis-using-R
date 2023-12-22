library(data.table)
data<-fread("c:/Tetuan.csv") #read the data table
dim(data) #dim of the table: 52.416 rows and 7 features
head(data) # first 6 rows
str(data) #find the type of every variable
library(lubridate)

#create new features
data$day <- as.numeric(format(as.Date(data$DateTime,format="%m/%d/%Y %H:%M"), format = "%d"))
data$month <- as.numeric(format(as.Date(data$DateTime,format="%m/%d/%Y %H:%M"), format = "%m"))
data$year <- as.numeric(format(as.Date(data$DateTime,format="%m/%d/%Y %H:%M"), format = "%Y"))
data$hour <- as.numeric(format(as.POSIXct(data$DateTime,format="%m/%d/%Y %H:%M"), format = "%H"))
data$minutes <- as.numeric(format(as.POSIXct(data$DateTime,format="%m/%d/%Y %H:%M"), format = "%M"))
data$quarter <- lubridate::quarter(data$month)
data$weekday <- wday(format(as.POSIXct(data$DateTime,format="%m/%d/%Y %H:%M"))) #week starts on Sunday 
data$yearday <- yday(format(as.POSIXct(data$DateTime,format="%m/%d/%Y %H:%M")))
data$season <- 1 #winter: December, January, February
data$season[data$month>2&data$month<6] <- 2 #spring: March, April, May
data$season[data$month>5&data$month<9] <- 3 #summer: June, July, August
data$season[data$month>8&data$month<12] <- 4 #autumn: September, October, November

data <- data[,-1] #drop the column DateTime as it is not needed anymore

library("dplyr")
data <- data %>% #rename columns in order to be shorter
       rename("Z1 PC" = "Zone 1 Power Consumption",
              "Z2 PC" = "Zone 2  Power Consumption",
              "Z3 PC" = "Zone 3  Power Consumption")

#Corellation matrix with ggplot2
library(ggplot2)
library(ggcorrplot)
library("gridExtra") #place diagrams together
corr1<-round(cor(data[,-7:-15]), 1) #no time data
g1 <- ggcorrplot(corr1, method = "circle",
            lab=TRUE,
			lab_size = 4,
			title="Correlogram of Tetuan dataset regarding the weather",
			type = "lower",
			ggtheme=theme_bw)

corr2<-round(cor(data[,-9]), 1) #include new features - ignore the year (always 2017)
g2 <- ggcorrplot(corr2, method = "circle",
            lab=TRUE,
			lab_size = 4,
			title="Correlogram of Tetuan dataset with the new features",
			type = "lower",
			ggtheme=theme_bw)

grid.arrange(g1, g2, ncol = 2, nrow=1)








#save the dataset to see the changes 
write.csv(data, "C:\\Users\\apost\\Desktop\\hello.csv", row.names=FALSE)


#First diagrams: barplots Average power consumption for each Zone (Z1&Z2&Z3) per month

diagram1 <- data[,.(avgZ1=mean(`Z1 PC`)), by=.(month)] #average consumption for Z1 for the 12 months
diagram2 <- data[,.(avgZ2=mean(`Z2 PC`)), by=.(month)] #average consumption for Z2 for the 12 months
diagram3 <- data[,.(avgZ3=mean(`Z3 PC`)), by=.(month)] #average consumption for Z3 for the 12 months

plot1 <- ggplot(diagram1, aes(x = as.factor(month), y = avgZ1, fill=month)) + #I use month as a factor because it is numeric, also fill=month is numeric and that is why the colors are like this
                   geom_bar(stat='identity', width=.8) +                      # if it was fill=as.factor(month) then we would have 12 different colors in the barplot
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 37000)) +
				   labs(title = "Average Power Consumption for Zone 1 in every month",
                   x = "month",
                   y = "Average Zone 1 Power Consumption(KW)")+
				   theme(legend.position = "none")

plot2 <- ggplot(diagram2, aes(x = as.factor(month), y = avgZ2, fill=month)) + 
                   geom_bar(stat='identity', width=.8) + 
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 37000)) +
				   labs(title = "Average Power Consumption for Zone 2 in every month",
                   x = "month",
                   y = "Average Zone 2 Power Consumption(KW)")+
				   theme(legend.position = "none")

plot3 <- ggplot(diagram3, aes(x = as.factor(month), y = avgZ3, fill=month)) + 
                   geom_bar(stat='identity', width=.8) +                   
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 37000)) +
				   labs(title = "Average Power Consumption for Zone 3 in every month",
                   x = "month",
                   y = "Average Zone 3 Power Consumption(KW)") +
				   theme(legend.position = "none")
grid.arrange(plot1, plot2, plot3, ncol = 2, nrow=2)

			   
#Barplots: Average power consumption for each Zone (Z1&Z2&Z3) per hour

d1 <- data[,.(avgZ1=mean(`Z1 PC`)), by=.(hour)]
d2 <- data[,.(avgZ2=mean(`Z2 PC`)), by=.(hour)]
d3 <- data[,.(avgZ3=mean(`Z3 PC`)), by=.(hour)]

p1 <- ggplot(d1, aes(x = as.factor(hour), y = avgZ1, fill=hour)) + 
                   geom_bar(stat='identity', width=.5) +                      
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000) ) + #limits = c(0, 45000)
				   labs(title = "Average Power Consumption for Zone 1 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "Average Power Consumption(KW)")+
				   theme(legend.position = "none")
				   
p2 <- ggplot(d2, aes(x = as.factor(hour), y = avgZ2, fill=hour)) + 
                   geom_bar(stat='identity', width=.5) +                      
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000) ) + #limits = c(0, 45000): we know that zone1 is powerful, we want to see the changes 
				   labs(title = "Average Power Consumption for Zone 2 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "")+
				   theme(legend.position = "none")

p3 <- ggplot(d3, aes(x = as.factor(hour), y = avgZ3, fill=hour)) + 
                   geom_bar(stat='identity', width=.5) +                      
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000)) + #limits = c(0, 45000)
				   labs(title = "Average Power Consumption for Zone 3 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "Average Power Consumption(KW)")+
				   theme(legend.position = "none")
grid.arrange(p1, p2, p3, ncol = 2, nrow=2)



# Boxplots: Consumption VS hour

p1 <- ggplot(data, aes(x = as.factor(hour), y =`Z1 PC`, fill=hour)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Power Consumption of Zone 1 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "Power Consumption(KW)")+
				   theme(legend.position = "none")

p2 <- ggplot(data, aes(x = as.factor(hour), y =`Z2 PC`, fill=hour)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Power Consumption of Zone 2 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "")+
				   theme(legend.position = "none")

p3 <- ggplot(data, aes(x = as.factor(hour), y =`Z3 PC`, fill=hour)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Power Consumption of Zone 3 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "Power Consumption(KW)")+
				   theme(legend.position = "none")
grid.arrange(p1, p2, p3, ncol = 2, nrow=2)










# A very simpe scatterplot: Average temperature by month 
diagram7 <- data[,.(avgtemp=mean(`Temperature`)), by=.(month)]
ggplot(diagram7, aes(x=as.factor(month), y=avgtemp)) +
          geom_point() +
          labs(title="Average temperature by month",
		  x = "month",
          y = "Average Temperature")
		  
# A very simple box plot: Temperature by month
ggplot(data, aes(x = as.factor(month), y =Temperature , fill=month)) + 
                   geom_boxplot() +  #notch=TRUE          
				   labs(title = "Ocurring temperatures at every month", #hour 0 belongs to 24:00
                   x = "month",
                   y = "Temperature")+
				   theme(legend.position = "none")


#attempt of a boxplot: hour and consumption of Z1
ggplot(data, aes(x = as.factor(hour), y =`Z1 PC` , fill=hour)) + 
                   geom_boxplot(varwidth=T) +  #notch=TRUE          
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000)) + #limits = c(0, 45000)
				   labs(title = "Average Power Consumption for Zone 3 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "Average Zone 3 Power Consumption(KW)")+
				   theme(legend.position = "none")#+
				   #geom_jitter(position=position_jitter(0.2))
				   
ggplot(data, aes(x = as.factor(hour), y =`Z2 PC` , fill=hour)) + 
                   geom_boxplot() +  #notch=TRUE          
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000)) + #limits = c(0, 45000)
				   labs(title = "Average Power Consumption for Zone 3 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "Average Zone 3 Power Consumption(KW)")+
				   theme(legend.position = "none")


				   
				   
				   
				   
ggplot(data, aes(x = as.factor(hour))) + 
                   geom_boxplot(aes(y=`Z1 PC`, fill="red"),varwidth=T) +  
				   geom_boxplot(aes(y=`Z2 PC`, fill=hour),varwidth=T) +
				   geom_boxplot(aes(y=`Z3 PC`,fill=hour),varwidth=T) +
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000)) + #limits = c(0, 45000)
				   labs(title = "Average Power Consumption for Zone 3 for every hour", #hour 0 belongs to 24:00
                   x = "hour",
                   y = "Average Zone 3 Power Consumption(KW)")+
				   theme(legend.position = "none")
				   
				   
				   
				   
				   



				   
				   
				   
				   
#time series

theme_set(theme_bw()) #the theme 
colors <- c("Zone 1" = "red", "Zone 2" = "green", "Zone 3" = "blue") #the colors of the lines
time_series<-data[, .(avgZ1=mean(`Z1 PC`), avgZ2=mean(`Z2 PC`), avgZ3=mean(`Z3 PC`)), by=month]
ggplot(time_series, aes(x=month)) +
geom_line(aes(y=avgZ1, color="Zone 1"), size=.8) +
geom_line(aes(y=avgZ2, color="Zone 2"), size=.8)+
geom_line(aes(y=avgZ3, color="Zone 3"), size=.8)+
labs(title="Average Power Consumption of each zone by month",
x="Month",
y="Average Power Consumption",
color ="Legend")+
scale_color_manual(name="", values = colors)+
scale_x_discrete(name ="Month", limits=1:12) +
ylim(5000, 40000)


#working days VS weekends: Lollipop diagrams

data$work <- "working day" #days from Monday to Friday (2 to 6)
data$work[data$weekday==1|data$weekday==7] <- "weekend" #Saturday&Sunday
		   
diagram_work1 <- data[,.(avgZ1=mean(`Z1 PC`)), by=.(work)] 			   				   
d1 <- ggplot(diagram_work1, aes(x = work, y = avgZ1, color =work)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=work, xend=work, y=0, yend=avgZ1))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 35000)) +
				   labs(x = "Zone 1",
                   y = "Average Power Consumption (KW)")+
				   theme(legend.position = "none")	

diagram_work2 <- data[,.(avgZ2=mean(`Z2 PC`)), by=.(work)] 			   				   
d2 <- ggplot(diagram_work2, aes(x = work, y = avgZ2, color = work)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=work, xend=work, y=0, yend=avgZ2))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 35000)) +
				   labs(x = "Zone 2",y ="")+
				   theme(legend.position = "none")

diagram_work3 <- data[,.(avgZ3=mean(`Z3 PC`)), by=.(work)] 			   				   
d3 <- ggplot(diagram_work3, aes(x = work, y = avgZ3, color=work)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=work, xend=work, y=0, yend=avgZ3))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 35000)) +
				   labs(x = "Zone 3", y = "")+
				   theme(legend.position = "none")
grid.arrange(d1, d2, d3, ncol = 3, nrow=2)				   
				   	   
#Wokring days VS weekends: Grouped barplots montlhy

d1 <- data[,.(avgZ1=mean(`Z1 PC`)), by=.(month, work)]	   
plot1 <- ggplot(d1, aes(x = as.factor(month), y = avgZ1, fill=work)) + 
                   geom_bar(stat='identity', width=.5, position = "dodge") +                      
				   labs(x = "Month",				   
                   y = "Average Power Consumption(KW)")+
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 37000))+
				   theme(legend.position = "bottom")
				   
d2 <- data[,.(avgZ2=mean(`Z2 PC`)), by=.(month, work)]	   
plot2 <- ggplot(d2, aes(x = as.factor(month), y = avgZ2, fill=work)) + 
                   geom_bar(stat='identity', width=.5, position = "dodge") +                      
				   labs(x = "Month",
				   y = "")+
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 37000))+
				   theme(legend.position = "bottom")

d3 <- data[,.(avgZ3=mean(`Z3 PC`)), by=.(month, work)]	   
plot3 <- ggplot(d3, aes(x = as.factor(month), y = avgZ3, fill=work)) + 
                   geom_bar(stat='identity', width=.5, position = "dodge") +                      
				   labs(x = "Month",
				   y = "")+
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 37000))+
                   theme(legend.position = "bottom")				   

grid.arrange(plot1, plot2, plot3, ncol = 3, nrow=1)					   
				   
				   
				   
#season 

d1 <- data[,.(avgZ1=mean(`Z1 PC`)), by=.(season)] 			   				   
p1 <- ggplot(d1, aes(x = season, y = avgZ1, color =season)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=season, xend=season, y=0, yend=avgZ1))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 40000)) +
				   labs(title= "Zone 1",
				   x = "Season",
                   y = "Average Power Consumption (KW)")+
				   theme(legend.position = "none")				   
				   
d2 <- data[,.(avgZ2=mean(`Z2 PC`)), by=.(season)] 			   				   
p2 <- ggplot(d2, aes(x = season, y = avgZ2, color =season)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=season, xend=season, y=0, yend=avgZ2))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000),limits = c(0, 40000)) +
				   labs(title= "Zone 2",
				   x = "Season",
                   y = "")+
				   theme(legend.position = "none")	

d3 <- data[,.(avgZ3=mean(`Z3 PC`)), by=.(season)] 			   				   
p3 <- ggplot(d3, aes(x = season, y = avgZ3, color =season)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=season, xend=season, y=0, yend=avgZ3))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 40000)) +
				   labs(title= "Zone 3",
				   x = "Season",
                   y = "")+
				   theme(legend.position = "none")
grid.arrange(p1, p2, p3, ncol = 3, nrow=2)

#quarter lollipops



d4 <- data[,.(avgZ1=mean(`Z1 PC`)), by=.(quarter)] 			   				   
p4 <- ggplot(d4, aes(x = quarter, y = avgZ1, color =quarter)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=quarter, xend=quarter, y=0, yend=avgZ1))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 40000)) +
				   labs(title= "Zone 1",
				   x = "Quarter",
                   y = "Average Power Consumption (KW)")+
				   theme(legend.position = "none")				   
				   
d5 <- data[,.(avgZ2=mean(`Z2 PC`)), by=.(quarter)] 			   				   
p5 <- ggplot(d5, aes(x = quarter, y = avgZ2, color =quarter)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=quarter, xend=quarter, y=0, yend=avgZ2))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000),limits = c(0, 40000)) +
				   labs(title= "Zone 2",
				   x = "Quarter",
                   y = "")+
				   theme(legend.position = "none")	

d6 <- data[,.(avgZ3=mean(`Z3 PC`)), by=.(quarter)] 			   				   
p6 <- ggplot(d6, aes(x = quarter, y = avgZ3, color =quarter)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=quarter, xend=quarter, y=0, yend=avgZ3))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 40000)) +
				   labs(title= "Zone 3",
				   x = "Quarter",
                   y = "")+
				   theme(legend.position = "none")
			   
grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow=2)









d2 <- data[,.(avgZ1=mean(`Z1 PC`)), by=.(quarter)] 			   				   
p2 <- ggplot(d2, aes(x = quarter, y = avgZ1, color =quarter)) +
                   geom_point(size=3)+
				   geom_segment(aes(x=quarter, xend=quarter, y=0, yend=avgZ1))+      
				   scale_y_continuous(breaks = seq(0, 40000, by = 5000), limits = c(0, 40000)) +
				   labs(title= "Average Power Consumption of each quarter",
				   x = "quarter",
                   y = "Average Power Consumption (KW)")+
				   theme(legend.position = "none")






#Temperature VS Power Consumption: Scatter plots

d1 <- data[,.(avgtemp=mean(`Temperature`), avgZ1=mean(`Z1 PC`)), by=.(yearday)]
p1 <- ggplot(d1, aes(x=avgtemp, y=avgZ1)) +
          geom_point(color="blue") +
          labs(title="Zone 1",
		  x = "Average Temperature per day",
          y = "Average Power Consumption per day(KW) ")+
		  geom_jitter(width=0.5, color="blue")

d2 <- data[,.(avgtemp=mean(`Temperature`), avgZ2=mean(`Z2 PC`)), by=.(yearday)]
p2 <- ggplot(d2, aes(x=avgtemp, y=avgZ2)) +
          geom_point(color="blue") +
          labs(title="Zone 2",
		  x = "Average Temperature per day",
          y = "")+
		  geom_jitter(width=0.5, color="blue")

d3 <- data[,.(avgtemp=mean(`Temperature`), avgZ3=mean(`Z3 PC`)), by=.(yearday)]
p3 <- ggplot(d3, aes(x=avgtemp, y=avgZ3)) +
          geom_point(color="blue") +
          labs(title="Zone 3",
		  x = "Average Temperature per day",
          y = "Average Power Consumption per day(KW)")+
		  geom_jitter(width=0.5, color="blue")
			   
grid.arrange(p1, p2, p3, ncol = 2, nrow=2)






#3D - Appendix 




library(plotly)
d <- data[,.(avgtemp=mean(`Temperature`), avgZ1=mean(`Z1 PC`), avghum=mean(`Humidity`)), by=.(yearday)]
plot_ly(d, x=~avgZ1, y = ~avgtemp, z = ~avghum, type="scatter3d", mode="markers")


library(plotly)

d <- data[,.(avgtemp=mean(`Temperature`), avgZ1=mean(`Z1 PC`), avghum=mean(`Humidity`)), by=.(yearday)]
fig1 <- plot_ly(d, x = ~avgZ1, y = ~avghum, z = ~avgtemp,
               marker = list(color = ~avgZ1, showscale = TRUE))
fig1 <- fig1 %>% add_markers()
fig1 <- fig1 %>% layout(scene = list(xaxis = list(title = 'Average Consumption'),
                                   yaxis = list(title = 'Average Humidity'),
                                   zaxis = list(title = 'Average Temperature')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                        ))


d <- data[,.(avgtemp=mean(`Temperature`), avgZ1=mean(`Z2 PC`), avghum=mean(`Humidity`)), by=.(yearday)]
fig2 <- plot_ly(d, x = ~avgZ2, y = ~avghum, z = ~avgtemp,
               marker = list(color = ~avgZ2, showscale = TRUE))
fig2 <- fig2 %>% add_markers()
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = 'Average Consumption'),
                                   yaxis = list(title = 'Average Humidity'),
                                   zaxis = list(title = 'Average Temperature')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                        ))
fig2












#Apendix

#corellation matrix with library corrplot
library(corrplot)
corrplot.mixed(cor(data[,-9]), upper.col = COL2('PRGn'),lower.col = 'Dark green',tl.cex = 0.7) #second library

#corellation matrix with library psych
library(psych)
cor.plot(data[,-9])


#barplots for weekday

d1 <- data[,.(avgZ1=mean(`Z1 PC`)), by=.(weekday)]
d2 <- data[,.(avgZ2=mean(`Z2 PC`)), by=.(weekday)]
d3 <- data[,.(avgZ3=mean(`Z3 PC`)), by=.(weekday)]

p1 <- ggplot(d1, aes(x = as.factor(weekday), y = avgZ1, fill=weekday)) + 
                   geom_bar(stat='identity', width=.5) +                      
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000), ,limits = c(0, 35000) ) + 
				   labs(title = "Zone 1",
                   x = "Day of the week",
                   y = "Average Consumption")+
				   theme(legend.position = "none")
				   
p2 <- ggplot(d2, aes(x = as.factor(weekday), y = avgZ2, fill=weekday)) + 
                   geom_bar(stat='identity', width=.5) +                      
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000),limits = c(0, 35000) ) + 
				   labs(title = "Zone 2", 
                   x = "Day of the week",
                   y = "")+
				   theme(legend.position = "none")

p3 <- ggplot(d3, aes(x = as.factor(weekday), y = avgZ3, fill=weekday)) + 
                   geom_bar(stat='identity', width=.5) +                      
				   scale_y_continuous(breaks = seq(0, 50000, by = 5000), ,limits = c(0, 35000)) + 
				   labs(title = "Zone 3", 
                   x = "Day of the week",
                   y = "")+
				   theme(legend.position = "none")
grid.arrange(p1, p2, p3, ncol = 3, nrow=2)


#boxplots of the weekday vs consumption

p1 <- ggplot(data, aes(x = as.factor(weekday), y =`Z1 PC`, fill=weekday)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 1", 
                   x = "Day of the week", #1 is Sunday
                   y = "Power Consumption(KW)")+
				   theme(legend.position = "none")

p2 <- ggplot(data, aes(x = as.factor(weekday), y =`Z2 PC`, fill=weekday)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 2", 
                   x = "Day of the week",
                   y = "")+
				   theme(legend.position = "none")

p3 <- ggplot(data, aes(x = as.factor(weekday), y =`Z3 PC`, fill=weekday)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 3", 
                   x = "Day of the week",
                   y = "Power Consumption(KW)")+
				   theme(legend.position = "none")
grid.arrange(p1, p2, p3, ncol = 3, nrow=2)


#boxplots vs month 

p1 <- ggplot(data, aes(x = as.factor(month), y =`Z1 PC`, fill=month)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 1", 
                   x = "Day of the week", #1 is Sunday
                   y = "Power Consumption(KW)")+
				   theme(legend.position = "none")

p2 <- ggplot(data, aes(x = as.factor(month), y =`Z2 PC`, fill=month)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 2", 
                   x = "Day of the week",
                   y = "")+
				   theme(legend.position = "none")

p3 <- ggplot(data, aes(x = as.factor(month), y =`Z3 PC`, fill=month)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 3", 
                   x = "Day of the week",
                   y = "Power Consumption(KW)")+
				   theme(legend.position = "none")
grid.arrange(p1, p2, p3, ncol = 2, nrow=2)









#temperature: low to high

data$temp_new<-cut(data$Temperature, breaks=c(-Inf,10,16,22,28, Inf), labels = c("very low","low","medium","high","very high"))

d1 <- ggplot(data, aes(x = temp_new, y = `Z1 PC`, fill=temp_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 1",
          x = "Temperature",
          y = "Power consumption(KW)")+
		  theme(legend.position = "none")

d2 <- ggplot(data, aes(x = temp_new, y = `Z2 PC`, fill=temp_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 2", 
          x = "Temperature",
          y = "")+
		  theme(legend.position = "none")

d3 <- ggplot(data, aes(x = temp_new, y = `Z3 PC`, fill=temp_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 3", 
          x = "Temperature",
          y = "")+
		  theme(legend.position = "none")
		  
grid.arrange(d1, d2, d3, ncol = 3, nrow=1)













#temperature & wind

d1 <- ggplot(data, aes(x = temp_new, y = `Z1 PC`, fill=wind_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 1",
          x = "Temperature",
          y = "Power consumption(KW)")+
		  theme(legend.position = "none")

d2 <- ggplot(data, aes(x = temp_new, y = `Z2 PC`, fill=wind_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 2", 
          x = "Temperature",
          y = "")+
		  theme(legend.position = "none")

d3 <- ggplot(data, aes(x = temp_new, y = `Z3 PC`, fill=wind_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 3", 
          x = "Temperature",
          y = "")+
		  theme(legend.position = "none")
		  
grid.arrange(d1, d2, d3, ncol = 3, nrow=2)




#temperature and humidity


d1 <- data[,.(avghum=mean(`Humidity`), avgtemp=mean(`Temperature`)), by=.(yearday)]
p1 <- ggplot(d1, aes(x=avghum, y=avgtemp)) +
          geom_point(color="blue") +
          labs(title="Zone 1",
		  x = "Average Humidity per day",
          y = "Average Temperature per day ")+
		  geom_jitter(width=0.5, color="blue")









#Humidity: low to high

data$humidity_new<-cut(data$Humidity, breaks=c(-Inf,40,60,80, Inf), labels = c("low","medium","high","very high"))

d1 <- ggplot(data, aes(x = humidity_new, y = `Z1 PC`, fill=humidity_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 1",
          x = "Humidity",
          y = "Power consumption(KW)")+
		  theme(legend.position = "none")

d2 <- ggplot(data, aes(x = humidity_new, y = `Z2 PC`, fill=humidity_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 2", 
          x = "Humidity",
          y = "")+
		  theme(legend.position = "none")

d3 <- ggplot(data, aes(x = humidity_new, y = `Z3 PC`, fill=humidity_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 3", 
          x = "Humidity",
          y = "")+
		  theme(legend.position = "none")
		  
grid.arrange(d1, d2, d3, ncol = 3, nrow=1)






#Function for producing boxplots

boxplot <- function(zone, d, data_x, data_y, name) {

return(ggplot(d, aes(x = data_x, y = data_y, fill=data_x)) + 
			geom_boxplot(varwidth=TRUE) +          
			labs(title = zone,
			x = name,
			y = "Power consumption(KW)")+
			theme(legend.position = "none"))
}
	
#Creation of the variable humidity_new

data$humidity_new<-cut(data$Humidity, breaks=c(-Inf,40,60,80, Inf), labels = c("low","medium","high","very high"))

#Creation of the boxplots using the function boxplot I created

d1 <- data[,.(hum = humidity_new, power = `Z1 PC`)]
p1 <- boxplot("Zone 1", d1, d1$hum, d1$power, "Humidity") 

d2 <- data[,.(hum = humidity_new, power = `Z2 PC`)]
p2 <- boxplot("Zone 2", d2, d2$hum, d2$power, "Humidity") 

d3 <- data[,.(hum = humidity_new, power = `Z3 PC`)]
p3 <- boxplot("Zone 3", d3, d3$hum, d3$power, "Humidity") 

grid.arrange(p1, p2, p3, ncol = 3, nrow=1)



#Temperature

#Function for producing boxplots

boxplot <- function(zone, d, data_x, data_y, name) {

return(ggplot(d, aes(x = data_x, y = data_y, fill=data_x)) + 
			geom_boxplot(varwidth=TRUE) +          
			labs(title = zone,
			x = name,
			y = "Power consumption(KW)")+
			theme(legend.position = "none"))
}
#Creation of the variable temperature_new	
	
data$temp_new<-cut(data$Temperature, breaks=c(-Inf,10,16,22,28, Inf), labels = c("very low","low","medium","high","very high"))

d1 <- data[,.(temp = temp_new, power = `Z1 PC`)]
p1 <- boxplot("Zone 1", d1, d1$temp, d1$power, "Temperature") 

d2 <- data[,.(temp = temp_new, power = `Z2 PC`)]
p2 <- boxplot("Zone 2", d2, d2$temp, d2$power, "Temperature") 

d3 <- data[,.(temp = temp_new, power = `Z3 PC`)]
p3 <- boxplot("Zone 3", d3, d3$temp, d3$power, "Temperature") 

grid.arrange(p1, p2, p3, ncol = 3, nrow=1)



#Wind speed



#Creation of the variable wind_new

data$wind_new<-cut(data$`Wind Speed`, breaks=c(-Inf, 2, Inf), labels = c("low", "high"))

#Creation of the boxplots using the function boxplot I created

d1 <- data[,.(wind = wind_new, power = `Z1 PC`)]
p1 <- boxplot("Zone 1", d1, d1$wind, d1$power, "Wind Speed") 

d2 <- data[,.(wind = wind_new, power = `Z2 PC`)]
p2 <- boxplot("Zone 2", d2, d2$wind, d2$power, "Wind Speed") 

d3 <- data[,.(wind = wind_new, power = `Z3 PC`)]
p3 <- boxplot("Zone 3", d3, d3$wind, d3$power, "Wind Speed") 

grid.arrange(p1, p2, p3, ncol = 3, nrow=2)











		  
#wind speed analysis

data$wind_new<-cut(data$`Wind Speed`, breaks=c(-Inf, 2, Inf), labels = c("low", "high"))

d1 <- ggplot(data, aes(x = wind_new, y = `Z1 PC`, fill=wind_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 1",
          x = "wind speed",
          y = "Power Consumption(KW)")+
		  theme(legend.position = "none")

d2 <- ggplot(data, aes(x = wind_new, y = `Z2 PC`, fill=wind_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 2", 
          x = "wind speed",
          y = "")+
		  theme(legend.position = "none")

d3 <- ggplot(data, aes(x = wind_new, y = `Z3 PC`, fill=wind_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 3", 
          x = "wind speed",
          y = "")+
		  theme(legend.position = "none")
		  
grid.arrange(d1, d2, d3, ncol = 3, nrow=1)	  
		  
		  
		  
		  
		 

d1 <- ggplot(data, aes(x = temp_new, y = `Z1 PC`, fill=humidity_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 1",
          x = "wind speed",
          y = "Power Consumption(KW)")+
		  theme(legend.position = "bottom")

d2 <- ggplot(data, aes(x =temp_new, y = `Z2 PC`, fill=humidity_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 2", 
          x = "wind speed",
          y = "")+
		  theme(legend.position = "bottom")

d3 <- ggplot(data, aes(x = temp_new, y = `Z3 PC`, fill=humidity_new)) + 
     geom_boxplot(varwidth=TRUE) +          
     labs(title = "Zone 3", 
          x = "wind speed",
          y = "")+
		  theme(legend.position = "bottom")
		  
grid.arrange(d1, d2, d3, ncol = 2, nrow=2)	  
		  













		 
		  
		  
		  
		  
#Histograms
d1<- ggplot(data = data, aes(x = Temperature)) + #temperature
geom_histogram(color="black", fill="white",binwidth=1)+
labs(title = "Histogram of occurring temperatures",
          y = "Number of observations")

d2<- ggplot(data = data, aes(x = Humidity)) + #humidity
geom_histogram(color="black", fill="white",binwidth=2)+
labs(title = "Histogram of occurring humidity",
          y = "")
		  
d3<- ggplot(data = data, aes(x = `Wind Speed`)) + #wind speed
geom_histogram(color="black", fill="white",binwidth=0.1)+
labs(title = "Histogram of occurring wind speed",
          y = "")

grid.arrange(d1, d2, d3, ncol = 3, nrow=1)







#Humidity VS Power Consumption: Scatter plots

d1 <- data[,.(avghum=mean(`Humidity`), avgZ1=mean(`Z1 PC`)), by=.(yearday)]
p1 <- ggplot(d1, aes(x=avghum, y=avgZ1)) +
          geom_point(color="blue") +
          labs(title="Zone 1",
		  x = "Average Humidity per day",
          y = "Average Power Consumption per day(KW) ")+
		  geom_jitter(width=0.5, color="blue")

d2 <- data[,.(avghum=mean(`Humidity`), avgZ2=mean(`Z2 PC`)), by=.(yearday)]
p2 <- ggplot(d2, aes(x=avghum, y=avgZ2)) +
          geom_point(color="blue") +
          labs(title="Zone 2",
		  x = "Average Humidity per day",
          y = "")+
		  geom_jitter(width=0.5, color="blue")

d3 <- data[,.(avghum=mean(`Humidity`), avgZ3=mean(`Z3 PC`)), by=.(yearday)]
p3 <- ggplot(d3, aes(x=avghum, y=avgZ3)) +
          geom_point(color="blue") +
          labs(title="Zone 3",
		  x = "Average Humidity per day",
          y = "Average Power Consumption per day(KW)")+
		  geom_jitter(width=0.5, color="blue")
			   
grid.arrange(p1, p2, p3, ncol = 2, nrow=1)




#Fuction for producing the scatterplots

scatterplot <- function(zone, d, data_x, data_y, name) {

return(ggplot(d, aes(x=data_x, y=data_y)) +
          geom_point(color="blue") +
          labs(title=zone,
		  x = paste("Average", name, "per day"),
          y = "Average Power Consumption per day(KW) ")+
		  geom_jitter(width=0.5, color="blue"))
}

#Humidity
d1 <- data[,.(avghum=mean(`Humidity`), avgZ1=mean(`Z1 PC`)), by=.(yearday)]
p1 <- scatterplot("Zone 1", d1, d1$avghum, d1$avgZ1, "Humidity")

d2 <- data[,.(avghum=mean(`Humidity`), avgZ2=mean(`Z2 PC`)), by=.(yearday)]
p2 <- scatterplot("Zone 2", d2, d2$avghum, d2$avgZ2, "Humidity")

d3 <- data[,.(avghum=mean(`Humidity`), avgZ3=mean(`Z3 PC`)), by=.(yearday)]
p3 <- scatterplot("Zone 3", d3, d3$avghum, d3$avgZ3, "Humidity")

grid.arrange(p1, p2, p3, ncol = 2, nrow=2)

#Temperature
d1 <- data[,.(avgtemp=mean(`Temperature`), avgZ1=mean(`Z1 PC`)), by=.(yearday)]
p1 <- scatterplot("Zone 1", d1, d1$avgtemp, d1$avgZ1, "Temperature")

d2 <- data[,.(avgtemp=mean(`Temperature`), avgZ2=mean(`Z2 PC`)), by=.(yearday)]
p2 <- scatterplot("Zone 2", d2, d2$avgtemp, d2$avgZ2, "Temperature")

d3 <- data[,.(avgtemp=mean(`Temperature`), avgZ3=mean(`Z3 PC`)), by=.(yearday)]
p3 <- scatterplot("Zone 3", d3, d3$avgtemp, d3$avgZ3, "Temperature")

grid.arrange(p1, p2, p3, ncol = 2, nrow=2)






#Temperature VS Power Consumption: Scatter plots

d1 <- data[,.(avgtemp=mean(`Temperature`), avgZ1=mean(`Z1 PC`)), by=.(yearday)]
p1 <- ggplot(d1, aes(x=avgtemp, y=avgZ1)) +
          geom_point(color="blue") +
          labs(title="Zone 1",
		  x = "Average Temperature per day",
          y = "Average Power Consumption per day(KW) ")

d2 <- data[,.(avgtemp=mean(`Temperature`), avgZ2=mean(`Z2 PC`)), by=.(yearday)]
p2 <- ggplot(d2, aes(x=avgtemp, y=avgZ2)) +
          geom_point(color="blue") +
          labs(title="Zone 2",
		  x = "Average Temperature per day",
          y = "")+
		  geom_jitter(width=0.5, color="blue")

d3 <- data[,.(avgtemp=mean(`Temperature`), avgZ3=mean(`Z3 PC`)), by=.(yearday)]
p3 <- ggplot(d3, aes(x=avgtemp, y=avgZ3)) +
          geom_point(color="blue") +
          labs(title="Zone 3",
		  x = "Average Temperature per day",
          y = "Average Power Consumption per day(KW)")+
		  geom_jitter(width=0.5, color="blue")
grid.arrange(p1, p2, p3, ncol = 2, nrow=2)	  












histogram <- function(name,data_x,binwidth) {

return(ggplot(data = data, aes(x = data_x)) + 
       geom_histogram(color="black", fill="white",binwidth=binwidth)+
       labs(title = paste("Histogram of", name),
            y = "Number of observations",
		    x = name))
}

grid.arrange(histogram("Temperature",data$Temperature,1),histogram("Humidity",data$Humidity,2),
histogram("Wind Speed",data$`Wind Speed`,0.1), ncol = 3, nrow=2)






		  
#Ramadan Analysis

data_Ramadan <- data[data$yearday>=172&data$yearday<=181]  #period 21/6 to 30/6

d <- data_Ramadan[,.(avgtemp=mean(`Temperature`), avgZ3=mean(`Z3 PC`)), by=.(day)]


αξωνας χ -> day 

ggplot(data_Ramadan, aes(x = as.factor(hour), y =`Z1 PC`, fill=as.factor(hour))) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 1", 
                   x = "Day",
                   y = "Power Consumption(KW)")+
				   theme(legend.position = "none")
				   
				   
ggplot(data_Ramadan, aes(x = as.factor(day), y =`Z1 PC`, fill=temp_new)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 1", 
                   x = "Day",
                   y = "Power Consumption(KW)")+
				   theme(legend.position = "none")
				   




#Boxplots Power Consumption VS Ramadan days
data_Ramadan <- data[data$yearday>=172&data$yearday<=181]  #period 21/6 to 30/6

p1 <- ggplot(data_Ramadan, aes(x = as.factor(day), y =`Z1 PC`, fill=temp_new)) + 
                   geom_boxplot() + 
				   geom_vline(xintercept = 5.5, linetype="dotted", 
                   color = "blue", size=2)+
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 1", 
                   x = "Day",
                   y = "Power Consumption(KW)")+
				   labs(fill="Temperature")+
				   theme(legend.position = "bottom")	

p2 <- ggplot(data_Ramadan, aes(x = as.factor(day), y =`Z2 PC`, fill=temp_new)) + 
                   geom_boxplot() + 
				   geom_vline(xintercept = 5.5, linetype="dotted", 
                   color = "blue", size=2)+
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 2", 
                   x = "Day",
                   y = "")+
				   labs(fill="Temperature")+
				   theme(legend.position = "bottom")

p3 <- ggplot(data_Ramadan, aes(x = as.factor(day), y =`Z3 PC`, fill=temp_new)) + 
                   geom_boxplot() + 
				   geom_vline(xintercept = 5.5, linetype="dotted", 
                   color = "blue", size=2)+
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 3", 
                   x = "Day",
                   y = "Power Consumption(KW)")+
				   labs(fill="Temperature")+
				   theme(legend.position = "bottom")		
				   
grid.arrange(p1, p2, p3, ncol = 2, nrow=2)				   
							   
				   
				   
				   
				   
				   
#time series for the days of Ramadan			   
				   
theme_set(theme_bw()) #the theme 
colors <- c("Zone 1" = "red", "Zone 2" = "green", "Zone 3" = "blue") #the colors of the lines
time_series<-data_Ramadan[, .(avgZ1=mean(`Z1 PC`), avgZ2=mean(`Z2 PC`), avgZ3=mean(`Z3 PC`)), by=day]
ggplot(time_series, aes(x=day)) +
geom_line(aes(y=avgZ1, color="Zone 1"), size=.8) +
geom_line(aes(y=avgZ2, color="Zone 2"), size=.8)+
geom_line(aes(y=avgZ3, color="Zone 3"), size=.8)+
labs(title="Average Power Consumption of each zone during the Ramadan",
x="Day",
y="Average Power Consumption",
color ="Legend")+
scale_color_manual(name="", values = colors)+
scale_x_discrete(name ="Day", limits=21:30) +
ylim(5000, 40000)






#temperature VS Power Consumption VS Humidity

ggplot(data, aes(x = temp_new, y =`Z1 PC`, fill=humidity_new)) + 
                   geom_boxplot() + 
                   scale_y_continuous(breaks = seq(0, 50000, by = 5000))+				   
				   labs(title = "Zone 1", 
                   x = "Temperature",
                   y = "Power Consumption(KW)")+
				   labs(fill="Humidity")+
				   theme(legend.position = "bottom")	




























theme_set(theme_bw()) #overall theme of the diagrams

histogram <- function(name,data_x,binwidth) {  #Function for producing histograms

return(ggplot(data = data, aes(x = data_x)) + 
       geom_histogram(color="black", fill="white",binwidth=binwidth)+
       labs(title = paste("Histogram of", name),
            y = "Number of observations",
		    x = name))
}

grid.arrange(histogram("Temperature",data$Temperature,1),histogram("Humidity",data$Humidity,2),
histogram("Wind Speed",data$`Wind Speed`,0.1), ncol = 3, nrow=1)





theme_set(theme_bw()) #overall theme of the diagrams

histogram <- function(name,data_x,binwidth) {  #Function for producing histograms

return(ggplot(data = data, aes(x = data_x)) + 
       geom_histogram(color="black", fill="white",binwidth=binwidth)+
       labs(title = paste("Histogram of", name),
            y = "Number of observations",
		    x = name))
}

grid.arrange(histogram("Zone 1",data$`Z1 PC`,1),histogram("Zone 2",data$`Z2 PC`,2),
histogram("Zone 3",data$`Z3 PC`,0.1), ncol = 3, nrow=1)