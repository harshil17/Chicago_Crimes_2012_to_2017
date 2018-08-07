getwd()
mydata <- read.csv("crimedataset.csv", header = TRUE, sep = ",")
mydata
str(mydata)
summary(mydata)

mydata <- subset(mydata, !duplicated(mydata$Case.Number))
mydata <- subset(mydata, !is.na(mydata$District))
mydata <- subset(mydata, !is.na(mydata$Ward))
mydata <- subset(mydata, !is.na(mydata$Community.Area))

summary(mydata)

head(mydata)

mydata$Date <- as.POSIXlt(mydata$Date, format = "%m/%d/%Y %H:%M")
                          
                    
head(mydata$Date)

#install.packages("chron")
library("chron")

#seperate time stamp from the date

mydata$time <- times(format(mydata$Date, "%H : %M : %S"))
head(mydata$time)

# Creating four time interval windows

time.tag <- chron(times = c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:00"))
time.tag


mydata$time.tag <- cut(mydata$time, breaks = time.tag, labels = c("00-06", "06-12", "12-18", "18-00"))
table(mydata$time.tag)

mydata$Date <- as.POSIXlt(strptime(mydata$Date, format = "%Y-%m-%d"))
head(mydata$Date)

mydata$day <- weekdays(mydata$Date, abbreviate = TRUE)
head(mydata$day)
mydata$month <- months(mydata$Date, abbreviate = TRUE)
head(mydata$month)


table(mydata$Primary.Type)

length(unique(mydata$Primary.Type))

mydata$crime <- as.character(mydata$Primary.Type)
mydata$crime <- ifelse(mydata$crime %in% c("CRIM SEXUAL ASSAULT", "PROSTITUTION", "SEX OFFENSE", "HUMAN TRAFFICKING", "PUBLIC INDECENCY"), 'SEX', mydata$crime)
mydata$crime <- ifelse(mydata$crime %in% c("MOTOR VEHICLE THEFT"), "MVT", mydata$crime)
mydata$crime <- ifelse(mydata$crime %in% c("GAMBLING","INTERFERENCE WITH PUBLIC OFFICER", "INTIMIDATION", "LIQUOR LAW VIOLATION", "OBSCENITY", "NON - CRIMINAL", " PUBLIC INDECENCY", "PUBLIC PEACE VIOLATION", "STALKING", "NON-CRIMINAL (SUBJECT SPECIFIED)"), "NONVIO", mydata$crime)
mydata$crime <- ifelse(mydata$crime %in% c(" NARCOTICS", "OTHER NARCOTIC VIOLATION"), "DRUG", mydata$crime)
mydata$crime <- ifelse(mydata$crime %in% c("OTHER OFFENSE"), "OTHER", mydata$crime)
mydata$crime <- ifelse(mydata$crime %in% c("KIDNAPPING", "WEAPONS VIOLATION", "OFFENSE INVOLVING CHILDREN", "CONCEALED CARRY LICENSE VIOLATION"), "VIO", mydata$crime)
mydata$crime <- ifelse(mydata$crime == "CRIMINAL DAMAGE", "DAMAGE", mydata$crime)
mydata$crime <- ifelse(mydata$crime == "CRIMINAL TRESPASS", "TRESPASS", mydata$crime)
mydata$crime <- ifelse(mydata$crime == "DECEPTIVE PRACTICE", "FRAUD", mydata$crime)



table(mydata$crime)
length(unique(mydata$crime))


library("ggplot2")

qplot(mydata$crime, xlab = "crime", main = "Crimes in Chicago") + scale_y_continuous("Number of Crimes")

qplot(mydata$time.tag, xlab ="Time of Day", main = "Crimes by time of day")+ scale_y_continuous("Number of Crimes")

mydata$day <- factor(mydata$day, levels=c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

qplot(mydata$day, xlab = "Day of Week", main = "Crime by Day of Week")+ scale_y_continuous("Number of Crimes")

mydata$month <- factor(mydata$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

qplot(mydata$month, xlab = "Month", main = "Crime By Month") + scale_y_continuous("Number of Crimes")

temp <- aggregate(mydata$crime, by = list(mydata$crime, mydata$time.tag), FUN = length)
temp
names(temp) <- c("crime", "time.tag", "count")
names(temp)


ggplot(temp, aes(x= crime, y= factor(time.tag))) +
  geom_tile(aes(fill=count)) + scale_x_discrete("Crime", expand = c(0,0)) +
  scale_y_discrete("Time of Day" , expand = c(0,-2)) +
  scale_fill_gradient("Number of Crimes", low= "pink", high= "blue") +
  theme_bw() + ggtitle("Crimes By Time Of Day") +
  theme(panel.grid.major = element_line(color = NA), panel.grid.minor = element_line(colour = NA))





