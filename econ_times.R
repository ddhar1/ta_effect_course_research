# Extracting and manipulating class time data from the econ files

library(stringr)
setwd( "~/UWE Research" )

# reads in the list of econ sections we have
times <- read.csv( "UCB Schedules/Econ1Classes.csv", header = T, stringsAsFactors = F)

colnames(times) <- c("ec1semester", "Full.Discussion", "Day.Time")

# military, isMonday, and isNTakes in a class time, outputs the class time in military format
military <- function( x ) {
  if ( is.na(x))
  {
    return(NA)
  }
  if( x <= 6 ) 
  {
    time <- x + 12
  }
  else {
    time <- x
    }
  return(time)
}


isMonday <- function(x)
{
  if ( x== "" )
  {
    return( NA )
  }
  if ( x == "T")
  {
    return("TTh") 
  }
  else if ( x == "M")
  {
    return("MW")
  }
  else if ( x == "W")
  {
    return("WF")
  }
  else
  {
    return(x)
  }
}

isMonday_econ2 <- function(x)
{
  if ( x== "" )
  {
    return( NA )
  }
  if ( x == "T")
  {
    return("T") 
  }
  else if ( x == "M")
  {
    return("MW")
  }
  else if ( x == "W")
  {
    return("WF")
  }
  else
  {
    return(x)
  }
}

# Data frame we'll merge with the student file.
econ1_times1 <- times
# extract the econ 1 section time
econ1_times1$ec1section <- as.numeric( str_extract( econ1_times1$Full.Discussion, "[0-9]+") )

# extracts section time and converts it to military
econ1_times1$ec1sectiontime <- sapply( as.numeric( gsub( "-", "", str_extract(econ1_times1$Day.Time, "[0-9]*-" ) ) ), military )
# extracts the first section day and applies isMonday function created above.
econ1_times1$ec1sectionday <- unlist(sapply( str_extract( econ1_times1$Day.Time, "[A-Z]?"), isMonday ))
econ1_times1 <- econ1_times1[ !is.na(econ1_times1$ec1sectiontime ),]
econ1_times1$ec1semester <- as.character(econ1_times1$ec1semester)
econ1_times1 <- econ1_times1[ econ1_times1$ec1semester != "2016 Fall",]
rownames(econ1_times1) <- 1:nrow(econ1_times1)
#IDK why there are so many duplicates in this file
#remove duplicates
econ1_times1 <- econ1_times1[!duplicated(econ1_times1[, c(1:2)], incomparables = FALSE) ,]
econ1_times1[duplicated(econ1_times1[, c(1:2)], incomparables = FALSE) | duplicated(econ1_times1[, c(1:2)], incomparables = FALSE, fromLast = T) ,]


all_econ_classes <- read.csv( "Schedules22.csv", header = T, fileEncoding = "UTF-8-BOM", stringsAsFactors = F)
colnames(all_econ_classes)[7] <- "day.time"

not_fall16 <- all_econ_classes[ all_econ_classes$semester != "2016 Fall", ]
not_fall16 <- dplyr::filter(not_fall16, !grepl('LEC', section))
not_fall16$section <- as.numeric( str_extract( not_fall16$section, "[0-9]+") )
#most classes have 2 sections a week
not_fall16$time <- sapply( as.numeric( gsub( "-", "", str_extract(not_fall16$day.time, "[0-9]*-" ) ) ), military )
not_fall16$day[ not_fall16$class !='2'] <- unlist(sapply( str_extract( not_fall16$day.time[ not_fall16$class !='2'], "[A-Z]?"), isMonday ))
#econ 2 has only 1 section a week
#not_fall16$day[ not_fall16$class == 2] <- unlist(sapply( str_extract( not_fall16$day.time[ not_fall16$class =='2'], "[A-Z]?[a-z]?"), isMonday_econ2 ))
not_fall16$day[ not_fall16$class == 2] <- str_extract( not_fall16$day.time[ not_fall16$class =='2'], "[A-Z]?[a-z]?")

#fall 2016
fall16 <- all_econ_classes[ all_econ_classes$semester == "2016 Fall", ]
rownames(fall16) <- 1:nrow(fall16)
fall16[64, c('v2')] = 'DIS'
fall16 <- dplyr::filter(fall16, grepl('DIS', v2))
fall16$section <- as.numeric( str_extract( fall16$section, "[0-9]+") )
fall16$day <- unlist(sapply( str_extract( fall16$class.1, "[A-Z]?"), isMonday ))
fall16$time = as.numeric( str_extract( fall16$v5, "[0-9]+") )

schedule_list_3 = read.csv( "Schedules3.csv", header = T, fileEncoding = "UTF-8-BOM", stringsAsFactors = F)
schedule_list_3 <- dplyr::filter(schedule_list_3, !grepl('LEC', section))
schedule_list_3$section <- as.numeric( str_extract( schedule_list_3$section, "[0-9]+") ) 
schedule_list_3 = schedule_list_3[schedule_list_3$day.time != "",]
schedule_list_3$time <- sapply( as.numeric( gsub( "-", "", str_extract(schedule_list_3$day.time, "[0-9]*-" ) ) ), military )
schedule_list_3$day <- unlist(sapply( str_extract( schedule_list_3$day.time, "[A-Z]?"), isMonday ))

econ2_times = not_fall16[ not_fall16$class == 2, c('semester', 'section', 'time', 'day')]
rownames(econ2_times) = 1:nrow(econ2_times)
#check duplicates
#econ2_times[duplicated(econ2_times[, c(1:2)], incomparables = FALSE) | duplicated(econ2_times[, c(1:2)], incomparables = FALSE, fromLast = T) ,]
econ2_times <- econ2_times[-c(41:50),]
colnames(econ2_times) = c('ec2semester', 'ec2section', 'ec2sectiontime', 'ec2sectionday')

econ100A_times = data.frame(rbind( fall16[ fall16$class == '100A', c('semester', 'section', 'time', 'day')], not_fall16[ not_fall16$class == '100A', c('semester', 'section', 'time', 'day')]))
#econ100A_times[duplicated(econ100A_times[, c(1:2)], incomparables = FALSE) | duplicated(econ100A_times[, c(1:2)], incomparables = FALSE, fromLast = T) ,]
colnames(econ100A_times) = c('ec100Asemester', 'ec100Asection', 'ec100Asectiontime', 'ec100Asectionday')

econ100B_times = data.frame(rbind( fall16[ fall16$class == '100B', c('semester', 'section', 'time', 'day')], not_fall16[ not_fall16$class == '100B', c('semester', 'section', 'time', 'day')]))
rownames(econ100B_times) = 1:nrow(econ100B_times)
econ100B_times[ duplicated(econ100B_times[,c("section", "semester")]) | duplicated(econ100B_times[,c("section", "semester")], fromLast = T),]
colnames(econ100B_times) = c('ec100Bsemester', 'ec100Bsection', 'ec100Bsectiontime', 'ec100Bsectionday')
#econ100B_times <- econ100B_times[-c(357:371),]

econ101B_times = data.frame(rbind( fall16[ fall16$class == '101B', c('semester', 'section', 'time', 'day')], not_fall16[ not_fall16$class == '101B', c('semester', 'section', 'time', 'day')]))
colnames(econ101B_times) = c('ec101Bsemester', 'ec101Bsection', 'ec101Bsectiontime', 'ec101Bsectionday')

econ101A_times = data.frame(rbind( fall16[ fall16$class == '101A', c('semester', 'section', 'time', 'day')], not_fall16[ not_fall16$class == '101A', c('semester', 'section', 'time', 'day')]))
colnames(econ101A_times) = c('ec101Asemester', 'ec101Asection', 'ec101Asectiontime', 'ec101Asectionday')
#  write code for fall 2016 and put it in the above dataframe (append)
# then we merge the above by to students.new by class, semester, and section 

#get econ 1 classes we don't have in the other list
econ1_times_otherlist = not_fall16[ not_fall16$class == 1 & !( not_fall16$semester %in% unique(econ1_times1$ec1semester)), c('semester', 'section', 'time', 'day')]
colnames(econ1_times_otherlist) = c('ec1semester', 'ec1section', 'ec1sectiontime', 'ec1sectionday')

econ1_fall16 = fall16[fall16$class == '1', c('semester', 'section', 'time', 'day')]
colnames(econ1_fall16)= c('ec1semester', 'ec1section', 'ec1sectiontime', 'ec1sectionday')
econ1_33 = schedule_list_3[ schedule_list_3$course == '1', c('semester', 'section', 'time', 'day')]
colnames(econ1_33) = c('ec1semester', 'ec1section', 'ec1sectiontime', 'ec1sectionday')

checkdupli1 = rbind( econ1_times1[econ1_times1 != "2016 Summer", c(1, 4:6)], econ1_times_otherlist, econ1_fall16, econ1_33[ econ1_33$ec1semester == "2016 Summer", ] )
rownames(checkdupli1) = 1:nrow(checkdupli1)
#check duplicates
checkdupli1[duplicated(checkdupli1[, c(1:2)], incomparables = FALSE) | duplicated(checkdupli1[, c(1:2)], incomparables = FALSE, fromLast = T) ,]

#remove na columns which, for some reason, exist
econ1_times = checkdupli1[ !is.na(checkdupli1$ec1section) & !is.na(checkdupli1$ec1semester), ]
#econ1_times = rbind( econ1_times2, econ1_3[econ1_3$ec1semester %in% setdiff( unique( econ1_3$ec1semester), unique( econ1_times2$ec1semester)),])
rownames(econ1_times) = 1:nrow(econ1_times)
#econ1_times = econ1_times[c(102:124,265:284),] #remove some duplicates

# check duplicates
econ1_times[duplicated(econ1_times[, c(1:2)], incomparables = FALSE) | duplicated(econ1_times[, c(1:2)], incomparables = FALSE, fromLast = T) ,]

