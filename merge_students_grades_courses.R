# Title: Econ Student Merge
# Author: Divya Dhar
# Data (last edit): 4/7/18
# About: We have a list of all students who have attempted to take an economics course. 
# We want to expand this file (while) to include courses taken, profs of the students, etc. for each student

# Run Econ1_time.R

# import econ major course enrollment 

library(tidyr)

setwd( "~/UWE Research" )
#run econ1_time.R first!
# run econ_graduate schools too!

# Read course enrollment data course.enroll has semester data/sections
#course.enroll.grades has grades
#course.enroll had new students
course.enroll <- read.table( "~/UWE Research/original/course.enrollment.data.econ.sections.2007.2017.txt", header = T, sep = "\t", as.is = T, stringsAsFactors = F)
course.enroll.grades <- read.table( "~/UWE Research/original/course.enrollment.data.econ.grades.2007.2017.v.2.txt", header = T, sep = "\t", as.is = T, stringsAsFactors = F)

# #course.enroll.2 - not sure what it has but importing just in case
# course.enroll.2 <- read.table("~/UWE Research/original/course.enrollment.data.econ.2007.2017(1).txt", header = T, sep = "\t", as.is = T)


# two ways to name course.enroll since i don't want to change  var names
course.enroll.sections1 <- read.table( "~/UWE Research/original/course.enrollment.data.econ.sections.2007.2017.txt", header = T, sep = "\t", as.is = T, stringsAsFactors = F)

#, Grade.Points.Nbr2 = scale(Grade.Points.Nbr),
#standardize the grades by class
# note that this replaces course.enroll.grades as opposed to making a new column for standard grades
library(dplyr)
course.enroll.grades = course.enroll.grades %>%
  group_by( .dots =c('year.term','course.number' ) ) %>%
  mutate(Grade.Points.Nbr_std = scale( Grade.Points.Nbr ))

# indicator for whether or not the student passed the exam, 2 = pass, 1 = not pass
course.enroll.grades$pass <- ifelse( course.enroll.grades$Grade.Points.Nbr >= 1.7, 2, 1)

#not sure about the purpose of this dataset
original.course.enroll <- read.table( "~/UWE Research/original/course.enrollment.data.econ.2007.2017(1).txt", header = T, sep = "\t", as.is = T, stringsAsFactors = F)
# for own reference: original.course.enroll unique ppsk: 25640; course.enroll (sections) unique ppsk: 26451; course.enroll.grades unique ppsk: 26094




# Import original list of students. Currently only has student demographics
students <- read.table( "~/UWE Research/original/uc.berkeley.econ.2007.2016 nov 8 (1).txt", header = T, as.is =  T, sep = "\t", stringsAsFactors = F)
students <- students[!duplicated( students$ppsk ), ]
students <- students[students$entry == "NEW FRESHMEN", ]
rownames(students) <- 1:nrow(students)
 #just do this on transfers w/ students[ students$entry == "ADVANCED STANDING",]
#students <- students[ students$entry == "ADVANCED STANDING",]

# students.new will be the student file + undergraduate courses ('version control')
students.new <- students

#!!!---- We need to get a list of undergraduate courses
#Get a unique list of courses
courses <- unique ( course.enroll.grades$course.number)

# Make another vector that will have logical information about whether or not
#the class is an undergrad or grad class (keep undergrad classes)
keep <- vector( mode = "logical", length = length( courses ))

#iterating through list - determining if it's an undergrad class
# keep[i] = true if it's undergrad, false if grad
for (i in 1:length( courses ) )
{ 
  # definitely keep lower division courses
  if( nchar( courses[i] )[1] == 1 | nchar( courses[i])[1] == 2 )
      {
        keep[i] <- T
  }
  else
  {
    # may be a grad class - check if it has a 2 in the front
    if( !is.na( as.numeric(substring( courses[i], 1, 1) ) ) )
    {
      if( substring( courses[i], 1, 1 ) == '2')
      {
        keep[i] <- F
      }
      else
      {
        keep[i] <- T
      }
    }
    else
    {
      if( substring( courses[i], 2, 2) == '2' )
      {
        keep[i] <- F
      }
      else
      {
        keep[i] <- T
      }
    }
  }
}

# now we have a list of undergrad classes! unlist command in the index part of course list
# in order to return a list that only had true in keep
ugrad.courses <- courses[ unlist(keep)]

#If I didn't right this when I was just starting to learn R
# I would have probably made a new variable for each type of course that had
# that course's section, grades, semesters for each students. and then merged that 
# with the students file. 
# instead i use a for loop that  goes through each student
# looks for all the classes a that student took
# 


# making ugrad courses a string so we can now
# add columns to student list
ugrad.courses <- lapply( ugrad.courses, toString )

# we want the student file to include the grade, section, and semester associated with a student and a course
# we'll have to make 4* # of undergrad courses to the students file.
addendum<- vector( mode = "character", length = length( ugrad.courses )*5)
addendum.temp<- vector( mode = "character", length = length( ugrad.courses )*5 )
counter <- 1 #we want a column for semester it was taken, gpa, section (which is useless right now but u never know lol)

# so we did this to make an array of student course names
for( j in ugrad.courses )
{
  addendum[ counter ] <- paste( "ec", j, 'semester', sep= "")
  addendum.temp[ counter ] <- j
  counter <- counter + 1
  addendum[ counter ] <- paste("ec", j, 'grade', sep= "" )
  addendum.temp[ counter ] <- j
  counter <- counter + 1
  addendum[counter] <- paste("ec", j, 'section', sep= "" )
  addendum.temp[ counter ] <- j
  counter <- counter + 1
  addendum[counter] <- paste("ec", j, 'pass', sep = "")
  counter <- counter + 1
  addendum[counter] <- paste("ec", j, 'stdgrade', sep = "")
  counter <- counter + 1
}

w <- 1
while( w <= length( ugrad.courses ) )
{
  students.new.colnames <- colnames( students.new )
  students.new <- data.frame( students.new,  vector( length = length(students.new$ppsk)), vector( length = length(students.new$ppsk)), vector( "numeric", length = length(students$ppsk)), vector( "numeric", length = length(students$ppsk)), vector( "numeric", length = length(students$ppsk)) )
  colnames( students.new ) <- c( students.new.colnames, ugrad.courses[w], ugrad.courses[w], ugrad.courses[w],ugrad.courses[w],ugrad.courses[w]  )
  w <- w+1
}

# i was running out of names and didn't want to mess up the students file so now
# students.new2 is the list of students 
students.new2 <- students.new

#course_lettergrades_econ <- data.frame( rep( "", nrow(students.new2) ) )
#course_lettergrades_standardized_econ <- data.frame( rep( "", nrow(students.new2) ) )


students_nosections <- setdiff(students.new2$ppsk, course.enroll$ppsk)

students_nosections <- c( students_nosections, 1313005)

#this for loop iterature through our list of students on the student list (students.new2)
# finds that same student on courses enroll, and inputs what class they took,
# which semester, etc.

for( k in 1:length(students.new2$ppsk) ) #iterate through students
{
  #   if( students.new2$ppsk[k] %in% c(1313005, 1485948) )
  #   {
  #     y <- which(course.enroll.grades$ppsk == students.new2$ppsk[k], arr.ind=TRUE ) #find which courses they were enrolled in
  #     
  #     for ( l in y ) #iterate through list of courses they took a grade in.
  #       # y is an index for row a course with grade is listed in.
  #     {
  #       #print( course.enroll.grades$course.number[l] )
  #       find.course.cols <- which( colnames( students.new ) == course.enroll.grades$course.number[l], arr.ind = T )
  #       # now we have columns of courses semester, section and grade
  #       #print( find.course.cols )
  #       #print(students.new2$ppsk[k])
  #       
  #       if ( length( find.course.cols ) == 3 )
  #       {
  #         # this is semester
  #         students.new2[ k , find.course.cols[1] ] <- as.character( course.enroll.grades[ l, 2 ] )
  #         
  #         #grade
  #         #using letter grade
  #         students.new2[ k , find.course.cols[2] ] <- course.enroll.grades[ l, 14 ][[1]]
  #         #using grade number
  #         #students.new2[ k , find.course.cols[2] ] <- course.enroll.grades[ l, 13 ]
  # 
  #         # if( length( course.enroll[ course.enroll$ppsk == students.new2$ppsk[k] & course.enroll$year.term == course.enroll.grades[ l, 2 ] & course.enroll$course.number == course.enroll.grades$course.number[l], 7 ] ) )
  #         # {
  #         #   if ( length(course.enroll[ course.enroll$ppsk == students.new2$ppsk[k] & course.enroll$year.term == course.enroll.grades[ l, 2 ] & course.enroll$course.number == course.enroll.grades$course.number[l], 7 ]) > 0 )
  #         #   {# section only if it exists otherwise we may run into issues?
  #         #   students.new2[ k , find.course.cols[3] ] <- course.enroll[ course.enroll$ppsk == students.new2$ppsk[k] & course.enroll$year.term == course.enroll.grades[ l, 2 ] & course.enroll$course.number == course.enroll.grades$course.number[l], 7 ]
  #         #   
  #         #   }
  #         # }
  #         
  #         if( length( course.enroll[ course.enroll$ppsk == students.new2$ppsk[k] & course.enroll$year.term == course.enroll.grades[ l, 2 ] & course.enroll$course.number == course.enroll.grades$course.number[l], 7 ] ) )
  #         {
  #           if ( length(course.enroll[ course.enroll$ppsk == students.new2$ppsk[k] & course.enroll$year.term == course.enroll.grades[ l, 2 ] & course.enroll$course.number == course.enroll.grades$course.number[l], 7 ]) > 0 )
  #           {# section only if it exists otherwise we may run into issues?
  #             students.new2[ k , find.course.cols[3] ] <- course.enroll[ course.enroll$ppsk == students.new2$ppsk[k] & course.enroll$year.term == course.enroll.grades[ l, 2 ] & course.enroll$course.number == course.enroll.grades$course.number[l], 7 ]
  #             
  #           }
  #         }
  #         
  #       }
  #     }
  #     
  #   }
  # else{
  # 
  # }
  y <- which(course.enroll.grades$ppsk == students.new2$ppsk[k], arr.ind=TRUE ) #find which courses they were enrolled in
  
  for ( l in y ) #iterate through rows of courses they took a grade in.
    # y is an index for row a course with grade is listed in.
  {
    find.course.cols <- which( colnames( students.new2 ) == course.enroll.grades$course.number[l], arr.ind = T )
    # now we have columns of courses semester, section and grade
    #print( find.course.cols )
    #print(students.new2$ppsk[k])
    
    if ( length( find.course.cols ) == 5 ) #the number of columns for a course
    {

      # this is semester
      students.new2[ k , find.course.cols[1] ] <- as.character( course.enroll.grades$year.term[ l ] )
      
      #using letter grade 
      #students.new2[ k , find.course.cols[2] ] <- course.enroll.grades[ l, 14 ][[1]]
      #using grade number
      students.new2[ k , find.course.cols[2] ] <- course.enroll.grades$Grade.Points.Nbr[ l ]
      students.new2[ k , find.course.cols[5] ] <- course.enroll.grades$Grade.Points.Nbr_std[ l ]
      students.new2[ k , find.course.cols[4] ] <- course.enroll.grades$pass[ l ]
      
      #print( course.enroll.grades$Grade.Points.Nbr[ l ])
      # if the course is an intro econ course
      # if( ( course.enroll.grades$course.number[l] %in% c( "1", "2", "100A", "101A", "100B", "101B") ) )
      # {
      #   if ( length(course.enroll$section[ course.enroll$ppsk == students.new2$ppsk[k] & course.enroll$year.term == course.enroll.grades$year.term[ l] & course.enroll$course.number == course.enroll.grades$course.number[l]]) == 1 )
      #   {
      #     # find the section through matching ppsk, year.term, and course number of the class
      #     students.new2[ k , find.course.cols[3] ] <- course.enroll$section[ course.enroll$ppsk == students.new2$ppsk[k] & course.enroll$year.term == course.enroll.grades$year.term[l] & course.enroll$course.number == course.enroll.grades$course.number[l]]
      #   }
      # }
      if (!( students.new$ppsk[k] %in% students_nosections) )
      {
        if( course.enroll.grades$course.number[l] %in% c( "1", "2", "100A", "101A", "100B", "101B") )
        {
          if( 0 != length(course.enroll$section[ course.enroll$ppsk == students.new2$ppsk[k] &
                                                 course.enroll$year.term == course.enroll.grades$year.term[l] & 
                                                 course.enroll$course.number == course.enroll.grades$course.number[l]])  ) 
          {
            students.new2[ k , find.course.cols[3] ] <- course.enroll$section[ course.enroll$ppsk == students.new2$ppsk[k] &
                                                                                 course.enroll$year.term == course.enroll.grades$year.term[l] & 
                                                                                 course.enroll$course.number == course.enroll.grades$course.number[l]]
            }
          
        }
      }
      
    }
    
  }
}

colnames( students.new2 ) <- c( colnames( students), addendum )


start <- length( colnames( students ) )

#some cells showed false instead of NA, so I replaced them all with
#with a blank cell
one.two.three <- 1
for( i in 1:length( addendum ) )
{
  if ( one.two.three == 1)
  {
    students.new2[ , start + i ][ students.new2[, start + i ] == "FALSE" ] <- ""
    one.two.three <- one.two.three + 1
  }
  else
  {
    if ( one.two.three == 2)
    {
      students.new2[ , start + i ][ students.new2[, start + i ] == 0 ] <- ""
      one.two.three <- one.two.three + 1
    }
    else
    {
      if (one.two.three == 3)
      {
        students.new2[ , start + i ][ students.new2[, start + i ] == 0 ] <- ""
        one.two.three <- 1 + one.two.three
      }
      else
      {
        if( one.two.three == 4 )
        {
          students.new2[ , start + i ][ students.new2[, start + i ] == 0 ] <- ""
          one.two.three <- 1 + one.two.three
        }
        else
        {
          if( one.two.three == 5 )
          {
            students.new2[ , start + i ][ students.new2[, start + i ] == 0 ] <- ""
            one.two.three <- 1
          }
        }
      }
    }
  }
}

students.new3 <- students.new2 #read.csv( file = "~/UWE Research/students.courses.gsi.demo.merge.csv", header = T)

course.prereq <- read.table( file = "D:/#Home/Divya/Documents/UWE Research/original/course.enrollment.data.econ.prereq.grades.2007.2017.txt", header = T, sep = "\t", as.is = T, stringsAsFactors = F)


course.prereq = course.prereq %>%
  group_by( .dots =c('year.term','course.number' ) ) %>%
  mutate(Grade.Points.Nbr_Std = scale( Grade.Points.Nbr ))


# we first need a list of courses in the file
# don't worry about department for now, it's usually the same
prereq.courses <- unique( course.prereq$course.number )
prereq.courses <- unlist( lapply( prereq.courses, toString ) )

#also the students who are listed in students but not in course.enroll (which has sections)
#just messing about
#students.no.section <- setdiff( students$ppsk, unique(course.enroll$ppsk))


addendum<- vector( mode = "character", length = length( prereq.courses )*3 )
addendum.temp<- vector( mode = "character", length = length( prereq.courses )*3 )
counter <- 1 

stat <- c( "20", "21", "134", "131A", "88", "W21")
math <- c( "1A", "16A", "16B", "1B")
ieor <- c( "172")
counter <- 1
for( j in prereq.courses )
{
  if( j %in% stat )
  {
    addendum[ counter ] <- paste( "stat", j, 'semester', sep= "")
    addendum.temp[ counter ] <- j
    counter <- counter + 1
    addendum[ counter ] <- paste("stat", j, 'grade', sep= "" )
    addendum.temp[ counter ] <- j
    counter <- counter + 1
    addendum[ counter ] <- paste("stat", j, 'stdgrade', sep= "" )
    addendum.temp[ counter ] <- j
    counter <- counter + 1
    #addendum[counter] <- paste("stat", j, 'section', sep= "" )
    #addendum.temp[ counter ] <- j
    #counter <- counter + 1
  }
  else if ( j %in% math )
  {
    addendum[ counter ] <- paste( "math", j, 'semester', sep= "")
    addendum.temp[ counter ] <- j
    counter <- counter + 1
    addendum[ counter ] <- paste("math", j, 'grade', sep= "" )
    addendum.temp[ counter ] <- j
    counter <- counter + 1
    addendum[ counter ] <- paste("math", j, 'stdgrade', sep= "" )
    addendum.temp[ counter ] <- j
    counter <- counter + 1
    #addendum[counter] <- paste("math", j, 'section', sep= "" )
    #addendum.temp[ counter ] <- j
    #counter <- counter + 1
  }
  else
  {
  addendum[ counter ] <- paste( "ieor", j, 'semester', sep= "")
  addendum.temp[ counter ] <- j
  counter <- counter + 1
  addendum[ counter ] <- paste("ieor", j, 'grade', sep= "" )
  addendum.temp[ counter ] <- j
  counter <- counter + 1
  addendum[ counter ] <- paste("ieor", j, 'stdgrade', sep= "" )
  addendum.temp[ counter ] <- j
  counter <- counter + 1
  #addendum[counter] <- paste("ieor", j, 'section', sep= "" )
  #addendum.temp[ counter ] <- j
  #counter <- counter + 1
    }
}


students.new.colnames2 <- colnames( students.new3 )
students.new4 <- students.new3

# add column names for the new prereq courses that are added to the student list.
w <- 1
while( w <= length( prereq.courses ) )
{
  students.new.colnames2 <- colnames( students.new4 )
  students.new4 <- data.frame( students.new4, vector( length = length(students$ppsk)), vector( "numeric", length = length(students$ppsk)), vector( "numeric", length = length(students$ppsk)) )
  colnames( students.new4 ) <- c( students.new.colnames2, prereq.courses[w], prereq.courses[w],  prereq.courses[w])
  w <- w+1
}

# finds that same student on course prereq list, and inputs what prereqs (semester, grade, etc).
for( k in 1:length(students.new4$ppsk) ) #iterate through students
{
  y <- which(course.prereq$ppsk == students.new4$ppsk[k], arr.ind=TRUE ) #find which courses they were enrolled in
  
  for ( l in y ) #iterate through list of courses they took a grade in.
    # y is an index for row a course with grade is listed in.
  {
    
    find.course.cols <- which( colnames( students.new4 ) == course.prereq$course.number[l], arr.ind = T )
    # now we have columns of courses semester, section and grade
    #print( find.course.cols )
    #print(yo$ppsk[k])
    
    if ( length( find.course.cols ) == 3 )
    {
      # this is semester
      students.new4[ k , find.course.cols[1] ] <- as.character( course.prereq$year.term[ l ] )
      
      #grade
      #this is grade number
      students.new4[ k , find.course.cols[2] ] <- course.prereq$Grade.Points.Nbr[ l ]
      students.new4[ k , find.course.cols[3] ] <- course.prereq$Grade.Points.Nbr_Std[ l ]
      
      #letter grade
      #students.new4[ k , find.course.cols[2] ] <- course.prereq$Grade.Nm[ l ]
      
      # we're not going to bother with section for right now
      #students.new4[ k , find.course.cols[3] ] <- course.prereq[ course.enroll$ppsk == yo$ppsk[k] & course.enroll$year.term == course.enroll.grades[ l, 2 ] & course.enroll$course.number == course.enroll.grades$course.number[l], 7 ]
      
    }
    
  }
  
}

colnames( students.new4 ) <- c( colnames( students.new3), addendum )

# stata doesn't take NA's well, so I just replaced them with a empty cell
students.new4[is.na(students.new4)] <- ""

start <- length( colnames( students.new3 ) )

#some cells showed false instead of NA, so I replaced them all with
#with a blank cell
one.two<- 1
for( i in 1:length( addendum ) )
{
  if ( one.two == 1)
  {
    students.new4[ , start + i ][ students.new4[, start + i ] == "FALSE" ] <- ""
    one.two <- one.two + 1
  }
  else if ( one.two == 2 )
  {
      students.new4[ , start + i ][ students.new4[, start + i ] == 0 ] <- ""
      one.two <- 1 + one.two
  }
  else
  {
    students.new4[ , start + i ][ students.new4[, start + i ] == 0 ] <- ""
    one.two <- 1
  }
}


### gsi demographics
# i import a list of GSIs
# i seperate each gsi by class and clean each class in order to merge this with my final dataframe
gsi_names_demos <- read.csv("gsi_file_again.csv", stringsAsFactors = F, header = T, fileEncoding = "UTF-8-BOM")
colnames(gsi_names_demos) <- gsub( "X", "", colnames(gsi_names_demos))

gsi.demo_withsubs_ec1 <- gsi_names_demos[ gsi_names_demos$course.number == "1", -which(colnames(gsi_names_demos) %in% c("course.number", ""))]
colnames(gsi.demo_withsubs_ec1) <-  paste0("ec1",colnames(gsi.demo_withsubs_ec1))
gsi.demo_withsubs_ec1[ duplicated(gsi.demo_withsubs_ec1[, c(2:3)] ) | duplicated(gsi.demo_withsubs_ec1[, c(2:3)], fromLast = T),]

gsi.demo_withsubs_ec2 <- gsi_names_demos[ gsi_names_demos$course.number == "2", -which(colnames(gsi_names_demos) %in% c("course.number", ""))]
colnames(gsi.demo_withsubs_ec2) <-  paste0("ec2",colnames(gsi.demo_withsubs_ec2))
gsi.demo_withsubs_ec2[ duplicated(gsi.demo_withsubs_ec2[, c(2:3)] ) | duplicated(gsi.demo_withsubs_ec2[, c(2:3)], fromLast = T),]


gsi.demo_withsubs_ec100A <- gsi_names_demos[ gsi_names_demos$course.number == "100A", -which(colnames(gsi_names_demos) %in% c("course.number", ""))]
colnames(gsi.demo_withsubs_ec100A) <-  paste0("ec100A",colnames(gsi.demo_withsubs_ec100A))
gsi.demo_withsubs_ec100A[ duplicated(gsi.demo_withsubs_ec100A[, c(2:3)] ) | duplicated(gsi.demo_withsubs_ec100A[, c(2:3)], fromLast = T),]

gsi.demo_withsubs_ec100B <- gsi_names_demos[ gsi_names_demos$course.number == "100B", -which(colnames(gsi_names_demos) %in% c("course.number", ""))]
rownames(gsi.demo_withsubs_ec100B) <- 1:nrow(gsi.demo_withsubs_ec100B)
gsi.demo_withsubs_ec100B[ duplicated(gsi.demo_withsubs_ec100B[, c(2:3)] ) | duplicated(gsi.demo_withsubs_ec100B[, c(2:3)], fromLast = T),]
colnames(gsi.demo_withsubs_ec100B) <-  paste0("ec100B",colnames(gsi.demo_withsubs_ec100B))
gsi.demo_withsubs_ec100B <- gsi.demo_withsubs_ec100B[-c(206, 208, 210, 212, 214, 216 ),]

gsi.demo_withsubs_ec101A <- gsi_names_demos[ gsi_names_demos$course.number == "101A", -which(colnames(gsi_names_demos) %in% c("course.number", ""))]
colnames(gsi.demo_withsubs_ec101A) <-  paste0("ec101A",colnames(gsi.demo_withsubs_ec101A))
rownames(gsi.demo_withsubs_ec101A) <- 1:nrow(gsi.demo_withsubs_ec101A)
gsi.demo_withsubs_ec101A[ duplicated(gsi.demo_withsubs_ec101A[, c(2:3)] ) | duplicated(gsi.demo_withsubs_ec101A[, c(2:3)], fromLast = T),]
gsi.demo_withsubs_ec101A <- gsi.demo_withsubs_ec101A[-c(49, 51, 53, 55), ]

gsi.demo_withsubs_ec101B <- gsi_names_demos[ gsi_names_demos$course.number == "101B", -which(colnames(gsi_names_demos) %in% c("course.number", ""))]
colnames(gsi.demo_withsubs_ec101B) <-  paste0("ec101B",colnames(gsi.demo_withsubs_ec101B))
rownames(gsi.demo_withsubs_ec101B) <- 1:nrow(gsi.demo_withsubs_ec101B)

gsi.demo_withsubs_ec101B[ duplicated(gsi.demo_withsubs_ec101B[, c(2:3)] ) | duplicated(gsi.demo_withsubs_ec101B[, c(2:3)], fromLast = T),]


# calculates the ratio a certain appears in array
ratio <- function( x, term )
{
  value <- length(x[x == term])  / length(x)
  return(value)
}

ratio2 <- function( x, term )
{
  value <- length(x[x %in% term ])  / length(x)
  return(value)
}

gsi.demo_withsubs_ec1$ec1_gsi_race2[gsi.demo_withsubs_ec1$ec1_gsi_race ==  "2 or More Races - Non-URM"] <- "other"
gsi.demo_withsubs_ec1$ec1_gsi_race2[gsi.demo_withsubs_ec1$ec1_gsi_race == "White/Other" | gsi.demo_withsubs_ec1$ec1_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec1$ec1_gsi_race2[ gsi.demo_withsubs_ec1$ec1_gsi_race == "Asian/Pacific Islander"  | gsi.demo_withsubs_ec1$ec1_gsi_race_sub %in% c("South Asian", "East Asian")] <- "asian"
gsi.demo_withsubs_ec1$ec1_gsi_race2[gsi.demo_withsubs_ec1$ec1_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec1$ec1_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino")] <- "URM"

gsi.demo_withsubs_ec1$ec1_gsi_race_sa_as_urm[gsi.demo_withsubs_ec1$ec1_gsi_race ==  "2 or More Races - Non-URM"] <- "other"
gsi.demo_withsubs_ec1$ec1_gsi_race_sa_as_urm[gsi.demo_withsubs_ec1$ec1_gsi_race == "White/Other" | gsi.demo_withsubs_ec1$ec1_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec1$ec1_gsi_race_sa_as_urm[gsi.demo_withsubs_ec1$ec1_gsi_race == "Asian/Pacific Islander" | gsi.demo_withsubs_ec1$ec1_gsi_race_sub == "East Asian"] <-  "asian"
gsi.demo_withsubs_ec1$ec1_gsi_race_sa_as_urm[gsi.demo_withsubs_ec1$ec1_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec1$ec1_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino", "South Asian")] <-  "URM"

gsi.demo_withsubs_ec2$ec2_gsi_race2[ gsi.demo_withsubs_ec2$ec2_gsi_race ==  "2 or More Races - Non-URM"] <- "other"
gsi.demo_withsubs_ec2$ec2_gsi_race2[gsi.demo_withsubs_ec2$ec2_gsi_race == "White/Other" | gsi.demo_withsubs_ec2$ec2_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec2$ec2_gsi_race2[gsi.demo_withsubs_ec2$ec2_gsi_race == "Asian/Pacific Islander"  | gsi.demo_withsubs_ec2$ec2_gsi_race_sub %in% c("South Asian", "East Asian")] <- "asian"
gsi.demo_withsubs_ec2$ec2_gsi_race2[gsi.demo_withsubs_ec2$ec2_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec2$ec2_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino")] <- "URM"

gsi.demo_withsubs_ec2$ec2_gsi_race_sa_as_urm[gsi.demo_withsubs_ec2$ec2_gsi_race ==  "2 or More Races - Non-URM"] <-  "other"
gsi.demo_withsubs_ec2$ec2_gsi_race_sa_as_urm[gsi.demo_withsubs_ec2$ec2_gsi_race == "White/Other" | gsi.demo_withsubs_ec2$ec2_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec2$ec2_gsi_race_sa_as_urm <- ifelse( gsi.demo_withsubs_ec2$ec2_gsi_race == "Asian/Pacific Islander" | gsi.demo_withsubs_ec2$ec2_gsi_race_sub == "East Asian", "asian", "" )
gsi.demo_withsubs_ec2$ec2_gsi_race_sa_as_urm[gsi.demo_withsubs_ec2$ec2_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec2$ec2_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino", "South Asian")] <-"URM"


gsi.demo_withsubs_ec100A$ec100A_gsi_race2[gsi.demo_withsubs_ec100A$ec100A_gsi_race ==  "2 or More Races - Non-URM"] <- "other"
gsi.demo_withsubs_ec100A$ec100A_gsi_race2[gsi.demo_withsubs_ec100A$ec100A_gsi_race == "White/Other" | gsi.demo_withsubs_ec100A$ec100A_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec100A$ec100A_gsi_race2[gsi.demo_withsubs_ec100A$ec100A_gsi_race == "Asian/Pacific Islander"  | gsi.demo_withsubs_ec100A$ec100A_gsi_race_sub %in% c("South Asian", "East Asian")] <-  "asian"
gsi.demo_withsubs_ec100A$ec100A_gsi_race2[gsi.demo_withsubs_ec100A$ec100A_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec100A$ec100A_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino")] <- "URM"

gsi.demo_withsubs_ec100A$ec100A_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100A$ec100A_gsi_race ==  "2 or More Races - Non-URM"] <- "other"
gsi.demo_withsubs_ec100A$ec100A_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100A$ec100A_gsi_race == "White/Other" | gsi.demo_withsubs_ec100A$ec100A_gsi_race_sub == "White"] <-  "white"
gsi.demo_withsubs_ec100A$ec100A_gsi_race_sa_as_urm[ gsi.demo_withsubs_ec100A$ec100A_gsi_race == "Asian/Pacific Islander" | gsi.demo_withsubs_ec100A$ec100A_gsi_race_sub == "East Asian"] <-  "asian"
gsi.demo_withsubs_ec100A$ec100A_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100A$ec100A_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec100A$ec100A_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino", "South Asian")] <- "URM"

gsi.demo_withsubs_ec100B$ec100B_gsi_race2[gsi.demo_withsubs_ec100B$ec100B_gsi_race ==  "2 or More Races - Non-URM"] <- "other"
gsi.demo_withsubs_ec100B$ec100B_gsi_race2[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "White/Other" | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec100B$ec100B_gsi_race2[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "Asian/Pacific Islander"  | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub %in% c("South Asian", "East Asian")] <- "asian"
gsi.demo_withsubs_ec100B$ec100B_gsi_race2[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino")] <- "URM"

gsi.demo_withsubs_ec100B$ec100B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100B$ec100B_gsi_race ==  "2 or More Races - Non-URM"] <- "other"
gsi.demo_withsubs_ec100B$ec100B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "White/Other" | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec100B$ec100B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "Asian/Pacific Islander" | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub == "East Asian"] <-  "asian"
gsi.demo_withsubs_ec100B$ec100B_gsi_race_sa_as_urm[ gsi.demo_withsubs_ec100B$ec100B_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino", "South Asian")] <-"URM"
                                                   
gsi.demo_withsubs_ec101B$ec101B_gsi_race2[gsi.demo_withsubs_ec101B$ec101B_gsi_race ==  "2 or More Races - Non-URM"] <-  "other"
gsi.demo_withsubs_ec101B$ec101B_gsi_race2[gsi.demo_withsubs_ec101B$ec101B_gsi_race == "White/Other" | gsi.demo_withsubs_ec101B$ec101B_gsi_race_sub == "White"] <-  "white"
gsi.demo_withsubs_ec101B$ec101B_gsi_race2[gsi.demo_withsubs_ec101B$ec101B_gsi_race == "Asian/Pacific Islander"  | gsi.demo_withsubs_ec101B$ec101B_gsi_race_sub %in% c("South Asian", "East Asian")] <-  "asian"
gsi.demo_withsubs_ec101B$ec101B_gsi_race2[ gsi.demo_withsubs_ec101B$ec101B_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec101B$ec101B_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino")] <- "URM"
                                                   
gsi.demo_withsubs_ec101B$ec101B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec101B$ec101B_gsi_race ==  "2 or More Races - Non-URM"] <- "other"
gsi.demo_withsubs_ec101B$ec101B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec101B$ec101B_gsi_race == "White/Other" | gsi.demo_withsubs_ec101B$ec101B_gsi_race_sub == "White"] <-  "white"
gsi.demo_withsubs_ec101B$ec101B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec101B$ec101B_gsi_race == "Asian/Pacific Islander" | gsi.demo_withsubs_ec101B$ec101B_gsi_race_sub == "East Asian"] <-  "asian"
gsi.demo_withsubs_ec101B$ec101B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec101B$ec101B_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec101B$ec101B_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino", "South Asian")] <- "URM"


gsi.demo_withsubs_ec100B$ec100B_gsi_race2[gsi.demo_withsubs_ec100B$ec100B_gsi_race ==  "2 or More Races - Non-URM"] <-  "other"
gsi.demo_withsubs_ec100B$ec100B_gsi_race2[ gsi.demo_withsubs_ec100B$ec100B_gsi_race == "White/Other" | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec100B$ec100B_gsi_race2[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "Asian/Pacific Islander"  | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub %in% c("South Asian", "East Asian")] <-  "asian"
gsi.demo_withsubs_ec100B$ec100B_gsi_race2[ gsi.demo_withsubs_ec100B$ec100B_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino")] <- "URM"

gsi.demo_withsubs_ec100B$ec100B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100B$ec100B_gsi_race ==  "2 or More Races - Non-URM"] <-  "other"
gsi.demo_withsubs_ec100B$ec100B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "White/Other" | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec100B$ec100B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "Asian/Pacific Islander" | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub == "East Asian"] <-  "asian"
gsi.demo_withsubs_ec100B$ec100B_gsi_race_sa_as_urm[gsi.demo_withsubs_ec100B$ec100B_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec100B$ec100B_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino", "South Asian")] <-  "URM"

gsi.demo_withsubs_ec101A$ec101A_gsi_race2[gsi.demo_withsubs_ec101A$ec101A_gsi_race ==  "2 or More Races - Non-URM"] <-  "other"
gsi.demo_withsubs_ec101A$ec101A_gsi_race2[gsi.demo_withsubs_ec101A$ec101A_gsi_race == "White/Other" | gsi.demo_withsubs_ec101A$ec101A_gsi_race_sub == "White"] <-  "white"
gsi.demo_withsubs_ec101A$ec101A_gsi_race2[gsi.demo_withsubs_ec101A$ec101A_gsi_race == "Asian/Pacific Islander"  | gsi.demo_withsubs_ec101A$ec101A_gsi_race_sub %in% c("South Asian", "East Asian")] <-  "asian"
gsi.demo_withsubs_ec101A$ec101A_gsi_race2[gsi.demo_withsubs_ec101A$ec101A_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec101A$ec101A_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino")] <-  "URM"

gsi.demo_withsubs_ec101A$ec101A_gsi_race_sa_as_urm[gsi.demo_withsubs_ec101A$ec101A_gsi_race ==  "2 or More Races - Non-URM"] <-  "other"
gsi.demo_withsubs_ec101A$ec101A_gsi_race_sa_as_urm[gsi.demo_withsubs_ec101A$ec101A_gsi_race == "White/Other" | gsi.demo_withsubs_ec101A$ec101A_gsi_race_sub == "White"] <- "white"
gsi.demo_withsubs_ec101A$ec101A_gsi_race_sa_as_urm[gsi.demo_withsubs_ec101A$ec101A_gsi_race == "Asian/Pacific Islander" | gsi.demo_withsubs_ec101A$ec101A_gsi_race_sub == "East Asian"] <- "asian"
gsi.demo_withsubs_ec101A$ec101A_gsi_race_sa_as_urm[gsi.demo_withsubs_ec101A$ec101A_gsi_race == "Underrepresented Minority"  | gsi.demo_withsubs_ec101A$ec101A_gsi_race_sub %in% c("PI", "black", "Black", "hispanic", "latino", "South Asian")] <-  "URM"


# calculates the ratio of GSIs in a lecture/class by a GSI type (female, asian, etc!)
gsi.demo_withsubs_ec1 <- gsi.demo_withsubs_ec1 %>%
  group_by(.dots=c('ec1semester')) %>%
  mutate(ec1_ratio_female = ratio(ec1_gsi_gender, term = "Female"), ec1_ratio_urm2 =ratio(ec1_gsi_race2, term = "URM"), ec1_ratio_asian2 =ratio(ec1_gsi_race2, term = "asian"), ec1_ratio_white2 =ratio(ec1_gsi_race2, term = "white"), 
         ec1_ratio_urm3 = ratio( ec1_gsi_race_sa_as_urm, term = "URM"), ec1_ratio_asian3 = ratio( ec1_gsi_race_sa_as_urm, term = "asian"),  ec1_ratio_white3 = ratio( ec1_gsi_race_sa_as_urm, term = "white"))

gsi.demo_withsubs_ec2 <- gsi.demo_withsubs_ec2 %>%
  group_by(.dots=c('ec2semester')) %>%
  mutate(ec2_ratio_female = ratio(ec2_gsi_gender, term = "Female"), ec2_ratio_urm2 =ratio(ec2_gsi_race2, term = "URM"), ec2_ratio_asian2 =ratio(ec2_gsi_race2, term = "asian"), ec2_ratio_white2 =ratio(ec2_gsi_race2, term = "white"), 
         ec2_ratio_urm3 = ratio( ec2_gsi_race_sa_as_urm, term = "URM"), ec2_ratio_asian3 = ratio( ec2_gsi_race_sa_as_urm, term = "asian"),  ec2_ratio_white3 = ratio( ec2_gsi_race_sa_as_urm, term = "white"))

gsi.demo_withsubs_ec100A <- gsi.demo_withsubs_ec100A %>%
  group_by(.dots=c('ec100Asemester')) %>%
  mutate(ec100A_ratio_female = ratio(ec100A_gsi_gender, term = "Female"), ec100A_ratio_urm2 =ratio(ec100A_gsi_race2, term = "URM"), ec100A_ratio_asian2 =ratio(ec100A_gsi_race2, term = "asian"), ec100A_ratio_white2 =ratio(ec100A_gsi_race2, term = "white"), 
         ec100A_ratio_urm3 = ratio( ec100A_gsi_race_sa_as_urm, term = "URM"), ec100A_ratio_asian3 = ratio( ec100A_gsi_race_sa_as_urm, term = "asian"),  ec100A_ratio_white3 = ratio( ec100A_gsi_race_sa_as_urm, term = "white"))

gsi.demo_withsubs_ec100B <- gsi.demo_withsubs_ec100B %>%
  group_by(.dots=c('ec100Bsemester')) %>%
  mutate(ec100B_ratio_female = ratio(ec100B_gsi_gender, term = "Female"), ec100B_ratio_urm2 =ratio(ec100B_gsi_race2, term = "URM"), ec100B_ratio_asian2 =ratio(ec100B_gsi_race2, term = "asian"), ec100B_ratio_white2 =ratio(ec100B_gsi_race2, term = "white"), 
         ec100B_ratio_urm3 = ratio( ec100B_gsi_race_sa_as_urm, term = "URM"), ec100B_ratio_asian3 = ratio( ec100B_gsi_race_sa_as_urm, term = "asian"),  ec100B_ratio_white3 = ratio( ec100B_gsi_race_sa_as_urm, term = "white"))


gsi.demo_withsubs_ec101A <- gsi.demo_withsubs_ec101A %>%
  group_by(.dots=c('ec101Asemester')) %>%
  mutate(ec101A_ratio_female = ratio(ec101A_gsi_gender, term = "Female"), ec101A_ratio_urm2 =ratio(ec101A_gsi_race2, term = "URM"), ec101A_ratio_asian2 =ratio(ec101A_gsi_race2, term = "asian"), ec101A_ratio_white2 =ratio(ec101A_gsi_race2, term = "white"), 
         ec101A_ratio_urm3 = ratio( ec101A_gsi_race_sa_as_urm, term = "URM"), ec101A_ratio_asian3 = ratio( ec101A_gsi_race_sa_as_urm, term = "asian"),  ec101A_ratio_white3 = ratio( ec101A_gsi_race_sa_as_urm, term = "white"))

gsi.demo_withsubs_ec101B <- gsi.demo_withsubs_ec101B %>%
  group_by(.dots=c('ec101Bsemester')) %>%
  mutate(ec101B_ratio_female = ratio(ec101B_gsi_gender, term = "Female"), ec101B_ratio_urm2 =ratio(ec101B_gsi_race2, term = "URM"), ec101B_ratio_asian2 =ratio(ec101B_gsi_race2, term = "asian"), ec101B_ratio_white2 =ratio(ec101B_gsi_race2, term = "white"), 
         ec101B_ratio_urm3 = ratio( ec101B_gsi_race_sa_as_urm, term = "URM"), ec101B_ratio_asian3 = ratio( ec101B_gsi_race_sa_as_urm, term = "asian"),  ec101B_ratio_white3 = ratio( ec101B_gsi_race_sa_as_urm, term = "white"))



# merge demographics with students file
students.new5 <- merge( students.new4, gsi.demo_withsubs_ec1, by = c("ec1semester","ec1section"), all.x = T, all.y = F )
students.new5 <- merge( students.new5, gsi.demo_withsubs_ec2, by = c("ec2semester", "ec2section"), all.x = T, all.y = F)
students.new5 <- merge( students.new5, gsi.demo_withsubs_ec100A, by = c("ec100Asemester", "ec100Asection"), all.x = T, all.y = F )
students.new5 <- merge( students.new5, gsi.demo_withsubs_ec100B, by = c("ec100Bsemester", "ec100Bsection"), all.x = T, all.y = F )
students.new5 <- merge( students.new5, gsi.demo_withsubs_ec101A, by = c("ec101Asemester", "ec101Asection"), all.x = T, all.y = F )
students.new5 <- merge( students.new5, gsi.demo_withsubs_ec101B, by = c("ec101Bsemester", "ec101Bsection"), all.x = T, all.y = F )

#add section times and days, based on the
#source("D:\#Home\Divya\Documents\UWE Research\UCB Schedules\Econ1_time.R") #use actual .R file.
students.new6 <- merge( students.new5, econ1_times, by = c("ec1semester", "ec1section"), all.x = T, all.y = F, sort = F )

# must be two so check duplicates
students.new6 <- merge( students.new6, econ2_times, by = c("ec2semester", "ec2section"), all.x = T, all.y = F, sort = F )
students.new6 <- merge( students.new6, econ100A_times, by = c("ec100Asemester", "ec100Asection"), all.x = T, all.y = F, sort = F )
students.new6 <- merge( students.new6, econ100B_times, by = c("ec100Bsemester", "ec100Bsection"), all.x = T, all.y = F, sort = F )
students.new6 <- merge( students.new6, econ101B_times, by = c("ec101Bsemester", "ec101Bsection"), all.x = T, all.y = F, sort = F )
students.new6 <- merge( students.new6, econ101A_times, by = c("ec101Asemester", "ec101Asection"), all.x = T, all.y = F, sort = F )


students.new7 <- students.new6


#Professors!
#I import a professor file, remove duplicate semesters
# merge them for the preliminary classes
profs <- read.csv( "prof_schedule.csv",header = T, stringsAsFactors = F, fileEncoding = "UTF-8-BOM")

profs = profs[ !duplicated( profs[,c(1:2)], incomparables = F) , ]


profs_ec1 = profs[ profs$course == "1", c(1,3) ]
profs_ec1[ duplicated( profs_ec1[,c(1:2)], incomparables = F) |duplicated( profs_ec1[,c(1:2)], incomparables = F, fromLast = T) , ]
colnames( profs_ec1 )= c( 'ec1semester', 'ec1prof')
profs_ec2 = profs[ profs$course == "2", c(1,3) ]
colnames( profs_ec2 )= c( 'ec2semester', 'ec2prof')
profs_ec100A = profs[ profs$course == "100A", c(1,3) ]
colnames( profs_ec100A )= c( 'ec100Asemester', 'ec100Aprof')
profs_ec100B = profs[ profs$course == "100B", c(1,3) ]
colnames( profs_ec100B )= c( 'ec100Bsemester', 'ec100Bprof')
profs_ec101A = profs[ profs$course == "101A", c(1,3) ]
colnames( profs_ec101A )= c( 'ec101Asemester', 'ec101Aprof')
profs_ec101B = profs[ profs$course == "101B", c(1,3) ]
colnames( profs_ec101B )= c( 'ec101Bsemester', 'ec101Bprof')


students.new7 <- merge( students.new7, profs_ec1, by = c( "ec1semester"), all.x = T, all.y = F, sort = F)
students.new7 <- merge( students.new7, profs_ec2, by = c( "ec2semester"), all.x = T, all.y = F, sort = F)
students.new7 <- merge( students.new7, profs_ec100A, by = c( "ec100Asemester"), all.x = T, all.y = F, sort = F)
students.new7 <- merge( students.new7, profs_ec100B, by = c( "ec100Bsemester"), all.x = T, all.y = F, sort = F)
students.new7 <- merge( students.new7, profs_ec101A, by = c( "ec101Asemester"), all.x = T, all.y = F, sort = F)
students.new7 <- merge( students.new7, profs_ec101B, by = c( "ec101Bsemester"), all.x = T, all.y = F, sort = F)

################################################################################################################################t
# not very useful
#now we want to add an indicator for a male or female professor for econ 1
#prof_female <- read.csv( "~/UWE Research/UCB Schedules/Econ1_prof_f.csv", header = T, stringsAsFactors = F)
#colnames(prof_female) <- c("temp_ec1semester","ec1_prof_gender")


#pool econ 1 and econ 2
for (i in 1:length(students.new7$ppsk) )
{
  if( students.new7$ec1semester[i] != "" && students.new7$ec1grade[i] != "") {
    students.new7$ec12choice[i] = 1
    students.new7$ec12sectionday[i] = students.new7$ec1sectionday[i]
    students.new7$ec12sectiontime[i] = students.new7$ec1sectiontime[i]
    students.new7$ec12_gsi_gender[i] = students.new7$ec1_gsi_gender[i]
    students.new7$ec12_gsi_race[i] = students.new7$ec1_gsi_race[i]
    students.new7$ec12_gsi_race_sub[i] = students.new7$ec1_gsi_race_sub[i]
    students.new7$ec12section[i] = students.new7$ec1section[i]
    students.new7$ec12semester[i] = students.new7$ec1semester[i]
    students.new7$ec12_gsi_school[i] = students.new7$ec1school[i]
    
  } else if( students.new7$ec2semester[i] != "" && students.new7$ec2grade[i] != "" ) {
    students.new7$ec12choice[i] = 2
    students.new7$ec12sectionday[i] = students.new7$ec2sectionday[i]
    students.new7$ec12sectiontime[i] = students.new7$ec2sectiontime[i]
    students.new7$ec12_gsi_gender[i] = students.new7$ec2_gsi_gender[i]
    students.new7$ec12_gsi_race[i] = students.new7$ec2_gsi_race[i]
    students.new7$ec12_gsi_race_sub[i] = students.new7$ec2_gsi_race_sub[i]
    students.new7$ec12section[i] = students.new7$ec2section[i]
    students.new7$ec12semester[i] = students.new7$ec2semester[i]
    students.new7$ec12_gsi_school[i] = students.new7$ec2school[i]
  } else {
    students.new7$ec12choice[i] = ""
    students.new7$ec12sectionday[i] = ""
    students.new7$ec12sectiontime[i] = ""
    students.new7$ec12_gsi_gender[i] = ""
    students.new7$ec12_gsi_race[i] = ""
    students.new7$ec12_gsi_race_sub[i] = ""
    students.new7$ec12section[i] = ""
    students.new7$ec12semester[i] = ""
    
  }
}

students.new10 <- students.new7


# treating econ 1 and 2 as the same class. It's an introductory econ course and you either take one or the other
for (i in 1:length(students.new10$ppsk) )
{
  if( students.new10$ec1semester[i] != "" && students.new10$ec1grade[i] != "") {
    students.new10$ec12_gsi_school[i] = students.new10$ec1_gsi_school[i] 
    students.new10$ec12_gsi_name[i] = students.new10$ec1_gsi_name[i] 
    students.new10$ec12prof[i] = students.new10$ec1prof[i] 
    students.new10$ec12grade[i] = students.new10$ec1grade[i] 
    
    
    
  } else if( students.new10$ec2semester[i] != "" && students.new10$ec2grade[i] != "" ) {
    students.new10$ec12_gsi_school[i] = students.new10$ec2_gsi_school[i] 
    students.new10$ec12_gsi_name[i] = students.new10$ec2_gsi_name[i]
    students.new10$ec12prof[i] = students.new10$ec2prof[i] 
    students.new10$ec12grade[i] = students.new10$ec2grade[i] 
    
    
  } else {
    students.new10$ec12_gsi_school[i] = ""
    students.new10$ec12_gsi_name[i] = ""
    students.new10$ec12prof[i] = ""
    students.new10$ec12grade[i] = ""
    
    
  }
}

students.new12 <- students.new10

# this function converts a semester into a numeric format for comparison
# spring 2007 is the first semester 
# 2017 summer is the most recent semester
# there has to be an easier way to do this 
semnum <- function( y )
{
  x <- y
  x[ x == "2007 Spring"] <- 1
  x[ x == "2007 Summer"]  <- 2
  x[ x == "2007 Fall"]  <- 3
  x[ x == "2008 Spring"]  <- 4
  x[ x == "2008 Summer"]  <- 5
  x[ x == "2008 Fall"]  <- 6
  x[ x == "2009 Spring"]  <- 7
  x[ x == "2009 Summer"]  <- 8
  x[ x == "2009 Fall"]  <- 9
  x[ x == "2010 Spring"]  <- 10
  x[ x == "2010 Summer"]  <- 11
  x[ x == "2010 Fall"]  <- 12
  x[ x == "2011 Spring"]  <- 13
  x[ x == "2011 Summer"]  <- 14
  x[ x == "2011 Fall"]  <- 15
  x[ x == "2012 Spring"]  <- 16
  x[ x == "2012 Summer"]  <- 17
  x[ x == "2012 Fall"]  <- 18
  x[ x == "2013 Spring"]  <- 19
  x[ x == "2013 Summer"]  <- 20
  x[ x == "2013 Fall"]  <- 21
  x[ x == "2014 Spring"]  <- 22
  x[ x == "2014 Summer"]  <- 23
  x[ x == "2014 Fall"]  <- 24
  x[ x == "2015 Spring"]  <- 25
  x[ x == "2015 Summer"]  <- 26
  x[ x == "2015 Fall"]  <- 27
  x[ x == "2015 Spring"]  <- 28
  x[ x == "2015 Summer"]  <- 29
  x[ x == "2015 Fall"]  <- 30
  x[ x == "2016 Spring"]  <- 31
  x[ x == "2016 Summer"]  <- 32
  x[ x == "2016 Fall"]  <- 33
  x[ x == "2017 Spring"]  <- 34
  x[ x == "2017 Summer"]  <- 35
  
  return(x)
}

# convert each semseter to a semester number
students.new12$ec101Bsemnum <- semnum(students.new12$ec101Bsemester )
students.new12$ec101Asemnum <- semnum(students.new12$ec101Asemester )
students.new12$ec100Asemnum  <-semnum(students.new12$ec100Asemester )
students.new12$ec100Bsemnum  <- semnum(students.new12$ec100Bsemester )
students.new12$ec1semnum  <- semnum(students.new12$ec1semester )
students.new12$ec2semnum  <- semnum(students.new12$ec2semester )
students.new12$ec12semesternum  <- semnum(students.new12$ec12semester )
students.new12$entrysemnum  <- semnum(paste( students.new12$year, students.new12$term ) )
students.new12$endsemnum <- semnum(paste( students.new12$year.degree, students.new12$term.degree))

students.new12$entrysemnum[ students.new12$entrysemester == " "] <- ""
students.new12$endsemnum[ students.new12$endsemester == " "] <- ""

students.new13 <- students.new12

#write.csv( students.new12, "~/UWE Research/output/students.new2_b.csv", row.names = F)

rownames(students.new13) <- 1:nrow(students.new13)

#write.csv( students.new13, "~/UWE Research/output/transferstudents.new_gradesnum.csv", row.names = F)
#write.csv( students.new13, "~/UWE Research/output/transferstudents.new_gradesnum_std.csv", row.names = F)
#write.csv( students.new13, "~/UWE Research/output/transferstudents.new_gradesletter.csv", row.names = F)


# # converting to numeric for later -> this will ruin the dataset if grades are  letters
students.new13$ec100Agrade <- as.numeric(students.new13$ec100Agrade)
students.new13$ec101Agrade <- as.numeric(students.new13$ec101Agrade)
students.new13$ec101Bgrade <- as.numeric(students.new13$ec101Bgrade)
students.new13$ec100Bgrade <- as.numeric(students.new13$ec100Bgrade)


# gets the first class a student took that went into the econ major prereq GPA
# if checks which was the earlier class
# if both prereqs were taken in the same semeser, we see which had a higher grade
# if student took both 100 and 101 of same prereq, ignore
students.new13$firsttheory_class <-
  ifelse( !is.na(students.new13$ec100Agrade) & is.na(students.new13$ec101Agrade) ,
          # there's a ec100A grade
          ifelse( !is.na(students.new13$ec100Bgrade) & is.na(students.new13$ec101Bgrade), #check 100B grade

                  ifelse( students.new13$ec100Asemnum < students.new13$ec100Bsemnum, #check if 100A or 100B came first
                          "ec100Agrade",
                          ifelse( students.new13$ec100Asemnum == students.new13$ec100Bsemnum,
                                  ifelse( students.new13$ec100Agrade < students.new13$ec100Bgrade,
                                          "ec100Bgrade",
                                          "ec100Agrade"), # always use econ100A or 101A as the tiebreaker
                                  "ec100Bgrade" )),
                  ifelse( !is.na(students.new13$ec101Bgrade) & is.na(students.new13$ec100Bgrade), #only econ 101b and econ 100A
                          ifelse( students.new13$ec100Asemnum < students.new13$ec101Bsemnum, #ec101b EXISTS
                                  "ec100Agrade",
                                  ifelse( students.new13$ec100Asemnum == students.new13$ec101Bsemnum,
                                          ifelse( students.new13$ec100Agrade < students.new13$ec101Bgrade
                                                  , "ec101Bgrade", "ec100Agrade"), # always use econ100A or 101A as the tiebreaker
                                          "ec101Bgrade" ) # econ101B if
                                  ),
                          ifelse( !is.na(students.new13$ec101Bgrade) & !is.na(students.new13$ec100Bgrade),
                                  "",
                                  "ec100Agrade")) # then we only have econ 100A

                  ),
          # there isn't a 100A grade! check 101A
          ifelse(!is.na(students.new13$ec101Agrade) & is.na(students.new13$ec100Agrade),
                 ifelse( !is.na(students.new13$ec100Bgrade) & is.na(students.new13$ec101Bgrade), #101A grade? check 100B
                         # 100B grade? check if 101A or 100B came first
                         ifelse( students.new13$ec101Asemnum < students.new13$ec100Bsemnum, #check if 100A or 100B came first
                                 "ec101Agrade",
                                 ifelse( students.new13$ec101Asemnum == students.new13$ec100Bsemnum,
                                         ifelse( students.new13$ec101Agrade < students.new13$ec100Bgrade,
                                                 "ec100Bgrade",
                                                 "ec101Agrade"),
                                         "ec100Bgrade")),
                         # no 100B grade? check 101B
                         ifelse( !is.na( students.new13$ec101Bgrade) & is.na(students.new13$ec100Bgrade),
                                 # there's a 101B grade! now either 100A or 101B came first
                                 ifelse( students.new13$ec101Asemnum < students.new13$ec101Bsemnum,
                                         "ec101Agrade",
                                         # now either they were in the same sem or one is less
                                         ifelse(
                                           students.new13$ec101Asemnum == students.new13$ec101Bsemnum,
                                           #Just put 101A/100A by default
                                           ifelse( students.new13$ec101Agrade < students.new13$ec101Bgrade,
                                                   "ec101Bgrade",
                                                   "ec101Agrade"),"ec101Bgrade")
                                         ),
                                 ifelse( !is.na(students.new13$ec101Bgrade) & !is.na(students.new13$ec100Bgrade),
                                         "",
                                         "ec101Agrade")
                                 )
                 ),
                 ifelse( !is.na(students.new13$ec101Agrade) & !is.na(students.new13$ec100Agrade),
                         NA,  # don't bother if both econ 101a and econ 100a
                         ifelse( !is.na(students.new13$ec101Bgrade) & is.na(students.new13$ec100Bgrade),
                                 "ec101Bgrade", # don'tb other if both econ 101a and 100A
                                 ifelse( is.na(students.new13$ec101Bgrade) & !is.na(students.new13$ec100Bgrade),
                                         "ec100Bgrade", NA )
                               )
                      )
                 )
          )


# this gets the firsttheory grade
# I could have gotten the grade based on the previous code but alas
students.new13$firsttheory_grade <-
  ifelse( !is.na(students.new13$ec100Agrade) & is.na(students.new13$ec101Agrade) ,
          # there's a ec100A grade
          ifelse( !is.na(students.new13$ec100Bgrade) & is.na(students.new13$ec101Bgrade), #check 100B grade

                  ifelse( students.new13$ec100Asemnum < students.new13$ec100Bsemnum, #check if 100A or 100B came first
                          students.new13$ec100Agrade,
                          ifelse( students.new13$ec100Asemnum == students.new13$ec100Bsemnum,
                                  ifelse( students.new13$ec100Agrade < students.new13$ec100Bgrade,
                                          students.new13$ec100Bgrade,
                                          students.new13$ec100Agrade), # always use econ100A or 101A as the tiebreaker
                                  students.new13$ec100Bgrade )),
                  ifelse( !is.na(students.new13$ec101Bgrade) & is.na(students.new13$ec100Bgrade), #only econ 101b and econ 100A
                          ifelse( students.new13$ec100Asemnum < students.new13$ec101Bsemnum, #ec101b EXISTS
                                  students.new13$ec100Agrade,
                                  ifelse( students.new13$ec100Asemnum == students.new13$ec101Bsemnum,
                                          ifelse( students.new13$ec100Agrade < students.new13$ec101Bgrade
                                                  , students.new13$ec101Bgrade, students.new13$ec100Agrade), # always use econ100A or 101A as the tiebreaker
                                          students.new13$ec101Bgrade ) # econ101B if
                          ),
                          ifelse( !is.na(students.new13$ec101Bgrade) & !is.na(students.new13$ec100Bgrade),
                                  NA,
                                  students.new13$ec100Agrade)) # then we only have econ 100A

          ),
          # there isn't a 100A grade! check 101A
          ifelse(!is.na(students.new13$ec101Agrade) & is.na(students.new13$ec100Agrade),
                 ifelse( !is.na(students.new13$ec100Bgrade) & is.na(students.new13$ec101Bgrade), #101A grade? check 100B
                         # 100B grade? check if 101A or 100B came first
                         ifelse( students.new13$ec101Asemnum < students.new13$ec100Bsemnum, #check if 100A or 100B came first
                                 students.new13$ec101Agrade,
                                 ifelse( students.new13$ec101Asemnum == students.new13$ec100Bsemnum,
                                         ifelse( students.new13$ec101Agrade < students.new13$ec100Bgrade,
                                                 students.new13$ec100Bgrade,
                                                 students.new13$ec101Agrade),
                                         students.new13$ec100Bgrade)),
                         # no 100B grade? check 101B
                         ifelse( !is.na( students.new13$ec101Bgrade) & is.na(students.new13$ec100Bgrade),
                                 # there's a 101B grade! now either 100A or 101B came first
                                 ifelse( students.new13$ec101Asemnum < students.new13$ec101Bsemnum,
                                         students.new13$ec101Agrade,
                                         # now either they were in the same sem or one is less
                                         ifelse(
                                           students.new13$ec101Asemnum == students.new13$ec101Bsemnum,
                                           #Just put 101A/100A by default
                                           ifelse( students.new13$ec101Agrade < students.new13$ec101Bgrade,
                                                   students.new13$ec101Bgrade,
                                                   students.new13$ec101Agrade),students.new13$ec101Bgrade)
                                 ),
                                 ifelse( !is.na(students.new13$ec101Bgrade) & !is.na(students.new13$ec100Bgrade),
                                         NA,
                                         students.new13$ec101Agrade)
                         )
                 ),
                 ifelse( !is.na(students.new13$ec101Agrade) & !is.na(students.new13$ec100Agrade),
                         NA,  # don't bother if both econ 101a and econ 100a
                         ifelse( !is.na(students.new13$ec101Bgrade) & is.na(students.new13$ec100Bgrade),
                                 students.new13$ec101Bgrade, # don't other if both econ 101a and 100A
                                 ifelse( is.na(students.new13$ec101Bgrade) & !is.na(students.new13$ec100Bgrade),
                                         students.new13$ec100Bgrade, NA )
                         )
                 )
          )
  )



# figures out the whether math 1A or math 16A was used
# as the first calc prereq, if at all (doesn't apply if student took AP calc or CC course)
students.new13$math1Agrade <- as.numeric(students.new13$math1Agrade)
students.new13$math16Agrade <- as.numeric(students.new13$math16Agrade)
students.new13$math1Bgrade <- as.numeric(students.new13$math1Bgrade)
students.new13$math16Bgrade <- as.numeric(students.new13$math16Bgrade)

students.new13$mathA <- ifelse(!is.na(students.new13$math1Agrade),
                               ifelse( !is.na(students.new13$math16Agrade),
                                       ifelse( students.new13$math1Agrade > students.new13$math16Agrade,
                                               "math1A", "math16A"),

                                       "math16A"),
                               ifelse( !is.na(students.new13$math16Agrade),
                                       "math16A", "")
                                   )


students.new13$mathAgrade <- ifelse(!is.na(students.new13$math1Agrade),
                               ifelse( !is.na(students.new13$math16Agrade),
                                       ifelse( students.new13$math1Agrade > students.new13$math16Agrade,
                                               students.new13$math1Agrade, students.new13$math16Agrade),

                                       students.new13$math1Agrade),
                               ifelse( !is.na(students.new13$math16Agrade),
                                       students.new13$math16Agrade, NA)
                               )

students.new13$mathB <- ifelse(!is.na(students.new13$math1Agrade),
                               ifelse( !is.na(students.new13$math16Agrade),
                                       ifelse( students.new13$math1Agrade > students.new13$math16Agrade,
                                               "math1B", "math16B"),

                                       "math16B"),
                               ifelse( !is.na(students.new13$math16Agrade),
                                       "math16B", "")
)


students.new13$mathBgrade <- ifelse(!is.na(students.new13$math1Bgrade),
                               ifelse( !is.na(students.new13$math16Bgrade),
                                       ifelse( students.new13$math1Bgrade > students.new13$math16Bgrade,
                                               students.new13$math1Bgrade, students.new13$math16Bgrade),

                                       students.new13$math1Bgrade),
                               ifelse( !is.na(students.new13$math16Bgrade),
                                       students.new13$math16Bgrade, NA)
                               )

# figures out which stat class student took for major prereq
students.new13$stat20grade <- as.numeric(students.new13$stat20grade)
students.new13$stat21grade <- as.numeric(students.new13$stat21grade)
students.new13$stat134grade <- as.numeric(students.new13$stat134grade)
students.new13$stat88grade <- as.numeric(students.new13$stat88grade)
students.new13$statW21grade <- as.numeric(students.new13$statW21grade)
students.new13$stat131Agrade <- as.numeric(students.new13$stat131Agrade)
students.new13$ieor172grade <- as.numeric(students.new13$ieor172grade)

# I didn't calculate the prereq gpa since I didn't have the grade in data 8
statclass_list <- c("stat20", "stat21", "stat131A", "statW21","stat134", "stat88", "ieor172")
students.new12$stat20semnum <- semnum(students.new12$stat20semester )
students.new12$stat21semnum <- semnum(students.new12$stat21semester )
students.new12$statW21semnum  <-semnum(students.new12$statW21semester )
students.new12$stat134semnum  <- semnum(students.new12$stat134semester )
students.new12$stat88semnum  <- semnum(students.new12$stat88semester )
students.new12$ieor172semnum  <- semnum(students.new12$ieor172semester )

# for loop makes sure that student didn't take more than 1 stat class
# if they took more than one stat class, we take highest stat class grade
# there's an indicator if student took 2 stat classes
# note that i didn't do this for calc courses though this could still be an issue.
students.new13$tookstatsmorethan1time <- 0
for ( i in 1:nrow(students.new13 ) )
{
  statclass_taken <- as.numeric( c(  !is.na(students.new13$stat20grade[i]),  !is.na(students.new13$stat21grade[i]) ,
                  !is.na(students.new13$stat131Agrade[i]),
                  !is.na(students.new13$statW21grade[i]),
                  !is.na(students.new13$stat134grade[i]), !is.na(students.new13$stat88grade[i]),
                  !is.na(students.new13$ieor172grade[i]) ) )

  if( sum( statclass_taken  ) == 0 )
  {
    students.new13$statclassnum[i] <- ""
  }
  else if ( sum( statclass_taken  )  ==  1)
  {
    students.new13$statclassnum[i] <- statclass_list[which.max(statclass_taken)]
  }
  else
  {

    highest_stat <- c(students.new12$stat20semnum[i], students.new12$stat21semnum[i],
                      students.new12$statW21semnum[i], students.new12$stat134semnum[i],
                      students.new12$stat88semnum[i],students.new12$ieor172semnum[i] )
    students.new13$statclassnum[i] <- statclass_list[which.min(highest_stat )]
    students.new13$tookstatsmorethan1time[i] <- 1
  }


}


students.new13[students.new13 == "\n"] <- ""
students.new13[is.na(students.new13)] <- ""


act_sat <- read.csv( "act_sat_conversion.csv", header = T)
colnames(act_sat) <- c("act.composite", "sat_fromact")
act_sat <- act_sat[ !is.na(act_sat$act.composite), ]
act_sat <- act_sat[ !duplicated(act_sat$act.composite), ]


students.new14 <- merge( students.new13, act_sat, all.x = T, all.y = F, by = "act.composite", sort = F  )


students.new14$sat_act_as_sat <- ifelse( students.new14$sat.total != "", 
                                         students.new14$sat.total, students.new14$sat_fromact )



###############
# This portion of the code now uses the above dataset in order to make a student x class level dataset
# if a student took two classes, they'll appear in the below dataset twice instead of once. 
# We want to see the results of TA demographics on student outcomes using different forms of the regression
#
##############
students.new11 <- students.new14


students.new11[students.new11 == "\n"] <- ""
students.new11[is.na(students.new11)] <- ""


# creates an indicator for whether or not student took a required econ theory course 
#or a required econometrics theory course
for ( i in 1:length(students.new11$ppsk) )
{
  if( students.new11$ec100Agrade[i] == "" & students.new11$ec100Bgrade[i] == "" & students.new11$ec101Agrade[i] == "" & students.new11$ec101Bgrade[i]== ""  )
  {
    students.new11$tookonetheory[i] = 0
  } 
  else {
    students.new11$tookonetheory[i]= 1
  }
  if ( students.new11$ec140grade[i] != "" | students.new11$ec141grade[i] != "" ) #check if student took ec140
  {
    students.new11$tookmetrics[i] = 1
  } else {
    students.new11$tookmetrics[i] = 0
  }
  
  # maybe later put tooktwotheory
}

# these are the columns we want to keep in previous dataframe
# colsstack = c( 'ppsk', 'year', 'GradYr', 'degree.major.1', 'degree.major.2', 'degree.major.3', 'gender', 'ethnic.l1','ethnic.l3','residency',
#     'term', 'first.generation.college.2','sat_act_as_sat','major.intended.college', 'major.intended.division', 'major.intended.department',           
#     'tookonetheory', 'tookmetrics', 'ec1semester', 'ec2semester', 'ec101Asemester', 'ec101Bsemester', 'ec100Asemester', 'ec100Bsemester', 
#    'ec1grade', 'ec2grade', 'ec101Agrade', 'ec101Bgrade', 'ec100Agrade', 'ec100Bgrade', 
#    'ec1sectionday', 'ec2sectionday', 'ec101Asectionday', 'ec101Bsectionday', 'ec100Asectionday', 'ec100Bsectionday', 
#    'ec1sectiontime', 'ec2sectiontime', 'ec101Asectiontime', 'ec101Bsectiontime', 'ec100Asectiontime', 'ec100Bsectiontime', 
#    'ec1_gsi_race', 'ec2_gsi_race', 'ec1_gsi_gender', 'ec2_gsi_gender', 
   # 'ec1_gsi_school', 'ec2_gsi_school', 'ec101A_gsi_school', 'ec101B_gsi_school', 'ec100A_gsi_school', 'ec100B_gsi_school',
   # 'ec1_gsi_name', 'ec2_gsi_name', 'ec101A_gsi_name', 'ec101B_gsi_name', 'ec100A_gsi_name', 'ec100B_gsi_name',
   # 'ec1prof', 'ec2prof', 'ec101Aprof', 'ec101Bprof', 'ec100Aprof', 'ec100Bprof',
   # 'ec1section','ec2section', 'ec100Asection', 'ec100Bsection', 'ec101Asection', 'ec101Bsection',
   # 'ec1_ratio_white', 'ec2_ratio_white', 'ec100A_ratio_white', 'ec101A_ratio_white',
   # 'ec101B_ratio_white', 'ec100B_ratio_white', 'ec1_ratio_urm', 'ec2_ratio_urm', 'ec100A_ratio_urm',
   # 'ec101A_ratio_urm', 'ec101B_ratio_urm', 'ec100B_ratio_urm', 'ec1_ratio_asian', 'ec2_ratio_asian',
   # 'ec100A_ratio_asian', 'ec101A_ratio_asian', 'ec101B_ratio_asian', 'ec100B_ratio_asian',
   # 'ec1_ratio_female', 'ec2_ratio_female', 'ec100A_ratio_female', 'ec100B_ratio_female', 'ec101A_ratio_female', 'ec101B_ratio_female')


colsstack <-  c( 'ppsk', 'year', 'GradYr', 'degree.major.1', 'degree.major.2', 'degree.major.3', 
              'gender', 'ethnic.l1','ethnic.l3','residency','term', 'first.generation.college.2',
          'sat_act_as_sat','major.intended.college', 'major.intended.division','major.intended.department',
           'tookonetheory', 'tookmetrics', 'entrysemnum', 'ec12semesternum', 'endsemnum',
          'ec1semnum', 'ec2semnum', 'ec100Asemnum', 'ec100Bsemnum','ec101Asemnum', 'ec101Bsemnum', 
          'ec1stdgrade', 'ec2stdgrade', 'ec101Astdgrade', 'ec101Bstdgrade', 'ec100Astdgrade', 'ec100Bstdgrade',
          'ec1pass', 'ec2pass', 'ec101Apass', 'ec101Bpass', 'ec100Apass', 'ec100Bpass',
          'ec1semester','ec2semester', 'ec101Asemester', 'ec101Bsemester', 'ec100Asemester','ec100Bsemester',
          'ec1grade', 'ec2grade', 'ec101Agrade', 'ec101Bgrade', 'ec100Agrade', 'ec100Bgrade',
          'ec1section','ec2section', 'ec100Asection', 'ec100Bsection', 'ec101Asection', 'ec101Bsection', 
          'ec1sectionday', 'ec2sectionday', 'ec101Asectionday', 'ec101Bsectionday', 'ec100Asectionday', 'ec100Bsectionday', 
          'ec1sectiontime', 'ec2sectiontime', 'ec101Asectiontime', 'ec101Bsectiontime', 'ec100Asectiontime', 'ec100Bsectiontime', 
          'ec1_gsi_race', 'ec2_gsi_race', 'ec100A_gsi_race', 'ec100B_gsi_race', 'ec101A_gsi_race', 'ec101B_gsi_race', 
          'ec1_gsi_school', 'ec2_gsi_school', 'ec101A_gsi_school', 'ec101B_gsi_school', 'ec100A_gsi_school', 'ec100B_gsi_school',
          'ec1_gsi_name', 'ec2_gsi_name', 'ec101A_gsi_name', 'ec101B_gsi_name', 'ec100A_gsi_name', 'ec100B_gsi_name',
          'ec1prof', 'ec2prof', 'ec101Aprof', 'ec101Bprof', 'ec100Aprof', 'ec100Bprof', 
          'ec1_gsi_name', 'ec2_gsi_name', 'ec101A_gsi_name', 'ec101B_gsi_name', 'ec100A_gsi_name', 'ec100B_gsi_name',
          'ec1_ratio_white2', 'ec2_ratio_white2', 'ec100A_ratio_white2', 'ec101A_ratio_white2',
          'ec101B_ratio_white2', 'ec100B_ratio_white2', 'ec1_ratio_urm2', 'ec2_ratio_urm2', 'ec100A_ratio_urm2',
          'ec101A_ratio_urm2', 'ec101B_ratio_urm2', 'ec100B_ratio_urm2', 'ec1_ratio_asian2', 'ec2_ratio_asian2',
          'ec100A_ratio_asian2', 'ec101A_ratio_asian2', 'ec101B_ratio_asian2', 'ec100B_ratio_asian2',
          'ec1_ratio_white3', 'ec2_ratio_white3', 'ec100A_ratio_white3', 'ec101A_ratio_white3',
          'ec101B_ratio_white3', 'ec100B_ratio_white3', 'ec1_ratio_urm3', 'ec2_ratio_urm3', 'ec100A_ratio_urm3',
          'ec101A_ratio_urm3', 'ec101B_ratio_urm3', 'ec100B_ratio_urm3', 'ec1_ratio_asian3', 'ec2_ratio_asian3',
          'ec100A_ratio_asian3', 'ec101A_ratio_asian3', 'ec101B_ratio_asian3', 'ec100B_ratio_asian3',
          'ec1_ratio_female', 'ec2_ratio_female', 'ec100A_ratio_female', 'ec100B_ratio_female', 'ec101A_ratio_female', 'ec101B_ratio_female',
          'ec1_gsi_race_sub', 'ec2_gsi_race_sub', 'ec100A_gsi_race_sub', 'ec100B_gsi_race_sub', 'ec101A_gsi_race_sub', 'ec101B_gsi_race_sub')



students_enrollmajor <- subset( students.new11, !(ec1grade == "" & ec2grade == " ") , select = colsstack  )

students_enrollmajor$temp_col <- NA


# Use dpylr in order to rearrange the dataset. It mutates a new row for every course 
# and the course's corresponding characteristics
students_enrollmajor2 <- 
  students_enrollmajor %>%
  gather( temp_col, value, -ppsk, -year, -GradYr, -degree.major.1, -degree.major.2, -degree.major.3, -gender, -ethnic.l1, -ethnic.l3, -residency, -term, -first.generation.college.2, -sat_act_as_sat, -major.intended.college, -tookonetheory, -tookmetrics, -major.intended.division, -major.intended.department,
          -entrysemnum, -endsemnum, -ec12semesternum) %>%
  mutate( course = gsub("(.*)(grade|semester|sectionday|sectiontime|_gsi_school|_gsi_name|_gsi_race|_gsi_gender|prof|section|semnum|stdgrade|pass|_ratio_urm2|_ratio_white2|_ratio_asian2|_ratio_urm3|_ratio_white3|_ratio_asian3|_ratio_female|gsi_race_sub)", "\\1", temp_col),
     column_name = gsub("(.*)(grade|semester|sectionday|sectiontime|_gsi_school|_gsi_name|_gsi_race|_gsi_gender|prof|section|semnum|stdgrade|pass|_ratio_urm2|_ratio_white2|_ratio_asian2|_ratio_urm3|_ratio_white3|_ratio_asian3|_ratio_female|gsi_race_sub)","\\2", temp_col)) %>%
  select( -temp_col ) %>%
  spread( column_name, value )


colnames(students_enrollmajor2)[grep("_", colnames(students_enrollmajor2))] = gsub("_", "", colnames(students_enrollmajor2)[grep("_", colnames(students_enrollmajor2))] )

students_enrollmajor2[ is.na(students_enrollmajor2) ] = ""


students_enrollmajor2 <- students_enrollmajor2[ !students_enrollmajor2$grade == "", ]

students_enrollmajor2[ duplicated(students_enrollmajor2[, c("ppsk", "course")] )   ,   ]

students_enrollmajor2$semnum  <- as.numeric( as.character(students_enrollmajor2$semnum))

#final dataset
write.csv( students_enrollmajor2, "students_stack5.csv", row.names = F)

students_enrollmajor3 <- students_enrollmajor2


