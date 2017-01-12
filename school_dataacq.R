# JMJPFU

# 1 Sept 2016

# Grade data

temp <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

colnames(temp) <- names(stud_grade)

stud_grade <- rbind(stud_grade,temp)

# Attendence data

#stud_attend <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

temp <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

stud_attend <- rbind(stud_attend,temp)

# Enrollment data

#stud_enrollment <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

temp <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

stud_enrollment <- rbind(stud_enrollment,temp)

stud_enrollment_16 <- rbind(stud_enrollment_16,temp)

# Calender Out Data

#Org_calender <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

temp <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

Org_calender <- rbind(Org_calender,temp)






### Transcript data

stud_tran <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

temp <- read.csv("C:/Toms/customers/Edex/DeSoto_10/excel/Book1.csv")

stud_tran <- rbind(stud_tran,temp)

# lets hold the transcript data for now and deal with some thing else