# Define the column widths based on the positions given in the description
column_widths <- c(5, 1, 4, 1, 3, 1, 3, 1, 3)

# Read the SCI.DAT file using read.fwf()
sci_data <- read.fwf("SCI.DAT", widths = column_widths, 
                     col.names = c("Centre", "NA1", "CandidateID", "NA2", "Gender", "NA3", "WrittenScore", "NA4", "CourseworkScore"))

# Drop the NA columns that were added as placeholders for spaces
sci_data <- sci_data[, c("Centre", "CandidateID", "Gender", "WrittenScore", "CourseworkScore")]

# View the first few rows of the data
head(sci_data)

# Write the data to a CSV file
write.csv(sci_data, file = "sci_data.csv", row.names = FALSE)

###################

# Read the EMPLOY.DAT file using read.table()
employ_data <- read.table("EMPLOY.DAT", 
                          col.names = c("AreaID", "Cell", "Gender", "Qualification", "EmployedInCell", "TotalInCell"),
                          fill = TRUE, strip.white = TRUE)

# Convert numeric columns to appropriate data types
employ_data$Gender <- as.numeric(employ_data$Gender)
employ_data$Qualification <- as.numeric(employ_data$Qualification)
employ_data$EmployedInCell <- as.numeric(employ_data$EmployedInCell)
employ_data$TotalInCell <- as.numeric(employ_data$TotalInCell)

# View the first few rows of the data
head(employ_data)

# Write the data to a CSV file
write.csv(employ_data, file = "employ_data.csv", row.names = FALSE)

#########################

# Define column widths based on the description provided for the ILEA567.DAT file
dat_widths <- c(1, 3, 2, 2, 2, 1, 1, 2, 1, 1)

# Read the ILEA567.DAT file using read.fwf()
ilea_data <- read.fwf("ILEA567.DAT", widths = dat_widths, 
                      col.names = c("Year", "School", "ExamScore", "PercentFSM", "PercentVR1", "Gender", "VRBand", "EthnicGroup", "SchoolGender", "SchoolDenomination"),
                      na.strings = c(" "), strip.white = TRUE)

# Convert relevant columns to factors
ilea_data$Year <- factor(ilea_data$Year, levels = 1:3, labels = c("1985", "1986", "1987"))
ilea_data$School <- ilea_data$School
ilea_data$Gender <- factor(ilea_data$Gender, levels = 0:1, labels = c("Male", "Female"))
ilea_data$VRBand <- factor(ilea_data$VRBand, levels = 1:3, labels = c("VR3", "VR1", "VR2"))
ilea_data$EthnicGroup <- factor(ilea_data$EthnicGroup, levels = 1:11, labels = c("ESWI", "African", "Arab", "Bangladeshi", "Caribbean", "Greek", "Indian", "Pakistani", "S.E.Asian", "Turkish", "Other"))
ilea_data$SchoolGender <- factor(ilea_data$SchoolGender, levels = 1:3, labels = c("Mixed", "Male", "Female"))
ilea_data$SchoolDenomination <- factor(ilea_data$SchoolDenomination, levels = 1:3, labels = c("Maintained", "Church of England", "Roman Catholic"))

# View the first few rows of the data
head(ilea_data)

# Write the data to a CSV file
write.csv(ilea_data[, c(2:4, 6:7, 10)], file = "ilea_data.csv", row.names = FALSE)

