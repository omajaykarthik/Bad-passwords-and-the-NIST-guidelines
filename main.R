rm(list = ls())
#------------------------------------------------
# Importing the tidyverse library
library(tidyverse)

# Loading in datasets/users.csv 
users <- read_csv("users.csv")

# Counting how many users we've got
length(users$user_name)


# Taking a look at the 12 first users
users$user_name[1:12]

#-----------------------------------------------
#2. Passwords should not be too short
#-----------------------------------------------

# Calculating the lengths of users' passwords
users$length <- nchar(users$password)
  

# Flagging the users with too short passwords
# or users$too_short <- str_length(users$password)<8
users$too_short <- nchar(users$password)<8



# Counting the number of users with too short passwords
sum(users$too_short==TRUE)

# Taking a look at the 12 first rows
users[1:12,]

#-----------------------------------------------
#3. Common passwords people use
#-----------------------------------------------
# Reading in the top 10000 passwords
common_passwords <- read_lines("\\datasets\\10_million_password_list_top_10000.txt")

# Taking a look at the top 100
common_passwords[1:100]

#-----------------------------------------------
#4. Passwords should not be common passwords
#-----------------------------------------------

# Flagging the users with passwords that are common passwords
users$common_password = users$password %in% common_passwords

# Counting the number of users using common passwords
sum(users$common_password==TRUE)

# Taking a look at the 12 first rows
users[1:12,]

#-----------------------------------------------
#5. Passwords should not be common words
#-----------------------------------------------

# Reading in a list of the 10000 most common words
words <- read_lines("\\datasets\\google-10000-english.txt")


# Flagging the users with passwords that are common words
users$common_word <- users$password %in% words

# Counting the number of users using common words as passwords
sum(users$common_password==TRUE)

# Taking a look at the 12 first rows
users[1:12,]



#-----------------------------------------------
#6. Passwords should not be your name
#-----------------------------------------------
library("stringr")

# Extracting first and last names into their own columns
users$first_name <- str_extract(users$user_name,"^\\w+")
users$last_name <- str_extract(users$user_name,'\\w+$')

# Flagging the users with passwords that matches their names
users$uses_name <- (users$password == users$first_name | users$password == users$last_name)

# Counting the number of users using names as passwords
sum(users$uses_name==TRUE)

# Taking a look at the 12 first rows
users[1:12,]



#-----------------------------------------------
#7. Passwords should not be repetitive
#-----------------------------------------------

# Splitting the passwords into vectors of single characters
split_passwords <- strsplit(users$password,"")



# Picking out the max number of repeat characters for each password
users$max_repeats <- sapply(split_passwords, function(split_password) {
  x = toString(split_password)
  rle(x)$length
})

# Flagging the passwords with >= 4 repeats
users$too_many_repeats <- lapply(users$max_repeats,max) >=4



# Taking a look at the users with too many repeats
#get index method one ->>> match(users$too_many_repeats,TRUE)
#method 2 >>>> too_many_repeats_index = which(users$too_many_repeats %in% TRUE)

temp = users[lapply(users$max_repeats,max) >=4,]

