## ----setup, include=FALSE-----------------------------------------------------
library(evolved)
library(LSD)
options(rmarkdown.html_vignette.check_title = FALSE)

## -----------------------------------------------------------------------------
print(x = "hello world")

## -----------------------------------------------------------------------------
print(x = "This will be printed in the console") #"This will not"

## -----------------------------------------------------------------------------
one_to_thirteen <- seq(from = 1, to = 13, by = 1)

## -----------------------------------------------------------------------------
one_to_thirteen

## -----------------------------------------------------------------------------
class(one_to_thirteen)

## -----------------------------------------------------------------------------
seq(from = -0.56, to = 1, by = 0.2)

## -----------------------------------------------------------------------------
LETTERS

class(LETTERS)

# Numbers can be converted to characters as well:
one_to_thirteen_character <- as.character(x = one_to_thirteen)

# Characters are always marked with quotation marks. 
one_to_thirteen_character

# Even if you can read 
# something as a number, R interprets as a character everything
# that is inside quotation marks.
class(one_to_thirteen_character)

## -----------------------------------------------------------------------------
mean(x = one_to_thirteen)

## -----------------------------------------------------------------------------
mean(x = one_to_thirteen)

## -----------------------------------------------------------------------------
mean(one_to_thirteen) 

## -----------------------------------------------------------------------------
2 + 3

2 * 3 + 1

2 * (3 + 1)

## -----------------------------------------------------------------------------
9 ^ 2 #this is exponentiation

log(8, base = 2) #taking the logarithm of a number given a specific base

## -----------------------------------------------------------------------------
one_to_ten <- 1:10
one_to_ten^2

## -----------------------------------------------------------------------------
# Code below will take the log2 of numbers from 2 to
# 100 by an even space of 2 units
log(seq(2, 100, by = 2), base = 2) 

## -----------------------------------------------------------------------------
zero_to_ten <- seq(from = 0, to = 10, by = 2) #here we use the seq() function, 
# attributed to the "zero_to_ten" object

# Note we also use the "from", "to", and "by" arguments.

## -----------------------------------------------------------------------------
zero_to_ten

## -----------------------------------------------------------------------------
one_to_ten <- seq(from = 1, to = 10, by = 0.5) 

one_to_ten
# Note that the elements themselves 
# (and the number of elements) changed

## -----------------------------------------------------------------------------
one_to_ten + 0.56 #addition (or) subtraction

one_to_ten*2 #multiplication

one_to_ten/2 #division

## -----------------------------------------------------------------------------
paste("text pasted to", one_to_ten)

## -----------------------------------------------------------------------------
class(paste("text pasted to", one_to_ten))

## -----------------------------------------------------------------------------
c(one_to_ten, NA, 34)

## -----------------------------------------------------------------------------
new_one_to_ten <- c(one_to_ten, NA, "word")

class(new_one_to_ten)

## -----------------------------------------------------------------------------
new_one_to_ten <- as.factor(new_one_to_ten)

class(new_one_to_ten)

new_one_to_ten <- as.character(new_one_to_ten)

class(new_one_to_ten)

## -----------------------------------------------------------------------------
as.numeric(new_one_to_ten)

## -----------------------------------------------------------------------------
#Pay attention to the bracketed numbers:

onehundred_to_twohundred <- 100:200

onehundred_to_twohundred <- onehundred_to_twohundred + 1.53

onehundred_to_twohundred 

## -----------------------------------------------------------------------------
onehundred_to_twohundred[56] #if you want the 56th element

onehundred_to_twohundred[24] #if you want the 24th element

onehundred_to_twohundred[13] #if you want the 13th element

## -----------------------------------------------------------------------------
onehundred_to_twohundred[22] <- 0

onehundred_to_twohundred[22] 

onehundred_to_twohundred

## -----------------------------------------------------------------------------
vector1 <- c(1,3)

class(vector1)

vector2 <- c(4,5)

class(vector2)

# You can bind vectors (or matrices) by rows...
tentative_matrix1 <- rbind(vector1, vector2) 

class(tentative_matrix1)

#... or by columns
tentative_matrix2 <- cbind(vector1, vector2) 

class(tentative_matrix2)

#and each action generates different outputs:

tentative_matrix1

tentative_matrix2

# Note also that doing...
tentative_matrix1 * tentative_matrix2

#... is different from doing:
tentative_matrix1 %*% tentative_matrix2

## -----------------------------------------------------------------------------
numbers <- seq(-10, 10, by = 1)
numbers

test_result <- numbers > 0 # here we are asking which numbers are larger then 0
test_result

numbers == 10 # here we are asking which number is exactly equal to 10

# Note that the "is equal" test is created by two consecutive equal signs 
# (i.e., "=="), since just one equal sign ("=") has the same meaning as the 
# attribution arrow (i.e., "<-").

## -----------------------------------------------------------------------------
df <- data.frame(test_result, numbers)

class(df)

## -----------------------------------------------------------------------------
colnames(df) <- c("is_positive", "number")

df

## -----------------------------------------------------------------------------
# First column stores a logical vector:
class(df[,1])

# Second column stores a numerical vector:
class(df[,2])

## -----------------------------------------------------------------------------
# First 7 rows printed:
head(df, n = 7)

# Last 5 rows printed:
tail(df, n = 5)

## -----------------------------------------------------------------------------
df[7,1] # selecting the object in the 7th row and 1st column

df[7:15, 1:2] # taking objects in the 7th to the 15th rows and in both columns

## -----------------------------------------------------------------------------
df[7:15,]

## ---- eval=FALSE--------------------------------------------------------------
#  # Running the code below, you will install ape from The Comprehensive R Archive
#  # Network (a.k.a. CRAN - more about it in the additional reading).
#  install.packages("ape")

## -----------------------------------------------------------------------------
library(ape) #note the lack of quotation marks in the use of this function

## ---- include=FALSE-----------------------------------------------------------
# deleting aux folders if they exist:
if("subfolder_relative" %in% list.files()){
  unlink("subfolder_relative", recursive = T)  
}

if("subfolder_absolute" %in% list.files()){
  unlink("subfolder_absolute", recursive = T)  
}

## -----------------------------------------------------------------------------
rnorm(n = 10, mean = 0, sd = 1)

## -----------------------------------------------------------------------------
runif(n = 1, min = 0, max = 1)

## -----------------------------------------------------------------------------
normalNumbers <- rnorm(n = 10000, mean = 0, sd = 1)
hist(normalNumbers)

## -----------------------------------------------------------------------------
anotherNormalNumbers <- rnorm(n = 10000, mean = 3, sd = 1)
plot(x = normalNumbers, y = anotherNormalNumbers)

## -----------------------------------------------------------------------------
LSD::heatscatter(x = normalNumbers, y = anotherNormalNumbers) 

## -----------------------------------------------------------------------------
dt <- cbind(normalNumbers, anotherNormalNumbers)
boxplot(dt)

## -----------------------------------------------------------------------------
#
xx <- seq(from = 0, to = 20, by = 0.01) 
#we will only draw some of the points in the real number line

plot(x = xx, y = xx ^ 2, type = "l", 
     main = "Simple function drawing", 
     ylab = "Squared value", xlab = "value") 
# The "type" argument specifies that I want to connect the points 
# with a line. The "ylab", "xlab", and "main" arguments provide the labels

## -----------------------------------------------------------------------------
# Conditionally coloring specific points:

high_10 <- normalNumbers > 1 
# The "condition" will be whether the point's value is larger than 1
# Here we will use a logical test (is the point's value > 1) 
# to evaluate each element in a vector of numbers ("normalNumbers")

cols <- rep("black", times = length(normalNumbers)) 
# We will create a vector of color names of the same length 
# as the length of the "normalNumbers" object

cols[high_10] <- "grey75" 
# We will use the logical vector we created ("high_10")
# to specify which elements in the "cols" vector should be changed to grey75
# (this changes only the elements listed as "TRUE" in the logical vector)

# Then, with all this information, we can draw a new plot, with the colors 
# specifying something we are interested in visualizing:
plot(normalNumbers, col = cols)
# Note that the points are added in the order of their position in the
# vector - which creates the "grainy" look around y=1

## -----------------------------------------------------------------------------
plot(normalNumbers, col = cols)

abline(h = 1, col = "red", lwd = 6) # Draws a horizontal line at y=0 
# (note the "line width" argument is named lwd)

# Drawing a vertical, thinner line (added after the red line, because of the 
# order of the commands) separating the points at each half of the 
# "normalNumbers" object
abline(v = length(normalNumbers)/2, col = "blue", lwd = 3) 

## -----------------------------------------------------------------------------
count <- 1:10 # what do this line do?

# Now, we will use loop syntax:
for(number in count){
  # In each "lap" (also known as "iteration") of the loop,
  # the value of the object "number" will be updated
  # to one of the values in the vector "count". These 
  # updates will follow the order of the numbers in the 
  # vector "count"
  
  print(paste("The count is at", number, "right now"))
  
  # If you don't get what the function "paste" does, 
  # check its help page by typing "help(paste)" in 
  # the console
  
} # To finish the loop, you have to close these brackets here

## ---- include=FALSE-----------------------------------------------------------
# cleaning directories created in this tutorial:

# deleting aux folders if they exist:
if("subfolder_relative" %in% list.files()){
  unlink("subfolder_relative", recursive = T)  
}

if("subfolder_absolute" %in% list.files()){
  unlink("subfolder_absolute", recursive = T)  
}

