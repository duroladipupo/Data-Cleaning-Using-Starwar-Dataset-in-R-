################################################################################
####      Data wrangling -- 
####    Basic data cleaning, Indexing and subsetting, 
####    Pre-processing tasks and the pipe operators
####    Summarizing based on qualitative attributes
###         
################################################################################
#tidyverse

###Set your working directory######
#####   setwd( "C:/.......")  ######
setwd("C:/Users/apple/Desktop/R PROG PRACTICE FILES")
getwd()

############DATA WRANGLING#################

####CLEANING DATA####

#####The process of identifying, correcting,################################
#####or removing inaccurate raw data for  #####################################
#####data analysis.A necessary first ###############################
#####step towards an analysis-ready dataset ###################################

library(tidyverse)

#farmiliarize yourself with the dataset
starwars 
View(starwars)
dim(starwars)
str(starwars)
glimpse(starwars)
summary(starwars)
View(starwars)
table(starwars$hair_color)
unique(starwars$hair_color)

#convert character to factor
unique(starwars$gender)
starwars$gender<-as.factor(starwars$gender)
class(starwars$gender)


######################MISSING DATA#############################

mean(starwars$height)
summary(starwars$height)
mean(starwars$height, na.rm = T)   ##to remove missing data

##checking missing data
unique(starwars$height)
unique(starwars$skin_color)
unique(starwars$eye_color)
unique(starwars$hair_color)
###omit observations (by df)
starwars %>% 
  select (name, gender, hair_color, height) %>% 
  na.omit()   ##observations reduced. but we do not know which observations we have removed

#to check observations that are omitted
starwars %>% 
  select(name, gender, hair_color, height) %>% 
  filter(!complete.cases(.)) ##this pastes/prints only cases with omitted variables (NAs)

###dropping NA for height
starr<- starwars %>% 
  select(name, gender, hair_color, height) %>% 
  drop_na(height) %>% View

dim(starr)
starwars_clean <- starwars %>% 
  select(name, gender, hair_color, height) %>% 
  #filter(!complete.cases(.)) %>% 
  drop_na(height) %>%
  mutate(hair_color = replace_na(hair_color, "none"))#replace with another value
starwars_clean  

###remove NA
starwars %>% 
  select(name, gender, hair_color, height) %>%
  drop_na(height) ## to remove na if that is justified

###remove NAs from the entire dataframe
starwars %>% 
  na.omit()   ##remove na from the entire observations and the number of cases reduce significantly

###replace NAs in an identified numeric column with mean
starwars$height<-as.double(starwars$height)
class(starwars$height)
starwars %>% 
  mutate(height = replace_na(height, "mean"(height, na.rm = TRUE))) %>% view
dim(starwars)

##replace NA with mean for all numeric variables/columns
starwars %>% 
  mutate_if(is.numeric, ~replace_na(.,mean(., na.rm = TRUE)))%>% view

unique(starwars$height)#check that NAs are no more included

unique(starwars$hair_color)#values/elements in the vector
unique(starwars$height)#values/elements in the vector
?winsorize
############CHECK AND CORRECT FOR OUTLIERS#####################
library(statar)
sum(starwars$height > quantile(starwars$height, 0.95)) #looking at number of values above 95th percentile 
sum(starwars_clean$height > quantile(starwars_clean$height, .95))#looking at number of values above 95th percentile
winsorize(starwars_clean $height, probs = c(0.05, 0.95))#winsorize height at top and bottom
starwars_clean$height

starwars_clean <- starwars_clean %>% #This code then index the wonsorized height into the df 
  mutate(wins_height = 
           winsorize(starwars_clean$height, probs = c(0.05, 0.95)))#another variable created
starwars_clean$wins_height
starwars_clean$height

############DUPLICATES######################
##create a dataframe
names <- c("John", "Ade", "Sola", "John")
age <- c(22, 33, 44, 22)
team <- data.frame(names, age)
team
duplicated(team)## is each of these cases duplicated?
team[!duplicated(team), ]##John (duplicated is deleted)
team
#you need to make it an object or overwrite the ealier df
team <- team[!duplicated(team), ]
team

##tidyverse way
team %>% distinct ()%>% 
  View

###to recode gender responses into 1 and 2 or 1 and 0, anything...
starwars %>% 
  select(name, gender) %>% 
  mutate(gender_coded = recode(gender, 
                               "masculine" = 1,
                               "feminine"= 2))

###################INDEXING A VECTOR IN R#######################
#Indexing a vector in R
temperatures <- c(12.9, 13.2, 15.0, 19.2, 21.4, 24.5, 29.5, 24.2, 20.5, 14.5, 10.2, 9.8)
temperatures
#imagine we would like to obtain the number 12.9 , the first temperature avaiable in our temperatures vector

temperatures[1]                     #[] is our index anchor

#slicing or feeding a vector to the index
temperatures[1:5]   #1 to 5
temperatures[c(1, 5, 7)]
temperatures[c(1, 5, 30)]##R returns 30 as NA (not available)
#Named vectors are also possible
#we can also assign name properties to the values 
names(temperatures) <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
names(temperatures)
temperatures["Jan"]
temperatures[c('Jan','Mar')]
temperatures[c('Jan','Mar')]

#############MATRIX INDEXING#####################
matrix_example <- matrix(
  data = c(1000,122,-500,-15),
  nrow = 2,
  ncol = 2
)
View(matrix_example) #can we then pick 1000 from the matrix. To do this we use comma(,) to specify row and columns
matrix_example[1,1] # row 1, column 1
matrix_example[1,2]  # row 1, column 2
matrix_example[,2]  # all rows, column 2

##working with named matrix
rownames(matrix_example) <- c('John','May')
colnames(matrix_example) <- c('Income','Expenses')
(matrix_example)
matrix_example['John','Income'] ##to index Johns income
matrix_example[c('John','May'),'Income'] ##same principle works

#OR
matrix_example[c(1, 2), 'Income']
#OR
matrix_example[c(1, 2), 1]

###########################INDEXING OF A DATAFRAME.############################# 
##Lets make use of the iris data in R
iris
dim(iris)
glimpse(iris)
iris_df <- iris
iris_df[1,3]#pick up the first row, third column, that contains the value 1.4
iris_df[, 4:5]#pick up the all row,  column 4-5, that contains the value 1.4
iris_df$Species  #additionally, the Dollar sign ($) can be used to index a column
iris_df[iris_df$Species == 'setosa',]##index all the Species that are setosa in the data frame above
iris_df[iris_df$Species == 'setosa','Sepal.Width'] 
iris_df[iris_df$Species == 'setosa','Sepal.Width']
iris_df[iris_df$Species == 'setosa',c('Sepal.Width', 'Sepal.Length')]

##use more conditionals on the index, retrieve setosa and virginica in the same column (Species)##
iris_df[(iris_df$Species == 'setosa') | (iris_df$Species == 'virginica'),c('Sepal.Width', 'Sepal.Length')]

###subsetting in R. There are different ways

# Subsetting data frame
mtcars
print(mtcars)
# Subsetting data frame
print(mtcars['hp'])

# First 10 cars and remove columns 1 and 2
print(mtcars[1:10, -c(1, 2)])

#Subsetting
mtcars <- subset(mtcars, disp > 160,
                 select = c(hp))###### this will display 'hp' that has equivlent value to 'disp' > 160
mtcars

##########summarise datasets####################
iris
library(tidyverse)
df<-iris
df1<- summarise(df, mean(Sepal.Length))
df1 



##############Let's create mean and sd of Sepal Length.######################
df2 <- summarise(df, Mean = mean(Sepal.Length),SD = sd(Sepal.Length))
df2

df3<-summarise(group_by(df, Species),## summarise by a categorical variable
               Mean=mean(Sepal.Length),
               SD=sd(Sepal.Length))
df3
##OR
df4<-df %>%
  group_by(Species) %>%
  summarise(Mean = mean(Sepal.Length),
            SD=sd(Sepal.Length))
df4

df5<-df %>% 
  group_by(Species) %>%
  summarise(sum = sum(Sepal.Length),
            SD=sd(Sepal.Length))####sum of each specie
df5

#######minimum and maximum of sepal length########
df6<-df %>%
  group_by(Species) %>%
  summarise(Min = min(Sepal.Length),
            Max=max(Sepal.Length))
df6

####Count of...#########
df7<-df %>%
  group_by(Species) %>%
  summarise(Sepal.Length = n())%>%
  arrange(Sepal.Length) ###### this will give the total of each species in a row######
df7

####First and Last values ...####
#Some cases first cases or position identification is important, #
#then you can make use of first, last or nth position of a group.#
df8<-df %>%
  group_by(Species) %>%
  summarise(First = first(Sepal.Length),
            Last=last(Sepal.Length))
df8

