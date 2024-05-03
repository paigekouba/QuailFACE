# Root Data Plotting Script for Saaniya
# Thu May 2, 2024

# This script will help you visualize the data on root features that you got from RhizoVision Explorer. 

# First we need to load the data
# if you're on a Latimer Lab desktop, you can do that like this:

rootimage <- read.csv(####"features_4.26.24_final.csv") # this line is creating a new object "rootimage", which is the same as the Excel spreadsheet but now in R-land. R calls this data object type a dataframe. Because we're following tidydata principles, each variable gets its own column, and each row is a unique observation (one seedling).
# you can check out what it looks like in a couple ways:
View(rootimage) # see the whole thing at once
head(rootimage) # see just the first six rows, all columns
str(rootimage) # a summary of its structure, so all the column names followed by the type of data and the first several observations in that column. 
# Notice how the columns come with a label (int, chr, or num, meaning integer, character, or number). 

# Okay cool! Now  we need to add a column to the rootimage dataframe, indicating what treatment group each seedling came from.
# Here I'm making a new dataframe called "lookup", which is just each plot # (1-16) listed alongside its treatment level, in the order they were at the experiment site:
lookup <- data.frame(as.character(c(1:16)), c("AW","ED","AD","EW",
                                              "AW","ED","EW","AD",
                                              "ED","AW","EW","AD",
                                              "ED","AW","EW","AD"))
names(lookup) <- c("Plot","Tmt") # renaming the columns to be prettier

rootimage <- merge(lookup, rootimage, by = 'Plot') # this line takes every row in rootimage, looks up its Plot value in lookup, and brings back the value in the Tmt column for that row of the lookup dataframe.

# last but not least, we want to add another new column for the species name ("Spp"), which we can extract from the code much like we did in Excel functions last week
rootimage <- rootimage %>% 
  mutate(Spp = substr(Code, nchar(Code)-2,nchar(Code)-2))
# the second line is saying "mutate"=add a column called "Spp" which is created by taking the "Code" character string for each row and creating a "substr"=substring. substr() is a function (one of millions of handy ones in R) which needs several *arguments* like this: function(data, argument1, argument2, argument3, ...). In this case, it's saying substr([what column should I look under], [what is the # of the character string to start the substring], [what is the # of the character string to end the substring]). So translated to our example: substr([take the Code for this row], [start at the second-to-last letter in the string], [end at the same place]). nchar(Code) is the R equivalent of len(Code) in Excel, which gave us the # of letters in the Code.

# Now, at last, the fun part! Use this line to make plots of the different variables measured, based on what you found in your literature review. Don't forget to make comments indicating what your plots show.
# sidenote, ggplot is AMAZINGLY powerful and cand do basically anything you dream up. We'll keep it simple to start. Just like the function substr() above, ggplot() is a function with a couple extra arguments you need to specify. The general format is ggplot(data, aes [short for aesthetic] (x , y)). For our purposes, we'll add Tmt as a "group" variable, which means group together all the observations within a Tmt category.

ggplot(rootimage, aes(x=Tmt, y=Number.of.Root.Tips, group = Tmt)) + 
  geom_boxplot() + # this says: using that data and plotting the things I asked for ^, make a boxplot
  facet_grid(~Spp) # this says, make a separate plot for each species

# That's it! Replace "Number.of.Root.Tips" with any of the 21 columns that are numbers or integers. Be careful with capitalization because R is very picky.