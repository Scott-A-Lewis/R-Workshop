# Import ‘Albanian_data_for_workshop.csv’ under the name ‘my_albanian_data’
my_albanian_data <- read.csv("Albanian_data_for_workshop.csv")

# View the data
View(my_albanian_data)

# Create a subset of the data so that you are just looking at speakers of the Tosk dialect (abbreviated to 'T' in the dataset)
# Call this 'my_tosk_data'
my_tosk_data <- subset(my_albanian_data, Dialect == "T")

# Find the number of rows in 'my_tosk_data'
nrow(my_tosk_data)

## Moving on to dplyr
# Sticking with 'my_tosk_data', you will notice that there is a horrific mistake: 'Participant' has actually been labelled
# 'Victim'. We need to change this.
# Rename the column 'Victim' to 'Participant'
library(dplyr)
my_tosk_data <- my_tosk_data %>% rename(Participant = Victim)

# Use str() to take another look at the structure of the data. 
str(my_tosk_data)
# There are some names like Segment_label, Position_label and Rise_time. Let's change these to Segment, Position and RT.
my_tosk_data <- my_tosk_data %>% rename(Segment = Segment_label, Position = Position_label, RT = Rise_time)

# You will notice that there are 21 columns. This is a bit cumbersome to work with.
# Create a new data frame called 'my_tosk_data_simplified', selecting the columns: 'Participant', 'Segment', 'Position', 'RT'
# 'Sex' and 'Age'
my_tosk_data_simplified <- my_tosk_data %>% select(Participant, Segment, Position, RT, Sex, Age)

# RT (Rise time) is measured in seconds. It would be easier to work with if with converted this to milliseconds by
# multiplying the value by 1000.
my_tosk_data_simplified <- my_tosk_data_simplified %>% mutate(RT = RT*1000)

# Extract the mean values for Rise time in milliseconds for each Participant by Segment and Position under the name: 'mean_RT'
# Call the summary of the data 'RT_summary'
RT_summary <- my_tosk_data_simplified %>% group_by(Participant, Segment, Position) %>% summarise(mean_RT = mean(RT))

# Relabel the segments in RT_summary, so that 'q' becomes '/c/' and 'ç' becomes "/tʃ/"
RT_summary <- RT_summary %>% mutate(Segment = recode_factor(Segment,  q = "/c/", ç = "/tʃ/"))

# Use ggplot to create a boxplot for the RT_summary data
# Plot Segment on the x-axis and the mean_RT on the y-axis.
ggplot(RT_summary, aes(x = Segment, y = mean_RT)) + 
  geom_boxplot()

# Using the previous plot, change the colour of the boxplots on the basis of Position
ggplot(RT_summary, aes(x = Segment, y = mean_RT)) + 
  geom_boxplot(aes(fill = Position)) 

# Again, building upon the previous plot, change the labels of the x and y axes, to 'Phoneme' and 'Rise Time (milliseconds)'
ggplot(RT_summary, aes(x = Segment, y = mean_RT)) + 
  geom_boxplot(aes(fill = Position)) + 
  labs(x = "Phoneme", y = "Rise Time (milliseconds)")

ggplot(RT_summary, aes(x = Segment, y = mean_RT)) + 
  geom_boxplot(aes(fill = Position)) + 
  xlab("Phoneme") +
  ylab("Rise Time (milliseconds)")

# If you wanted to change the order of the position labels 'on' and 'med', you could reverse the x-axis labels and the order
# of the colours for fill in the legend, by adding these two lines of code:
ggplot(RT_summary, aes(x = Segment, y = mean_RT)) + 
  geom_boxplot(aes(fill = Position)) + labs(x = "Phoneme", y = "Rise Time (milliseconds)") + 
  scale_x_discrete(limits = rev) +
  scale_fill_discrete(limits = rev)

# Create a bar plot using ggplot, looking at just a subset of the data.
# The subsetted condition you need to focus in on is 'Position == "on"'
# When thinking about aesthetic mapping, include Segment along the x-axis.
# Include mean_RT along the y-axis.
ggplot(subset(RT_summary, Position == "on"), aes(x = Segment, y = mean_RT, fill = Segment)) + 
  geom_bar(stat = "identity", position = position_dodge()) + facet_grid(~ Participant)
