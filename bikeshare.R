#------------------------------------------------------------------
# filname: 
#   bikeshare.R
# desc:
#   The questions and answers for the bikeshare project with
#   using R for the udacity course:
#   - Programming for Data Science with R Nanodegree Program
#------------------------------------------------------------------

install.packages('ggplot2')
install.packages('Rmisc')

library('ggplot2')
library('Rmisc')

# Load the csv files into dataset.
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Peeking at column names.
colnames(chi)

#-----------------------------------------------------------------1
# Some things we learned
# Men bike more than women.
# This applies for new york and chicago.
# also there is this drop between people born in 1990 to 2000.
# young people in this age range tend to not use the bikeshare 
# compared to the older people.
by(chi$Birth.Year, chi$Gender, summary)
by(ny$Birth.Year, ny$Gender, summary)

# Simple histogram of birth year
g1 <- ggplot(aes(group = 'birthyear', x = Birth.Year, color = Gender), data = subset(chi, (Gender != ''))) + geom_histogram(binwidth = 1) +
  ggtitle('Histogram of birth year of bikers in chicago') +
  scale_x_continuous(limit = c(1950, 2000)) +
  facet_wrap(~Gender)

g2 <- ggplot(aes(group = 'birthyear', x = Birth.Year, color = Gender), data = subset(ny, (Gender != ''))) + geom_histogram(binwidth = 1) +
  ggtitle('Histogram of birth year of bikers in new york') +
  scale_x_continuous(limit = c(1950, 2000)) +
  facet_wrap(~Gender)

multiplot(g1, g2, cols=1)

#-----------------------------------------------------------------2
# duration of daily bike share in washington
# does the subscriber use bike more, or just customer?
by(wash$Trip.Duration, wash$User.Type, summary)
by(wash$Trip.Duration, wash$User.Type, sum)

# duration of bike share for short trips < 1 hour.
b1 <- ggplot(aes(x = Trip.Duration, fill = User.Type, color = User.Type), data = subset(wash, !is.na(Trip.Duration))) +
  geom_histogram() +
  scale_x_continuous(limit = c(0, 3600), breaks=c(60,900,1800,3600),labels=c("1min","15min","30min","1hour")) +
  facet_wrap(~User.Type) +
  scale_fill_manual(name= "User Type", values = c("grey", "grey"))+
  scale_color_manual(name = "User Type", values = c("darkgreen", "darkblue"))

# boxplot
b2 <- ggplot(aes(x = Trip.Duration, fill = User.Type, color = User.Type), data = subset(wash, !is.na(Trip.Duration))) +
  geom_boxplot() +
  scale_x_continuous(limit = c(0, 3600), breaks=c(60,900,1800,3600),labels=c("1min","15min","30min","1hour")) +
  facet_wrap(~User.Type) +
  scale_fill_manual(name= "User Type", values = c("grey", "grey"))+
  scale_color_manual(name = "User Type", values = c("darkgreen", "darkblue"))

multiplot(b1, b2, cols=1)

# duration of bike share for daily trips (until 1 week)
ggplot(aes(x = Trip.Duration, fill = User.Type, color = User.Type), data = subset(wash, !is.na(Trip.Duration))) +
  geom_boxplot() +
  scale_x_continuous(limit = c(86400, 604800), breaks=c(86400, 172800, 604800),labels=c("1day","2days","1week")) +
  facet_wrap(~User.Type) +
  scale_fill_manual(name= "User Type", values = c("grey", "grey"))+
  scale_color_manual(name = "User Type", values = c("darkgreen", "darkblue"))

ggplot(aes(x = Trip.Duration, fill = User.Type, color = User.Type), data = subset(wash, !is.na(Trip.Duration))) +
  geom_histogram() +
  scale_x_continuous(limit = c(86400, 604800), breaks=c(86400, 172800, 604800),labels=c("1day","2days","1week")) +
  facet_wrap(~User.Type) +
  scale_fill_manual(name= "User Type", values = c("grey", "grey"))+
  scale_color_manual(name = "User Type", values = c("darkgreen", "darkblue"))

#-----------------------------------------------------------------3
# what is the most common start station and 
# the most common end station in new york?
top5.end <- tail(names(sort(table(ny$End.Station))), 5)
top5.start <- tail(names(sort(table(ny$End.Station))), 5)

ggplot(aes(x = End.Station), 
       data = subset(ny, End.Station %in% top5.end)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)

ggplot(aes(x = Start.Station), 
       data = subset(ny, Start.Station %in% top5.start)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)
