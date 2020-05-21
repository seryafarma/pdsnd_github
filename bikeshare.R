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

draw_hist_birthyear_n_gender <- function(dataset, title) {
  ggplot(aes(group = 'birthyear', x = Birth.Year, color = Gender), data = subset(dataset, (Gender != ''))) + geom_histogram(binwidth = 1) +
    ggtitle(title) +
    scale_x_continuous(limit = c(1950, 2000)) +
    facet_wrap(~Gender)  
}

draw_hist_birthyear_n_gender(chi, 'Histogram of birth year of bikers in Chicago')
draw_hist_birthyear_n_gender(ny, 'Histogram of birth year of bikers in New York')

# Simple histogram of birth year
g1 <- ggplot(aes(group = 'birthyear', x = Birth.Year, color = Gender), data = subset(chi, (Gender != ''))) + geom_histogram(binwidth = 1) +
  ggtitle('Histogram of birth year of bikers in chicago') +
  scale_x_continuous(limit = c(1950, 2000)) +
  facet_wrap(~Gender)

g2 <- ggplot(aes(group = 'birthyear', x = Birth.Year, color = Gender), data = subset(ny, (Gender != ''))) + geom_histogram(binwidth = 1) +
  ggtitle('Histogram of birth year of bikers in new york') +
  scale_x_continuous(limit = c(1950, 2000)) +
  facet_wrap(~Gender)

# Show this in a multiple plot canvas using multiplot()
multiplot(g1, g2, cols=1)

#-----------------------------------------------------------------2
# duration of daily bike share in washington
# does the subscriber use bike more, or just customer?
by(wash$Trip.Duration, wash$User.Type, summary)
by(wash$Trip.Duration, wash$User.Type, sum)

# duration of bike share for short trips < 1 hour.
b1 <- ggplot(aes(x = Trip.Duration, fill = User.Type, color = User.Type), data = subset(wash, !is.na(Trip.Duration))) +
  geom_histogram() +
  ggtitle('Histogram of Bikeshare Users for Short trips < 1 hour') + 
  scale_x_continuous(limit = c(0, 3600), breaks=c(60,900,1800,3600),labels=c("1min","15min","30min","1hour")) +
  facet_wrap(~User.Type) +
  scale_fill_manual(name= "User Type", values = c("grey", "grey"))+
  scale_color_manual(name = "User Type", values = c("darkgreen", "darkblue"))

  
# boxplot
b2 <- ggplot(aes(x = Trip.Duration, fill = User.Type, color = User.Type), data = subset(wash, !is.na(Trip.Duration))) +
  geom_boxplot() +
  ggtitle('Box Plot of Bikeshare Users for Short trips < 1 hour') +
  scale_x_continuous(limit = c(0, 3600), breaks=c(60,900,1800,3600),labels=c("1min","15min","30min","1hour")) +
  facet_wrap(~User.Type) +
  scale_fill_manual(name= "User Type", values = c("grey", "grey"))+
  scale_color_manual(name = "User Type", values = c("darkgreen", "darkblue"))

# Show this in a multiple plot canvas using multiplot()
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
end_station_ds <- subset(ny, End.Station %in% top5.end)
# statistical summary for the top 5 end stations
by(end_station_ds, end_station_ds$End.Station %in% top5.end, summary)

top5.start <- tail(names(sort(table(ny$Start.Station))), 5)
start_station_ds <- subset(ny, Start.Station %in% top5.start)
# statistical summary for the top 5 start stations
by(start_station_ds, start_station_ds$Start.Station %in% top5.start, summary)

bar_plot_station <- function(subset_station, x_axis, title) {
  bar_plot_st = NULL
  if (x_axis == 'start') {
    bar_plot_st = ggplot(aes(x = Start.Station), data = subset_station)
  } else if (x_axis == 'end') {
    bar_plot_st = ggplot(aes(x = End.Station), data = subset_station)
  }
  bar_plot_st +
    geom_bar() + 
    geom_text(stat='count', aes(label=..count..), vjust=-1) +
    ggtitle(title)  
}

bar_plot_station(end_station_ds, 'end', 'Most common bikeshare end station in newyork')
bar_plot_station(start_station_ds, 'start', 'Most common bikeshare start station in newyork')


