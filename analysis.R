#loading packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(usmap)

#loading massive dataframe
prison_df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#cleaning the data frames (and grouping them by race)
aapi_clean_prison_df <- prison_df %>%
  filter(, year >= 1990, na.rm = TRUE) %>%
  filter(, total_pop > 0, na.rm = TRUE) %>%
  filter(, aapi_pop_15to64 > 0, na.rm = TRUE) %>%
  select(, "year", "state", "county_name", "total_pop", "total_pop_15to64", "aapi_pop_15to64", "region", "division", "total_jail_pop", "aapi_jail_pop")

white_clean_prison_df <- prison_df %>%
  filter(, year >= 1990, na.rm = TRUE) %>%
  filter(, total_pop > 0, na.rm = TRUE) %>%
  filter(, white_pop_15to64 > 0, na.rm = TRUE) %>%
  select(, "white_pop_15to64", "white_jail_pop", "year", "state", "county_name", "total_pop", "total_pop_15to64", "region", "division", "total_jail_pop")

black_clean_prison_df <- prison_df %>%
  filter(, year >= 1990, na.rm = TRUE) %>%
  filter(, total_pop > 0, na.rm = TRUE) %>%
  filter(, black_pop_15to64 > 0, na.rm = TRUE) %>%
  select(, "black_pop_15to64", "black_jail_pop", "year", "state", "county_name", "total_pop", "total_pop_15to64", "region", "division", "total_jail_pop")

native_clean_prison_df <- prison_df %>%
  filter(, year >= 1990, na.rm = TRUE) %>%
  filter(, total_pop > 0, na.rm = TRUE) %>%
  filter(, native_pop_15to64 > 0, na.rm = TRUE) %>%
  select(, "native_pop_15to64", "native_jail_pop", "year", "state", "county_name", "total_pop", "total_pop_15to64", "region", "division", "total_jail_pop")

latinx_clean_prison_df <- prison_df %>%
  filter(, year >= 1990, na.rm = TRUE) %>%
  filter(, total_pop > 0, na.rm = TRUE) %>%
  filter(, latinx_pop_15to64 > 0, na.rm = TRUE) %>%
  select(, "latinx_pop_15to64", "latinx_jail_pop", "year", "state", "county_name", "total_pop", "total_pop_15to64", "region", "division", "total_jail_pop")


#calculating summary information
summary_info <- list()
  summary_info$num_states <- unique(select(clean_prison_df, state))
  summary_info$num_counties <- unique(select(clean_prison_df, county_name))

#add columns for incarceration rates to dataframes
#aapi
aapi_clean_prison_df <- mutate(aapi_clean_prison_df, aapi_incarceration_rate = aapi_jail_pop/aapi_pop_15to64)
summary_info$max_aapi_incar_rate <- max(aapi_clean_prison_df$aapi_incarceration_rate, na.rm = TRUE)
summary_info$mean_aapi_incar_rate <- mean(aapi_clean_prison_df$aapi_incarceration_rate, na.rm = TRUE)
summary_info$min_aapi_incar_rate <- min(aapi_clean_prison_df$aapi_incarceration_rate, na.rm = TRUE)

summary_info$suspicious_counties_aapi <- unique(aapi_clean_prison_df[aapi_clean_prison_df$aapi_incarceration_rate > 1, "county_name"], na.rm = TRUE)
summary_info$suspicious_counties_aapi <- summary_info$suspicious_counties_aapi[-1]

#white
white_clean_prison_df <- mutate(white_clean_prison_df, white_incarceration_rate = white_jail_pop/white_pop_15to64)
summary_info$max_white_incar_rate <- max(white_clean_prison_df$white_incarceration_rate, na.rm = TRUE)
summary_info$mean_white_incar_rate <- mean(white_clean_prison_df$white_incarceration_rate, na.rm = TRUE)
summary_info$min_white_incar_rate <- min(white_clean_prison_df$white_incarceration_rate, na.rm = TRUE)

summary_info$suspicious_counties_white <- unique(white_clean_prison_df[white_clean_prison_df$white_incarceration_rate > 1, "county_name"], na.rm = TRUE)
summary_info$suspicious_counties_white <- summary_info$suspicious_counties_white[-1]

#black
black_clean_prison_df <- mutate(black_clean_prison_df, black_incarceration_rate = black_jail_pop/black_pop_15to64)
summary_info$max_black_incar_rate <- max(black_clean_prison_df$black_incarceration_rate, na.rm = TRUE)
summary_info$mean_black_incar_rate <- mean(black_clean_prison_df$black_incarceration_rate, na.rm = TRUE)
summary_info$min_black_incar_rate <- min(black_clean_prison_df$black_incarceration_rate, na.rm = TRUE)

summary_info$suspicious_counties_black <- unique(black_clean_prison_df[black_clean_prison_df$black_incarceration_rate > 1, "county_name"], na.rm = TRUE)
summary_info$suspicious_counties_black <- summary_info$suspicious_counties_black[-1]

#native
native_clean_prison_df <- mutate(native_clean_prison_df, native_incarceration_rate = native_jail_pop/native_pop_15to64)
summary_info$max_native_incar_rate <- max(native_clean_prison_df$native_incarceration_rate, na.rm = TRUE)
summary_info$mean_native_incar_rate <- mean(native_clean_prison_df$native_incarceration_rate, na.rm = TRUE)
summary_info$min_native_incar_rate <- min(native_clean_prison_df$native_incarceration_rate, na.rm = TRUE)

summary_info$suspicious_counties_native <- unique(native_clean_prison_df[native_clean_prison_df$native_incarceration_rate > 1, "county_name"], na.rm = TRUE)
summary_info$suspicious_counties_native <- summary_info$suspicious_counties_native[-1]

#latinx
latinx_clean_prison_df <- mutate(latinx_clean_prison_df, latinx_incarceration_rate = latinx_jail_pop/latinx_pop_15to64)
summary_info$max_latinx_incar_rate <- max(latinx_clean_prison_df$latinx_incarceration_rate, na.rm = TRUE)
summary_info$mean_latinx_incar_rate <- mean(latinx_clean_prison_df$latinx_incarceration_rate, na.rm = TRUE)
summary_info$min_latinx_incar_rate <- min(latinx_clean_prison_df$latinx_incarceration_rate, na.rm = TRUE)

summary_info$suspicious_counties_latinx <- unique(latinx_clean_prison_df[latinx_clean_prison_df$latinx_incarceration_rate > 1, "county_name"], na.rm = TRUE)
summary_info$suspicious_counties_latinx <- summary_info$suspicious_counties_latinx[-1]

#Find the numbers of suspicious counties, and then the total number, excluding duplicates
summary_info$num_suspicious_counties_aapi <- length(summary_info$suspicious_counties_aapi)
summary_info$num_suspicious_counties_white <- length(summary_info$suspicious_counties_white)
summary_info$num_suspicious_counties_black <- length(summary_info$suspicious_counties_black)
summary_info$num_suspicious_counties_native <- length(summary_info$suspicious_counties_native)
summary_info$num_suspicious_counties_latinx <- length(summary_info$suspicious_counties_latinx)

summary_info$all_suspicious_counties <- unique(c(summary_info$suspicious_counties_aapi, summary_info$suspicious_counties_white, summary_info$suspicious_counties_black, summary_info$suspicious_counties_native, summary_info$suspicious_counties_latinx))
summary_info$total_num_suspicious_counties <- length(summary_info$all_suspicious_counties)


#Lets prepare variables for the first chart
#We will evaluate the number of suspicious counties in each year for each race, since 1990 to 2018.
#aapi
year_column <- list()

number_aapi_suspicions <- list()

for(i in 1:29){
  year_column[[length(year_column) + 1]] <- i + 1989
}
for(b in 1:29){
number_aapi_suspicions[[b]] <- nrow(filter(filter(aapi_clean_prison_df, aapi_incarceration_rate > 1, na.rm = TRUE), year == b + 1989, na.rm = TRUE))
}

aapi_suspicions_yearly_df <- rbind(year_column, number_aapi_suspicions)
aapi_suspicions_yearly_df <- as.data.frame(t(aapi_suspicions_yearly_df))
aapi_suspicions_yearly_df <- as.data.frame(lapply(aapi_suspicions_yearly_df, unlist))

#white
number_white_suspicions <- list()

for(h in 1:29){
  number_white_suspicions[[h]] <- nrow(filter(filter(white_clean_prison_df, white_incarceration_rate > 1, na.rm = TRUE), year == h + 1989, na.rm = TRUE))
}

white_suspicions_yearly_df <- rbind(year_column, number_white_suspicions)
white_suspicions_yearly_df <- as.data.frame(t(white_suspicions_yearly_df))
white_suspicions_yearly_df <- as.data.frame(lapply(white_suspicions_yearly_df, unlist))

#black
number_black_suspicions <- list()

for(y in 1:29){
  number_black_suspicions[[y]] <- nrow(filter(filter(black_clean_prison_df, black_incarceration_rate > 1, na.rm = TRUE), year == y + 1989, na.rm = TRUE))
}

black_suspicions_yearly_df <- rbind(year_column, number_black_suspicions)
black_suspicions_yearly_df <- as.data.frame(t(black_suspicions_yearly_df))
black_suspicions_yearly_df <- as.data.frame(lapply(black_suspicions_yearly_df, unlist))

#latinx
number_latinx_suspicions <- list()

for(l in 1:29){
  number_latinx_suspicions[[l]] <- nrow(filter(filter(latinx_clean_prison_df, latinx_incarceration_rate > 1, na.rm = TRUE), year == l + 1989, na.rm = TRUE))
}

latinx_suspicions_yearly_df <- rbind(year_column, number_latinx_suspicions)
latinx_suspicions_yearly_df <- as.data.frame(t(latinx_suspicions_yearly_df))
latinx_suspicions_yearly_df <- as.data.frame(lapply(latinx_suspicions_yearly_df, unlist))

#native
number_native_suspicions <- list()

for(n in 1:29){
  number_native_suspicions[[n]] <- nrow(filter(filter(native_clean_prison_df, native_incarceration_rate > 1, na.rm = TRUE), year == n + 1989, na.rm = TRUE))
}

native_suspicions_yearly_df <- rbind(year_column, number_native_suspicions)
native_suspicions_yearly_df <- as.data.frame(t(native_suspicions_yearly_df))
native_suspicions_yearly_df <- as.data.frame(lapply(native_suspicions_yearly_df, unlist))

#combining the dataframes
all_suspicions_yearly_df <- merge(x=aapi_suspicions_yearly_df, y=white_suspicions_yearly_df, by="year_column")
all_suspicions_yearly_df <- merge(x=all_suspicions_yearly_df, y=black_suspicions_yearly_df, by="year_column")
all_suspicions_yearly_df <- merge(x=all_suspicions_yearly_df, y=native_suspicions_yearly_df, by="year_column")
all_suspicions_yearly_df <- merge(x=all_suspicions_yearly_df, y=latinx_suspicions_yearly_df, by="year_column")


#making the line graph
ggplot(data = all_suspicions_yearly_df, aes(x = year_column)) +
  geom_line(aes(y = number_aapi_suspicions, colour = "Asian/Pac-Islander")) +
  geom_line(aes(y = number_white_suspicions, colour = "White")) +
  geom_line(aes(y = number_black_suspicions, colour = "Black")) +
  geom_line(aes(y = number_native_suspicions, colour = "Native American")) +
  geom_line(aes(y = number_latinx_suspicions, colour = "Latinx")) +
  scale_colour_manual("", 
                      breaks = c("Asian/Pac-Islander", "White", "Black", "Native American", "Latinx"),
                      values = c("red", "green", "blue", "yellow", "purple")) +
  xlab("Year") +
  scale_y_continuous("Number of Counties", limits = c(0,40)) + 
  labs(title="Suspicious Counties per Year")

#making the second chart
suspicious_native_clean_prison_df <- filter(native_clean_prison_df, native_incarceration_rate > 1, na.rm = TRUE)
ggplot(suspicious_native_clean_prison_df, aes(x = total_pop_15to64, y=native_incarceration_rate)) +
  geom_point()

#making the map
suspicious_latinx_clean_prison_df <- filter(latinx_clean_prison_df, latinx_incarceration_rate > 1, na.rm = TRUE)

suspicious_black_clean_prison_df <- filter(black_clean_prison_df, black_incarceration_rate > 1, na.rm = TRUE)

suspicious_aapi_clean_prison_df <- filter(aapi_clean_prison_df, aapi_incarceration_rate > 1, na.rm = TRUE)

#combining:
purged_latinx_clean_df <- select(suspicious_latinx_clean_prison_df, "year", "state", "county_name", "total_pop_15to64", "region", "division")
purged_black_clean_df <- select(suspicious_black_clean_prison_df, "year", "state", "county_name", "total_pop_15to64", "region", "division")
purged_aapi_clean_df <- select(suspicious_aapi_clean_prison_df, "year", "state", "county_name", "total_pop_15to64", "region", "division")
purged_native_clean_df <- select(suspicious_native_clean_prison_df, "year", "state", "county_name", "total_pop_15to64", "region", "division")

suspicious_total_clean_df <- rbind(x = purged_latinx_clean_df, y = purged_black_clean_df, all = TRUE)

suspicious_total_clean_df <- rbind(x = suspicious_total_clean_df, y = purged_aapi_clean_df, all = TRUE)

suspicious_total_clean_df <- rbind(x = suspicious_total_clean_df, y = purged_native_clean_df, all = TRUE)

suspicious_total_clean_1999_df <- suspicious_total_clean_df %>%
  filter(year == 1999)

#making the map
IA_ <- nrow(subset(suspicious_total_clean_1999_df, state == "IA"))
ID_ <- nrow(subset(suspicious_total_clean_1999_df, state == "ID"))
IL_ <- nrow(subset(suspicious_total_clean_1999_df, state == "IL"))
IN_ <- nrow(subset(suspicious_total_clean_1999_df, state == "IN"))
KS_ <- nrow(subset(suspicious_total_clean_1999_df, state == "KS"))
KY_ <- nrow(subset(suspicious_total_clean_1999_df, state == "KY"))
LA_ <- nrow(subset(suspicious_total_clean_1999_df, state == "LA"))
MI_ <- nrow(subset(suspicious_total_clean_1999_df, state == "MI"))
MO_ <- nrow(subset(suspicious_total_clean_1999_df, state == "MO"))
ND_ <- nrow(subset(suspicious_total_clean_1999_df, state == "ND"))
NE_ <- nrow(subset(suspicious_total_clean_1999_df, state == "NE"))
NM_ <- nrow(subset(suspicious_total_clean_1999_df, state == "NM"))
OK_ <- nrow(subset(suspicious_total_clean_1999_df, state == "OK"))
TN_ <- nrow(subset(suspicious_total_clean_1999_df, state == "TN"))
TX_ <- nrow(subset(suspicious_total_clean_1999_df, state == "TX"))
UT_ <- nrow(subset(suspicious_total_clean_1999_df, state == "UT"))
WI_ <- nrow(subset(suspicious_total_clean_1999_df, state == "WI"))
WV_ <- nrow(subset(suspicious_total_clean_1999_df, state == "WV"))

suspicious_quantities <- c(IA_, ID_, IL_, IN_, KS_, KY_, LA_, MI_, MO_, ND_, NE_, NM_, OK_, TN_, TX_, UT_, WI_, WV_)
state <- c("IA", "ID", "IL", "IN", "KS", "KY", "LA", "MI", "MO", "ND", "NE", "NM", "OK", "TN", "TX", "UT", "WI", "WV")

summary_table <- data.frame(state, suspicious_quantities)

#Ok. Actually making the map this time
plot_usmap(data=summary_table, values = "suspicious_quantities") +
  scale_fill_continuous(low="gray", high="blue", name = "Number of Suspiciously High Incarceration Rates") +
  ggtitle("Number of Racial Incarceration Rates Above 1 per County, USA 1999")