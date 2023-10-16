# OASDI Table Scrape Repository

This repository contains R code for scraping all tables included the Social Security Administration Trustees Report from years 2014 to 2022. The code inserts the tables into an easily accessible nested list. I am currently working on expanding the scrape to earlier years. 

Scraping the tables requires collection of a unique identifying HTML table names for each table. This code consolidates all information needed to scrape the tables into a single data frame, **main_table**, and scrapes all of the tables into a nested list, **raw_data**.


## Installation

There are no files necessary to use this code. 

## Disclaimer

It should be noted that this code directly scrapes the tables included in the Trustees Reports, regardless of whether the Social Security Administration has changed the index of specific tables over the years.

For example, in 2014, section VI.G (ESTIMATES FOR OASDI AND HI, SEPARATE AND COMBINED) table G9 contains: *OASDI and HI Annual Non-interest Income, Cost, and Balance in **Current Dollars**, Calendar Years 2014-90*

From 2015-2022, section VI.G table G9 contains: *OASDI and HI Annual Non-interest Income, Cost, and Balance in **CPI-Indexed** [insert year] Dollars, Calendar Years [insert year]-2100* and G10 contains: *OASDI and HI Annual Non-interest Income, Cost, and Balance in **Current Dollars**, Calendar Years [insert year]-2100*

In 2014, the 9th is in current dollars and there is no table for CPI-Index dollars. In years 2015-2022, the 9th is CPI-Indexed and the 10th is in current dollars.

The nested list contains the tables exactly as they are listed on the OASDI website.

## Example Code

Below I have written a code snippet to demonstrate how the final dataset can be navigated. Upon inspection of the nested list, raw_data, I found the "Selected Economic Variables" table is contained in VI.G.6 for all years 2014-2022. The code snippet does the following:

- Writes a function that takes the table for each year and assigns the table to its own dataset
- Notice for years 2014-2022, I implement a command to make the first observation the variable names; this is a result of the change in html format used by the SSA prior to 2019
~~~
#Function that takes first row of dataframe and makes the first row the variable names, then removes first row
  firstrow_as.colnames <- function(df) {
    my.names <- df[1, ]
    colnames(df) <- my.names
    df <- df[-1, ]
    return(df)
  }
#2022  
  VI_G_6_2022 <- rawdata[[5]][[7]][[6]][[1]][[1]]
  VI_G_6_2022 <- VI_G_6_2022[,c(2,4)]
  
#2021
  VI_G_6_2021 <- rawdata[[5]][[7]][[6]][[2]][[1]]
  VI_G_6_2021 <- VI_G_6_2021[,c(2,4)]
  
#2020 
  VI_G_6_2020 <- rawdata[[5]][[7]][[6]][[3]][[1]]
  VI_G_6_2020 <- VI_G_6_2020[,c(2,4)]
  
#2019
  VI_G_6_2019 <- rawdata[[5]][[7]][[6]][[4]][[1]]
  VI_G_6_2019 <- VI_G_6_2019[,c(2,4)]
  
#2018
  VI_G_6_2018 <- rawdata[[5]][[7]][[6]][[5]][[1]]
  VI_G_6_2018 <- firstrow_as.colnames(VI_G_6_2018)
  VI_G_6_2018 <- VI_G_6_2018[,c(2,4)]
  
#2017
  VI_G_6_2017 <- rawdata[[5]][[7]][[6]][[6]][[1]]
  VI_G_6_2017 <- firstrow_as.colnames(VI_G_6_2017)
  VI_G_6_2017 <- VI_G_6_2017[,c(2,4)]
  
#2016
  VI_G_6_2016 <- rawdata[[5]][[7]][[6]][[7]][[1]]
  VI_G_6_2016 <- firstrow_as.colnames(VI_G_6_2016)
  VI_G_6_2016 <- VI_G_6_2016[,c(2,4)]  
  
#2015
  VI_G_6_2015 <- rawdata[[5]][[7]][[6]][[8]][[1]]
  VI_G_6_2015 <- firstrow_as.colnames(VI_G_6_2015)
  VI_G_6_2015 <- VI_G_6_2015[,c(2,4)]
  
#2014
  VI_G_6_2014 <- rawdata[[5]][[7]][[6]][[9]][[1]]
  VI_G_6_2014 <- firstrow_as.colnames(VI_G_6_2014)
  VI_G_6_2014 <- VI_G_6_2014[,c(2,4)]
~~~

- Loops through each data frame to filter for 2 columns: the Average Wage Index, and the prediction calendar year
- Filters the data frame to only contain the average wage prediction for the year 2050
- Adds a column to indicate the report year for each observation
- Assigns the filtered data frames to new "advaned" data frames
~~~
data_frames <- list(VI_G_6_2014, VI_G_6_2015, VI_G_6_2016, VI_G_6_2017, VI_G_6_2018, VI_G_6_2019, VI_G_6_2020, VI_G_6_2021, VI_G_6_2022)

# Loop through each data frame
for (i in 1:length(data_frames)) {
  # Make a new column to distinguish the year of the report; Make every value the year of the report
  data_frames[[i]]$Report_Year <- rep(2014 + i - 1, nrow(data_frames[[i]]))
  
  # Find the row where the second row of data equals "Low-cost"
  low_cost_row <- which(data_frames[[i]][, 2] == "Low-cost:")
  
  # Delete all rows below the "Low-cost" row
  data_frames[[i]] <- data_frames[[i]][1:low_cost_row, ]
  
  # Delete all rows where the first row does not equal 2050
  data_frames[[i]] <- data_frames[[i]][data_frames[[i]][, 1] == 2050, ]
  
}


VI_G_6_2014_adv <- data_frames[[1]]
VI_G_6_2015_adv <- data_frames[[2]]
VI_G_6_2016_adv <- data_frames[[3]]
VI_G_6_2017_adv <- data_frames[[4]]
VI_G_6_2018_adv <- data_frames[[5]]
VI_G_6_2019_adv <- data_frames[[6]]
VI_G_6_2020_adv <- data_frames[[7]]
VI_G_6_2021_adv <- data_frames[[8]]
VI_G_6_2022_adv <- data_frames[[9]]
~~~

- Mergers each advanced data frame together and plots the values, to show how the SSA's prediction for the average wage index in the year 2050 has changed over time

~~~
# Edits a variable name for year 2017 to allow for merging
colnames(VI_G_6_2017_adv)[2] <- "Averagewage index"

#Combines data frames
combined <- rbind(VI_G_6_2014_adv, VI_G_6_2015_adv, VI_G_6_2016_adv, VI_G_6_2017_adv, VI_G_6_2018_adv, VI_G_6_2019_adv, VI_G_6_2020_adv, VI_G_6_2021_adv, VI_G_6_2022_adv)

combined %>%
  ggplot(aes(x=Report_Year, y=`Averagewage index`, group = 1)) +
  geom_point()+
  geom_line()+
  labs(x = "Report Year", y = "Average Wage Index")+
  labs(title = "Average Wage Index in 2050: Estimations from 2014-2022")+
  theme(plot.title = element_text(hjust = 0.5))

~~~
- The output is as follows:

![Average wage!](https://i.ibb.co/SfTLT6L/Avg-Wage-2050.png)


