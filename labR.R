
library(tidyverse)

library(rvest)
library(dplyr)
library(ggplot2)
library(moments)




###### task 5 



##################5a)

# direct to html to screper 
democracy=read_html("https://en.wikipedia.org/wiki/Democracy_Index")
d_all.tables=html_nodes(democracy, "table")
list_by_country=as.data.frame ( html_table(d_all.tables[6],fill = TRUE))
#View(list_by_country)

#GDP 
gdp=read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita")
all.tables_gdp=html_nodes(gdp, "table")
gdp_table=as.data.frame ( html_table(all.tables_gdp[2],fill = TRUE))
colnames(gdp_table)

#change column name 
colnames(list_by_country)
colnames(gdp_table)
colnames(gdp_table)[1] = "Country"
#colnames(gdp_table)[9] = "delete"
#gdp_table = gdp_table %>% select(-c("delete"))
colnames(gdp_table)
#View(gdp_table)

#clean country column 

#str_trim() : fuction that might also work 
#fix the red dot  
gdp_table$Country <- gsub("\\ ", "", gdp_table$Country)
gdp_table$Country <- str_replace(gdp_table$Country, "\\*$", "")

#View(gdp_table)

#setting dummy list by country df
dumjoined_df_q5 = list_by_country

colnames(gdp_table) <-c("Country", "UN_Region", "IMF_Estimate", "IMF_Year", "World_Bank_Estimate", "World_Bank_Year", "CIA_Estimate", "CIA_Year")

#View(gdp_table)

gdp_table$Country <- str_replace(gdp_table$Country, "Congo", "Republic of the Congo")
gdp_table$Country <- gsub("DR Republic of the Congo", "Democratic Republic of the Congo", gdp_table$Country)

typeof(list_by_country)

# merge to list by country 
joined_df_q5 <- merge(list_by_country,gdp_table,by="Country",all=TRUE)
#View(joined_df_q5)













# Population size

#html and scraper 
ps=read_html("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population")
all.table_ps=html_nodes(ps, "table")
ps_table=as.data.frame ( html_table(all.table_ps[2],fill = TRUE))

#View(ps_table)


#change column name 

colnames(ps_table)[2] = "Country"


#str_trim() : fuction that might also work 
#fix the red dot  
ps_table$Country <- gsub("\\ ", "", ps_table$Country)
ps_table$Country <- str_replace(ps_table$Country, "\\*$", "")

# Setting new coluns name 
colnames(ps_table) <-c("Rank", "Country", "Population_Numbers", "Population_%_of_the_world", "Date_Source", "Source (official or from the United Nations)", "Notes")

# name fixing 
ps_table$Country <- str_replace(ps_table$Country, "Congo", "Republic of the Congo")
ps_table$Country <- gsub("DR Republic of the Congo", "Democratic Republic of the Congo", ps_table$Country)

# Merge 
joined_df_q5 <- merge(joined_df_q5,ps_table,by="Country",all=TRUE)
#View(joined_df_q5)

















# incarnation rates

#html and scraper 
ir=read_html("https://en.wikipedia.org/wiki/List_of_countries_by_incarceration_rate")
all.table_ir=html_nodes(ir, "table")
ir_table=as.data.frame ( html_table(all.table_ir[2],fill = TRUE))
head(ir_table)
#change column name 
colnames(ir_table)[1] = "Country"

#str_trim() : fuction that might also work 
#fix the red dot  
ir_table$Country <- gsub("\\ ", "", ir_table$Country)
ir_table$Country <- str_replace(ir_table$Country, "\\*$", "")
ir_table$Country <- str_replace(ir_table$Country, "\\* \\[Note\\]$", "")


# Setting new coluns name 
print(colnames(ir_table))
colnames(ir_table) <-c("Country", "Region", "Count", "Rate_per_100K", "Male_%", "Female_%", "National_%", "Foreign_%", "Occupancy_%", "Remand_%")

# name fixing 
ir_table$Country <- str_replace(ir_table$Country, "Congo", "Republic of the Congo")
ir_table$Country <- gsub("DR Republic of the Congo", "Democratic Republic of the Congo", ir_table$Country)

#View(ir_table)
# Merge 
joined_df_q5 <- merge(joined_df_q5,ir_table,by="Country",all=TRUE)
#View(joined_df_q5)














#area

#html and scraper 
area=read_html("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area")
all.table_area=html_nodes(area, "table")
area_table=as.data.frame ( html_table(all.table_area[2],fill = TRUE))
head(area_table)

#change column name 
colnames(area_table)[2] = "Country"

#str_trim() : fuction that might also work 
#fix the red dot  
area_table$Country <- gsub("\\ ", "", area_table$Country)
area_table$Country <- str_replace(area_table$Country, "\\*$", "")
area_table$Country <- str_replace(area_table$Country, "\\* \\[Note\\]$", "")

# Setting new coluns name 
print(colnames(area_table))
colnames(area_table) <-c("Rank", "Country", "Count", "Tot_in_sq_KM_(sq_mi)", "Land_in_sq_KM_(sq_mi)", "Water_in_sq_KM_(sq_mi)", "Notes")
# name fixing 
area_table$Country <- str_replace(area_table$Country, "Congo", "Republic of the Congo")
area_table$Country <- gsub("DR Republic of the Congo", "Democratic Republic of the Congo", area_table$Country)
#View(area_table)

# Merge 
joined_df_q5 <- merge(joined_df_q5,area_table,by="Country",all=TRUE)
#View(joined_df_q5)





########


#removing exist territories 

row_numbers <- which(grepl("United Kingdom", joined_df_q5$Country))
print(row_numbers)

row_numbers <- which(grepl("United States", joined_df_q5$Country))
print(row_numbers)


row_numbers <- which(grepl("France", joined_df_q5$Country))
print(row_numbers)

row_numbers <- which(grepl("Norway", joined_df_q5$Country))
print(row_numbers)

row_numbers <- which(grepl("Netherlands", joined_df_q5$Country))
print(row_numbers)

row_numbers <- which(grepl("Australia", joined_df_q5$Country))
print(row_numbers)

row_numbers <- which(grepl("China", joined_df_q5$Country))
print(row_numbers)

row_numbers <- which(grepl("Denmark", joined_df_q5$Country))
print(row_numbers)

row_numbers <- which(grepl("Chile", joined_df_q5$Country))
print(row_numbers)


rows_to_delete <- c(4, 13, 34, 43, 45, 55, 96, 113, 187, 220, 234, 264, 291, 9, 120, 208, 224, 294, 300,
                    61, 102, 105, 106, 196, 233, 238, 241, 308,
                    41, 145, 210, 273, 20, 38, 73, 232, 253, 256, 21, 60, 62,
                    66, 128, 202, 5, 1, 68, 69, 90, 94, 131, 166, 168,77 ,98 ,116 ,152 ,58 ,83)

junk_columns <-c(5,1,68,69,90,94)
joined_df_q5 <- joined_df_q5[-rows_to_delete, ]
joined_df_q5$CIA_Estimate <- as.numeric(gsub(",", "", joined_df_q5$CIA_Estimate))
joined_df_q5 <- joined_df_q5[joined_df_q5$Country != "World", ]

#View(joined_df_q5)



###############5b)

regression <- lm(CIA_Estimate ~ X2022, data = joined_df_q5)
results <- summary(regression)
intercept <- coef(results)[1]
slope <- coef(results)[2]

cat("The intercept is :",intercept)
cat("The slope is : ",slope)

ggplot(joined_df_q5, aes(x = X2022, y = CIA_Estimate)) +
  geom_point() +
  xlab("X2022") +
  ylab("CIA_Estimate") +
  
  # Add the linear regression line
  geom_smooth(method = "lm", se = FALSE)

#we need to describe the results



# Replace non-numeric values in column Rate_per_100K with NA


joined_df_q5$Rate_per_100K[!is.na(joined_df_q5$Rate_per_100K) & !grepl("^-?\\d*\\.?\\d+$", joined_df_q5$Rate_per_100K)] <- NA


regression2 <- lm(Rate_per_100K ~ X2022, data = joined_df_q5)
results2 <- summary(regression2)
intercept2 <- coef(results2)[1]
slope2 <- coef(results2)[2]

cat("The intercept is :",intercept2)
cat("The slope is : ",slope2)

model <- lm(Rate_per_100K ~ X2022, data = joined_df_q5)

# Plot the scatter plot of the data
plot(joined_df_q5$X2022, joined_df_q5$Rate_per_100K, xlab = "X2022", ylab = "Rate_per_100K")

# Add the regression line
abline(model,col="blue")












#task 6 



##################6)



#a)
View(joined_df_q5)
a <- as.numeric( joined_df_q5$CIA_Estimate)
a
plot(ecdf(a))

Prcnt_X <- quantile(joined_df_q5$CIA_Estimate , c(0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
print(Prcnt_X)



#b)

#removing rows with na in "population % column"

na_rows <- which(is.na(joined_df_q5$`Population_%_of_the_world`))
# Print the row numbers
print(na_rows)

new_df <- joined_df_q5[-na_rows, ]
View(new_df)
joined_df_q5$`Population_%_of_the_world` <- gsub("%", "", joined_df_q5$`Population_%_of_the_world`)
new_df$`Population_%_of_the_world` <- gsub("%", "", new_df$`Population_%_of_the_world`)


new_df$`Population_%_of_the_world` <- as.numeric(new_df$`Population_%_of_the_world`)
new_df <- new_df[order(new_df$CIA_Estimate), ]
new_df$Cumulative_Probability <- cumsum(new_df$`Population_%_of_the_world`) / sum(new_df$`Population_%_of_the_world`)
ggplot(new_df, aes(x = CIA_Estimate, y = Cumulative_Probability)) +
  geom_step() +
  xlab("GDP (PPP) per capita") +
  ylab("Cumulative Probability") +
  ggtitle("Empirical CDF for GDP (PPP) per capita") +
  theme_minimal()
print(new_df$CIA_Estimate)

Prcnt_Y <- quantile(new_df$CIA_Estimate, c(0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
print(Prcnt_Y)


#c)


print(joined_df_q5$`Land_in_sq_KM_(sq_mi)`)
#clean NA cells and creating new df 
#na_rows1 <- which(is.na(joined_df_q5$`Land_in_sq_KM_(sq_mi)`))
# Print the row numbers
#print(na_rows1)

dfc <- joined_df_q5



na_rows3 <- which(is.na(joined_df_q5$CIA_Estimate))
# Print the row numbers
print(na_rows3)

dfc <- joined_df_q5[-na_rows3, ]


#clean the "-"

View(dfc)
#clean parenthesis and another regex 
print(dfc$`Land_in_sq_KM_(sq_mi)`)
dfc$`Land_in_sq_KM_(sq_mi)`<- gsub(",", "", dfc$`Land_in_sq_KM_(sq_mi)`)
dfc$`Land_in_sq_KM_(sq_mi)` <- gsub("\\s*\\(\\d+(,\\d+)*\\)", "", dfc$`Land_in_sq_KM_(sq_mi)`)
dfc$`Land_in_sq_KM_(sq_mi)` <- gsub("\\s*\\(.*\\)", "", dfc$`Land_in_sq_KM_(sq_mi)`)
print(dfc$`Land_in_sq_KM_(sq_mi)`)
dfc$`Land_in_sq_KM_(sq_mi)` <- gsub("\\s*-\\s*", "", dfc$`Land_in_sq_KM_(sq_mi)`)
View(dfc)
dfc$`Land_in_sq_KM_(sq_mi)` <- gsub("negligible", "", dfc$`Land_in_sq_KM_(sq_mi)`)

#######
# Replace blank cells with NA
dfc$`Land_in_sq_KM_(sq_mi)`[dfc$`Land_in_sq_KM_(sq_mi)` == ""] <- NA


# Convert to numeric, treating NA strings as NA values
cleaned <- as.numeric(dfc$`Land_in_sq_KM_(sq_mi)`, na.strings = "NA")
print(cleaned)
na_rows1 <- which(is.na(cleaned))
# Print the row numbers
print(na_rows1)

dfc <- dfc[-na_rows1, ]

View(dfc)


# Sort the dataframe by CIA_Estimate
dfc <- dfc[order(dfc$CIA_Estimate), ]
View(dfc)

print(order(dfc$CIA_Estimate))
# Convert the Land_in_sq_KM_(sq_mi) column to numeric
dfc$`Land_in_sq_KM_(sq_mi)`<- as.numeric(dfc$`Land_in_sq_KM_(sq_mi)`)


# Calculate cumulative probability
dfc$Cumulative_Probability <- cumsum(dfc$`Land_in_sq_KM_(sq_mi)`) / sum(dfc$`Land_in_sq_KM_(sq_mi)`)
print( dfc$Cumulative_Probability )

# Calculate cumulative probability
print(dfc$Cumulative_Probability)
# Plot the empirical CDF
ggplot(dfc, aes( x= CIA_Estimate, y = Cumulative_Probability)) +
  geom_step() +
  xlab("CIA Estimate") +
  ylab("Cumulative Probability") +
  ggtitle(" CDF for person in the world by land area ") +
  theme_minimal()

Prcnt_X <- quantile(joined_df_q5$CIA_Estimate , c(0.25, 0.5, 0.75, 0.95), na.rm = TRUE)
print(Prcnt_X)






