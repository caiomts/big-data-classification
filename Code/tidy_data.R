### Libraries
library(data.table)
library(tidyverse)
library(R.utils)


### Read Data
column_names = read.csv("..\\Data\\names.csv", header=FALSE) # Column names!
dt = fread("..\\Data\\census-income.data.gz", col.names=column_names[,2])
df = as_tibble(dt)

### Tidying data
df = df %>% 
  unique() %>% # Dropping duplicates
  mutate(AHRSPAY = AHRSPAY/100) # wage per hour in unit of dollars

df[df == '?'] = NA # Replace "?" by NA








