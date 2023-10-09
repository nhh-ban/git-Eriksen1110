# BAN 400
# Assignment Git

# Loads libraries
library(tidyverse)

## Problem 2
# Read the data
data_source <- "http://www.sao.ru/lv/lvgdb/article/suites_dw_Table1.txt"
data_raw <- readLines(data_source)

# Inspect the data
data_raw

## Search for a pattern of "-----" in the data
## grepl return true if a element contains "-----", false otherwise
## which return the indices for the elements thats returned true 
separator_row_number <- which(grepl("-----", data_raw))


# Gets the header for the different columns
header <- data_raw[separator_row_number-1] %>% 
  strsplit(split = "\\|") %>%  #Splits the string
  unlist() %>% #Unlist the Char object
  str_trim() # Trims the whitespaces

# Saves the variable description
var_descriptions <- data_raw[1:(separator_row_number - 2)]

## Reads the data into a data frame
galaxies_df <- read_delim(file = "suites_dw_Table1.txt",
                          delim = "|", ## Using "|" as the delimeter
                          skip = separator_row_number, ## Skip the x numers of rows, means starting the row under the "-----"
                          col_names = header) ## Add a header to the datafram

# Prints the 10 first rows of the data frame                            
print(galaxies_df, n=10)

# Trims all the columns for unnecessary white spaces
galaxies_df <- galaxies_df %>% 
  mutate(across(everything(), str_trim))
  


### Problem 3

# Convert the a_26 column to numeric
galaxies_df$a_26 <- as.numeric(as.character(galaxies_df$a_26))

galaxies_df %>%
  filter(D <= 11) %>% 
  ggplot(aes(x=a_26)) +
  geom_histogram(bins= 50, fill="lightgreen", color="black", alpha=0.7) +
  labs(title="Distribution of Galaxy Diameters",
       x="Diameter (a_26) in kpc",
       y="Number of Galaxies") +
  theme_minimal()

# From the plot we have a peak for small object, and thus
# can't conlude that small objects are underrepresented. 

### Problem 4

raw <- readLines("UCNG_Table4.txt")
df <- read_delim(file = "UCNG_Table4.txt",
                 delim = "|",
                 skip = 2,
                 col_names = str_trim(unlist(strsplit(raw[1], split="\\|"))))

# Remove leading and trailing whitespaces
df <- df %>%
  mutate(across(everything(), str_trim))

# Convert 'cz' and 'error' columns to numeric
df$cz <- as.numeric(df$cz)
df$error <- as.numeric(df$error)

df

df_merges <- merge(df, galaxies_df)
df_merges$D <- as.numeric(df_merges$D)

df_merges %>% 
  ggplot(aes(x=cz, y=D, na.rm=T)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", se=F) + 
  xlab("Velocity of eahc galax (CZ)") + 
  ylab("Distnace from us (D in MPc)") + 
  ggtitle("Velocity of each galax compared its distance from us") +
  theme_minimal()

## Did not conclude anything on this task
