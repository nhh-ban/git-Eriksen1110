# BAN 400

# Loads libraries
library(tidyverse)
library(ggplot2)

## Problem 2
# Read the data
data_raw <- readLines("suites_dw_Table1.txt")

separator_row_number <- which(grepl("-----", data_raw))

header <- data_raw[separator_row_number-1] %>% 
  strsplit(split = "\\|") %>%  #Splits the string
  unlist() %>% #Unlist the Char objcet
  str_trim() # Trims the whitespaces

var_descriptions <- data_raw[1:(separator_row_number - 2)]

# Read the data into a data frame, skipping the lines before the separator and the separator itself
galaxies_df <- read_delim(file = "suites_dw_Table1.txt",
                          delim = "|",
                          skip = separator_row_number,
                          col_names = header)
                            
print(galaxies_df, n=10)

galaxies_df <- galaxies_df %>% 
  mutate(across(everything(), str_trim))
  

# Convert the a_26 column to numeric
galaxies_df$a_26 <- as.numeric(as.character(galaxies_df$a_26))

galaxies_df %>% 
  ggplot(aes(x=a_26)) +
  geom_histogram(binwidth = 0.5, fill="lightgreen", color="black", alpha=0.7) +
  labs(title="Distribution of Galaxy Diameters",
       x="Diameter (a_26) in kpc",
       y="Number of Galaxies") +
  theme_minimal() +
  theme(panel.grid.minor = element_line(color = "gray", linetype = "dotted"))



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
