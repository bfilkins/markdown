# Author: Brendan Filkins
# Date: 4/12/2022

# Load Packages
library(pdftools)
library(dplyr)
library(ggplot2)
library(stringr)

# Define function for replacing non-alpha numeric characters
remove_non_alpha <- function(x,y) {gsub("[^[:alnum:]]", paste(y), str_trim(x))}

# Read data from pdf
pdf_text_data <- pdf_text("CV/Brendan_CV.pdf") 

# Save plain text for submission
fileConn<-file("Brendan_resume.txt")
writeLines(pdf_text_data, fileConn)
close(fileConn)

# Transform Data
data <- tibble(letter = unlist(strsplit(pdf_text_data,split = ""))) %>%
  mutate(
    clean_letter = str_to_lower(remove_non_alpha(letter,""))
    ) %>%
  filter(
    is.na(as.numeric(clean_letter)),
    clean_letter != "") %>%
  group_by(clean_letter) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(percent_of_total = n/sum(n))

# Create plot
plot <- data %>%
  ggplot(aes(x = clean_letter, y = n)) +
  geom_bar(stat = "identity", fill = "#3480eb") +
  xlab("character") +
  ylab("frequency")+
  coord_flip() +
  theme_minimal() +
  ggtitle("Brendan Filkins' resume histogram")

plot
# Save plot
ggsave("brendan_resume_letters.jpg")


