



# -------------------------------
# 1. Load required libraries
# -------------------------------
library(officer)
library(tidyverse)
library(readr)
library(stringr)

# -------------------------------
# 2. Set folder path
folder_path <- "C:/Users/Asus/Downloads/R for BAP/LexisNexis_Economic Sentiment"

# Case-insensitive pattern match for .docx and .DOCX
files <- list.files(folder_path, pattern = "(?i)\\.docx$", full.names = TRUE)
cat("🔍 Found", length(files), "Word files.\n")


# -------------------------------
# 3. Define article extraction function with print
# -------------------------------
extract_articles <- function(file_path) {
  cat("📄 Reading:", basename(file_path), "\n")  # log file being processed
  doc <- read_docx(file_path)
  text <- docx_summary(doc) %>%
    filter(content_type == "paragraph") %>%
    pull(text) %>%
    str_trim() %>%
    discard(~ .x == "")
  
  if (length(text) == 0) return(NULL)
  
  tibble(
    file = basename(file_path),
    full_text = paste(text, collapse = "\n")
  )
}

# -------------------------------
# 4. STEP 1: Test on a few files
# -------------------------------
sample_files <- files[1:min(5, length(files))]
cat("🚀 Testing on", length(sample_files), "files...\n")

pb_sample <- progress_estimated(length(sample_files))

sample_articles <- map_dfr(sample_files, ~{
  pb_sample$tick()$print()
  extract_articles(.x)
})

write_csv(sample_articles, "sample_articles_LexisNexis.csv")
cat("✅ Saved sample_articles_LexisNexis.csv with", nrow(sample_articles), "rows.\n")

# -------------------------------
# 5. STEP 2: Full aggregation
# -------------------------------
cat("📦 Starting full aggregation on", length(files), "files...\n")

pb_all <- progress_estimated(length(files))

all_articles <- map_dfr(files, ~{
  pb_all$tick()$print()
  extract_articles(.x)
})

write_csv(all_articles, "all_articles_LexisNexis.csv")
cat("✅ Saved all_articles_LexisNexis.csv with", nrow(all_articles), "rows.\n")


# Load required packages
library(tidyverse)
library(stringr)
library(readr)

# Step 1: Load the dataset
articles <- read_csv("all_articles_LexisNexis.csv")

# Step 2: Basic diagnostics (optional but helpful)
cat("Initial rows:", nrow(articles), "\n")
glimpse(articles)

# Step 3: Clean the data
cleaned_articles <- articles %>%
  # Remove duplicates
  distinct() %>%
  # Remove rows with missing or empty body
  filter(!is.na(full_text), str_trim(full_text) != "") %>%
  # Remove rows with extremely short body texts (e.g., < 100 characters)
  filter(str_length(full_text) > 100) %>%
  # Clean up whitespace, line breaks, multiple spaces
  mutate(
    full_text = str_replace_all(full_text, "\\s+", " "),
    full_text = str_trim(full_text)
  )

# Optional: print how many were removed
cat("Remaining rows after cleaning:", nrow(cleaned_articles), "\n")

# Step 4: Save cleaned data
write_csv(cleaned_articles, "cleaned_articles_LLM_ready.csv")
cat("✅ Saved cleaned CSV for LLM analysis.\n")




