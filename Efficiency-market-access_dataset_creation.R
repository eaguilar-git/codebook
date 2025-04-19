#+ This is the first file for my DEA analysis. Here I reshape the utilities data and then I run DEA.
#+ Version: April 19, 2025


# Installing libraries that I might use in the assingment ------
# Data wrangling and reshaping
library(tidyverse)   # includes dplyr, tidyr, readr, ggplot2, etc.
library(readxl)      # to read Excel files
library(tidyr)       # especially for pivot_longer and pivot_wider
library(dplyr)       # for filtering, grouping, mutating, etc.

install.packages("writexl")
library(writexl)

# Optional (for output/summary)
library(knitr)       # to print clean tables
library(kableExtra)  # to style tables for markdown or HTML output

# Data envelopment analysis
install.packages("Benchmarking")
library(Benchmarking)


# Loading file with data -----
df <- read_excel("complete_upbeat.xlsx")
names(df)

## Printing unique list of indicators
unique_indicators <- df %>% 
  pull(Indicators) %>% 
  unique()

cat("Unique values in 'Indicators' column:\n")
print(unique_indicators)


# Creating a new DF and datafile for running DEA -----
# 1. Define your selected indicators
selected_indicators <- c(
  "Number of employees",
  "Total Assets_USDvalue(m)",
  "Net Profit Margin",
  "Customers",
  "Distribution Losses",
  "Operating and debt service cost recovery (billed revenue), excluding subsidies (%)"
)

# 2. Aggregate & pivot to wide format (mean over years)
agg_df <- df %>%
  filter(Indicators %in% selected_indicators) %>%
  group_by(
    `Utility Name`, Utilities, Country, Region,
    `Income group`, `Lending category`, Ownership, Type,
    Indicators
  ) %>%
  summarize(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = Indicators,
    values_from = mean_value
  )

# 3. Keep only identifier cols + those selected indicators present
utility_cols <- c(
  "Utility Name", "Utilities", "Country", "Region",
  "Income group", "Lending category", "Ownership", "Type"
)
cols_to_keep <- c(utility_cols, intersect(selected_indicators, names(agg_df)))
final_df <- agg_df %>% select(all_of(cols_to_keep))

# 4. Drop rows missing any of these required columns
required_cols <- c(
  "Number of employees",
  "Total Assets_USDvalue(m)",
  "Net Profit Margin",
  "Customers",
  "Distribution Losses"
)
final_df <- final_df %>% drop_na(all_of(required_cols))

# 5. Create Transformed Distribution Losses = 1 - Distribution Losses
if ("Distribution Losses" %in% names(final_df)) {
  final_df <- final_df %>%
    mutate(`Transformed Distribution Losses` = 1 - `Distribution Losses`)
}

# 6. Reorder columns:
#    identifiers → Number of employees → Total Assets… → Net Profit Margin → Customers → Distribution Losses → Transformed Distribution Losses → Recovery (end)
desired_order <- c(
  utility_cols,
  "Number of employees",
  "Total Assets_USDvalue(m)",
  "Net Profit Margin",
  "Customers",
  "Distribution Losses",
  "Transformed Distribution Losses",
  "Operating and debt service cost recovery (billed revenue), excluding subsidies (%)"
)
existing_order <- intersect(desired_order, names(final_df))
final_df <- final_df %>% select(all_of(existing_order))

# 7. Inspect first rows
print(head(final_df))

# 8. Save to Excel in your working directory
library(writexl)
write_xlsx(final_df, "data_for_DEA.xlsx")
message("Saved data_for_DEA.xlsx to: ", getwd())


# Running DEA ------
# 1. Load your DEA‐ready data
df <- read_excel("data_for_DEA.xlsx")

# 2. Make sure the DEA inputs/outputs are numeric
num_cols <- c(
  "Number of employees",
  "Total Assets_USDvalue(m)",
  "Net Profit Margin",
  "Customers",
  "Transformed Distribution Losses"
)
df <- df %>%
  mutate(across(all_of(num_cols), as.numeric))

# 3. Define which columns are inputs and outputs
input_columns  <- c("Number of employees", "Total Assets_USDvalue(m)")
output_columns <- c("Net Profit Margin", "Customers", "Transformed Distribution Losses")

# 4. Build matrices for the DEA function
X <- as.matrix(df %>% select(all_of(input_columns)))
Y <- as.matrix(df %>% select(all_of(output_columns)))

# 5. Run an input‐oriented CCR (CRS) DEA model
dea_result <- dea(
  X           = X,
  Y           = Y,
  RTS         = "crs",      # constant returns to scale
  ORIENTATION = "in"        # input‐oriented
)

# 6. Extract efficiency scores and bind back to your data.frame
df$Efficiency <- dea_result$eff

# 7. (Optional) Preview
print(head(df %>% select(`Utility Name`, Efficiency)))

# 8. Save the results to a new Excel file
write_xlsx(df, "DEA_Results.xlsx")
message("DEA results saved to: ", file.path(getwd(), "DEA_Results.xlsx"))


# Preparing DEA and market structure data sets to combine them ----
## First I am making sure that the names are the same
# 1. Read in the DEA results and market structure data
dea_results  <- read_excel("DEA_Results.xlsx")
market_data  <- read_excel("market_structure_data.xlsx")

# 2. Filter market structure data for the year 2012
market_data_2012 <- market_data %>%
  filter(Year == 2012)

# 3. Extract unique country/economy names
dea_countries  <- dea_results %>%
  pull(Country) %>%
  na.omit() %>%
  unique()

economy_names  <- market_data_2012 %>%
  pull(`Economy Name`) %>%
  na.omit() %>%
  unique()

# 4. Check presence and report
all_present <- all(dea_countries %in% economy_names)
cat(
  "Are all 'Country' values from DEA results present in\n",
  "  'Economy Name' from market structure data (2012)? ", all_present, "\n",
  sep = ""
)

if (!all_present) {
  missing_countries <- setdiff(dea_countries, economy_names)
  cat(
    "The following countries are missing in the market structure data:\n",
    paste(missing_countries, collapse = ", "), "\n"
  )
}

## Change this names in DEA Results file:  Côte d’Ivoire to Ivory Coast, Micronesia, Fed. Sts. to Federated States of Micronesia, Kyrgyz Republic to Kyrgyzstan, Congo, Dem. Rep. to Democratic Republic of the Congo, St. Lucia to Saint Lucia
# 1. Read in your DEA results
dea_results <- read_excel("DEA_Results.xlsx")

# 2. Recode the country names
dea_results <- dea_results %>%
  mutate(Country = recode(Country,
                          "Côte d’Ivoire"           = "Ivory Coast",
                          "Micronesia, Fed. Sts."   = "Federated States of Micronesia",
                          "Kyrgyz Republic"         = "Kyrgyzstan",
                          "Congo, Dem. Rep."        = "Democratic Republic of the Congo",
                          "St. Lucia"               = "Saint Lucia"
  ))

# 3. Save to a new file (or overwrite)
write_xlsx(dea_results, "DEA_Results_cleaned.xlsx")
message("Saved cleaned file as DEA_Results_cleaned.xlsx in: ", getwd())

## Check that country names from DEA are in market structure
# 1. Read in the cleaned DEA results and the market-structure data
dea_cleaned   <- read_excel("DEA_Results_cleaned.xlsx")
market_data   <- read_excel("market_structure_data.xlsx")

# 2. Extract unique Country and Economy Name values
dea_countries <- dea_cleaned %>%
  pull(Country) %>%
  na.omit() %>%
  unique()

economy_names <- market_data %>%
  pull(`Economy Name`) %>%
  na.omit() %>%
  unique()

# 3. Check if all DEA countries appear in the Economy Name column
all_present <- all(dea_countries %in% economy_names)

cat(
  "All 'Country' values from DEA_Results_cleaned present in\n",
  "  'Economy Name' of market_structure_data? ", all_present, "\n",
  sep = ""
)

# 4. If any are missing, list them
if (!all_present) {
  missing <- setdiff(dea_countries, economy_names)
  cat(
    "Missing countries in market_structure_data:\n",
    paste(missing, collapse = ", "), "\n"
  )
}

# Combining DEA and market structure datasets -----
# 1. Load DEA results and add Year = 2012
df_dea <- read_excel("DEA_Results_cleaned.xlsx") %>%
  mutate(Year = 2012)

# 2. Load market structure data, filter for 2012, rename & select
df_combined <- read_excel("market_structure_data.xlsx") %>%
  filter(Year == 2012) %>%
  rename(Country = `Economy Name`) %>%
  select(
    Country, Year,
    `Simplified Market Structure Type`,
    Regulator,
    `Private IPP Established`,
    Unbundling,
    `Out of VIU`
  )

# 3. Merge DEA results with 2012 policy indicators
df_merged <- left_join(df_dea, df_combined, by = c("Country", "Year"))

# 4. Reorder columns
final_columns <- c(
  "Utility Name", "Utilities", "Country", "Region", "Income group", "Lending category", "Ownership", "Type",
  "Number of employees", "Total Assets_USDvalue(m)", "Net Profit Margin", "Customers", "Distribution Losses",
  "Transformed Distribution Losses",
  "Operating and debt service cost recovery (billed revenue), excluding subsidies (%)",
  "Efficiency", "Simplified Market Structure Type", "Regulator",
  "Private IPP Established", "Unbundling", "Out of VIU"
)
existing_cols <- final_columns[final_columns %in% names(df_merged)]
df_merged <- df_merged %>% select(all_of(existing_cols))

# 5. Save the merged file
write_xlsx(df_merged, "DEA_Results_Market_Structure.xlsx")
message("Updated file saved as: ", file.path(getwd(), "DEA_Results_Market_Structure.xlsx"))


# Preparing DEA-market structure file to combine with access --------
# 1. Read in the merged DEA + market structure results
df_dea_market <- read_excel("DEA_Results_Market_Structure.xlsx")

# 2. Read in your access data
access_data <- read_excel("access_data.xlsx")

# 3. Extract unique Country values from DEA file and unique Country Name from access data
dea_countries   <- df_dea_market %>%
  pull(Country) %>%
  na.omit() %>%
  unique()

access_countries <- access_data %>%
  pull(`Country Name`) %>%
  na.omit() %>%
  unique()

# 4. Check which DEA countries are not in the access data
missing_countries <- setdiff(dea_countries, access_countries)

# 5. Report results
if (length(missing_countries) == 0) {
  message("All DEA 'Country' values are present in the 'Country Name' column of access_data.xlsx.")
} else {
  cat(
    "The following countries from DEA_Results_Market_Structure.xlsx are NOT\n",
    "found in the 'Country Name' column of access_data.xlsx:\n",
    paste(missing_countries, collapse = ", "), "\n",
    sep = ""
  )
}

# In access file, replace following names: Cote d'Ivoire to Ivory Coast, Micronesia, Fed. Sts. to Federated States of Micronesia, Kyrgyz Republic to Kyrgyzstan, Congo, Dem. Rep. to Democratic Republic of the Congo, St. Lucia to Saint Lucia
# 1. Read in your access data
access_data <- read_excel("access_data.xlsx")

# 2. Recode the country names in the "Country Name" column
access_data_cleaned <- access_data %>%
  mutate(`Country Name` = recode(
    `Country Name`,
    "Cote d'Ivoire"         = "Ivory Coast",
    "Micronesia, Fed. Sts." = "Federated States of Micronesia",
    "Kyrgyz Republic"       = "Kyrgyzstan",
    "Congo, Dem. Rep."      = "Democratic Republic of the Congo",
    "St. Lucia"             = "Saint Lucia"
  ))

# 3. (Optional) Inspect the recoded names
print(unique(access_data_cleaned$`Country Name`))

# 4. Save the cleaned data to a new Excel file
write_xlsx(access_data_cleaned, "access_data_cleaned.xlsx")
message("Saved cleaned access data as: ", file.path(getwd(), "access_data_cleaned.xlsx"))

# Final check that all country names in dea results are in access
# 1. Read in the DEA + market structure results
df_dea_market <- read_excel("DEA_Results_Market_Structure.xlsx")

# 2. Read in the cleaned access data
access_data_cleaned <- read_excel("access_data_cleaned.xlsx")

# 3. Extract unique country names
dea_countries    <- df_dea_market %>%
  pull(Country) %>%
  na.omit() %>%
  unique()

access_countries <- access_data_cleaned %>%
  pull(`Country Name`) %>%
  na.omit() %>%
  unique()

# 4. Identify any missing countries
missing_countries <- setdiff(dea_countries, access_countries)

# 5. Report
if (length(missing_countries) == 0) {
  message("All countries in DEA_Results_Market_Structure.xlsx are present in access_data_cleaned.xlsx.")
} else {
  cat(
    "The following countries from DEA_Results_Market_Structure.xlsx\n",
    "are NOT found in access_data_cleaned.xlsx:\n",
    paste(missing_countries, collapse = ", "), "\n",
    sep = ""
  )
}

# Comibining DEA-market structure file with accesss file ------
# 1. Read in the DEA + market structure results
dea_df <- read_excel("DEA_Results_Market_Structure.xlsx")

# 2. Read in the cleaned access data
access_df <- read_excel("access_data_cleaned.xlsx")

# 3. Merge on Country / Country Name and bring in the 2022 access value
df_with_access <- dea_df %>%
  left_join(
    access_df %>% select(`Country Name`, `2022`),
    by = c("Country" = "Country Name")
  ) %>%
  rename(`Access 2022` = `2022`)

# 4. (Optional) Inspect the new column
print(head(df_with_access %>% select(Country, `Access 2022`)))

# 5. Save to a new file
write_xlsx(df_with_access, "DEA_Results_Market_Structure_with_Access.xlsx")
message("Saved DEA_Results_Market_Structure_with_Access.xlsx to ", getwd())


# Final data set -----
## In file "DEA_Results_Market_Structure_with_Access.xlsx", I want to change column names: Utility Name to Utility Long Name, Utilities to Utility Short Name, Total Assets_USDvalue(m) to Total Assets, Operating and debt service cost recovery (billed revenue), excluding subsidies (%) to Operating and Debt Service Cost Recovery, Efficiency to Efficiency Score, and Simplified Market Structure Type to Market Structure. Save new file as Efficiency_Markets_Access.xlsx"

# 1. Read in the existing combined file
df <- read_excel("DEA_Results_Market_Structure_with_Access.xlsx")

# 2. Rename columns and drop 'Lending category'
df_renamed <- df %>%
  rename(
    `Utility Long Name`                         = `Utility Name`,
    `Utility Short Name`                        = Utilities,
    `Total Assets`                              = `Total Assets_USDvalue(m)`,
    `Operating and Debt Service Cost Recovery`  = `Operating and debt service cost recovery (billed revenue), excluding subsidies (%)`,
    `Efficiency Score`                          = Efficiency,
    `Market Structure`                          = `Simplified Market Structure Type`,
    `Income Group`                              = `Income group`,
    `Employees`                                 = `Number of employees`
  ) %>%
  select(-`Lending category`)

# 3. Move Ownership and Type to after Utility Short Name
cols <- names(df_renamed)
# Remove Ownership and Type
cols_no <- cols[!cols %in% c("Ownership", "Type")]
# Position after Utility Short Name
pos <- match("Utility Short Name", cols_no)
# Build new order
new_order <- append(cols_no, values = c("Ownership", "Type"), after = pos)
# Reorder the data.frame
df_final <- df_renamed %>% select(all_of(new_order))

# 4. Save as CSV
write_csv(df_final, "Efficiency_Markets_Access.csv")
message("Saved Efficiency_Markets_Access.csv to: ", getwd())