library(jsonlite)
library(tidyverse)
library(httr)
library(purrr)
library(stringr)

json_string <- read_json('./gename_price.json')


df <- data.frame(name = names(json_string), price = unlist(json_string),stringsAsFactors = FALSE)
df <- df[!(df$name %in% c("%LAST_UPDATE%", "%LAST_UPDATE_F%")), ]
df$price <- as.numeric(df$price)
rownames(df) <- NULL

json_string <- read_json('./geid_price.json')


df1 <- data.frame(id = names(json_string), price = unlist(json_string),stringsAsFactors = FALSE)
df1 <- df1[!(df1$id %in% c("%LAST_UPDATE%", "%LAST_UPDATE_F%")), ]
df1$price <- as.numeric(df1$price)
rownames(df1) <- NULL

json_string <- read_json('./gename_id_map.json')

df_map <- data.frame(name = names(json_string), id = unlist(json_string),stringsAsFactors = FALSE)
df_map <- df_map[!(df_map$name %in% c("%LAST_UPDATE%", "%LAST_UPDATE_F%")), ]
rownames(df_map) <- NULL


df_combined <- merge(df, df_map, by = "name")

df_combined <- merge(df_combined, df1)

head(df_combined)

rm(df, df1, df_map)

top_200_items <- df_combined %>%
  slice_max(order_by = price, n = 200)

get_item_with_retry <- function(item_id, max_attempts = 3) {
  url <- paste0("https://secure.runescape.com/m=itemdb_rs/api/catalogue/detail.json?item=", item_id)
  attempt <- 1
  
  while (attempt <= max_attempts) {
    # tryCatch handles the 'premature EOF' error without crashing the script
    result <- tryCatch({
      response <- GET(url, timeout(10)) # Added a 10s timeout
      
      if (status_code(response) == 200) {
        content <- fromJSON(content(response, "text", encoding = "UTF-8"))
        return(content$item)
      } else if (status_code(response) == 429) {
        message("Rate limited (429). Sleeping longer...")
        Sys.sleep(5)
      }
      NULL
    }, error = function(e) {
      message(paste("Attempt", attempt, "failed for ID", item_id, ":", e$message))
      return(NULL)
    })
    
    if (!is.null(result)) return(result)
    
    # If we failed, wait and try again
    attempt <- attempt + 1
    Sys.sleep(2) # Wait 2 seconds before retrying
  }
  
  return(NULL)
}

# Now run the loop
item_list_results <- list()

for (id in top_200_items$id) {
  message(paste("Fetching:", id))
  item_list_results[[as.character(id)]] <- get_item_with_retry(id)
  Sys.sleep(0.8) # Slowed down slightly for stability
}

saveRDS(item_list_results, 'item_list_results.rds')

str(item_list_results[1], max.level = 3)

listviewer::jsonedit(item_list_results[1:3])

purrr::map_chr(item_list_results[1:5], "name")


extract_item_data <- function(item) {
  if (is.null(item)) return(NULL)
  
  data.frame(
    id = item$id,
    name = item$name,
    description = item$description,
    day30_change = item$day30$change,
    day90_change = item$day90$change,
    day180_change = item$day180$change,
    stringsAsFactors = FALSE
  )
}

item_changes_df <- map_df(item_list_results, extract_item_data)

item_changes_df <- item_changes_df %>%
  mutate(across(ends_with("_change"), ~ as.numeric(str_replace(.x, "%", ""))))

# View the results
head(item_changes_df)

saveRDS(item_changes_df, 'item_changes_df.rds')


top_200_items$id <- as.numeric(top_200_items$id)
top_200_items <- top_200_items %>%
  inner_join(item_changes_df, by = c("id", "name"))

head(top_200_items)


top_200_items <- top_200_items %>%
  relocate(name, price, day30_change, day90_change, day180_change, id, description)

saveRDS(top_200_items, 'top_200_items.rds')


# Calculate the mean and median change for each period
market_summary <- top_200_items %>%
  summarise(
    avg_30d  = mean(day30_change, na.rm = TRUE),
    avg_90d  = mean(day90_change, na.rm = TRUE),
    avg_180d = mean(day180_change, na.rm = TRUE),
    # Median is often better to avoid being skewed by one massive outlier
    med_30d  = median(day30_change, na.rm = TRUE),
    med_90d  = median(day90_change, na.rm = TRUE),
    med_180d = median(day180_change, na.rm = TRUE)
  )

print(market_summary)

# Calculate weighted average (Price * Change / Total Market Cap of Top 200)
total_value <- sum(top_200_items$price, na.rm = TRUE)

weighted_inflation <- top_200_items %>%
  summarise(
    weighted_30d  = sum(price * day30_change, na.rm = TRUE) / total_value,
    weighted_90d  = sum(price * day90_change, na.rm = TRUE) / total_value,
    weighted_180d = sum(price * day180_change, na.rm = TRUE) / total_value
  )

print(weighted_inflation)

