# ============================================================
#  TripCraft — Quick Demo
# ============================================================

library(TripCraft)

# ---- 1. Load Data ------------------------------------------
df <- tc_load()
cat("Loaded:", nrow(df), "attractions across", length(unique(df$city)), "cities\n\n")

# ---- 2. Itinerary Generator --------------------------------
filtered  <- tc_filter(df, "Delhi")
clustered <- tc_cluster(filtered, k = 3)
itin      <- tc_itinerary(clustered, days = 3)

required_cols <- c("day", "attraction_name", "vibe", "avg_duration_hrs", "arrive_time")

cat("===== ITINERARY: DELHI (3 Days) =====\n")
for (i in seq_along(itin)) {
  day_df <- itin[[i]][, required_cols, drop = FALSE]
  cat(sprintf("\n-- Day %d: %s --\n", i, itin[[i]]$day_label[1]))
  print(day_df, row.names = FALSE)
}


# ---- 3. Budget Breakdown -----------------------------------
budget <- tc_budget(itin)

print("Budget:")
print(budget$total)

stopifnot(budget$total >= 0)
print("Budget breakdown:")
print(budget)

stopifnot(budget$total >= 0)
# ---- 4. Shiny App ------------------------------------------


app <- tc_run()

print("Shiny app object:")
print(app)

