# Find total run time

# Get list of time.txt file paths
files <- list.files(path = "outputs", pattern = "time.txt", full.names = TRUE, recursive = TRUE)

# Get capacity from the folder name
capacity <- sapply(strsplit(files, split="_", fixed=TRUE), function(x) (x[2]))

# Import content of each file
txt <- lapply(files, scan, what = "character")

# Extract times and convert to numeric
times <- as.numeric(unlist(lapply(txt, `[[`, 1)))

# Find total time in hours
total_h <- sum(times)
print(total_h)

# Get time of the scenario with 20 bed capacity
idx = match("20", capacity)
print(times[idx])
