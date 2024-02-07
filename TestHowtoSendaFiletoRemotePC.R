my_data <- data.frame(
  Name = c("John", "Jane", "Bob"),
  Age = c(25, 30, 22),
  Score = c(90, 85, 95)
)

# Save the data to an RDS file in memory
rds_file <- tempfile(fileext = ".rds")
saveRDS(my_data, file = rds_file)

remote_address <- "192.168.0.26"
remote_folder <- "E:\University of Glasgow\Thesis\Examples"
remote_username <- "Jorge"

# Transfer the file to the remote Windows desktop using scp
# Replace "username", "hostname", and "path_to_remote_folder" with your information
remote_command <- sprintf(
  'scp "%s" jorge@192.168.0.26:"E:\"',
  rds_file
)

# Run the system command to transfer the file
system(remote_command)

system(sprintf('scp "%s" %s@%s:"%s/"',rds_file,remote_username,remote_address,remote_folder))
# Clean up the local temporary file
unlink(rds_file)
