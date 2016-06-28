
data_dir <- "data"
file_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zipfile <- "Coursera-SwiftKey.zip"
file_path <- paste(data_dir, zipfile, sep = "/")

if(!file.exists(file_path)) {
        dir.create(data_dir)
        download.file(file_url, destfile = file_path, method = "curl")
        unzip(file_path, data_dir)
}