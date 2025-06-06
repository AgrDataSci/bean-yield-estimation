# ..................................
# This script fetches the data from ClimMob using the API Client 
# An API key and access to the data (via the share project service)
# is required

# load packages
library("ClimMobTools")

# read file with API key, this is a txt file 
# indicating the key(s) and the server from which the data should 
# be retrieved. Its a matrix with two coluns, the first being the key and the second 
# being the server
key <- read.table("token/api-key.txt", sep = ",")
key <- data.frame(key = key[,1], server = key[, 2])
key <- key[1,]



key <- read.table("token/api-key.txt", sep = ",")
key <- data.frame(key = key[,1], server = key[, 2])
key <- key[1,]



# get the list of projects from the user indicated in the API key
projects <- data.frame()



for(i in seq_along(key$key)) {
  projects <- rbind(projects, 
                    getProjectsCM(key$key[i], server = key$server[i]))
}


# select by userowner
project_id <- c("bea22kilin", "bean22reco")


projects <- projects[projects$project_id %in% project_id, ]

# make a data.frame with project name, userowner, API key and server
# but not save it, to keep the API key confidential
serverdata <- projects[,c("project_id", "user_owner", "server")]
serverdata <- merge(serverdata, key, by = "server")

# fetch the data from climmob
cmdata <- list()

for(i in seq_along(projects$project_id)) {
  
  d_i <- getDataCM(serverdata$key[i], 
                   project = serverdata$project_id[i],
                   userowner = serverdata$user_owner[i],
                   server = serverdata$server[i],
                   as.data.frame = FALSE)
  
  cmdata[[i]] <- d_i
  
}

# save it 
dir.create("processing", showWarnings = FALSE)
save(projects, cmdata, file = "processing/trial-data.rda")

