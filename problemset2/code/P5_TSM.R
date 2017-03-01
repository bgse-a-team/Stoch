# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------
#
# 125006 Stochastic Models and Optimization
#
# (Authors) Daniel Bestard Delgado, Michael Cameron, 
#           Hans-Peter Höllwirth, Akhil Lohia
# (Date)    03.2017

# ----------------------------------------------------------------------
# Loading data
# ----------------------------------------------------------------------

# house cleaning
rm( list=ls() )

# load libraries

# set working directory
interactive <- TRUE
if (interactive) {
    setwd("/Users/Hans-Peter/Documents/Masters/14D006/problemsets/problemset2/code")
} 

# load data
data <- as.data.frame(read.table("../data/TSM_Uruguay.txt", header=FALSE, sep=" "))
N <- nrow(data)

# ----------------------------------------------------------------------
# Compute distance matrix
# ----------------------------------------------------------------------
dm <- matrix(10000, N, N)
for (i in 1:N) {
    for (j in 1:N) {
        if (i != j)
            dm[i,j] <- round(sqrt((data[i,2] - data[j,2])**2 + (data[i,3] - data[j,3])**2),3)
    }
}

# ----------------------------------------------------------------------
# Nearest neighbor algorithm
# ----------------------------------------------------------------------
visited <- rep(0,N)
path <- 0

# starting point
city <- 1
visited[city] <- TRUE

# repeatedly visit nearst not-yet visited neighbor
for (i in 2:N) {
    leg <- min(dm[city, !visited])
    path <- path + leg
    
    potentials <- which(dm[city,] == leg)
    for (j in 1:length(potentials)) {
        if (!visited[potentials[j]])
            city <- potentials[j] 
    }
    visited[city] <- TRUE
}

# return to starting point
path <- path + dm[city,1]


# ----------------------------------------------------------------------
# Insertion Heuristics
# ----------------------------------------------------------------------
visited <- rep(0,N)
pm <- matrix(0,N,N)
path <- 0

# start with initial subtour 
visited[1] <- visited[2] <- visited[3] <- TRUE
pm[1,2] <- pm[1,3] <- pm[2,3] <- 1

while (0 %in% visited){
    # find closest city to subtour
    leg <- min(dm[!!visited, !visited])

    potentials <- which(dm[,] == leg, arr.ind=TRUE)
    for (k in 1:nrow(potentials)) {
        if (!!visited[potentials[k,1]] & !visited[potentials[k,2]]) {
            city <- as.numeric(potentials[k,2])
            visited[city] <- TRUE
        }
    }    

    # find shortest detour to city
    detour <- list(len=1000000, from=0, to=0)
    for (k in 1:N) {
        for (l in k:N) {
            if (pm[k,l] == 1) {
                pot.detour.len <- dm[k,city] + dm[l,city] - dm[k,l]
                if (pot.detour.len < detour$len) {
                    detour$len <- dm[k,city] + dm[l,city] - dm[k,l]
                    detour$from <- k
                    detour$to <- l
                }
            }
        }
    }
    
    # add city to subtour
    pm[ min(detour$from, detour$to), max(detour$from, detour$to) ] <- 0
    pm[ min(detour$from, city), max(detour$from, city) ] <- 1
    pm[ min(detour$to,   city), max(detour$to,   city) ] <- 1
    path <- path + detour$len
}


