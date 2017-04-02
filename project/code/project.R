# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------
#
# 14D006 Stochastic Models and Optimization
#
# (Authors) Daniel Bestard Delgado, Michael Cameron, 
#           Hans-Peter Höllwirth, Akhil Lohia
# (Date)    03.2017

# ----------------------------------------------------------------------
# Initialize starting position
# ----------------------------------------------------------------------
initial.condition <- function() {
    down <- runif(1)
    if (down < 0.35) {
        d <- 4
        x <- sample(1:100)[1]
        y <- sample(1:x)[1]
    } else if (down < 0.65) {
        d <- 3
        x <- sample(1:100)[1]
        y <- sample(1:x)[1]
    } else if (down < 0.90) {
        d <- 2
        c <- runif(1)
        if (c < 0.25)
            x <- sample(1:50)[1]
        else
            x <- sample(51:100)[1]
        y <- sample(1:x)[1]
    } else {
        d <- 1
        c <- runif(1)
        if (c < 0.25)
            x <- sample(1:75)[1]
        else
            x <- sample(76:100)[1]
        y <- min(x,10)
    } 
    return(list(d=d,x=x,y=y))
}

# ----------------------------------------------------------------------
# Simulate drive with given policy
# ----------------------------------------------------------------------
simulate.drive <- function(mu) {
    t <- 1
    i <- list()
    r <- 0
    i[[t]] <- initial.condition()
    
    terminated <- FALSE
    while(!terminated) {
        u <- mu[[i[[t]]$d]][i[[t]]$x, i[[t]]$y] # policy action
        #print(u)
        outcome <- runif(1)
        
        # PASS attempt
        if (u == 'P') {
            if (outcome < 0.05) { # interception
                terminated <- TRUE
                x <- i[[t]]$x - rpois(1,12) + 2
                if (x > 100) { # opponent's touchdown
                    x <- 0
                    r <- -6.8
                }
            } else if (outcome < 0.50) { # pass incomplete
                x <- i[[t]]$x
            } else if (outcome < 0.55) { # sacked
                x <- i[[t]]$x + rpois(1,6)
                if (x > 100) { # safety
                    terminated <- TRUE
                    x <- 20
                    r <- -2.0 
                }
            } else { # pass complete
                x <- i[[t]]$x - rpois(1,12) + 2
                if (x > 100) { # safety
                    terminated <- TRUE
                    x <- 20
                    r <- -2.0 
                }
            }
        }
        
        # RUN attempt
        if (u == 'R') {
            if (outcome < 0.05) { # fumble
                terminated <- TRUE
                x <- i[[t]]$x
                if (x > 100) { # opponent's touchdown
                    x <- 0
                    r <- -6.8
                } 
            } else { # run
                x <- i[[t]]$x - rpois(1,6) + 2
                if (x > 100) { # safety
                    terminated <- TRUE
                    x <- 20
                    r <- -2.0 
                }
            }
        }  
        
        # PUNT attempt
        if (u == 'U') {
            terminated <- TRUE
            x <- i[[t]]$x - 6*rpois(1,10) + 6
            if (x < 0) x <- 20
        }   
        
        # KICK attempt
        if (u == 'K') {
            terminated <- TRUE
            if (outcome < max(0, 0.95 - 0.95*i[[t]]$x/60)) { # successful field goal
                x <- 20
                r <- 3
            } else { # missed field goal
                x <- i[[t]]$x
            }
        }  
        
        if (x <= 0) { # touchdown
            terminated <- TRUE
            x <- 20 
            r <- 6.8
        }
        
        # increase down
        d <- i[[t]]$d + 1 
        
        # calculate new distance to first down
        y <- i[[t]]$y + x - i[[t]]$x
        if (y <= 0) { # new first down
            y <- min(x,10)
            d <- 1
        } 
        if (d > 4) terminated <- TRUE 

        if (!terminated) {
            t <- t + 1
            i[[t]] <- list(d=d, x=x, y=y)
        }
    }
    
    # expected score of opponent's drive
    r <- r - 6.8*x/100
    return(list(i=i, r=r))
}


# ----------------------------------------------------------------------
# Heuristic policy
# ----------------------------------------------------------------------
heuristic.policy <- function() {
    mu <- list()
    mu[[1]] <- matrix('P',100,100)
    mu[[2]] <- matrix('P',100,100)
    mu[[2]][,1:3] <- 'R' 
   
    mu[[3]] <- matrix('R',100,100)
    mu[[4]] <- matrix('R',100,100)
    
    return(mu)
}

# ----------------------------------------------------------------------
# Compute expected reward of given policy
# ----------------------------------------------------------------------
expected.reward <- function(mu) {
    Ne <- 10000
    r <- rep(0,Ne)
    for (i in 1:Ne) {
        r[i] <- simulate.drive(mu)$r
    }
    return(mean(r))
}

# ----------------------------------------------------------------------
# Generate samples for a given policy
# ----------------------------------------------------------------------
generate.sample <- function(mu) {
    Ns <- 10000
    D <- list()
    for (i in 1:Ns) {
        D[[i]] <- simulate.drive(mu)
    }
    return(D)
}
