# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------
#
# 14D006 Stochastic Models and Optimization
#
# (Authors) Daniel Bestard Delgado, Michael Cameron, 
#           Hans-Peter Höllwirth, Akhil Lohia
# (Date)    04.2017

# house cleaning
rm(list = ls())

# load libraries
if(!require(nnet)) {
  install.packages('nnet')
}
library(nnet)

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
simulate.drive <- function(mu, i.start=NA) {
    t <- 1
    i <- list()
    r <- 0
    
    # initial state
    i[[t]] <- i.start
    if (all(is.na(i.start))) {
        i[[t]] <- initial.condition()
    }
    
    terminated <- FALSE
    while(!terminated) {
        u <- mu[[i[[t]]$d]][i[[t]]$x, i[[t]]$y] # policy action
        outcome <- runif(1)
        flag <- TRUE
        
        # PASS attempt
        if (u == 'P') {
            if (outcome < 0.05) { # interception
                terminated <- TRUE
                x <- i[[t]]$x - (rpois(1,12) - 2)
                if (x <= 0) { # intercepted in opponent's end zone
                    x <- 20
                }
                else if (x > 100) { # opponent's touchdown
                    x <- 0
                    r <- -6.8
                    flag <- FALSE
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
                x <- i[[t]]$x - (rpois(1,12) - 2)
                if (x > 100) { # safety
                    terminated <- TRUE
                    x <- 20
                    r <- -2.0 
                }
            }
        }
        
        # RUN attempt
        else if (u == 'R') {
            if (outcome < 0.05) { # fumble
                terminated <- TRUE
                x <- i[[t]]$x
                if (x <= 0) { # fumbled in opponent's endzone
                  x <- 20
                }
                else if (x > 100) { # opponent's touchdown
                    x <- 0
                    r <- -6.8
                    flag <- FALSE
                }
                
            } else { # run
                x <- i[[t]]$x - (rpois(1,6) - 2)
                if (x > 100) { # safety
                    terminated <- TRUE
                    x <- 20
                    r <- -2.0 
                }
            }
        }  
        
        # PUNT attempt
        else if (u == 'U') {
            terminated <- TRUE
            x <- i[[t]]$x - (6*rpois(1,10) + 6)
            if (x < 0) x <- 20
        }   
        
        # KICK attempt
        else if (u == 'K') {
            terminated <- TRUE
            if (outcome < max(0, 0.95 - (0.95*(i[[t]]$x))/60)) { # successful field goal
                x <- 20
                r <- 3
            } else { # missed field goal
                x <- i[[t]]$x
            }
        }  
        
        if (x <= 0 & flag==TRUE) { # touchdown
            terminated <- TRUE
            x <- 20 
            r <- 6.8
        }
        
        # increase down
        d <- (i[[t]]$d) + 1 
        
        # calculate new distance to first down
        y <- i[[t]]$y + x - i[[t]]$x
        if (y <= 0) { # new first down
            y <- min(x,10)
            d <- 1
        } 
        if (d > 4) terminated <- TRUE
        
        if (terminated) {
            i[[t]]$pen <- TRUE
        }

        if (!terminated) {
            i[[t]]$pen <- FALSE
            t <- t + 1
            i[[t]] <- list(d=d, x=x, y=y)
        }
    }
    
    # expected score of opponent's drive
    r <- r - ((6.8*x)/100)
    return(list(i=i, r=r))
}

# ----------------------------------------------------------------------
# Compute expected reward of given policy and initial state
# ----------------------------------------------------------------------
expected.reward <- function(mu, i.start, Ne=10000) {
    m <- mean(sapply(1:Ne, function(x) {
      return(simulate.drive(mu,i.start)$r)
      }))
    return(m)
}

i.start <- list(d=1, x=80, y=10)

# ----------------------------------------------------------------------
# Generate samples for a given policy
# ----------------------------------------------------------------------
generate.sample <- function(mu, Ns=1000) {
    D <- list()
    for (i in 1:Ns) {
        D[[i]] <- simulate.drive(mu)
    }
    return(D)
}

# ----------------------------------------------------------------------
# (1) Best heuristic policy
# ----------------------------------------------------------------------
dummy.heuristic.policy <- function() {
    mu <- list()
    mu[[1]] <- matrix('P',100,100)
    mu[[2]] <- matrix('P',100,100)
    mu[[2]][,1:3] <- 'R' 
    
    mu[[3]] <- matrix('R',100,100)
    mu[[4]] <- matrix('U',100,100)
    
    return(mu)
}

get.heuristic.policy <- function(options) {
    mu <- list()
    r <- 100
    c <- 100
    mu[[1]] <- matrix('P',r,c)
    mu[[2]] <- matrix('P',r,c)
    mu[[2]][,1:2] <- 'R' 
    
    mu[[3]] <- matrix('P',r,c)
    for (x in 1:r) {
        for (y in 1:x) {
            if (x < (41)) {
                if (y < 3) mu[[3]][x,y] <- options$o3a1
                else       mu[[3]][x,y] <- options$o3a2
            } else {
                if (y < 3) mu[[3]][x,y] <- options$o3b1
                else       mu[[3]][x,y] <- options$o3b2
            }
        }
    }
    
    mu[[4]] <- matrix('P',r,c)
    for (x in 1:r) {
        for (y in 1:x) {
            if (x < (41)) {
                if (y < 3) mu[[4]][x,y] <- options$o4a1
                else       mu[[4]][x,y] <- options$o4a2
            } else {
                if (y < 3) mu[[4]][x,y] <- options$o4b1
                else       mu[[4]][x,y] <- options$o4b2
            }
        }
    }    
    
    return(mu)    
}

best.heuristic.policy <- function() {
    down3.options <- c('R','P')
    down4.a.options <- c('R','P','K')
    down4.b.options <- c('R','P','U')
    
    i.start <- list(d=1, x=80, y=10)
    J.best <- -10 
    
    for (o3a1 in 1:length(down3.options)) {
        for (o3a2 in 1:length(down3.options)) {
            for (o3b1 in 1:length(down3.options)) {
                for (o3b2 in 1:length(down3.options)) {
                    for (o4a1 in 1:length(down4.a.options)) {
                        for (o4a2 in 1:length(down4.a.options)) {
                            for (o4b1 in 1:length(down4.b.options)) {
                                for (o4b2 in 1:length(down4.b.options)) {
                                    options <- list(o3a1=down3.options[o3a1],
                                                    o3a2=down3.options[o3a2],
                                                    o3b1=down3.options[o3b1],
                                                    o3b2=down3.options[o3b2],
                                                    o4a1=down4.a.options[o4a1],
                                                    o4a2=down4.a.options[o4a2],
                                                    o4b1=down4.b.options[o4b1],
                                                    o4b2=down4.b.options[o4b2])
                                    mu <- get.heuristic.policy(options)
                                    
                                    J <- expected.reward(mu, i.start)
                                    print(J)
                                    if (J > J.best) {
                                        J.best <- J
                                        options.best <- options
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    return(list(options=options.best, J=J.best))    
}

# result from best.heuristic.policy function
options.best <- list(o3a1='P', o3a2='P',
                     o3b1='R', o3b2='P',
                     o4a1='R', o4a2='K',
                     o4b1='R', o4b2='U')
expected.reward(get.heuristic.policy(options.best), i.start)



# ----------------------------------------------------------------------
# (2) Neural network
# ----------------------------------------------------------------------
feature.vector <- function(i) {
    # translate the x and y to standardized features
    return(c(0.01*(i$x), 0.01*(i$y)))
}

# translate sample data into data frame for neural network training
df.sample <- function(D, down) {
    df <- data.frame()
    row <- 1
    for (l in 1:length(D)) { # l-th sample trajectory
        for (t in 1:length(D[[l]]$i)) { # t-th state
            if (D[[l]]$i[[t]]$d == down) {
                df[row, 1] <- feature.vector(D[[l]]$i[[t]])[1] 
                df[row, 2] <- feature.vector(D[[l]]$i[[t]])[2]
                df[row, 3] <- D[[l]]$r
                row <- row + 1
            }
        }
    } 
    if (row>1) colnames(df) <- c('x', 'y', 'J')
    return(df)
}

# train neural network for particular down
train.nn <- function(D, down, Nt, R) {
    df <- df.sample(D, down)
    if (nrow(df) > 0) {r <- nnet(formula = J ~ x + y, data=df, size=R, linout = TRUE) }
    else {r <- NA}
    return(r)
}

# train 4 neural networks, one for each down
train.nns <- function(D, Nt=100, R=20) {
    r <- list()
    for (d in 1:4) {
        r[[d]] <- train.nn(D, d, Nt, R)
    }
    return(r)
}

# estimate reward-to-go from trained neural network
estimate.Js <- function(r) {
    J <- list()
    df <- expand.grid(seq(1,100), seq(1,100))
    colnames(df) <- c('x','y')
    for (d in 1:4) {
        #J[[d]] <- cbind(df, compute(r[[d]], covariate=df)$net.result)
        if (all(!is.na(r[[d]]))) {
            J[[d]] <- cbind(df, predict(r[[d]], df))
        }
        else {
            J[[d]] <- cbind(df,0)
        }
    }
    return(J)
}


# ----------------------------------------------------------------------
# (3) Dynamic programming algorithm
# ----------------------------------------------------------------------
transition.prob.dummy <- function() {
    p <- list()
    for (u in 1:4) {
        p[[u]] <- matrix(1/(10000*10000), 10000, 10000)
    }
    return(p)
}

transition.prob <- function(D, mu) {
    # initialize transition probabilities
    p <- list()
    for (u in 1:4) {
        p[[u]] <- matrix(0, 10000, 10000)
    }
    
    # compute empirical transition probablities
    U <- c('P','R','U','K')
    transitions <- rep(0,4)
    for (l in 1:length(D)) { # l-th sample trajectory
        if (length(D[[l]]$i) > 1) {
            for (t in 2:length(D[[l]]$i)) { # t-th state
                t.i <- D[[l]]$i[[t-1]]
                t.j <- D[[l]]$i[[t]]
                
                u <- mu[[t.i$d]][t.i$x, t.j$y] # policy action
                u.idx <- which(U==u)
                transitions[u.idx] <- transitions[u.idx] + 1 
                
                p.i <- t.i$x + (t.i$x - 1) * t.i$y
                p.j <- t.j$x + (t.j$x - 1) * t.j$y
                p[[u.idx]][p.i,p.j] <- p[[u.idx]][p.i,p.j] + 1
            }
        }
    } 
    
    # normalize transition probabilities
    for (u in 1:4) {
        if (transitions[u] > 0)
            p[[u]] <- p[[u]] / transitions[u]
    }
    
    return(p)
}

# get reward-to-go approximation for status i from neural network output
J.approx <- function(Js, i) {
    return(Js[[i$d]][Js[[i$d]][,1]==i$x & Js[[i$d]][,2]==i$y,3])
}

# update policy based on new neural network setup r
update.policy <- function(mu, r, D) {
    U <- c('P','R','U','K')
    Js <- estimate.Js(r)
    p <- transition.prob(D, mu)
    
    for (l in 1:length(D)) { # l-th sample trajectory
        for (t in 1:length(D[[l]]$i)) { # t-th state                
            i <- D[[l]]$i[[t]]
            s <- rep(0,4)
            for (u in 1:length(U)) {
                for(jd in 1:4) {
                    for (jx in 1:100) {
                        for (jy in 1:100) {
                            p.iju <- p[[u]][i$x+(i$x-1)*i$y, jx+(jx-1)*jy]
                            if (p.iju > 0)
                                s[u] <- s[u] + p.iju * J.approx(Js, list(d=jd, x=jx, y=jy))
                        }
                    }
                }
            }
            # print(s)
            mu[[i$d]][i$x,i$y] <- U[which.max(s)]
        }
    }
    return(mu)
}

# ----------------------------------------------------------------------
# (4) API and OPI algorithm
# ----------------------------------------------------------------------
API <- list(Np=10, Ne=8000, Ns=8000, Nt=8000)
OPI <- list(Np=1, Ne=10000, Ns=1, Nt=1)
config <- list(Np=1, Ne=100, Ns=50, Nt=100) # for test purposes

approx.policy.iteration <- function(config) {
    i.start <- list(d=1, x=80, y=10)
    J <- c()
    
    # 1. initial policy
    mu <- list()
    mu[[1]] <- dummy.heuristic.policy()
    
    for (k in 1:50) {
        # (2.a) obtain estimate for expected reward 
        if (k %% config$Np == 0) {
            J <- c(J, expected.reward(mu[[k]], i.start, config$Ne))
            print(J)
        }
        
        # (2.b) generate sample trajectories
        cat('.')
        D <- generate.sample(mu[[k]], config$Ns)
        
        # (2.c) train parameter vector
        cat('.')
        r <- train.nns(D, config$Nt, R=20) 
        
        # (2.d) set new policy
        cat('.')
        mu[[k+1]] <- update.policy(mu[[k]], r, D)
        #mu[[k+1]] <- mu[[k]]
    }
    J <- c(J, expected.reward(mu[[k+1]], i.start, config$Ne))
    return(J)
}

J <- approx.policy.iteration(config)
