# Function to get the vector of simbol
options(digits=10)
return_vector <- function(simbol){
    vector <- c(0,0)
    if(simbol=='r'){vector <- c(1,0)}
    if(simbol=='l'){vector <- c(-1,0)}
    if(simbol=='u'){vector <- c(0,1)}
    if(simbol=='d'){vector <- c(0,-1)}
    vector
}

# Given "at least how many city blocks away", "number of moves" and "number of trials", calculate the probability.
prob <- function(threshold, steps, trials){
    set.seed(100)
    simulation <- c()
    for(i in 1:trials){
        end_point <- c(0,0)
        move <- sample(c('r','l','u','d'), steps, replace=TRUE)
        for (i in 1:steps){
            end_point <- end_point + return_vector(move[i])
        }
        sum <- sum(abs(end_point) >= threshold)
        result <- if(sum==0){FALSE}else{TRUE}
        simulation <- c(result, simulation)
    }
    sum(simulation)/length(simulation)  
}

# Question 1
prob(3,10,1000)

# Question 2
prob(10,60,1000)


# Function for question 3 & 4
prob_ever <- function(threshold, steps, trials){
    set.seed(100)
    simulation <- c()
    for(i in 1:trials){
        end_point <- c(0,0)
        move <- sample(c('r','l','u','d'), steps, replace=TRUE)
        for (i in 1:steps){
            end_point <- end_point + return_vector(move[i])
            if(sum(abs(end_point) >= threshold)>0){result <- TRUE; break}
            else{result <- FALSE}
        }
        simulation <- c(result, simulation)
    }
    sum(simulation)/length(simulation)  
}

# Question 3
prob_ever(5,10,1000)

# Question 4
prob_ever(10,60,1000)


# Function for question 5 & 6
prob_56 <- function(steps, trials){
    set.seed(100)
    simulation <- c()
    for(i in 1:trials){
        end_point <- c(0,0)
        move <- sample(c('r','l','u','d'), steps, replace=TRUE)
        ever <- FALSE
        for (i in 1:steps){
            end_point <- end_point + return_vector(move[i])
            if(end_point[1] > 1){ever<-TRUE}
        }
        if((end_point[1] < -1)&(ever==TRUE)){result<-TRUE}
        else{result<-FALSE}
        simulation <- c(result, simulation)
    }
    sum(simulation)/length(simulation)  
}

# Question 5
prob_56(10,1000)

# Question 6
prob_56(30,1000)


# Function for Question 7 & 8
ave_move <- function(threshold, trials){
    set.seed(100)
    simulation <- c()
    for(i in 1:trials){
        end_point <- c(0,0)
        steps <- 0
        repeat{
            move <- return_vector(sample(c('r','l','u','d'), 1))
            end_point <- end_point + move
            steps <- steps + 1
            if(abs(end_point[1])==threshold|abs(end_point[2])==threshold){break}
        }
        simulation <- c(simulation, steps)
    }
    mean(simulation)
}

# Question 7
ave_move(10,1000)

# Question 8
ave_move(60,1000)
