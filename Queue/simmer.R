library(simmer)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(10) %>%
  log_("I must leave")

bank <-
  simmer("bank") %>%
  add_generator("Customer", customer, at(5))

bank %>% run(until = 100)
#> 5: Customer0: Here I am
#> 15: Customer0: I must leave
#> simmer environment: bank | now: 15 | next: 
#> { Monitor: in memory }
#> { Source: Customer | monitored: 1 | n_generated: 1 }
bank %>% get_mon_arrivals()
#>        name start_time end_time activity_time finished replication
#> 1 Customer0          5       15            10     TRUE           1

###############random 

library(simmer)

set.seed(10212)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(10) %>%
  log_("I must leave")

bank <-
  simmer("bank") %>%
  add_generator("Customer", customer, at(rexp(1, 1/5)))

bank %>% run(until = 100)
#> 7.8393: Customer0: Here I am
#> 17.8393: Customer0: I must leave
#> simmer environment: bank | now: 17.8393046225526 | next: 
#> { Monitor: in memory }
#> { Source: Customer | monitored: 1 | n_generated: 1 }
bank %>% get_mon_arrivals()
#>        name start_time end_time activity_time finished replication
#> 1 Customer0   7.839305  17.8393            10     TRUE           1




# Function to specify a series of waiting times, that loop around
loop <- function(...) {
  time_diffs <- c(...)
  i <- 0
  function() {
    if (i < length(time_diffs)) {
      i <<- i+1
    } else {
      i <<- 1
    }
    return(time_diffs[i])
  }
}

x <- loop(10, 7, 20)
x(); x(); x(); x(); x()
#> [1] 10
#> [1] 7
#> [1] 20
#> [1] 10
#> [1] 7

library(simmer)

# Function to specify a series of waiting times in a loop
loop <- function(...) {
  time_diffs <- c(...)
  i <- 0
  function() {
    if (i < length(time_diffs)) {
      i <<- i+1
    } else {
      i <<- 1
    }
    return(time_diffs[i])
  }
}

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(loop(7, 10, 20)) %>%
  log_("I must leave")


bank <-
  simmer("bank") %>%
  add_generator("Customer", customer, at(2, 5, 12))

bank %>% run(until = 400)
#> 2: Customer0: Here I am
#> 5: Customer1: Here I am
#> 9: Customer0: I must leave
#> 12: Customer2: Here I am
#> 15: Customer1: I must leave
#> 32: Customer2: I must leave
#> simmer environment: bank | now: 32 | next: 
#> { Monitor: in memory }
#> { Source: Customer | monitored: 1 | n_generated: 3 }
bank %>% get_mon_arrivals()
#>        name start_time end_time activity_time finished replication
#> 1 Customer0          2        9             7     TRUE           1
#> 2 Customer1          5       15            10     TRUE           1
#> 3 Customer2         12       32            20     TRUE           1

library(simmer)

# Create a template trajectory
customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(1) %>% # The timeout of 1 is a placeholder to be overwritten later
  log_("I must leave")

# Create three copies of the template
Klaus <- customer
Tony <- customer
Evelyn <- customer

# Modify the timeout of each copy
Klaus[2] <- timeout(trajectory(), 10)
Tony[2] <- timeout(trajectory(), 7)
Evelyn[2] <- timeout(trajectory(), 20)

# Check that the modifications worked
Klaus
#> trajectory: Customer's path, 3 activities
#> { Activity: Log          | message: Here I am, level: 0 }
#> { Activity: Timeout      | delay: 10 }
#> { Activity: Log          | message: I must lea..., level: 0 }
Tony
#> trajectory: Customer's path, 3 activities
#> { Activity: Log          | message: Here I am, level: 0 }
#> { Activity: Timeout      | delay: 7 }
#> { Activity: Log          | message: I must lea..., level: 0 }
Evelyn
#> trajectory: Customer's path, 3 activities
#> { Activity: Log          | message: Here I am, level: 0 }
#> { Activity: Timeout      | delay: 20 }
#> { Activity: Log          | message: I must lea..., level: 0 }

bank <-
  simmer("bank") %>%
  add_generator("Klaus", Klaus, at(5)) %>%
  add_generator("Tony", Tony, at(2)) %>%
  add_generator("Evelyn", Evelyn, at(12))

bank %>% run(until = 400)
#> 2: Tony0: Here I am
#> 5: Klaus0: Here I am
#> 9: Tony0: I must leave
#> 12: Evelyn0: Here I am
#> 15: Klaus0: I must leave
#> 32: Evelyn0: I must leave
#> simmer environment: bank | now: 32 | next: 
#> { Monitor: in memory }
#> { Source: Klaus | monitored: 1 | n_generated: 1 }
#> { Source: Tony | monitored: 1 | n_generated: 1 }
#> { Source: Evelyn | monitored: 1 | n_generated: 1 }
bank %>% get_mon_arrivals()
#>      name start_time end_time activity_time finished replication
#> 1   Tony0          2        9             7     TRUE           1
#> 2  Klaus0          5       15            10     TRUE           1
#> 3 Evelyn0         12       32            20     TRUE           1

library(simmer)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(12) %>%
  log_("I must leave")

bank <-
  simmer("bank") %>%
  add_generator("Customer", customer, from_to(0, 41, function() {10}))

bank %>% run(until = 400)
#> 0: Customer0: Here I am
#> 10: Customer1: Here I am
#> 12: Customer0: I must leave
#> 20: Customer2: Here I am
#> 22: Customer1: I must leave
#> 30: Customer3: Here I am
#> 32: Customer2: I must leave
#> 40: Customer4: Here I am
#> 42: Customer3: I must leave
#> 52: Customer4: I must leave
#> simmer environment: bank | now: 52 | next: 
#> { Monitor: in memory }
#> { Source: Customer | monitored: 1 | n_generated: 5 }
bank %>% get_mon_arrivals()
#>        name start_time end_time activity_time finished replication
#> 1 Customer0          0       12            12     TRUE           1
#> 2 Customer1         10       22            12     TRUE           1
#> 3 Customer2         20       32            12     TRUE           1
#> 4 Customer3         30       42            12     TRUE           1
#> 5 Customer4         40       52            12     TRUE           1


.
.
.
.
.
.
.
.
.
.
#https://r-simmer.org/articles/simmer-04-bank-1.html

