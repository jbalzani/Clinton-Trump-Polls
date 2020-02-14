#Clinton-Trump polls
#calculate the CI for the spread of each poll
#note: some question wording may be from HarvardX staff
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create polls table
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# calculate CIs
xbar <- (polls$spread + 1)/2
xbar
cis <- polls %>% mutate(X_hat = spread, se = 2*sqrt(xbar*(1-xbar)/samplesize), lower = X_hat - qnorm(.975) * se, upper = X_hat + qnorm(.975) * se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
head(cis)


# see how many times the CIs contained the results
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
head(ci_data)
# Create an object called `p_hits` that summarizes the proportion of 
#confidence intervals that contain the actual value. Print this object to the console.
p_hits <- mutate(ci_data, hit = ifelse(actual_spread >= lower & 
                                         actual_spread <= upper,1,0)) %>% 
  summarize(proportion_hits = mean(hit))
proportion_hits <- p_hits
proportion_hits

# Create an object called `p_hits` that summarizes the proportion of hits
#for each pollster that has at least 5 polls.
p_hits <- ci_data %>% mutate(hit = ifelse(actual_spread >= lower & 
                                            actual_spread <= upper,1,0)) %>% 
  group_by(pollster) %>%
  filter(n() >= 5) %>% summarize(proportion_hits = mean(hit), n = n(), grade = grade[1]) %>%
  arrange(desc(proportion_hits))

# Create `p_hits` that summarizes the proportion of hits for each state 
#that has more than 5 polls.
p_hits <- ci_data %>% mutate(hit = ifelse(actual_spread >= lower & 
                                            actual_spread <= upper,1,0)) %>% 
  group_by(state) %>%
  filter(n() >= 5) %>% 
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))
p_hits

# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(x = state, y = proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Create an object called `errors` that calculates the difference between
#the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = spread> 0 & 
                           actual_spread > 0 | spread < 0 & actual_spread < 0)

# Examine the last 6 rows of `errors`
tail(errors)

# Create an object called `errors` that calculates the difference between 
#the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, 
                         hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for 
#each state that has more than 5 polls
p_hits <- errors %>% group_by(state) %>%
  filter(n() > 5) %>%
  summarize(proportion_hits = mean(hit), n = n()) %>%
  arrange(desc(proportion_hits))

# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(x = state, y = proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+", "A", "A-", "B+") | is.na(grade)) %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(x = state, y = error)) +
  geom_boxplot() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+") | is.na(grade)) %>%
  mutate(state = reorder(state, error)) %>%
  group_by(state) %>%
  filter(n() >= 5) %>%
  ggplot(aes(x = state, y = error)) +
  geom_boxplot() +
  geom_point()