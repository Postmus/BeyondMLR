chicago_airbnb$overall_satisfaction <- sapply(
  chicago_airbnb$overall_satisfaction,
  function(x) {
    rnorm(1, mean = x, sd = 1)  # Sample from N(mean = x, sd = 1)
  }
)

n_neighborhoods <- length(unique(chicago_airbnb$neighborhood))  # Number of neighborhoods
sigma_neighborhood <- 0.8 # Variability of the neighborhood effect

# Generate random effects for neighborhoods
neighborhood_effects <- rnorm(n_neighborhoods, mean = 0, sd = sigma_neighborhood)

# Add the neighborhood effect to the existing satisfaction ratings
chicago_airbnb$overall_satisfaction <- chicago_airbnb$overall_satisfaction +
  neighborhood_effects[as.numeric(chicago_airbnb$neighborhood)]

chicago_airbnb$overall_satisfaction <- chicago_airbnb$overall_satisfaction / (range(chicago_airbnb$overall_satisfaction)[2] - range(chicago_airbnb$overall_satisfaction)[1]) 

chicago_airbnb$overall_satisfaction <- 4*(chicago_airbnb$overall_satisfaction - min(chicago_airbnb$overall_satisfaction)) + 1

##############################


summary(lmer(log_price ~ BikeScore + (1 | neighborhood), data = chicago_airbnb))
summary(lmer(log_price ~ BikeScore + (1 | district/neighborhood), data = chicago_airbnb))

summary(lmer(log_price ~ bedrooms + (1 | neighborhood), data = chicago_airbnb))
summary(lmer(log_price ~ bedrooms + (1 | district/neighborhood), data = chicago_airbnb))

summary(lmer(log_price ~ room_type + bedrooms + minstay + (1 | neighborhood), data = chicago_airbnb))

summary(lmer(log_price ~ TransitScore + (1 | neighborhood), data = chicago_airbnb))
summary(lm(log_price ~ TransitScore, data = chicago_airbnb))

summary(lmer(log_price ~ minstay + (1 | neighborhood), data = chicago_airbnb))
summary(lm(log_price ~ minstay, data = chicago_airbnb))

##########################

summary(lmer(log_price ~ (overall_satisfaction_c + room_type + accommodates_c + 
                                               bedrooms_c + minstay_c)*(TransitScore_c + BikeScore_c) 
                                             + (1 | neighborhood), data = chicago_airbnb))

step(lmer(log_price ~ (overall_satisfaction_c + room_type + accommodates_c + 
                            bedrooms_c + minstay_c)*(TransitScore_c + BikeScore_c) 
             + (1 | neighborhood), data = chicago_airbnb))


listing_summary <- chicago_airbnb %>%
  group_by(district) %>%
  summarise(num_neighborhoods = n_distinct(neighborhood))

ggplot(listing_summary, aes(x = reorder(district, num_neighborhoods), y = num_neighborhoods)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Number of Neighborhoods per District",
       x = "Distruct",
       y = "Number of Neighborhoods") 



model.3level <- lmer(log_price ~ 1 + (1 | district/neighborhood), data = chicago_airbnb)
summary(model.3level)

model.2level <- lmer(log_price ~ 1 + (1 | neighborhood), data = chicago_airbnb)
summary(model.2level)

anova(model.2level, model.3level, refit = FALSE)

model.3level.L1 <- lmer(log_price ~ overall_satisfaction_c + room_type + accommodates_c +
                          bedrooms_c + minstay_c + (1 | district/neighborhood), data = chicago_airbnb)
summary(model.3level.L1)

model.2level.L1 <- lmer(log_price ~ overall_satisfaction_c + room_type + accommodates_c +
                          bedrooms_c + minstay_c + (1 | neighborhood), data = chicago_airbnb)
summary(model.2level.L1)

anova(model.3level.L1)
anova(model.2level.L1)

model.3level.L2 <- lmer(log_price ~ TransitScore_c + BikeScore_c + (1 | district/neighborhood), data = chicago_airbnb)
summary(model.3level.L2)

model.2level.L2 <- lmer(log_price ~ TransitScore_c + BikeScore_c + (1 | neighborhood), data = chicago_airbnb)
summary(model.3level.L2)

anova(model.3level.L2)
anova(model.2level.L2)
