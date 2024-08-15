library(glmmTMB)
library(ggplot2)
library(dplyr)
library(tidyr)

#### Serbian
#### Party-Specific ####
# Load data
serbian_ref <- read.csv('/Users/busraalbayrak/Desktop/MY498-Capstone-Project/data/rs_minutes_tks.csv')
serbian_ref$Date <- as.Date(serbian_ref$Date, format = "%Y-%m-%d")
serbian_ref$Year <- as.factor(format(serbian_ref$Date, "%Y"))
serbian_ref <- serbian_ref %>%
  mutate(Speaker_party = ifelse(Speaker_party %in% c("LDP","SDS", "Nova", "LSV"), "PRS", Speaker_party)) 
serbian_ref <-  subset(serbian_ref, Speaker_party %in% c("SNS", "DSS", "DS", "SPS", "PRS", "SRS"))

# Relevel the factor to set "SNS" as the reference category
serbian_ref$Speaker_party <- relevel(factor(serbian_ref$Speaker_party), ref = "SNS")

# Fit the models
econ_model <- glmmTMB(econ_counts ~ Speaker_party + Year + (1 | Speaker_name), family = nbinom2, data = serbian_ref)
gov_model <- glmmTMB(gov_counts ~ Speaker_party + (1 | Speaker_name), family = nbinom2, data = serbian_ref)
id_model <- glmmTMB(id_counts ~ Speaker_party + Year + (1 | Speaker_name), family = nbinom2, data = serbian_ref)
sec_model <- glmmTMB(sec_counts ~ Speaker_party + Year + (1 | Speaker_name), family = nbinom2, data = serbian_ref)

# Function to extract coefficients, p-values, and confidence intervals
extract_coefs <- function(model, dv) {
  coefs <- summary(model)$coefficients$cond
  pvals <- summary(model)$coefficients$cond[, "Pr(>|z|)"]
  data.frame(
    term = gsub("Speaker_party", "", rownames(coefs)),
    estimate = coefs[, "Estimate"],
    conf.low = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
    conf.high = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
    p.value = pvals,
    DV = dv
  ) 
}

# Function to add significance stars based on p-values
add_stars <- function(p.value) {
  ifelse(p.value < 0.001, "***",
         ifelse(p.value < 0.05, "**",
                ifelse(p.value < 0.1, "*", "")))
}

# Extract coefficients and prepare data
econ_tidy <- extract_coefs(econ_model, "Economy")
gov_tidy <- extract_coefs(gov_model, "Governance")
id_tidy <- extract_coefs(id_model, "Identity")
sec_tidy <- extract_coefs(sec_model, "Security")

# Combine results and add significance stars
combined_results <- bind_rows(econ_tidy, gov_tidy, id_tidy, sec_tidy)
combined_results <- combined_results %>%
  mutate(stars = add_stars(p.value),
         term_label = paste(term, stars, sep = " ")) %>%
  filter(term %in% c("SNS", "DSS", "DS", "SPS", "PRS", "SRS")) 



# Function to plot coefficients for each DV
plot_coefficients <- function(data) {
  ggplot(data, aes(x = term_label, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, color = "red") +
    coord_flip() +
    scale_y_continuous(limits = c(-0.8, 0.9)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(
      title = paste(unique(data$DV)),
      x = "Speaker Party",
      y = "Coefficient Estimate"
    )
}

# Plot for each DV
econ_plot_rs <- plot_coefficients(filter(combined_results, DV == "Economy"))
gov_plot_rs <- plot_coefficients(filter(combined_results, DV == "Governance"))
id_plot_rs <- plot_coefficients(filter(combined_results, DV == "Identity"))
sec_plot_rs <- plot_coefficients(filter(combined_results, DV == "Security"))

# Save plots or display them
print(econ_plot_rs)
print(gov_plot_rs)
print(id_plot_rs)
print(sec_plot_rs)

ggsave("economy_plot_rs.png", plot = econ_plot_rs)
ggsave("governance_plot_rs.png", plot = gov_plot_rs)
ggsave("identity_plot_rs.png", plot = id_plot_rs)
ggsave("security_plot_rs.png", plot = sec_plot_rs)


#### Party-Orientation ####
serbian_ref <- read.csv('/Users/busraalbayrak/Desktop/MY498-Capstone-Project/data/rs_minutes_tks.csv')
serbian_ref$Date <- as.Date(serbian_ref$Date, format = "%Y-%m-%d")
serbian_ref$Year <- as.factor(format(serbian_ref$Date, "%Y"))

serbian_ref <- serbian_ref %>%
  mutate(Party_orientation = ifelse(Party_orientation == "Centre to centre-right", "Centre-right", Party_orientation)) %>%
  filter(Party_orientation != "-") 

serbian_ref$Party_orientation <- factor(serbian_ref$Party_orientation)
serbian_ref$Party_orientation <- relevel(serbian_ref$Party_orientation, ref = "Big tent")

econ_model <- glmmTMB(econ_counts ~ Party_orientation + Year + (1 | Speaker_name), family = nbinom2, data = serbian_ref)
gov_model <- glmmTMB(gov_counts ~ Party_orientation + (1 | Speaker_name), family = nbinom2, data = serbian_ref)
id_model <- glmmTMB(id_counts ~ Party_orientation + Year + (1 | Speaker_name), family = nbinom2, data = serbian_ref)
sec_model <- glmmTMB(sec_counts ~ Party_orientation + Year + (1 | Speaker_name), family = nbinom2, data = serbian_ref)

summary(econ_model)
# Function to extract coefficients, p-values, and confidence intervals
extract_coefs <- function(model, dv) {
  coefs <- summary(model)$coefficients$cond
  pvals <- summary(model)$coefficients$cond[, "Pr(>|z|)"]
  data.frame(
    term = gsub("Party_orientation", "", rownames(coefs)),
    estimate = coefs[, "Estimate"],
    conf.low = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
    conf.high = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
    p.value = pvals,
    DV = dv
  ) 
}

# Function to add significance stars based on p-values
add_stars <- function(p.value) {
  ifelse(p.value < 0.001, "***",
         ifelse(p.value < 0.05, "**",
                ifelse(p.value < 0.1, "*", "")))
}

# Extract coefficients and prepare data
econ_tidy <- extract_coefs(econ_model, "Economy")
gov_tidy <- extract_coefs(gov_model, "Governance")
id_tidy <- extract_coefs(id_model, "Identity")
sec_tidy <- extract_coefs(sec_model, "Security")

# Combine results and add significance stars
combined_results <- bind_rows(econ_tidy, gov_tidy, id_tidy, sec_tidy)
combined_results <- combined_results %>%
  mutate(stars = add_stars(p.value),
         term_label = paste(term, stars, sep = " ")) %>%
  filter(term %in% c(
    "Centre", "Centre-right", "Centreleft ", "Farright", "Right", "Left"))

# Function to plot coefficients for each DV
plot_coefficients <- function(data) {
  ggplot(data, aes(x = term_label, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, color = "red") +
    coord_flip() +
    scale_y_continuous(limits = c(-1.6, 1.4)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(
      title = paste(unique(data$DV)),
      x = "Party Orientation",
      y = "Coefficient"
    )
}


# Plot for each DV
econ_or_plot_rs<- plot_coefficients(filter(combined_results, DV == "Economy"))
gov_or_plot_rs<- plot_coefficients(filter(combined_results, DV == "Governance"))
id_or_plot_rs<- plot_coefficients(filter(combined_results, DV == "Identity"))
sec_or_plot_rs<- plot_coefficients(filter(combined_results, DV == "Security"))

# Save plots or display them
print(econ_or_plot_rs)
print(gov_or_plot_rs)
print(id_or_plot_rs)
print(sec_or_plot_rs)

ggsave("economyor_plot_rs.png", plot = econ_or_plot_rs)
ggsave("governanceor_plot_rs.png", plot = govor_plot_rs)
ggsave("identityor_plot_rs.png", plot = idor_plot_rs)
ggsave("securityor_plot_rs.png", plot = secor_plot_rs)
#### TURKISH ####

#### Party-Specific ####
turkish_ref <- read.csv("/Users/busraalbayrak/Desktop/MY498-Capstone-Project/data/tr_minutes_tks.csv")
colnames(turkish_ref)
turkish_ref$Date <- as.Date(turkish_ref$Date, format = "%Y-%m-%d")
turkish_ref$Year <- as.numeric(format(turkish_ref$Date, "%Y"))

turkish_ref <- turkish_ref %>%
  mutate(Speaker_party = ifelse(Speaker_party %in% c("BDP","BDP;DBP", "DBP"), "HDP", Speaker_party))  %>%
  mutate(Speaker_party = ifelse(Speaker_party %in% c("EMEP", "TIP", "TİP"), "TİP", Speaker_party)) %>%
  mutate(Speaker_party = ifelse(Speaker_party %in% c("CHP", "AKP", "MHP", "HDP", "IYI"), Speaker_party, "Other")) 
turkish_ref <-  subset(turkish_ref, Speaker_party %in% c("CHP", "AKP", "MHP", "HDP", "IYI"))

turkish_ref$Speaker_party <- factor(turkish_ref$Speaker_party)

turkish_ref$Speaker_party <- relevel(turkish_ref$Speaker_party, ref = "AKP")


# Ensure Year is a factor
turkish_ref$Year <- as.factor(turkish_ref$Year)

# Fit the models
econ_model <- glmmTMB(econ_counts ~ Speaker_party + Year + (1 | Speaker_name), family = nbinom2, data = turkish_ref)
gov_model <- glmmTMB(gov_counts ~ Speaker_party + (1 | Speaker_name), family = nbinom2, data = turkish_ref)
id_model <- glmmTMB(id_counts ~ Speaker_party + Year + (1 | Speaker_name), family = nbinom2, data = turkish_ref)
sec_model <- glmmTMB(sec_counts ~ Speaker_party + Year + (1 | Speaker_name), family = nbinom2, data = turkish_ref)


# Function to extract coefficients, p-values, and confidence intervals
extract_coefs <- function(model, dv) {
  coefs <- summary(model)$coefficients$cond
  pvals <- summary(model)$coefficients$cond[, "Pr(>|z|)"]
  data.frame(
    term = gsub("Speaker_party", "", rownames(coefs)),
    estimate = coefs[, "Estimate"],
    conf.low = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
    conf.high = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
    p.value = pvals,
    DV = dv
  ) 
}

# Function to add significance stars based on p-values
add_stars <- function(p.value) {
  ifelse(p.value < 0.001, "***",
         ifelse(p.value < 0.05, "**",
                ifelse(p.value < 0.1, "*", "")))
}

# Extract coefficients and prepare data
econ_tidy <- extract_coefs(econ_model, "Economy")
gov_tidy <- extract_coefs(gov_model, "Governance")
id_tidy <- extract_coefs(id_model, "Identity")
sec_tidy <- extract_coefs(sec_model, "Security")

# Combine results and add significance stars
combined_results <- bind_rows(econ_tidy, gov_tidy, id_tidy, sec_tidy)
combined_results <- combined_results %>%
  mutate(stars = add_stars(p.value),
         term_label = paste(term, stars, sep = " ")) %>%
  filter(term %in% c("AKP", "MHP", "IYI", "CHP", "HDP")) 



# Function to plot coefficients for each DV
plot_coefficients <- function(data) {
  ggplot(data, aes(x = term_label, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, color = "red") +
    coord_flip() +
    scale_y_continuous(limits = c(-0.85, 0.85)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(
      title = paste(unique(data$DV)),
      x = "Speaker Party",
      y = "Coefficient Estimate"
    )
}

# Plot for each DV
econ_plot_tr <- plot_coefficients(filter(combined_results, DV == "Economy"))
gov_plot_tr <- plot_coefficients(filter(combined_results, DV == "Governance"))
id_plot_tr <- plot_coefficients(filter(combined_results, DV == "Identity"))
sec_plot_tr <- plot_coefficients(filter(combined_results, DV == "Security"))

# Save plots or display them
print(econ_plot_tr)
print(gov_plot_tr)
print(id_plot_tr)
print(sec_plot_tr)

ggsave("economy_plot_tr.png", plot = econ_plot_tr)
ggsave("governance_plot_tr.png", plot = gov_plot_tr)
ggsave("identity_plot_tr.png", plot = id_plot_tr)
ggsave("security_plot_tr.png", plot = sec_plot_tr)

turkish_ref$Year <- as.factor(turkish_ref$Year)
model <- glmer(sentiment_binary ~ Year + Speaker_party + (1 | Speaker_name),
               data = turkish_ref, family = binomial(), 
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 10)
summary(model)

####Party-Orientation ####
turkish_ref <- read.csv("/Users/busraalbayrak/Desktop/MY498-Capstone-Project/data/tr_minutes_tks.csv")
turkish_ref$Date <- as.Date(turkish_ref$Date, format = "%Y-%m-%d")
turkish_ref$Year <- as.factor(format(turkish_ref$Date, "%Y"))

turkish_ref <- turkish_ref %>%
  mutate(Party_orientation = ifelse(Party_orientation == "Far-right", "Right to far-right", Party_orientation)) %>%
  mutate(Party_orientation = ifelse(Party_orientation  %in% c("Left to far-left","Centre-left to left;Left" ,"Far-left"), "Left", Party_orientation)) %>%
  filter(Party_orientation != "-") 

category_counts <- turkish_ref %>%
  group_by(Party_orientation) %>%
  tally(name = "count")
categories_to_keep <- category_counts %>%
  filter(count >= 10) %>%
  pull(Party_orientation)

turkish_ref <- turkish_ref %>%
  filter(Party_orientation %in% categories_to_keep)

turkish_ref$Party_orientation <- factor(turkish_ref$Party_orientation)

turkish_ref$Party_orientation <- relevel(turkish_ref$Party_orientation, ref = "Right")

# Fit the models
econ_model <- glmmTMB(econ_counts ~ Party_orientation + Year + (1 | Speaker_name), family = nbinom2, data = turkish_ref)
gov_model <- glmmTMB(gov_counts ~ Party_orientation + (1 | Speaker_name), family = nbinom2, data = turkish_ref)
id_model <- glmmTMB(id_counts ~ Party_orientation + Year + (1 | Speaker_name), family = nbinom2, data = turkish_ref)
sec_model <- glmmTMB(sec_counts ~ Party_orientation + Year + (1 | Speaker_name), family = nbinom2, data = turkish_ref)

summary(econ_model)
# Function to extract coefficients, p-values, and confidence intervals
extract_coefs <- function(model, dv) {
  coefs <- summary(model)$coefficients$cond
  pvals <- summary(model)$coefficients$cond[, "Pr(>|z|)"]
  data.frame(
    term = gsub("Party_orientation", "", rownames(coefs)),
    estimate = coefs[, "Estimate"],
    conf.low = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
    conf.high = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"],
    p.value = pvals,
    DV = dv
  ) 
}

# Function to add significance stars based on p-values
add_stars <- function(p.value) {
  ifelse(p.value < 0.001, "***",
         ifelse(p.value < 0.05, "**",
                ifelse(p.value < 0.1, "*", "")))
}

# Extract coefficients and prepare data
econ_tidy <- extract_coefs(econ_model, "Economy")
gov_tidy <- extract_coefs(gov_model, "Governance")
id_tidy <- extract_coefs(id_model, "Identity")
sec_tidy <- extract_coefs(sec_model, "Security")

# Combine results and add significance stars
combined_results <- bind_rows(econ_tidy, gov_tidy, id_tidy, sec_tidy)
combined_results <- combined_results %>%
  mutate(stars = add_stars(p.value),
         term_label = paste(term, stars, sep = " ")) %>%
  filter(term %in% c(
    "Right to far-right", "Centre-left", "Centre-left to left", "Centre-right",
    "Far-left", "Left", "Left to far-left", "Far-right", "Left"
  ))

# Function to plot coefficients for each DV
plot_coefficients <- function(data) {
  ggplot(data, aes(x = term_label, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, color = "red") +
    coord_flip() +
    scale_y_continuous(limits = c(-1.4, 1.6)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(
      title = paste(unique(data$DV)),
      x = "Party Orientation",
      y = "Coefficient"
    )
}


# Plot for each DV
econ_or_plot_tr <- plot_coefficients(filter(combined_results, DV == "Economy"))
gov_or_plot_tr <- plot_coefficients(filter(combined_results, DV == "Governance"))
id_or_plot_tr <- plot_coefficients(filter(combined_results, DV == "Identity"))
sec_or_plot_tr <- plot_coefficients(filter(combined_results, DV == "Security"))

# Save plots or display them
print(econ_or_plot_tr)
print(gov_or_plot_tr)
print(id_or_plot_tr)
print(sec_or_plot_tr)
