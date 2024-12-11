
library(arules)
library(arulesViz)
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)


top_country_data <- orders %>% filter(Country %in% c("United Kingdom", "EIRE"))
top_country_trans <- as(split(top_country_data$StockCode, top_country_data$Invoice), "transactions")
summary(top_country_trans)

# Częstość występowania elementów
freqTopCountry  = itemFrequency(top_country_trans, type = "relative")
str(freqTopCountry)
summary(freqTopCountry)
freqTopCountry = sort(freqTopCountry, decreasing= TRUE)
print((freqTopCountry[freqTopCountry>=0.05]))
length(freqTopCountry[freqTopCountry>=0.05])
top_items <- freqTopCountry[freqTopCountry >= 0.05]
# Istnieje 31 elementów, które mają wsparcie większe lub równe 5%
# Maksymalne wsparcie względne jest na poziomie 14,3 %

supp_range_f = seq(0.005,0.14,0.005)
nbFSet <- vector(mode = "integer", length = length(supp_range_f))
for( i in 1:length(supp_range_f))
{
  nbFSet[i] = length(freqTopCountry[freqTopCountry>=supp_range_f[i]])
}
resf <- data.frame(supp_range_f, nbFSet)
View(resf)

itemFrequencyPlot(top_country_trans, type ="relative", support= 0.05, main = "15 najczęściej występujących elementów")
cat("Lista produktów, które występują w zbiorze najczęściej (wsparcie >= 0.05):", "\n")
cat(paste(names(top_items), top_items, sep = ": ", collapse = "\n"))


# Wykrywanie zbiorów częstych
aParam <- new("APparameter", "confidence"=0.3, "maxtime"=20,
              "support"=supp_range_f[1], "minlen"=1L, target='frequent itemsets') 
apriori_tc_tidlist <- c()
apriori_tc_count <- c()
for (i in (1:length(supp_range_f))){
  aParam@support <- supp_range_f[i]
  result <- apriori(top_country_trans, aParam)
  apriori_tc_tidlist[[i]] <- result
  apriori_tc_count[i] <- length(result)
}
print(sum(apriori_tc_count))
apriori_tc_count <- print(tibble(supp_range_f, apriori_tc_count))


# Generowanie reguł asocjacyjnych 
rules_top_countries <- apriori(top_country_trans, parameter = list(supp = 0.005, conf = 0.3, minlen = 2))
summary(rules_top_countries)
length(rules_top_countries)
str(rules_top_countries)
inspect(head(rules_top_countries))

plot(rules_top_countries, measure = c("support", "lift"), shading = "confidence")

# Reguły z największym wsparciem:
sorted_rules <- arules::sort(rules_top_countries, by = "support", decreasing = TRUE)
inspect(head(sorted_rules, 10))
inspectDT(head(sorted_rules,10))
sorted_rules_df <- as(sorted_rules, "data.frame")
print(sorted_rules_df[1:10, c("rules", "support", "confidence", "lift")])

LIFT_rules_top_countries <- subset(rules_top_countries, lift > 10)
inspect(LIFT_rules_top_countries)
length(LIFT_rules_top_countries)
LIFT_sorted_rules <- arules::sort(LIFT_rules_top_countries, by = "lift", decreasing = TRUE)
inspect(head(LIFT_sorted_rules, 10))
cat("\nSilne reguły asocjacyjne dla dwóch najbardziej dochodowych krajów:\n")
LIFT_sorted_rules_df <- as(LIFT_sorted_rules, "data.frame")
print(LIFT_sorted_rules_df[1:10, c("rules", "support", "confidence", "lift")])

plot(LIFT_rules_top_countries, shading="order", control=list(main = "Two-key plot" ))
plot(LIFT_rules_top_countries, method="matrix", measure="lift", engine = 'interactive')
top20_rules <- head(LIFT_sorted_rules, 20)
plot(top20_rules, method = "grouped", main = "Top 20 najlepszych reguł (lift)")


# Eksperymenty z różnymi parametrami
supp_range <- seq(0.005, 0.014, by = 0.005)
conf_range <- seq(0.2, 0.7, by = 0.1)
results <- list()
best_rules <- NULL
best_lifts <- c()
best_supp <- c()
best_conf <- c()

for (supp in supp_range) {
  for (conf in conf_range) {
    cat("Eksperyment - Support:", supp, " Confidence:", conf, "\n")
    rules <- apriori(top_country_trans, parameter = list(supp = supp, conf = conf, minlen = 2))
    if (length(rules) > 0) {
      # Przechowywanie reguł z najwyższym 'lift'
      sorted_rules <- sort(rules, by = "lift", decreasing = TRUE)
      results[[paste("Supp_", supp, "_Conf_", conf, sep = "")]] <- sorted_rules
      # Sprawdzanie, czy te reguły mają wyższy lift niż dotychczasowe
      if (is.null(best_rules) || quality(sorted_rules)[1, "lift"] > quality(best_rules)[1, "lift"]) {
        best_rules <- sorted_rules
      }
    }
  }
}
length(best_rules)
cat("\nNajlepsze reguły według lift:\n")
inspect(head(best_rules, n = 30))

experiment_results <- data.frame(
  Support = rep(supp_range, each = length(conf_range)),
  Confidence = rep(conf_range, times = length(supp_range)),
  RuleCount = sapply(results, length)
)

ggplot(experiment_results, aes(x = Support, y = Confidence, size = RuleCount, color = RuleCount)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Liczba reguł dla różnych parametrów Support i Confidence",
       x = "Support",
       y = "Confidence",
       size = "Liczba reguł",
       color = "Liczba reguł") +
  theme_minimal()


# reguły oparte o częste zbiory maksymalne
maxRul <- best_rules[is.maximal(best_rules) == TRUE]
summary(maxRul)
inspect(maxRul[1:10])

resTbl <- interestMeasure(best_rules,"improvement")
best_rules@quality$improvement <- resTbl
intres <- which(sapply(resTbl, function(x) {x > 0.8  && x <= 1 })==TRUE)
intersRule <- best_rules[intres] 
summary(intersRule)
inspect(intersRule)

plot(intersRule, method = "grouped", main = "Reguły ze współczynnikiem poprawy > 0.8")


# Analiza krajów z podziałem na grupy dochodowe

TotPriceCountry <- TotPriceCountry %>%
  mutate(
    IncomeGroup = case_when(
      SumTotCtry < 100000 ~ "Low Income",
      SumTotCtry >= 100000 & SumTotCtry < 500000 ~ "Medium Income",
      SumTotCtry >= 500000 ~ "High Income"
    )
  )
orders$IncomeGroup <- TotPriceCountry$IncomeGroup[match(orders$Country, TotPriceCountry$Country)]
summary(orders$IncomeGroup)


high_income_data <- orders %>% filter(IncomeGroup == "High Income")
medium_income_data <- orders %>% filter(IncomeGroup == "Medium Income")
low_income_data <- orders %>% filter(IncomeGroup == "Low Income")

high_income_trans <- as(split(high_income_data$StockCode, high_income_data$Invoice), "transactions")
medium_income_trans <- as(split(medium_income_data$StockCode, medium_income_data$Invoice), "transactions")
low_income_trans <- as(split(low_income_data$StockCode, low_income_data$Invoice), "transactions")

cat("Liczba transakcji High Income:", length(high_income_trans), "\n")
cat("Liczba transakcji Medium Income:", length(medium_income_trans), "\n")
cat("Liczba transakcji Low Income:", length(low_income_trans), "\n")


# Częstość występowania elementów
freqHigh  = itemFrequency(high_income_trans, type = "relative")
freqMedium  = itemFrequency(medium_income_trans, type = "relative")
freqLow  = itemFrequency(low_income_trans, type = "relative")
freqHigh = sort(freqHigh, decreasing= TRUE)
freqMedium = sort(freqMedium, decreasing= TRUE)
freqLow = sort(freqLow, decreasing= TRUE)
top_items_h <- freqHigh[freqHigh >= 0.05]
length(top_items_h)
top_items_m <- freqMedium[freqMedium >= 0.05]
length(top_items_m)
top_items_l <- freqLow[freqLow >= 0.05]
length(top_items_l)

cat("Lista produktów, które występują w zbiorze najczęściej (wsparcie >= 0.05):", "\n")
cat(paste(names(top_items_h), top_items_h, sep = ": ", collapse = "\n"))
cat(paste(names(top_items_m), top_items_m, sep = ": ", collapse = "\n"))
cat(paste(names(top_items_l), top_items_l, sep = ": ", collapse = "\n"))

df_high <- data.frame(
  item = names(top_items_h),
  frequency = as.numeric(top_items_h),
  group = "High Income"
)
df_medium <- data.frame(
  item = names(top_items_m),
  frequency = as.numeric(top_items_m),
  group = "Medium Income"
)
df_low <- data.frame(
  item = names(top_items_l),
  frequency = as.numeric(top_items_l),
  group = "Low Income"
)
df_all <- rbind(df_high, df_medium, df_low)

ggplot(df_all, aes(x = reorder(item, -frequency), y = frequency, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Porównanie częstotliwości występowania elementów w grupach dochodowych",
    x = "Produkt",
    y = "Częstotliwość względna"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2", name = "Grupa dochodowa")


# Wykrywanie zbiorów częstych
aParam <- new("APparameter", "confidence"=0.3, "maxtime"=20,
              "support"=supp_range_f[1], "minlen"=1L, target='frequent itemsets') 
apriori_H_tidlist <- c()
apriori_H_count <- c()
for (i in (1:length(supp_range_f))){
  aParam@support <- supp_range_f[i]
  result <- apriori(high_income_trans, aParam)
  apriori_H_tidlist[[i]] <- result
  apriori_H_count[i] <- length(result)
}
print(sum(apriori_H_count))
apriori_H_count <- print(tibble(supp_range_f, apriori_H_count))
# Algorytm APRIORI pozwolił na wykrycie 11354 zbiorów częstych
apriori_M_tidlist <- c()
apriori_M_count <- c()
for (i in (1:length(supp_range_f))){
  aParam@support <- supp_range_f[i]
  result <- apriori(medium_income_trans, aParam)
  apriori_M_tidlist[[i]] <- result
  apriori_M_count[i] <- length(result)
}
print(sum(apriori_M_count))
apriori_M_count <- print(tibble(supp_range_f, apriori_M_count))
# Algorytm APRIORI pozwolił na wykrycie 28996 zbiorów częstych
apriori_L_tidlist <- c()
apriori_L_count <- c()
for (i in (1:length(supp_range_f))){
  aParam@support <- supp_range_f[i]
  result <- apriori(low_income_trans, aParam)
  apriori_L_tidlist[[i]] <- result
  apriori_L_count[i] <- length(result)
}
print(sum(apriori_L_count))
apriori_L_count <- print(tibble(supp_range_f, apriori_L_count))


# Generowanie reguł
high_income_rules <- apriori(high_income_trans, parameter = list(supp = 0.005, conf = 0.5))
medium_income_rules <- apriori(medium_income_trans, parameter = list(supp = 0.005, conf = 0.5))
low_income_rules <- apriori(low_income_trans, parameter = list(supp = 0.005, conf = 0.5))

inspect(head(high_income_rules, 30))
inspect(head(medium_income_rules, 30))
inspect(head(low_income_rules, 30))

high_income_top_rules <- head(sort(high_income_rules, by = "support"), 50)
medium_income_top_rules <- head(sort(medium_income_rules, by = "support"), 50)
low_income_top_rules <- head(sort(low_income_rules, by = "support"), 50)

high_income_summary <- data.frame(
  product = labels(lhs(high_income_top_rules)),
  support = quality(high_income_top_rules)$support,
  confidence = quality(high_income_top_rules)$confidence,
  group = "High Income"
)
medium_income_summary <- data.frame(
  product = labels(lhs(medium_income_top_rules)),
  support = quality(medium_income_top_rules)$support,
  confidence = quality(medium_income_top_rules)$confidence,
  group = "Medium Income"
)
low_income_summary <- data.frame(
  product = labels(lhs(low_income_top_rules)),
  support = quality(low_income_top_rules)$support,
  confidence = quality(low_income_top_rules)$confidence,
  group = "Low Income"
)
all_summary <- rbind(high_income_summary, medium_income_summary, low_income_summary)

ggplot(all_summary, aes(x = support, y = confidence, color = group)) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Porównanie reguł asocjacyjnych w zależności od tego w jakiej grupie dochodowej jest dany kraj",
    x = "Support",
    y = "Confidence",
    color = "Grupa Dochodowa"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Zakresy parametrów
supp_range <- seq(0.005, 0.14, by = 0.005)
conf_range <- seq(0.2, 0.7, by = 0.1)
results <- list()

rule_counts <- data.frame(Support = numeric(0), Confidence = numeric(0), NumRules = numeric(0), IncomeGroup = character(0))
avg_lifts <- data.frame(Support = numeric(0), Confidence = numeric(0), AvgLift = numeric(0), IncomeGroup = character(0))

run_experiment <- function(transactions, income_group) {
  local_rule_counts <- data.frame(Support = numeric(0), Confidence = numeric(0), NumRules = numeric(0), IncomeGroup = character(0))
  local_avg_lifts <- data.frame(Support = numeric(0), Confidence = numeric(0), AvgLift = numeric(0), IncomeGroup = character(0))
  
  for (supp in supp_range) {
    for (conf in conf_range) {
      cat("Eksperyment - Support:", supp, " Confidence:", conf, " dla grupy:", income_group, "\n")
      rules <- apriori(transactions, parameter = list(supp = supp, conf = conf, minlen = 2))
      num_rules <- length(rules)
      if (num_rules > 0) {
        local_rule_counts <- rbind(local_rule_counts, data.frame(Support = supp, Confidence = conf, NumRules = num_rules, IncomeGroup = income_group))
        avg_lift <- mean(quality(rules)$lift, na.rm = TRUE)
        local_avg_lifts <- rbind(local_avg_lifts, data.frame(Support = supp, Confidence = conf, AvgLift = avg_lift, IncomeGroup = income_group))
      }
    }
  }
  list(rule_counts = local_rule_counts, avg_lifts = local_avg_lifts)
}

high_income_results <- run_experiment(high_income_trans, "High Income")
medium_income_results <- run_experiment(medium_income_trans, "Medium Income")
low_income_results <- run_experiment(low_income_trans, "Low Income")

rule_counts <- rbind(high_income_results$rule_counts, medium_income_results$rule_counts, low_income_results$rule_counts)
avg_lifts <- rbind(high_income_results$avg_lifts, medium_income_results$avg_lifts, low_income_results$avg_lifts)

high_income_rule_counts <- aggregate(NumRules ~ Support + Confidence, data = rule_counts[rule_counts$IncomeGroup == "High Income", ], sum)
print(high_income_rule_counts)
medium_income_rule_counts <- aggregate(NumRules ~ Support + Confidence, data = rule_counts[rule_counts$IncomeGroup == "Medium Income", ], sum)
print(medium_income_rule_counts)
low_income_rule_counts <- aggregate(NumRules ~ Support + Confidence, data = rule_counts[rule_counts$IncomeGroup == "Low Income", ], sum)
print(low_income_rule_counts)

ggplot(rule_counts, aes(x = Support, y = NumRules, color = Confidence)) +
  geom_point(size = 2) +
  facet_wrap(~IncomeGroup) +
  labs(title = "Zależność liczby reguł od Support i Confidence dla różnych grup dochodowych", 
       x = "Support", y = "Liczba reguł") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()
# Wykres średniego lift w zależności od Support i Confidence:
ggplot(avg_lifts, aes(x = Support, y = AvgLift, color = Confidence)) +
  geom_point(size = 2) +
  facet_wrap(~IncomeGroup) +
  labs(title = "Średni Lift w zależności od Support i Confidence dla różnych grup dochodowych", 
       x = "Support", y = "Średni Lift") +
  scale_color_gradient(low = "blue", high = "red") +
  theme_minimal()
