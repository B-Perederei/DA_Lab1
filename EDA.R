library(tidyverse)
library(ggplot2)
library(qqplotr)
library("scales")
library(ggridges)
library(patchwork)
library(viridis)
library(fmsb)
library(car)
library(ggpubr)
library(formattable)
options(dplyr.width = Inf)

# God, give your computer powers to run this file!
apps <- read_csv("Data/Google-Playstore-tidied.csv", locale = locale(encoding = "UTF-8"))

# Factorize columns
apps <- apps %>% mutate(
    Category = as.factor(Category),
    ContentRating = as.factor(ContentRating),
    Rating = as.ordered(Rating),
    MinimumInstalls = as.ordered(MinimumInstalls)
)

#------------------------------------------------------------------------------------------------------
# Bohdan Perederei
# Дескриптивний аналіз категорії
appsByCategory <- apps %>% group_by(Category) %>% count

summary(appsByCategory$n)
appsByCategory <- ggplot(appsByCategory, aes(x = reorder(Category, -n), y = n)) + 
	geom_bar(fill="skyblue", stat="identity") +
	labs(x = "Category", y = "Number of apps") +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("Plots/appsByCategory.png", plot = appsByCategory)

appsByCategory <- apps %>% group_by(Category) %>% count
gampelAppsByCategory <- appsByCategory %>% filter(n < median(n) - 3*mad(n) | n > median(n) + 3*mad(n)) %>% arrange(desc(n))

appsByCategory <- apps %>% group_by(Category) %>% count
qqAppsByCategory <- ggplot(appsByCategory, aes(sample = n)) +
	stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
	labs(x = "Quntiles of normal distribution", y = "Number of Apps") +
	theme(axis.title = element_text(size = 12),
	axis.text = element_text(size = 12))
ggsave("Plots/qqAppsByCategory.png", plot = qqAppsByCategory)

logQQAppsByCategory <- ggplot(appsByCategory, aes(sample = log(n))) +
	stat_qq_point() + stat_qq_line() + stat_qq_band(fill="skyblue") +
	labs(x = "Quntiles of normal distribution", y = "Number of Apps") +
	theme(axis.title = element_text(size = 12),
	axis.text = element_text(size = 12))
	ggsave("Plots/logQQAppsByCategory.png", plot = logQQAppsByCategory)

# MinimumInstalls
installsCount <- ggplot(apps, aes(x = MinimumInstalls)) + 
	geom_bar(fill="skyblue") +
	scale_y_continuous(labels = scales::comma) +
	labs(x = "Install category", y = "Number of apps") +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("Plots/installsCount.png", plot = installsCount)

installsCountEnlarged <- ggplot(apps, aes(x = MinimumInstalls)) + 
	geom_bar(fill="skyblue") +
	scale_y_continuous(labels = scales::comma) +
	labs(x = "Install category", y = "Number of apps") +
	coord_cartesian(ylim = c(0, 100)) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("Plots/installsCountEnlarged.png", plot = installsCountEnlarged)

# MaximumInstalls
summary(apps$MaximumInstalls)

gampelMaximumInstalls <- apps %>% filter(MaximumInstalls < median(MaximumInstalls) - 3*mad(MaximumInstalls) | MaximumInstalls > median(MaximumInstalls) + 3*mad(MaximumInstalls)) %>%
arrange(desc(MaximumInstalls), desc(MaximumInstalls))

# EDA
developers <- apps %>% group_by(DeveloperId) %>% summarise(MaximumInstalls = sum(MaximumInstalls)) %>% arrange(desc(MaximumInstalls), desc(MaximumInstalls)) %>% print(n = 100)
installsByCategory <- apps %>% filter(DeveloperId != "Google LLC" & MaximumInstalls < 100000000 & MaximumInstalls > 10000000) %>%
  group_by(Category) %>% 
  summarise(MaximumInstalls = mean(MaximumInstalls))

appsWithoutOutlies <- apps %>% filter(DeveloperId != "Google LLC" & MaximumInstalls < 500000000) %>%
	group_by(Category) %>% 
  	summarise(MaximumInstalls = mean(MaximumInstalls))

installsByCategoryPlot <- ggplot(appsWithoutOutlies, aes(x = reorder(Category, -MaximumInstalls), y = MaximumInstalls), stat="identity") + 
	geom_bar(fill="skyblue", stat="identity") +
	labs(x = "Category", y = "Billionss of Installs") +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("Plots/installsByCategory.png", plot = installsByCategoryPlot)

# Heatmap for Categoty + MinimumInstalls
installs1 <- apps %>% filter(MinimumInstalls > 0 & MaximumInstalls <= 50) %>%
	group_by(Category, MinimumInstalls) %>% 
  	count()

installs2 <- apps %>% filter(MinimumInstalls > 50 & MaximumInstalls <= 10000) %>%
	group_by(Category, MinimumInstalls) %>% 
  	count()

installs3 <- apps %>% filter(MinimumInstalls > 10000 & MaximumInstalls <= 1000000) %>%
	group_by(Category, MinimumInstalls) %>% 
  	count()

installs4 <- apps %>% filter(MinimumInstalls > 1000000 & MaximumInstalls <= 500000000) %>%
	group_by(Category, MinimumInstalls) %>% 
  	count()

installs5 <- apps %>% filter(MinimumInstalls > 500000000) %>%
	group_by(Category, MinimumInstalls) %>% 
  	count()

installsByCategoryHeatMap1 <- ggplot(installs1, aes(MinimumInstalls, Category, fill = n)) + 
	geom_tile() +
	scale_fill_gradient(low="white", high="red") +
	labs(x = "Minimum Installs", y = "Category", fill = "Number of apps")
ggsave("Plots/installsByCategoryHeatMap1.png", plot = installsByCategoryHeatMap1)

installsByCategoryHeatMap2 <- ggplot(installs2, aes(MinimumInstalls, Category, fill = n)) + 
	geom_tile() +
	scale_fill_gradient(low="white", high="red") +
	labs(x = "Minimum Installs", y = "Category", fill = "Number of apps")
ggsave("Plots/installsByCategoryHeatMap2.png", plot = installsByCategoryHeatMap2)

installsByCategoryHeatMap3 <- ggplot(installs3, aes(MinimumInstalls, Category, fill = n)) + 
	geom_tile() +
	scale_fill_gradient(low="white", high="red") +
	labs(x = "Minimum Installs", y = "Category", fill = "Number of apps")
ggsave("Plots/installsByCategoryHeatMap3.png", plot = installsByCategoryHeatMap3)

installsByCategoryHeatMap4 <- ggplot(installs4, aes(MinimumInstalls, Category, fill = n)) + 
	geom_tile() +
	scale_fill_gradient(low="white", high="red") +
	labs(x = "Minimum Installs", y = "Category", fill = "Number of apps")
ggsave("Plots/installsByCategoryHeatMap4.png", plot = installsByCategoryHeatMap4)

installsByCategoryHeatMap5 <- ggplot(installs5, aes(MinimumInstalls, Category, fill = n)) + 
	geom_tile() +
	scale_fill_gradient(low="white", high="red") +
	labs(x = "Minimum Installs", y = "Category", fill = "Number of apps")
ggsave("Plots/installsByCategoryHeatMap5.png", plot = installsByCategoryHeatMap5)

# Editor's Choice correlation
appsEditorsChoice <- apps %>% filter(MaximumInstalls > 10000 & MaximumInstalls < 10000000)
EditorsChoiceCorrelation <- ggplot(appsEditorsChoice, aes(x = MaximumInstalls, y = EditorsChoice, fill = EditorsChoice)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::comma)
ggsave("Plots/EditorsChoiceCorrelation.png", plot = EditorsChoiceCorrelation)

#------------------------------------------------------------------------------------------------------
# Anna Bozhenko

options(scipen = 999)
# ---------------------------------------------
#								FUNCTIONALITY
# ---------------------------------------------

count_bins <- function(variation) {
  n <- variation %>% unique %>% length
  return(1 + ceiling(log(n, base = 2)))
}

three_sigma <- function(variation) {
  ub <- mean(variation) + 3*sd(variation)
  lb <- mean(variation) - 3*sd(variation)
  return(c(lb, ub))
}

hampel_filter <- function(variation) {
  v_median <- median(variation)
  MADM_v = (function(t) abs(t - v_median))(variation) %>% median * 1.4826
  ub <- v_median + 3*MADM_v
  lb <- v_median - 3*MADM_v
  return(c(lb, ub))
}

IQR <- function(variation) {
  (quantile(variation, 0.75) - quantile(variation, 0.25)) %>% return
}

boxplot_filter <- function(variation) {
  iqr <- IQR(variation)
  ub <- quantile(variation, 0.75) + 1.5*iqr
  lb <- quantile(variation, 0.25) - 1.5*iqr
  return(c(lb, ub))
}

MADM <- function(variation) {
  v_median <- median(variation)
  (function(t) abs(t - v_median))(variation) %>% median * 1.4826 %>% return
}

compare_distribs <- function(variation, method) {
  thresholds <- method(variation)
  altered_variation <- variation[variation >= thresholds[1] & variation <= thresholds[2]]
  
  t1 <- tibble(`With outliers` = c(TRUE, FALSE),
                `Mean` = c(mean(variation), mean(altered_variation)),
               `Standart deviation` = c(sd(variation), sd(altered_variation)),
               `1 Qu` = c(quantile(variation, 0.25), quantile(altered_variation, 0.25)),
               `Median` = c(median(variation), median(altered_variation)),
               `3 Qu` = c(quantile(variation, 0.75), quantile(altered_variation, 0.75)),
               `Min` = c(min(variation), min(altered_variation)),
               `Max` = c(max(variation), max(altered_variation))
              )
                
  print(t1)
  
  print("Percentage change")
  cat("\nMean: ", 100 - (t1$Mean[2] * 100 / t1$Mean[1]), "%")
  cat("\nStandart deviation: ", 100 - (t1$`Standart deviation`[2] * 100 / t1$`Standart deviation`[1]), "%")
  cat("\n1 Qu: ", 100 - (t1$`1 Qu`[2] * 100 / t1$`1 Qu`[1]), "%")
  cat("\nMedian: ", 100 - (t1$Median[2] * 100 / t1$Median[1]), "%")
  cat("\n3 Qu: ", 100 - (t1$`3 Qu`[2] * 100 / t1$`3 Qu`[1]), "%")
  cat("\nMin: ", 100 - (t1$`Min`[2] * 100 / t1$`Min`[1]), "%")
  cat("\nMax: ", 100 - (t1$`Max`[2] * 100 / t1$`Max`[1]), "%")
}
# ---------------------------------------------
#								EXPLORATION
# ---------------------------------------------
# розподіл цін (USD) за кожен рік
years_with_costs <- apps %>% drop_na(Released) %>% filter(Currency == "USD" & Price > 0) %>% select(Released, Price)
years_with_costs <- years_with_costs %>% arrange(Released)
years_with_costs["Released"] <- format(years_with_costs$Released, "%Y")
# графік “вусатих скриньок” цін додатків для кожного року 
p <- ggplot(years_with_costs, aes(x = factor(Released), y = log(Price), color = Released)) +
  geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, years_with_costs$Price %>% log %>% max, 0.5)) +
  labs(title = "Apps costs per each year", x = "Years") + 
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

print(p)
ggsave("Plots/apps_costs_per_each_year.png", p)

# проаналізувати дискриптивні характеристики цін за кожен період
to_2014 <- years_with_costs %>% filter(Released <= 2014)
print(compare_distribs(to_2014$Price, hampel_filter))
print(hampel_filter(to_2014$Price))
print(boxplot_filter(to_2014$Price))
print(IQR(to_2014$Price))
#---------
between_15_20 <- years_with_costs %>% filter(Released >= "2015" & Released <= "2020")
print(compare_distribs(between_15_20$Price, hampel_filter))
print(hampel_filter(between_15_20$Price))
print(boxplot_filter(between_15_20$Price))
print(IQR(between_15_20$Price))
#---------
y_2021 <- years_with_costs %>% filter(Released == "2021")
print(compare_distribs(y_2021$Price, hampel_filter))
print(hampel_filter(y_2021$Price)[2])
print(boxplot_filter(y_2021$Price)[2])
print(IQR(y_2021$Price))

#Дослідити аудутиторію безкоштовних додатків впродовж років
free_apps_users <- apps %>% drop_na(c("ContentRating", "Released")) %>% subset(Free & ContentRating != "Everyone") %>% select(Released, ContentRating)
free_apps_users <- free_apps_users %>% mutate(Released = format(Released, "%Y"))

free_apps_users <- free_apps_users %>% group_by(Released, ContentRating) %>% count
groups <- unique(free_apps_users$ContentRating)

for (g in groups) {
  print(g)
  print(summary((free_apps_users %>% filter(ContentRating == g))$n))
}
#графік кількості безкоштовних додатків для користувачів конкретного типу за кожен рік(окрім Everyone)
p <- ggplot(free_apps_users, aes(x = Released, y = n, color = ContentRating, group = ContentRating)) +
  geom_point(color = "black") +
  geom_line(lwd = 1) +
  labs(title = "Free apps for every age-category(except ambiguous \"Everyone\") per each year", x = "Release year", y = "Apps number") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        plot.title = element_text(size = 30))
  
print(p)  
ggsave("Plots/users_per_year.png")

# ------------------------------------------------------------------------------------------------
# Чи платні додатки орієнтовані на певну вікову групу користувачів? 

cost_users <- apps %>% drop_na(c(ContentRating, Free)) %>% select(ContentRating, Free)

cost_users <- cost_users %>% group_by(ContentRating, Free) %>% count

# графік платних додатків для кожної вікової групи
p <- ggplot(cost_users %>% filter(!Free), aes(x = ContentRating, y = n)) +
  geom_bar(stat = "identity", width = 0.1) +
  labs(title = "Paid apps for each age group", x = "Age group", y = "Apps number") +
  scale_y_continuous(breaks = seq(0, max((cost_users %>% filter(!Free))$n), 5000)) +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 30))

print(p)
ggsave("Plots/paid_apps_orientation.png", p)
# ------------------------------------------------------------------------------------------------
# ціни на платні додатки серед молоді (Everyone 10+, Teen, Mature 17+)

paid_apps_youngers <- apps %>% drop_na(c(Free, ContentRating)) %>% filter(Currency == "USD" & !Free & !(ContentRating %in% c("Everyone", "Adults only 18+")))
paid_apps_youngers <- paid_apps_youngers %>% select(Price)
# графік цін додатків орієнтованих для дітей та молоді
p <- ggplot(paid_apps_youngers, aes(x = log(Price)), y = after_stat(density)) +
  geom_histogram() +
  labs(title = "Prices for youngsters") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))

print(p)
ggsave("Plots/youngsters_prices.png", p)

x_orig <- paid_apps_youngers$Price

print(compare_distribs(x_orig, hampel_filter))
print(compare_distribs(x_orig, boxplot_filter))

threshold <- boxplot_filter(x_orig)[2]
paid_apps_youngers_altered <- paid_apps_youngers %>% filter(Price <= threshold)

print(nrow(paid_apps_youngers_altered) / nrow(paid_apps_youngers))
summary(paid_apps_youngers_altered$Price)
# оцінка верхньої межі за квартилями
p1 <- ggplot(paid_apps_youngers, aes(x = log(Price))) +
  geom_boxplot(stat = "boxplot") + 
  coord_flip() +
  scale_x_continuous(breaks = seq(-4, max(log(x_orig)), 0.5)) +
  labs(title = "Original") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))

p2 <- ggplot(paid_apps_youngers_altered, aes(x = Price)) +
  geom_boxplot() + 
  coord_flip() +
  scale_x_continuous(breaks = seq(-4, max(paid_apps_youngers_altered$Price), 1)) +
  labs(title = "After Boxplot filter") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))

p <- p1 + p2
print(p)
ggsave("Plots/youngsters_prices_filtering.png", p)

# ------------------------------------------------------------------------------------------------
# кількість встановлень платних додатків (USD)

a <- apps %>% drop_na(Price) %>% filter(Currency == "USD" & Price > 0)

# розподілу цін для додатків
threshold <- boxplot_filter(a$Price)[2]
p <- ggplot(a, aes(x = log(Price))) + 
  geom_boxplot() + coord_flip() +
  geom_vline(xintercept=log(threshold), col = "blue", linewidth = 1) +
  labs(title = "Prices distribution") +
  scale_x_continuous(breaks = seq(-1, max(a$Price), 0.5)) +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))

print(p)

ggsave("Plots/prcices_boxplot.png", p)

a <- a %>% mutate(Released = format(Released, "%Y"), isExpensive = Price > threshold)
# кількість дорогих та дешевших додатків залежно від категорії встановлень
p <- ggplot(a, aes(x = MinimumInstalls, fill = isExpensive)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of expensive and cheap apps in each installs-n category", y = "Number of apps") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

print(p)
ggsave("Plots/number_of_paid_apps_per_install_category.png", p)   

# графік мінімальних встановлень платних додатків залежно від ціни
p <- ggplot(a, aes(x = Price, y = MinimumInstalls, color = isExpensive)) +
  geom_point(size = 2) +
  labs(title = "Minimum installs of paid apps", y = "Minimum installs number") +
  theme(axis.title = element_text(size = 25),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 30),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20))

print(p)
ggsave("Plots/paid_apps_installs_tendency.png", p)

#------------------------------------------------------------------------------------------------------
# Kubrak Valentin
#
#-------------------------------------------
#           Descriptive analysis
#-------------------------------------------

#------------------Size---------------------

p <- qqPlot(apps$Size)
ggsave("Plots/qqPlot_size.png", p)

p <- qqPlot(log(apps$Size))
ggsave("Plots/qqPlot_logSize.png", p)

summary(apps$Size)

p <- ggplot(apps, aes(x = Size)) + 
  geom_histogram(bins = 80, color="#00BFFF", fill = '#87CEEB', alpha=0.6, position = 'identity')
ggsave("Plots/full_hist_size.png", p)

gg1 <- ggplot(apps %>% filter(Size<160), aes(x = Size)) + 
  geom_histogram(bins = 40, color="#0000FF", fill = '#87CEEB', alpha=0.6, position = 'identity')

gg2 <- ggplot(apps %>% filter(Size>160), aes(x = Size)) + 
  geom_histogram(bins = 40, color="#800000", fill = '#FA8072', alpha=0.6, position = 'identity')

p <- ggarrange(gg1, gg2, ncol = 2, nrow = 1)
ggsave("Plots/doubled_hist_size.png", p)

p <- ggplot(apps, aes(x = MaximumInstalls, y = log(Size))) +
  geom_boxplot(
    color="#66CDAA",
    fill="#66CDAA",
    alpha=0.2,
    outlier.colour="#008080",
    outlier.fill="#008080",
    outlier.size=3
  )
ggsave("Plots/boxplot_maxInst_logSize.png", p)
   
#--------------Minimum Android--------------

p <- ggplot(apps %>% filter(Rating > 0), aes(x = MinimumAndroid, fill = as.factor(Rating))) + 
  geom_bar(position = position_stack(reverse = TRUE)) +
  theme_light() +
  labs(x = "Мінімальнонеобхідна версія Android", y = "Кількість",
       title = "Кількість додатків для кожної версії Android")
ggsave("Plots/bar_minAndroid.png", p)

without_na = subset(apps, !is.na(MinimumAndroid))
#Точковий графік відношення MinimumAndroid до MaximumInstalls
p <- ggplot(without_na, aes(x = MinimumAndroid, y = MaximumInstalls)) +
  geom_point()
ggsave("Plots/point_minAndroid_maxInst.png", p)

#Аби отримати більш менш рівномірний розподіл, прологарифмуємо змінну MaximumInstalls
p <- ggplot(apps, aes(x = MinimumAndroid, y = log(MaximumInstalls))) +
  geom_point()
ggsave("Plots/point_minAndroid_logMaxInst.png", p)

#boxplot відношення MinimumAndroid до MaximumInstalls
p <- ggplot(apps, aes(x = MinimumAndroid, y = log(MaximumInstalls))) +
  geom_boxplot(
    color="blue",
    fill="blue",
    alpha=0.2,
    notch=TRUE,
    notchwidth = 0.8,
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  )
ggsave("Plots/boxplot_minAndroid_logMaxInst.png", p)

#boxplot відношення MinimumAndroid до Size
p <- ggplot(without_na, aes(x = MinimumAndroid, y = log(Size))) +
  geom_boxplot(
    color="blue",
    fill="blue",
    alpha=0.2,
    notch=TRUE,
    notchwidth = 0.8,
    outlier.colour="red",
    outlier.fill="red",
    outlier.size=3
  )
ggsave("Plots/boxplot_minAndroid_logSize.png", p)

#-----------------Released------------------

p <- ggplot(apps, aes(x = Released)) + 
  geom_histogram(bins = 40, color="#B22222", fill = '#FA8072', alpha=0.6, position = 'identity')
ggsave("hist_released.png", p)

p <- ggplot(apps, aes(x = Released, y = Size)) +
  geom_boxplot(
    color="#CD5C5C",
    fill="#CD5C5C",
    alpha=0.2,
    outlier.colour="#B22222",
    outlier.fill="#B22222",
    outlier.size=3
  )
ggsave("Plots/boxplot_released_size.png", p)

p <- ggplot(apps, aes(x = Released, y = log(Size))) +
  geom_boxplot(
    color="#FA8072",
    fill="#FA8072",
    alpha=0.2,
    outlier.colour="#B22222",
    outlier.fill="#B22222",
    outlier.size=3
  )
ggsave("Plots/boxplot_released_logSize.png", p)

#----------------Last Updated---------------

p <- ggplot(apps, aes(x = LastUpdated)) + 
  geom_histogram(bins = 50, color="#C71585", fill = '#FFB6C1', alpha=0.6, position = 'identity')
ggsave("Plots/hist_lastUpdated.png", p)

p <- ggplot(apps, aes(x = LastUpdated, y = Rating)) +
  geom_boxplot(
    color="#FF4500",
    fill="#FF4500",
    alpha=0.2,
    outlier.colour="#FF8C00",
    outlier.fill="#FF8C00",
    outlier.size=3
  )
ggsave("Plots/boxplot_lastUpdated_rating.png", p)

#-------------------------------------------
#           Data visualization
#-------------------------------------------

#First graph-(1/2)--------------------------
#Відношення мінімально необхідної версії android до розміру додатку
   
list1 <- c()
for (i in sort(unique(apps$MinimumAndroid))){
  list1 <- c(list1, mean((apps %>% filter(MinimumAndroid == i))$Size, na.rm = TRUE))
}
graph_df1 <- data.frame(Version = sort(unique(apps$MinimumAndroid)), Size = list1)

p <- ggplot(graph_df1, aes(x = Version, y = Size, group=1)) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  geom_line(color = "black", linewidth = 0.8) +
  theme_light() +
  labs(x = "Версія Android", y = "Середній розмір додатку",
       title = "Відношення мінімально необхідної версії android до розміру додатку")
ggsave("Plots/plot_minAndroid_to_avgSize.png", p)

#First graph-(2/2)--------------------------
#Відношення дати релізу до розміру додатку

list2 <- c()
for (i in sort(unique(str_sub(apps$Released, start = 1, end = 4)))){
  list2 <- c(list2, mean((apps %>% filter(str_sub(Released, start = 1, end = 4) == i))$Size, na.rm = TRUE))
}
graph_df2 <- data.frame(DateOfRelease = sort(unique(str_sub(apps$Released, start = 1, end = 4))), Size = list2)


p <- ggplot(graph_df2, aes(x = DateOfRelease, y = Size, group=1)) +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  geom_line(color = "black", linewidth = 0.8) +
  theme_light() +
  labs(x = "Дата релізу", y = "Середній розмір додатку",
       title = "Відношення дати релізу до розміру додатку")
ggsave("Plots/plot_release_to_avgSize.png", p)

#Second graph-------------------------------
#Маємо гіпотезу, що чим новіші у додатку оновлення, тим білший в нього рейтинг
#Перевіримо це:

p <- ggplot(apps %>% filter(Rating > 0), aes(x = Rating, y = str_sub(LastUpdated, start = 1, end = 4), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(option = "C") +
  labs(x = "Рейтинг", y = "Рік останнього оновлення",
       title = 'Вплив дати останнього оновлення на рейтинг') +
  theme_light() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 9),
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8))
ggsave("Plots/plot_lastApdated_to_rating.png", p)

#Third graph--------------------------------
#Відношення мінімально необхідної версії android до середнього розміру додатку

temp_df <- subset(apps, AppId != "com.shootbubble.bubbledexlue")
list3 <- c()
for (i in sort(unique(temp_df$MinimumAndroid))){
  list3 <- c(list3, mean((temp_df %>% filter(MinimumAndroid == i))$MaximumInstalls, na.rm = TRUE))
}
graph_df3 <- as.data.frame(matrix(list3, ncol=length(list3)))
colnames(graph_df3) <- sort(unique(apps$MinimumAndroid[!is.na (apps$MinimumAndroid)]))
graph_df3 <- rbind(rep(max(list3), length(list3)), rep(0,length(list3)), graph_df3)


p <- radarchart(graph_df3,
           pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8 )
ggsave("Plots/radarchart_minAndroid_to_maxInst.png", p)

#------------------------------------------------------------------------------------------------------

#---------------------------
# Pylypchuk Artem 

apps <- apps %>% mutate(
    Rating = as.numeric(Rating),
)

# Descriptive analysis of the rating
r_data <- apps %>% 
  filter(!is.na(Rating)) %>% 
  select(Rating)
summary(r_data %>% select(Rating))
r_data %>% 
  filter(Rating < median(Rating)-3*mad(Rating) | Rating > median(Rating)+3*mad(Rating)) %>%
  arrange(desc(Rating))

r_data <- r_data %>% 
  filter(Rating != 0)
summary(r_data %>% select(Rating))
r_data %>% 
  filter(Rating < median(Rating)-3*mad(Rating) | Rating > median(Rating)+3*mad(Rating)) %>%
  arrange(desc(Rating))
#---------------------------

# The number of apps with content rating in percent
data <- apps %>%
  group_by(ContentRating) %>%
  summarise(count = n()) %>%
  mutate(count = formattable::percent(count/sum(count))) %>%
  arrange(count)
#---------------------------

# EDA
# The number of apps by rating
data_1 <- apps %>% 
  select(ContentRating, Rating) %>%
  filter(!is.na(Rating))

ggplot(data_1, aes(x=Rating, fill=ContentRating)) + 
  geom_bar(color = "black") +
  labs(x = "Rating", y = "Count", fill="Content Rating", title="The number of apps by rating") +
  scale_fill_brewer(breaks = c(data_1$ContentRating), direction=-1) +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))
ggsave("Plots/NumberOfApps.png")
#---------------------------

# The number of apps by content rating with zero rating and without
data_2 <- apps %>%
  select(ContentRating) %>%
  mutate(Count = (apps$Rating == 0)) %>%
  filter((!is.na(apps$Rating)))

ggplot(data_2, aes(x=fct_infreq(ContentRating), fill=Count)) + 
  geom_bar(position = "dodge", color="black") +
  labs(x="Content Rating", fill="Zero Rating?", title="The number of apps by content rating with zero rating and without") +
  scale_fill_discrete(labels = c(names(table(data_2$Count)))) +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))
ggsave("Plots/NumberOfAppsWithZeroAndWithout.png")
#---------------------------

# Content Rating by MaximumInstalls and RatingCount
data_3 <- apps %>% 
  select(MaximumInstalls, RatingCount, ContentRating)%>%
  filter(!is.na(MaximumInstalls) & !is.na(RatingCount))

ggplot(data_3, aes(x=MaximumInstalls, y=RatingCount, color=ContentRating)) +
  geom_point(size = 4) +
  labs(x = "MaximumInstalls", y = "RatingCount", title = "Content Rating by MaximumInstalls and RatingCount", color="ContentRating") +
  scale_color_brewer(breaks = c(data_3$ContentRating), direction=-1) +
  theme(plot.title = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))
ggsave("Plots/ContentRatingByMIandRC.png")
#---------------------------

# The number of apps by rating
data_4 <- apps %>% 
  select(ContentRating, Rating) %>%
  filter(!is.na(Rating) & Rating > 0)

ggplot(data_4, aes(x=Rating, fill=ContentRating)) + 
  geom_bar(color = "black") +
  labs(x = "Rating", y = "Count", fill="Content Rating", title="The number of apps by rating") +
  scale_fill_brewer(breaks = c(data_4$ContentRating), direction=-1) +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))
ggsave("Plots/NumberOfAppsByRating.png")
#---------------------------

# Average Rating of apps by content rating
data_5 <- aggregate(data_4$Rating, list(data_4$ContentRating), FUN=mean)
print(data_5)
# Average Rating of apps
print(mean(c(data_4$Rating), na.rm=TRUE))
#---------------------------

# Content Rating by MaximumInstalls and Rating
data_6 <- apps %>% 
  select(MaximumInstalls, Rating, ContentRating)%>%
  filter(!is.na(MaximumInstalls) & !is.na(Rating) & (Rating != 0))

ggplot(data_6, aes(x=Rating, y=MaximumInstalls, color=ContentRating)) +
  geom_point(size = 4) +
  labs(x = "Rating", y = "MaximumInstalls", title = "Content Rating by MaximumInstalls and Rating", color="ContentRating") +
  scale_color_brewer(breaks = c(data_6$ContentRating), direction=-1) +
  theme(plot.title = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 10))
ggsave("Plots/ContentRatingbyMaximumInstallsandRating.png")
#---------------------------