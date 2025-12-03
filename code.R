# Packages --------------------
library(readxl)
library(tidyverse)
library(vegan)

# Data, Standard length and Weight -------------------
data = read_excel("nannostomus.xlsx")
summary(data$CP)
sd(data$CP)
summary(data$Peso)
sd(data$Peso)

# Repletion index ---------------------
table(data$GR)
prop.table(table(data$GR)) * 100
data$GR

# FO and FPD ----------------------
data_filtrado <- data %>%
  filter(GR %in% c(2, 3))

diet = data_filtrado[,11:20]
diet_pa = decostand(diet, method = "pa")

FO_percent <- colSums(diet_pa) / nrow(diet_pa) * 100
FO_percent

FPD = colSums(diet)/nrow(diet)
FPD

set.seed(123) 

accum <- specaccum(diet_pa,
                   method = "random",     
                   permutations = 1000)  



accum_df <- data.frame(
  sites = accum$sites,
  richness = accum$richness,
  sd = accum$sd
)

accum_df$upper <- accum_df$richness + 1.96 * accum_df$sd
accum_df$lower <- pmax(accum_df$richness - 1.96 * accum_df$sd, 0)

fig2 = ggplot(accum_df, aes(x = sites, y = richness)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "skyblue", alpha = 0.4) +
  geom_line(size = 1.4, color = "blue4") +
  geom_point(size = 2, color = "blue4") +
  labs(x = "Number of stomachs",
       y = "Food items richness") +
  theme_classic(base_size = 18)+
  scale_x_continuous(limits = c(0,65), breaks = seq(0,65,by = 5))+
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12, by = 2))

fig2

ggsave("Figure_2.jpg", fig2, width = 6, height = 5)
