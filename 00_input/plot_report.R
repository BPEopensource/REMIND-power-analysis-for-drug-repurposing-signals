load("00_input/P1_params.RData")

library(ggplot2)
library(ggthemes)
library(data.table)
library(tidyr)



dt = data.table( 
  var = c(rep('Demographic', length(params$OR_demo)),
          rep('Comorbidity and HC utilitation', length(params$OR_hosp) + length(params$OR_oth)),
          rep('Drugs', length(params$OR_drug))),
  
  OR = c(params$OR_demo, 
         params$OR_hosp,
         params$OR_oth, 
         params$OR_drug), 
  
  frq = c(params$p_demo, 
          params$p_hosp,
          params$p_oth, 
          params$p_drug))


plot_data <- dt %>%
  pivot_longer(
    cols = c(frq, OR),
    names_to = "measure",
    values_to = "value"
  )

plot_data = as.data.table(plot_data)

plot_data[var == 'Drugs' &  measure == 'frq' & value >= 0.15]


# ---- frequencies
ggplot(plot_data[var == 'Drugs' & measure == 'frq' & value < 0.15], aes(x = value)) +
  geom_histogram(bins = 40, boundary = 0.001, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0,
             color = "red", linetype = "dashed", linewidth = 0.4) +
  scale_x_continuous(limits = c(0, 0.15)) +
  theme_bw(base_size = 14) +
  labs(
    x = "Frequency",
    y = "Count",
    caption = "All values > 0 (minimum: 1 per 1000)",
    title = NULL
  ) +
  theme(text = element_text(family = "serif"))

ggsave("drug_frq.png", width = 6, height = 3, dpi = 300)


# ---- ORs
ggplot(plot_data[var == 'Comorbidity and HC utilitation' & measure == 'OR' & value != 1], aes(x = value)) +
  geom_histogram(bins = 40, boundary = 1,
                 fill = "steelblue", color = "white") +
  geom_vline(xintercept = 1,
             color = "red", linetype = "dashed", linewidth = 0.5) +
  theme_bw(base_size = 14) +
  labs(
    x = "OR",
    y = "Count",
    caption = "All values ≠ 1",
    title = NULL
  ) +
  theme(text = element_text(family = "serif"))

ggsave("OR.png", width = 6, height = 3, dpi = 300)










