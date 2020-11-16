library(tidyverse)
library(fredr)
library(lubridate)
library(corrplot)

fredr_set_key("xyz")

# Taking a look at unemployment
#------------------------------------
u3 <- fredr(
  series_id = "UNRATE"
) %>% 
  rename("U-3 unemployment rate"=value) %>% 
  select(-series_id)

u6 <- fredr(
  series_id = "U6RATE"
) %>% 
  rename("U-6 unemployment rate"=value) %>% 
  select(-series_id)

labor_force_participation <- fredr(
  series_id = "CIVPART"
) %>% 
  rename("Labor force participation rate"=value) %>% 
  select(-series_id)

plot_data <- u3 %>%
  full_join(u6, by="date") %>%
  full_join(labor_force_participation, by="date") %>%
  pivot_longer(c("U-3 unemployment rate","U-6 unemployment rate",
                 "Labor force participation rate"),
               names_to="measure", values_to="value", values_drop_na=TRUE)

# Plot 1: U-3 and U-6
current_plot_data <- plot_data %>% 
  filter(measure %in% c("U-3 unemployment rate","U-6 unemployment rate"))

ggplot(data=current_plot_data, aes(x=date, y=value))+
  geom_line(aes(color=measure)) +
  labs(
    title = "Unemployment rate in the US over time",
    x = "Date",
    y = "Percentage of unemployed people"
  )

ggsave("unemployment1.png", path="./plots/", width=12, height=7)

# Plot 2: Unemployment and labor participation rates
ggplot(data=plot_data, aes(x=date, y=value))+
  geom_line(aes(color=measure)) +
  labs(
    title = "Unemployment rate in the US over time",
    x = "Date",
    y = "Percentage of unemployed people /\n Percentage of people in the labor force"
  )

ggsave("unemployment2.png", path="./plots/", width=12, height=7)

# Plot 3: Unemployment and labor participation rates, incl. recession bars
# Source: https://datamatters.blog/2011/08/15/recession-bars/
recessions = as_tibble(
  read.table(
    textConnection(
  "Peak, Trough
1948-11-01, 1949-10-01
1953-07-01, 1954-05-01
1957-08-01, 1958-04-01
1960-04-01, 1961-02-01
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01
2020-02-01, 2020-10-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)
)

ggplot(data=plot_data)+
  geom_line(aes(x=date, y=value, color=measure)) +
  labs(
    title = "Unemployment rate in the US over time",
    x = "Date",
    y = "Percentage of unemployed people /\n Percentage of people in the labor force"
  ) + 
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

ggsave("unemployment3.png", path="./plots/", width=12, height=7)

# Plot 4: Corona crisis zoom in
current_plot_data <- plot_data %>% 
  filter(year(date)==2020)

current_recessions <- recessions %>%
  filter(year(Peak)==2020)

ggplot(data=current_plot_data)+
  geom_line(aes(x=date, y=value, color=measure)) +
  labs(
    title = "Unemployment rate in the US in 2020",
    x = "Date",
    y = "Percentage of unemployed people /\n Percentage of people in the labor force"
  ) + 
  geom_rect(data=current_recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

ggsave("unemployment4.png", path="./plots/", width=12, height=7)

#------------------------------------














# Okun's Law
#--------------------------


gdp <- fredr(
  series_id = "GDP"
) %>% 
  rename("GDP"=value) %>% 
  select(-series_id)

potential_gdp <- fredr(
  series_id = "GDPPOT"
) %>% 
  rename("Potential GDP"=value) %>% 
  select(-series_id)

plot_data <- u3 %>%
  full_join(u6, by="date") %>%
  full_join(gdp, by="date") %>%
  full_join(potential_gdp, by="date") %>%
  pivot_longer(c("U-3 unemployment rate","U-6 unemployment rate",
                 "GDP","Potential GDP"),
               names_to="measure", values_to="value", values_drop_na=TRUE)

# Plot 1: GDP only
current_plot_data <- plot_data %>% 
  filter(measure %in% c("GDP","Potential GDP")) %>%
  filter(year(date) < 2021 & !(year(date)==2020 & month(date)>10))

ggplot(data=current_plot_data, aes(x=date, y=value))+
  geom_line(aes(color=measure)) +
  labs(
    title = "US economic growth over time",
    x = "Date",
    y = "Production in billions USD"
  )

ggsave("okun1.png", path="./plots/", width=12, height=7)

# Plot 2: Add unemployment data
# Value used to transform the data for 2nd Y axis
# https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
coeff <- 0.003

current_plot_data <- plot_data %>% 
  filter(year(date) < 2021 & !(year(date)==2020 & month(date)>10)) %>%
  mutate(value = ifelse(
    measure %in% c("U-3 unemployment rate","U-6 unemployment rate"), value/coeff, value
    ))

ggplot(data=current_plot_data)+
  geom_line(aes(x=date, y=value, color=measure)) +
  labs(
    title = "US economic growth and unemployment over time",
    x = "Date"
  ) +
  scale_y_continuous(
    name = "Production in billions USD",
    sec.axis = sec_axis( trans=~.*coeff, name="Percentage of unemployed people")
  )

ggsave("okun2.png", path="./plots/", width=12, height=7)

# Plot 3: Add recession bars
ggplot(data=current_plot_data)+
  geom_line(aes(x=date, y=value, color=measure)) +
  labs(
    title = "US economic growth and unemployment over time",
    x = "Date"
  ) +
  scale_y_continuous(
    name = "Production in billions USD",
    sec.axis = sec_axis( trans=~.*coeff, name="Percentage of unemployed people")
  ) +
  geom_rect(data=recessions, aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), fill='pink', alpha=0.4)

ggsave("okun3.png", path="./plots/", width=12, height=7)

# Plot 4: Correlation matrix
u3q <- fredr(
  series_id = "UNRATE",
  frequency = "q"
) %>% 
  rename("U-3 unemployment rate"=value) %>% 
  select(-series_id)

u6q <- fredr(
  series_id = "U6RATE",
  frequency = "q"
) %>% 
  rename("U-6 unemployment rate"=value) %>% 
  select(-series_id)

plot_data <- u3q %>%
  # full_join(u6q, by="date") %>%
  full_join(gdp, by="date") %>%
  full_join(potential_gdp, by="date") %>%
  drop_na() %>%
  select(-date)

nums <- plot_data[,unlist(lapply(plot_data, is.numeric))]
data_cor = cor(x=nums, use="everything")

png(filename = "./plots/okun4.png", height=700, width=700)
corrplot(data_cor)
dev.off()

plot_data <- u3q %>%
  full_join(u6q, by="date") %>%
  full_join(gdp, by="date") %>%
  full_join(potential_gdp, by="date") %>%
  drop_na() %>%
  select(-date)

nums <- plot_data[,unlist(lapply(plot_data, is.numeric))]
data_cor = cor(x=nums, use="everything")

png(filename = "./plots/okun5.png", height=700, width=700)
corrplot(data_cor)
dev.off()

#--------------------------