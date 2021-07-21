install.packages(c("devtools", 'ggplot2', 'tidymodels', "readr", "mgcv"))
devtools::install_github("ldurazo/kaggler")

library(ggplot2)
library(tidymodels)
library(readr)
library(mgcv)
library(kaggler)

kag_api <- 'kaggle datasets download -d gpreda/covid-world-vaccination-progress'

# make sure your kaggle.json file is on your project directory
kgl_auth(creds_file = 'kaggle.json')

dataset_owner = 'gpreda/covid-world-vaccination-progress'

response <- kgl_datasets_download_all(owner_dataset = dataset_owner)

download.file(response[["url"]], "vaccinations.zip", mode="wb")

unzip_result <- unzip("vaccinations.zip", exdir = getwd(), overwrite = TRUE)

csv_filename = "country_vaccinations.csv"

vaccinations <- read.csv(csv_filename, header = TRUE)

vaccinations <- vaccinations %>%
  mutate(date=as.Date(date, format = "%Y-%m-%d"))

countries_iso = c("BRA")

vaccinations_BRA <- vaccinations %>%
  filter(
    iso_code %in% countries_iso,
    people_vaccinated_per_hundred > 0.02
  ) %>%
  mutate(days = row_number());

### REGRESSÃO ###
formula <- gam(data = vaccinations_BRA ,people_vaccinated_per_hundred~s(days, bs="cs"))
model.stats <- tidy(formula, parametric = TRUE)

pred <- predict(formula, newdata = data.frame(days=0:1214),se.fit = TRUE)
predictions <- data.frame(days=0:1214, people_vaccinated_per_hundred = pred$fit)

full_vaccination <- (predictions %>%
  filter(
    people_vaccinated_per_hundred > 100
  )
)$days[1]
last_vaccination <- max(vaccinations_BRA$days)
end_vaccinations <- full_vaccination - max(vaccinations_BRA$days) 


### GRÁFICO 1 ###
ggplot(vaccinations_BRA, aes(x = date, y = daily_vaccinations, colour = iso_code)) +
  geom_col() +
  scale_y_continuous(name="Vacinações Diárias", labels= scales::comma) +
  labs(title="Quantidade de vacinas aplicadas por dia no Brasil")

### GRÁFICO 2 ###
ggplot(vaccinations_BRA, aes(x = days,y = people_vaccinated_per_hundred)) +
  geom_col() +
  stat_smooth(method = "gam", se = TRUE, fullrange = TRUE) +
  scale_y_continuous("Porcentagem de pessoas vacinadas", limits = c(0,105)) +
  scale_x_continuous("Dias", limits = c(0,full_vaccination+model.stats$estimate), breaks = seq(from=0, to=full_vaccination+model.stats$estimate, by = 20))+
  annotate("point", x=full_vaccination, y=100) +
  annotate("text", x=full_vaccination, y=105, label=full_vaccination) +
  annotate('text', x = 150, y = 100, label = paste("Dias faltando para fim da vacinação (1ª dose): ", end_vaccinations, '+/-', round(model.stats$estimate)))
  

### GRÁFICO 3###
# ggplot(vaccinations_BRA, aes(x = days,y = people_vaccinated_per_hundred)) +
#   geom_point() +
#   annotate("point", x=full_vaccination, y=100) +
#   annotate("text", x=full_vaccination, y=105, label=full_vaccination) +
#   scale_x_continuous("Dias", limits = c(0,full_vaccination+model.stats$estimate), breaks = seq(from=0, to=full_vaccination+model.stats$estimate, by = 20)) +
#   scale_y_continuous("Porcentagem de pessoas vacinadas", limits = c(0,105)) +
#   stat_smooth(method = "gam", se = TRUE, fullrange = TRUE) + 
#   annotate('text', x = 150, y = 100, label = paste("Dias faltando para fim da vacinação (1ª dose): ", end_vaccinations, '+/-', round(model.stats$estimate)))
#   
# 
