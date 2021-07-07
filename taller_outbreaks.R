rm(list=ls())

# topic -------------------------------------------------------------------

# R0, CFR, projection
# https://www.reconlearn.org/post/real-time-response-1.html
# https://www.reconlearn.org/post/real-time-response-2.html 

#' español
#' https://www.reconlearn.org/post/real-time-response-1-spanish.html
#' https://www.reconlearn.org/post/real-time-response-2-spanish.html

# libraries ---------------------------------------------------------------

library(readxl)
library(outbreaks)
library(incidence)
library(epicontacts)
library(distcrete)
library(epitrix)
library(EpiEstim)
library(projections)
library(ggplot2)
library(magrittr)
library(binom)
library(ape)
library(outbreaker2)
library(tidyverse)
library(naniar)
library(binom)
library(janitor)
#library(here)


# _ PART 1 _ ---------------------------------------------------------------


# importa -----------------------------------------------------------------

linelist <- read_excel("data-raw/linelist_20140701.xlsx", 
                       na = c("", "NA"))
contacts <- read_excel("data-raw/contacts_20140701.xlsx", 
                       na = c("", "NA"))

# explorar ----------------------------------------------------------------

linelist
#more data to recolect
#age?
linelist <- linelist %>% 
  #select(case_id,date_of_infection:date_of_outcome) %>% 
  gather(key,value,#-case_id,
         date_of_infection:date_of_outcome
  ) %>% 
  mutate(value=as.Date(value,format = "%Y-%m-%d")) %>% #change to lubridate
  spread(key,value)
linelist
linelist %>% glimpse()
naniar::miss_var_summary(linelist)

contacts
contacts %>% glimpse()
naniar::miss_var_summary(contacts)


# incubation period --------------------------------------------------------------------

#what is happening here?
linelist %>% 
  mutate(mistake=date_of_onset-date_of_infection) %>% 
  #values 0 or less
  filter(mistake>0)

#check for inconsistencies
linelist_mist <- linelist %>% 
  mutate(mistake=date_of_onset-date_of_infection) %>% 
  #values 0 or less
  filter(mistake<=0) %>% 
  select(case_id) %>% pull()

linelist_clean <- linelist %>% 
  filter(!(case_id %in% linelist_mist))


# case fatality ratio -----------------------------------------------------

#cfr = 60/(60+43) #known outcomes -> ok
#cfr = 60/(60+43+63) #unknown outcomes -> wrong!
table(linelist_clean$outcome, useNA = "ifany")

linelist_clean_crf <- linelist_clean %>% 
  count(outcome) %>% 
  spread(outcome,n) %>% 
  rename_all(.funs = list(make.names)) %>% 
  rename_all(.funs = list(str_to_lower)) %>% 
  mutate(n_known_outcome=death+recover,
         n_all=death+recover+x.na.,
         crf_ok=death/(n_known_outcome),
         crf_wrong=death/(n_all))

binom.confint(linelist_clean_crf$death,linelist_clean_crf$n_known_outcome,methods = "exact")
#en la poblacion
#en promedio
#the case fatality ratio is 0.58 with a 95% CI 
#from 0.48 to 0.68


# incidence ---------------------------------------------------------------

linelist_clean %>% glimpse()
#incidence of observed: onset!

#daily
i_daily <- incidence(linelist_clean$date_of_onset)
i_daily
plot(i_daily)

#weekly
i_weekly <- incidence(linelist_clean$date_of_onset,
                      interval = 7,
                      last_date = max(linelist_clean$date_of_hospitalisation,na.rm = T))
i_weekly
plot(i_weekly,border = "black") +
  scale_x_date(date_breaks = "1 week",  date_labels = "%W") 

# +
#   theme(axis.text.x = element_text(angle = 45))


# save --------------------------------------------------------------------

dir.create("data")
write_rds(linelist_clean,"data/linelist_clean.rds")
write_rds(i_daily,"data/i_daily.rds")
write_rds(i_weekly,"data/i_weekly.rds")
write_rds(contacts,"data/contacts.rds")


# _ PART 2 _ ---------------------------------------------------------------

# libraries ---------------------------------------------------------------

library(tidyverse)
library(outbreaks)
library(incidence)
library(epicontacts)
library(distcrete)
library(epitrix)
library(EpiEstim)
library(projections)
library(binom)
# library(ape)
# library(outbreaker2)
#library(here)


# read dataframes ---------------------------------------------------------

linelist_clean <- read_rds("data/linelist_clean.rds")
i_daily <- read_rds("data/i_daily.rds")
i_weekly <- read_rds("data/i_weekly.rds")
contacts <- read_rds("data/contacts.rds")

# growth rate w/ log-linear model -----------------------------------------

linelist_clean

class(i_weekly)
str(i_weekly)

#try to add a transform to undo the log transformation!
i_weekly %>% 
  as_tibble() %>% 
  #filter(counts>0) %>% 
  ggplot(aes(dates,log(counts))) +
  geom_point() +
  geom_smooth(method = "lm") + 
  xlab("date") + ylab("log weekly incidence")


# fit log linear model ----------------------------------------------------

f <- incidence::fit(i_weekly)
f
f %>% str()
f[[1]] %>% class()
f[[1]] %>% broom::tidy()
f[[1]] %>% broom::glance()

plot(i_weekly,fit = f)


# threshold ---------------------------------------------------------------

linelist_clean %>% glimpse()
linelist_clean %>% 
  mutate(diff_hosp_onset= date_of_hospitalisation - date_of_onset,
         diff_hosp_onset= as.numeric(diff_hosp_onset)) %>% 
  skimr::skim(diff_hosp_onset)

#BIAS!!!!
#we identify 22 days of maximum hospitalization
#correct by this time to avoid a substimation of the weekly incidence
#equivalent to ~3 weeks
#care must be taken to only fit to the point that there is epidemic growth
#USUALLY
#in analysis you adjust data to a gamma distribuiton
#an restrict to the 95% CI under that distribution

n_weeks_to_discard <- 3

min_date <- min(i_daily$dates)
max_date <- max(i_daily$dates) - n_weeks_to_discard * 7
# weekly truncated incidence
i_weekly_trunc <- subset(i_weekly, 
                         from = min_date, 
                         to = max_date) # discard last few weeks of data
# daily truncated incidence (not used for the linear regression but may be used later)
i_daily_trunc <- subset(i_daily, 
                        from = min_date, 
                        to = max_date) # remove last two weeks of data

# re-fit log linear model ----------------------------------------------------

f2 <- incidence::fit(i_weekly_trunc)
f2
#f %>% str()
f2[[1]] %>% class()
f2[[1]] %>% broom::tidy()
f2[[1]] %>% broom::glance()

plot(i_weekly,fit = f2)


# summarize both regressions ----------------------------------------------

#compare fitting
f[[1]] %>% broom::glance()
f2[[1]] %>% broom::glance()

#explore parameters
f2[[2]]$r
f2[[2]]$r.conf
f2[[2]]$doubling
f2[[2]]$doubling.conf

#doit it manually
log(2)/f2[[2]]$r
log(2)/rev(f2[[2]]$r.conf)



# contact tracing ----------------------------------------------------------------

# contacts %>% 
#   distinct(infector)
# contacts %>% 
#   distinct(case_id)
# contacts %>% 
#   distinct(infector,case_id)

epi_contacts <- make_epicontacts(linelist = linelist_clean,
                                 contacts = contacts,
                                 from = "infector",
                                 to = "case_id")

epi_contacts

# table(epi_contacts$contacts$source, useNA = "ifany")
epi_contacts$contacts %>% count(source)

p <- plot(epi_contacts, 
          node_shape = "gender", 
          shapes = c(m = "male", f = "female"), 
          node_color = "gender", 
          edge_color = "source", 
          selector = FALSE)
p


# match(x = contacts$case_id,table = linelist_clean$case_id) %>% 
#   length()
linelist_clean %>% 
  filter(magrittr::is_in(case_id,contacts$case_id))

#' gender seems to be 
#' more frequently infected
#' but not a better infectors

# whole data set
linelist_clean %>% 
  count(gender) %>% 
  mutate(prop=n/sum(n))

# infected
linelist_clean %>% 
  filter(magrittr::is_in(case_id,contacts$case_id)) %>% 
  count(gender) %>% 
  mutate(prop=n/sum(n))

# infector
linelist_clean %>% 
  filter(magrittr::is_in(case_id,contacts$infector)) %>% 
  count(gender) %>% 
  mutate(prop=n/sum(n))

# transmisibilidad --------------------------------------------------------


# estimacion intervalo serial ---------------------------------------------

#' tiempo entre
#' inicio de síntomas de caso infector e
#' inicio de síntomas de caso infectado

# unidad de medida = dias
si_obs <- get_pairwise(x = epi_contacts,attribute = "date_of_onset")
summary(si_obs)
hist(si_obs,breaks = 0:30)

# tidyverse alternatives
serial_interval_tibble <- epi_contacts$contacts %>% 
  as_tibble() %>% 
  left_join(y = epi_contacts$linelist %>% 
              as_tibble() %>% 
              select(id,"from_date_of_onset"=date_of_onset),
            by = c("from"="id")) %>% 
  left_join(y = epi_contacts$linelist %>% 
              as_tibble() %>% 
              select(id,"to_date_of_onset"=date_of_onset),
            by = c("to"="id")) %>% 
  mutate(serial_interval=to_date_of_onset-from_date_of_onset,
         serial_interval_num = as.double(serial_interval))

serial_interval_tibble %>% avallecam::print_inf()

serial_interval_tibble %>% 
  select(serial_interval,serial_interval_num) %>% 
  skimr::skim()

serial_interval_tibble %>% 
  ggplot(aes(x = serial_interval, y = ..density..)) +
  geom_histogram(binwidth = 1) +
  labs(x = "intervalo serial\n(tiempo entre inicio de sintomas de\ninfector e infectado)")


# usar epitrix ------------------------------------------------------------

si_fit <- epitrix::fit_disc_gamma(x = si_obs,w = 1)

si_fit

si_fit$distribution %>% str()

ggplot() +
  geom_histogram(data = serial_interval_tibble,
                 mapping = aes(x = serial_interval, y = ..density..),
                 binwidth = 1) +
  geom_line(data = si_fit$distribution$d(x = 0:30) %>% enframe(),
            mapping = aes(x = name,y = value)) +
  labs(x = "intervalo serial\n(tiempo entre inicio de sintomas de\ninfector e infectado)")


# R0 numero reproductivo -----------------------------------------------------

i_daily_trunc

config <- make_config(t_start =2,
                      t_end = length(i_daily_trunc$counts),
                      mean_si = si_fit$mu,
                      std_si = si_fit$sd)

R <- estimate_R(incid = i_daily_trunc,
                method = "parametric_si",
                config = config)

plot(R)

R_epiestim <- R$R %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  # glimpse()
  select(median_r,quantile_0_025_r,quantile_0_975_r,mean_r,std_r)

R_epiestim

epitrix::r2R0(r = f2$info$r,w = si_fit$distribution)

r0_from_lm <- epitrix::lm2R0_sample(x = f2$model,w = si_fit$distribution)

r0_from_lm %>% hist()
r0_from_lm %>% median()
r0_from_lm %>% quantile(c(0.025, 0.975))

#' comparacion de estimaciones
#' uso de epiestim 
#' suele subestimar en comparacion a 
#' uso de modelo log-lineal
#' ¿motivo? --> ask 


# proyeccion a corto plazo ------------------------------------------------

R_median <- R_epiestim %>% pull(median_r)
si <- si_fit$distribution

small_proj <- project(i_daily_trunc,# objeto de incidencia
                      R = R_median, # R estimado a utilizar
                      si = si,      # distribución de intervalo de serie
                      n_sim = 5,    # simula 5 trayectorias
                      n_days = 10,  # durante 10 días
                      R_fix_within = TRUE) # mantiene el mismo valor de R todos los días

small_proj

# mire cada trayectoria proyectada (como columnas):
as.matrix(small_proj)

#' generar un muestreo de 1000 R0

sample_R <- function(R_from_epiestim, n_sim = 1000)
{
  Rcero_mu <- R_from_epiestim %>% pull(mean_r)
  Rcero_sigma <- R_from_epiestim %>% pull(std_r)
  Rshapescale <- epitrix::gamma_mucv2shapescale(mu = Rcero_mu, 
                                                cv = Rcero_sigma / Rcero_mu)
  R_sample <- rgamma(n_sim, 
                     shape = Rshapescale$shape, 
                     scale = Rshapescale$scale)
  return(R_sample)
}

R_sample <- sample_R(R_from_epiestim = R_epiestim, n_sim = 1000)

R_sample %>% hist()

#' compare
#' valor de regresion lineal
#' y muestreo de media y stddev y crear distribucion

R_sample %>% 
  enframe() %>% 
  mutate(type="rsample") %>% 
  union_all(r0_from_lm %>% 
              enframe() %>% 
              mutate(type="rlinear")) %>% 
  ggplot(aes(x = value, fill = type)) +
  geom_histogram(position = position_identity(),alpha=0.5)



#' generar proyeccion para 100 valores
#' que provienen de R0 de modelo lineal

proj <- project(i_daily_trunc,# objeto de incidencia
                R = r0_from_lm, # R estimado a utilizar
                si = si,      # distribución de intervalo de serie
                n_sim = length(r0_from_lm),    # simula 1000 trayectorias
                n_days = 14,  # durante 14 días
                R_fix_within = TRUE) # mantiene el mismo valor de R todos los días

proj

plot(i_daily_trunc) %>% 
  add_projections(proj, c(0.025, 0.5, 0.975))

summary(proj)

#' generar para 1000 valores nuevos

proj <- project(i_daily_trunc,# objeto de incidencia
                R = R_sample, # R estimado a utilizar
                si = si,      # distribución de intervalo de serie
                n_sim = length(R_sample),    # simula 1000 trayectorias
                n_days = 14,  # durante 14 días
                R_fix_within = TRUE) # mantiene el mismo valor de R todos los días

proj

plot(i_daily_trunc) %>% 
  add_projections(proj, c(0.025, 0.5, 0.975))

summary(proj)

# resumen de proyeccion por fecha
apply(X = proj, MARGIN = 1, FUN = summary)

# suma acumulada de casos resumen por fecha
apply(X = apply(X = proj, MARGIN = 2, FUN = cumsum),
      MARGIN = 1,
      FUN = summary)


# parar -------------------------------------------------------------------

# agregar más incertidumbre -----------------------------------------------
#' (no desarrollado)
#' es decir
#' (copiado y pegado)

si_data <- data.frame(EL = rep(0L, length(si_obs)), 
                      ER = rep(1L, length(si_obs)), 
                      SL = si_obs, 
                      SR = si_obs + 1L) %>% 
  filter(!is.na(SL))

si_data
# any(si_data$SR - si_data$SL < 0)

config <- make_config(t_start = 2, 
                      t_end = length(i_daily_trunc$counts))

R_variableSI <- estimate_R(incid = i_daily_trunc, 
                           method = "si_from_data", 
                           si_data = si_data,
                           config = config)


# compruebe que la MCMC convergió
R_variableSI$MCMC_converged

plot(R_variableSI)


R_variableSI$R %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  # glimpse()
  select(median_r,quantile_0_025_r,quantile_0_975_r,mean_r,std_r)

# comparar con previo resultado
R_epiestim
#' se reduce la incertidumbre en muy poco


# Rt transmisibilidad variable en tiempo -------------------------------------

config <- make_config(list(mean_si = si_fit$mu, 
                           std_si = si_fit$sd))  

Rt <- estimate_R(incid = i_daily_trunc,
                 method = "parametric_si",
                 config = config)

plot(Rt)

Rt_epiestim <- Rt$R %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  # glimpse()
  select(median_r,quantile_0_025_r,quantile_0_975_r,mean_r,std_r)

Rt_epiestim %>% tail()


# Rt contrafactual --------------------------------------------------------

Rt <- estimate_R(incid = i_daily,
                 method = "parametric_si",
                 config = config)

plot(Rt)

Rt_epiestim <- Rt$R %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% 
  # glimpse()
  select(median_r,quantile_0_025_r,quantile_0_975_r,mean_r,std_r)

Rt_epiestim %>% tail()

