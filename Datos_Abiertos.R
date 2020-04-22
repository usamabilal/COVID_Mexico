rm(list=ls())
library(data.table)
library(tidyverse)
library(scales)
library(gridExtra)
library(ggrepel)
library(readxl)
library(broom)
library(lubridate)
# download once
file_download<-paste0("mx_", substr(Sys.time(), 1, 10), ".zip")
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", 
              destfile=file_download)
unzip(zipfile = file_download)
filename<-unzip(zipfile = file_download,list = T)$Name[1]
dta<-fread(filename)
# create broad age categories
dta<-dta %>% mutate(age_cat=floor(EDAD/5)*5,
                    age_cat=ifelse(age_cat>=85, 85, age_cat),
                    age_largecat=floor(EDAD/15)*15,
                    age_largecat=ifelse(age_largecat>=75, 75, age_largecat))
table(dta$age_cat)
table(dta$age_largecat)
# get std age pops
std.pop<-read.fwf(file="https://seer.cancer.gov/stdpopulations/stdpop.19ages.txt", widths=c(3, 3, 8),
                  col.names = c("std", "age", "pop")) %>% 
  filter(std==10) %>% 
  mutate(age=ifelse(age<=1, age, (age-1)*5),
         age=ifelse(age>=75, 75, age),
         std_pop=pop/sum(pop)) %>% 
  mutate(age=ifelse(age%in%c(0, 1, 75), age,
                    floor((age-5)/10)*10+5)) %>% 
  group_by(age) %>% 
  summarise(std_pop=sum(std_pop)) %>% 
  mutate(age_largecat=floor(age/15)*15) %>% 
  group_by(age_largecat) %>% 
  summarise(std_pop=sum(std_pop))
# lets take a look at chronic diseases
# create a simple yes/no for  diabetes, COPD, asthma, inmunosupressed, CVD, ckd
dta %>% select(DIABETES, EPOC, ASMA, INMUSUPR, CARDIOVASCULAR, RENAL_CRONICA) %>% 
  gather(var, val) %>% 
  group_by(var, val) %>% 
  summarise(n=n()) %>% 
  spread(val, n)
# assuming NAs are ignorable here...
dta<-dta %>% 
  mutate(any_chronic=as.numeric(DIABETES==1|EPOC==1|ASMA==1|INMUSUPR==1|CARDIOVASCULAR==1|RENAL_CRONICA==1),
         death=as.numeric(FECHA_DEF!="9999-99-99"),
         HABLA_LENGUA_INDIG=ifelse(HABLA_LENGUA_INDIG==1, "Si", 
                                   ifelse(HABLA_LENGUA_INDIG==2, "No", NA)),
         any_chronic=ifelse(any_chronic==1, "Si", "No"),
         SEXO=ifelse(SEXO==1, "MUJER",
                     ifelse(SEXO==2, "HOMBRE", NA)))
table(dta$any_chronic)
table(dta$death)

# exploring key variables, 1=yes, 2=no
table(dta$HABLA_LENGUA_INDIG) 
table(dta$NEUMONIA)
# death: date of death = 9999-99-99
table(dta$RESULTADO, dta$death)
#857 deaths with confirmed COVID -> matches Mexico national data
# 112 deaths with pending result (resultado=3)
prop.table(table(dta$RESULTADO, dta$death), margin=1)
# notice how case fataltiy rate differs a lot between positives (9%) and negaitves (1.6%)

# first: proof of concept -> do chronic diseases increase risk of death?
dta %>% 
  # exclude missing pneumonia (8 cases only)
  filter(NEUMONIA!=99) %>% 
  group_by(RESULTADO, any_chronic, age_largecat) %>% 
  summarise(neumonia=sum(NEUMONIA==1),
            # deaths date 9999-99-99 = didnt die
            deaths=sum(FECHA_DEF!="9999-99-99"),
            n=n()) %>% 
  mutate(prev=neumonia/n,
         mort=deaths/n) %>% 
  left_join(std.pop) %>% 
  mutate(adj=prev*std_pop,
         adjmort=mort*std_pop) %>% 
  group_by(RESULTADO, any_chronic) %>% 
  summarise(neumonia_adj_porcentaje=sum(adj)*100,
            mortalidad_adj_porcentaje=sum(adjmort)*100) %>% 
  ungroup() %>% 
  # get only positive cases
  filter(RESULTADO==1) %>% 
  select(-RESULTADO)

# estimate age-adjusted Pneumonia and mortality, by indigenous language 
age_adjusted<-dta %>% 
  # exclude missing indigenous language (~900 people, <2%)
  filter(HABLA_LENGUA_INDIG!=99) %>% 
  # exclude missing pneumonia (8 cases only)
  filter(NEUMONIA!=99) %>% 
  group_by(RESULTADO, HABLA_LENGUA_INDIG, age_largecat) %>% 
  summarise(neumonia=sum(NEUMONIA==1),
            # deaths date 9999-99-99 = didnt die
            deaths=sum(FECHA_DEF!="9999-99-99"),
            n=n()) %>% 
  mutate(prev=neumonia/n,
         mort=deaths/n) %>% 
  left_join(std.pop) %>% 
  mutate(adj=prev*std_pop,
         adjmort=mort*std_pop) %>% 
  group_by(RESULTADO, HABLA_LENGUA_INDIG) %>% 
  summarise(neumonia_adj_porcentaje=sum(adj)*100,
            mortalidad_adj_porcentaje=sum(adjmort)*100) %>% 
  ungroup() %>% 
  # get only positive cases
  filter(RESULTADO==1) %>% 
  select(-RESULTADO)

age_adjusted %>% gather(outcome, value, -HABLA_LENGUA_INDIG) %>% 
  ggplot(aes(x=outcome, y=value))+
  geom_col(aes(fill=HABLA_LENGUA_INDIG), position="dodge") +
  scale_y_continuous(expand=c(0,0), limits=c(0, 27))+
  scale_x_discrete(labels=c("Mortalidad", "Neumonia"))+
  scale_fill_brewer(type="qual", palette=2, name="Habla lengua indigena?")+
  labs(x="", 
       y="%",
       title="Proporcion de casos COVID-19 confirmados que presentan con neumonia o mueren",
       subtitle="(ajustado por edad)",
       caption="Fuente: Datos Abiertos COVID19") +
  theme_bw()+
  theme(axis.text=element_text(color="black", size=14),
        legend.text=element_text(color="black", size=20),
        legend.title=element_text(color="black", size=20, face="bold"),
        legend.position = c(0.2, 0.8))



# exploring a logistic model, with sequential adjustment
glm(death~as.factor(HABLA_LENGUA_INDIG), data=dta %>% 
      filter(RESULTADO==1, HABLA_LENGUA_INDIG!=99), family="binomial") %>% 
  tidy
glm(death~as.factor(HABLA_LENGUA_INDIG)+
      as.factor(age_largecat), data=dta %>% 
      filter(RESULTADO==1, HABLA_LENGUA_INDIG!=99), family="binomial") %>% 
  tidy
glm(death~as.factor(HABLA_LENGUA_INDIG)+
      as.factor(age_largecat)+as.factor(SEXO), data=dta %>% 
      filter(RESULTADO==1, HABLA_LENGUA_INDIG!=99), family="binomial") %>% 
  tidy
glm(death~as.factor(HABLA_LENGUA_INDIG)+
      as.factor(age_largecat)+
      as.factor(SEXO)+
      as.factor(any_chronic), data=dta %>% 
      filter(RESULTADO==1, HABLA_LENGUA_INDIG!=99), family="binomial") %>% 
  tidy
