#### Jamillah Rodriguez
#### language-survey.R
#### Looking at language survey results from GRAMMY experiment
#### Last updated: Feb 9 2023

# Remove items in environment
rm(list = ls())

# Set working directory
setwd("~/Dropbox/Research/CT language survey")

# Read files
survey = read.csv("survey.csv")

## Cleaning up factors
survey$Write_Triqui <- factor(survey$Write_Triqui, levels = c("mal", "regular", "bien", "excelente"))
survey$Understand_Triqui <- factor(survey$Understand_Triqui, levels = c("mal", "regular", "bien", "excelente"))
survey$Write_Triqui <- factor(survey$Write_Triqui, levels = c("mal", "regular", "bien", "excelente"))
survey$YourselfUse_Triqui <- factor(survey$YourselfUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))


# Libraries
library(ggplot2)
library(ggpubr)

# Remove participant with strange responses
survey = subset(survey, ID != "87417")

# Gender
xtabs(~gender, survey)

# Age
xtabs(~age, survey)
age = subset(survey$age, !is.na(survey$age))
mean(age)
median(age)

aggregate(age~gender, survey, mean)

# Education
xtabs(~education, survey)


# Location
xtabs(~location, survey)

#### General demographics ####


# Location and age

# Location
xtabs(~location + age, survey)

png("age-location.png", units="px", width=1600, height=1000, res=300)
p<-ggplot(survey, aes(x=location, y=age)) +
  geom_violin(trim=FALSE)
p + stat_summary(fun.data="mean_sdl", mult=1, 
                 geom="pointrange", width=0.2 ) +
  theme_bw() +
  labs(x = "Lugar", y = "Edad")+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()

# Location
xtabs(~location + age, survey)

png("age-education.png", units="px", width=1600, height=1000, res=300)
p<-ggplot(survey, aes(x=education, y=age)) +
  geom_violin(trim=FALSE)
p + stat_summary(fun.data="mean_sdl", mult=1, 
                 geom="pointrange", width=0.2 ) +
  theme_bw() +
  labs(x = "Educación", y = "Edad")+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()

# Age participants learned Triqui
# Almost all learned at 0-5yrs.

x = prop.table(xtabs(~ Age.Learn_Triqui + location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Age"

y = prop.table(xtabs(~ age.Learn_Spanish + location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Age"

z = prop.table(xtabs(~ Age.Learn_English + location, survey))
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Age"
z = z[-1,]
z = z[-7,]

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, p, x, y, z)


new$Age <- factor(new$Age, levels = c("0-5 años", "6-10 años", "11-15 años", "16-20 años", "más de 20 años", "nunca aprendí"))

png("age-languages-learned-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Age, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~location) +
  labs(x = "Edad participante aprendió idioma", y = "Porcentaje") + theme_bw() + 
theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()


# Age participants felt comfortable with language

x = prop.table(xtabs(~ Years.Comfort_Triqui + location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Age"

y = prop.table(xtabs(~ Years.Comfort_Spanish + location, survey),margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Age"

z = prop.table(xtabs(~ Years.Comfort_English + location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Age"
z = z[-1,]
z = z[-7,]

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, p, x, y, z)


new$Age <- factor(new$Age, levels = c("0-5 años", "6-10 años", "11-15 años", "16-20 años", "más de 20 años", "aún no me siento cómodo"))

png("age-languages-comfort-by-location.png", units="px", width=2700, height=1000, res=300)
ggplot(new, aes(x=Age, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~location) + 
  labs(x = "El participante de edad se sintió cómodo con el idioma", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()



# How much education with language

x = prop.table(xtabs(~ Years.Ed_Triqui+location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Age"
x = x[-1,]
x = x[-3,]

y = prop.table(xtabs(~ Years.Ed_Spanish+location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Age"
y = y[-1,]
y = y[-4,]

z = prop.table(xtabs(~ Years.Ed_English + location, survey),margin = 2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Age"
z = z[-1,]
z = z[-6,]

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Age <- factor(new$Age, levels = c("0-5 años", "6-10 años", "11-15 años", "16-20 años", "más de 20 años"))

png("years-education-language-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Age, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~location) +
  labs(x = "Años de educación en el idioma", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()



# Years lived in region

x = prop.table(xtabs(~ Country.Region_Triqui+location, survey),margin =2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Years"
x = x[-1,]
x = x[-5,]

y = prop.table(xtabs(~ Country.Region_Spanish+location, survey),margin=2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Years"


z = prop.table(xtabs(~ Country.Region_English+location, survey),margin=2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Years"
z = z[-1,]
z = z[-6,]

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Years <- factor(new$Years, levels = c("0-5 años", "6-10 años", "11-15 años", "16-20 años", "más de 20 años"))

png("years-region.png-by-location", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Years, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~location) + 
  labs(x = "Años ha pasado en un país/región donde se hablan los idiomas", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()




# How often speak with friends

x = prop.table(xtabs(~ U...FriendsUse_Triqui+ D...Location, survey),margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Frec"
x$D...Location = gsub("New York", "Nueva York", x$D...Location)



y = prop.table(xtabs(~ U...FriendsUse_Spanish + D...Location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Español"
names(y)[1] = "Frec"
y$D...Location = gsub("New York", "Nueva York", y$D...Location)

z = prop.table(xtabs(~ U...FriendsUse_English + D...Location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "Ingles"
names(z)[1] = "Frec"
z$D...Location = gsub("New York", "Nueva York", z$D...Location)

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Frec <- factor(new$Frec, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

library(ggplot2)
png("friends-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Frec, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~D...Location) + 
  labs(x = "Frecuencia de uso del idioma con amigos", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()




# How often speak with family

x = prop.table(xtabs(~ U...FamilyUse_Triqui + D...Location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Frec"
x$D...Location = gsub("New York", "Nueva York", x$D...Location)

y = prop.table(xtabs(~ U...FamilyUse_Spanish + D...Location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Español"
names(y)[1] = "Frec"
y$D...Location = gsub("New York", "Nueva York", y$D...Location)

z = prop.table(xtabs(~ U...FamilyUse_English + D...Location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "Ingles"
names(z)[1] = "Frec"
z$D...Location = gsub("New York", "Nueva York", z$D...Location)

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Frec <- factor(new$Frec, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

png("family-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Frec, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~D...Location) + 
  labs(x = "Frecuencia de uso del idioma con familia", y = "Porcentaje") + theme_bw() + 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()



# How often speak at work/school

x = prop.table(xtabs(~ Home.WorkUse_Triqui + location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Frec"

y = prop.table(xtabs(~ Home.WorkUse_Spanish + location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Frec"

z = prop.table(xtabs(~ Home.WorkUse_English + location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Frec"

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Frec <- factor(new$Frec, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

png("home-school-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Frec, y=Freq, fill = Idioma)) + facet_grid(~location) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) +
  labs(x = "Frecuencia de uso del idioma a escuela/trabajo", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()



# How often speak with yourself

x = prop.table(xtabs(~ YourselfUse_Triqui + location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Frec"

y = prop.table(xtabs(~ YourselfUse_Spanish + location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Frec"

z = prop.table(xtabs(~ YourselfUse_English + location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Frec"
z = z[-1,]
z = z[-6,]

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Frec <- factor(new$Frec, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

png("yourself-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Frec, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~location) + 
  labs(x = "Frecuencia de uso del idioma cuando habla consigo mismo", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()



# Speaking ability in all languages

x = prop.table(xtabs(~ Understand_Triqui + location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Skill"

y = prop.table(xtabs(~ Understand_Spanish + location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Skill"

z = prop.table(xtabs(~ Understand_English + location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Skill"

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Skill <- factor(new$Skill, levels = c("mal", "regular", "bien", "excelente"))

png("speaking-skill.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Skill, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~location) + 
  labs(x = "Habilidad de hablar", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()




# Comprehension ability in all languages



x = prop.table(xtabs(~ Understand_Triqui + location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Skill"

y = prop.table(xtabs(~ Understand_Spanish + location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Skill"

z = prop.table(xtabs(~ Understand_English + location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Skill"
z = z[-1,]
z = z[-5,]

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Skill <- factor(new$Skill, levels = c("mal", "regular", "bien", "excelente"))

png("comprehension-skill-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Skill, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) +
 facet_grid(~location)+
  labs(x = "Habilidad de comprensión", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()




# Reading ability in all languages

x = prop.table(xtabs(~ Read_Triqui + location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Skill"
x = x[-1,]
x = x[-5,]

y = prop.table(xtabs(~ Read_Spanish + location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Skill"

z = prop.table(xtabs(~ Read_English + location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Skill"
z = z[-1,]
z = z[-5,]

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Skill <- factor(new$Skill, levels = c("mal", "regular", "bien", "excelente"))

png("reading-skill-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Skill, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~location) + 
  labs(x = "Habilidad de lectura", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()



# Writing ability in all languages

x = prop.table(xtabs(~ Write_Triqui + location, survey), margin = 2)
x = data.frame(x)
x$Idioma = "Triqui"
names(x)[1] = "Skill"

y = prop.table(xtabs(~ Write_Spanish + location, survey), margin = 2)
y = data.frame(y)
y$Idioma = "Spanish"
names(y)[1] = "Skill"
y = y[-1,]
y = y[-5,]

z = prop.table(xtabs(~ Write_English + location, survey), margin = 2)
z = data.frame(z)
z$Idioma = "English"
names(z)[1] = "Skill"
z = z[-1,]
z = z[-5,]

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


new$Skill <- factor(new$Skill, levels = c("mal", "regular", "bien", "excelente"))

png("writing-skill-by-location.png", units="px", width=1600, height=1000, res=300)
ggplot(new, aes(x=Skill, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~location) +
  labs(x = "Habilidad de escritura", y = "Porcentaje") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()




#### Does age influence how often a participant speaks Triqui? ####
png("age-frequency.png", units="px", width=1600, height=1000, res=300)
p<-ggplot(survey, aes(x=YourselfUse_Triqui, y=age)) +
  geom_violin(trim=FALSE)
p + stat_summary(fun.data="mean_sdl", mult=1, 
                 geom="pointrange", width=0.2 ) +
  theme_bw() +
  labs(x = "Frequency of speaking Triqui", y = "Age")
dev.off()

# Does age influence how well a participant speaks Triqui?
png("age-speaking.png", units="px", width=1600, height=1000, res=300)
p<-ggplot(survey, aes(x=Understand_Triqui, y=age)) +
  geom_violin(trim=FALSE)
p + stat_summary(fun.data="mean_sdl", mult=1, 
                 geom="pointrange", width=0.2 ) +
  theme_bw() +
  labs(x = "Triqui speaking proficiency", y = "Age")
dev.off()


#### Does gender influence how well a participant speaks Triqui? ####
x = prop.table(xtabs(~ Understand_Triqui + gender, survey), margin = 2)

x = data.frame(x)

x$gender <- factor(x$gender, levels = c("Mujer", "Hombre"))


png("gender-speaking.png", units="px", width=1600, height=1000, res=300)
ggplot(x, aes(x=Understand_Triqui, y=Freq, fill=gender)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Habilidad de hablar triqui", y = "Porcentaje", theme = "Género") + theme_bw()
dev.off()

chisq.test(survey$gender, survey$Understand_Triqui)


# Does gender influence whether a participants can write Triqui?
x = prop.table(xtabs(~ Write_Triqui + gender, survey), margin = 2)

x = data.frame(x)

x$gender <- factor(x$gender, levels = c("Mujer", "Hombre"))


#png("gender-speaking.png", units="px", width=1600, height=1000, res=300)
ggplot(x, aes(x=Write_Triqui, y=Freq, fill=gender)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Habilidad de escribir triqui", y = "Porcentaje", theme = "Género") + theme_bw()
#dev.off()




# Does gender influence how often a participant speaks Triqui?
x = prop.table(xtabs(~ YourselfUse_Triqui + gender, survey), margin = 2)

x = data.frame(x)

x$gender <- factor(x$gender, levels = c("Mujer", "Hombre"))


png("gender-frequency.png", units="px", width=1600, height=1000, res=300)
ggplot(x, aes(x=YourselfUse_Triqui, y=Freq, fill=gender)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x = "Frecuencia de uso triqui", y = "Porcentaje (por género)", theme = "Género") + theme_bw() + ylim(0,0.6)
dev.off()

chisq.test(survey$gender, survey$YourselfUse_Triqui)

# Does age influence how well a participant writes Triqui?
png("age-writing.png", units="px", width=1600, height=1000, res=300)
p<-ggplot(survey, aes(x=Write_Triqui, y=age)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Triqui writing proficiency", y = "Age")
p
dev.off()

### Are speakers with Triqui-fluent friends more likely to speak Triqui themselves?

survey$FriendsUse_Triqui <- factor(survey$FriendsUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$YourselfUse_Triqui <- factor(survey$YourselfUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

x = data.frame(xtabs(~ YourselfUse_Triqui + FriendsUse_Triqui, survey))
x = subset(x, x$FriendsUse_Triqui != "")

png("yourself-friends.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "YourselfUse_Triqui", y = "FriendsUse_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "¿Con qué frecuencia hablas triqui?", y = "¿Con qué frecuencia tus amigos hablan triqui?") + theme_bw()
dev.off()


#### Are speakers with Triqui-fluent family more likely to speak Triqui themselves? ####

x = data.frame(xtabs(~ YourselfUse_Triqui + FamilyUse_Triqui, survey))
x = subset(x, x$FamilyUse_Triqui != "")

#png("yourself-family.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "YourselfUse_Triqui", y = "FamilyUse_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "¿Con qué frecuencia hablas triqui?", y = "¿Con qué frecuencia tu familia hablan triqui?") + theme_bw()
#dev.off()

#### Does writing proficiency correlate with speaking proficiency? #### 

x = data.frame(xtabs(~ Understand_Triqui + Write_Triqui, survey))

png("writing-triqui.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "Write_Triqui", y = "Understand_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Habilidad de escribir triqui", y = "Habilidad de hablar triqui") + theme_bw()
dev.off()

#### Age Learning and Years Comfort #### 

survey$H...Age.Learn_Triqui <- factor(survey$H...Age.Learn_Triqui, levels = c("0-5 años", "6-10 años", "16-20 años", "nunca aprendí"))

x = data.frame(xtabs(~ Age.Learn_Triqui + Years.Comfort_Triqui, survey))

png("Englishwork-Triquispeak.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "Age.Learn_Triqui", y = "Years.Comfort_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Age.Learn_Triqui", y = "Years.Comfort_Triqui") + theme_bw()
dev.off()


#### Years education and Years Comfort #### 

survey$Years.Ed_Triqui <- factor(survey$Age.Learn_Triqui, levels = c("0-5 años", "6-10 años", "16-20 años"))

x = data.frame(xtabs(~ Years.Ed_Triqui + Years.Comfort_Triqui, survey))

png("yearsed-yearscomfort-triqui.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "Years.Ed_Triqui", y = "Years.Comfort_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Years.Ed_Triqui", y = "Years.Comfort_Triqui") + theme_bw()
dev.off()



#### Family history and Years Comfort #### 

survey$FamilyHistory_Triqui <- factor(survey$FamilyHistory_Triqui, levels = c("0-5 años", "6-10 años", "16-20 años", "más de 20 años"))

x = data.frame(xtabs(~ FamilyHistory_Triqui + Years.Comfort_Triqui, survey))

png("familyTriqui-yearscomfort-triqui.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "FamilyHistory_Triqui", y = "Years.Comfort_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "FamilyHistory_Triqui", y = "Years.Comfort_Triqui") + theme_bw()
dev.off()

#### Family history and years in region #### 

survey$H...FamilyHistory_English <- factor(survey$H...FamilyHistory_English, levels = c("0-5 años", "6-10 años", "16-20 años", "más de 20 años"))
survey$H...Country.Region_English <- factor(survey$H...Country.Region_English, levels = c("0-5 años", "6-10 años", "16-20 años", "más de 20 años"))

x = data.frame(xtabs(~ H...FamilyHistory_English + H...Country.Region_English, survey))

png("familyEnglish-yearscomfort-English.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "H...FamilyHistory_English", y = "H...Country.Region_English", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "FamilyHistory_English", y = "Country.Region_English") + theme_bw()
dev.off()


### Work Environment ###
survey$H...Work.Enviro.History_Triqui <- factor(survey$H...Work.Enviro.History_Triqui , levels = c("0-5 años", "6-10 años", "16-20 años", "más de 20 años"))
survey$H...Country.Region_Triqui <- factor(survey$H...Country.Region_Triqui, levels = c("0-5 años", "6-10 años", "16-20 años", "más de 20 años"))

x = data.frame(xtabs(~ H...Work.Enviro.History_Triqui  + H...Country.Region_Triqui, survey))

png("familyEnglish-yearscomfort-English.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "H...Work.Enviro.History_Triqui ", y = "H...Country.Region_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Work.Enviro.History_Triqui ", y = "Country.Region_Triqui") + theme_bw()
dev.off()

#### Family history and Years Comfort #### 

survey$FamilyHistory_Triqui <- factor(survey$FamilyHistory_Triqui, levels = c("0-5 años", "6-10 años", "16-20 años", "más de 20 años"))

x = data.frame(xtabs(~ FamilyHistory_Triqui + Years.Comfort_Triqui, survey))

png("familyTriqui-yearscomfort-triqui.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "FamilyHistory_Triqui", y = "Years.Comfort_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "FamilyHistory_Triqui", y = "Years.Comfort_Triqui") + theme_bw()
dev.off()


#### Work Environment and Years Comfort #### 

survey$Work.Enviro.History_Triqui <- factor(survey$Work.Enviro.History_Triqui, levels = c("0-5 años", "6-10 años", "16-20 años", "más de 20 años"))

x = data.frame(xtabs(~ Work.Enviro.History_Triqui + Years.Comfort_Triqui, survey))

png("familyTriqui-yearscomfort-triqui.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "Work.Enviro.History_Triqui", y = "Years.Comfort_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Work.Enviro.History_Triqui", y = "Years.Comfort_Triqui") + theme_bw()
dev.off()



#### Age Learn Triqui and Family History #### 

survey$H...Age.Learn_Triqui <- factor(survey$H...Age.Learn_Triqui, levels = c("0-5 años", "6-10 años", "16-20 años", "nunca aprendí"))

survey$H...Age.Learn_Spanish <- factor(survey$H...Age.Learn_Spanish, levels = c("0-5 años", "6-10 años", "16-20 años", "nunca aprendí"))

x = data.frame(xtabs(~ H...Age.Learn_Triqui + H...FamilyHistory_Triqui, survey))

png("familyTriqui-yearscomfort-triqui.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "H...Age.Learn_Triqui", y = "H...FamilyHistory_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Age.Learn_Triqui", y = "FamilyHistory_Triqui") + theme_bw()
dev.off()




#### Age Learn Triqui and Family History #### 

survey$H...Years.Comfort_Triqui<- factor(survey$H...Years.Comfort_Triqui, levels = c("0-5 años", "6-10 años", "16-20 años", "aún no me siento cómodo"))

survey$H...Years.Comfort_Spanish<- factor(survey$H...Years.Comfort_Spanish, levels = c("0-5 años", "6-10 años", "16-20 años", "aún no me siento cómodo"))

x = data.frame(xtabs(~ H...Years.Comfort_Triqui + H...Years.Comfort_Spanish, survey))

png("familyTriqui-yearscomfort-triqui.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "H...Years.Comfort_Triqui", y = "H...Years.Comfort_Spanish", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "H...Years.Comfort_Triqui", y = "H...Years.Comfort_Spanish") + theme_bw()
dev.off()




#### Interactions ####

# Is there a correlation between speaking good English and poor Triqui?

survey$Understand_English <- factor(survey$Understand_English, levels = c("mal", "regular", "bien", "excelente"))
survey$Understand_Triqui <- factor(survey$Understand_Triqui, levels = c("mal", "regular", "bien", "excelente"))

x = data.frame(xtabs(~ Understand_English + Understand_Triqui, survey))

png("Englishspeak-Triquispeak.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "Understand_English", y = "Understand_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "English speaking proficiency", y = "Triqui speaking proficiency") + theme_bw()
dev.off()

####  Language use ####

# Friends
x = xtabs(~ U...FriendsUse_Triqui, survey)
x = data.frame(x)
x$Environment= "Amigos"
x$Idioma= "Triqui"
names(x)[1] = "Uso"

y = xtabs(~ U...FriendsUse_Spanish, survey)
y = data.frame(y)
y$Environment= "Amigos"
y$Idioma= "Español"
names(y)[1] = "Uso"


z = xtabs(~ U...FriendsUse_English, survey)
z = data.frame(z)
z$Environment= "Amigos"
z$Idioma= "Ingles"
names(z)[1] = "Uso"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


# Family
x = xtabs(~ U...FamilyUse_Triqui, survey)
x = data.frame(x)
x$Environment= "Familia"
x$Idioma= "Triqui"
names(x)[1] = "Uso"


y = xtabs(~ U...FamilyUse_Spanish, survey)
y = data.frame(y)
y$Environment= "Familia"
y$Idioma= "Español"
names(y)[1] = "Uso"


z = xtabs(~ U...FamilyUse_English, survey)
z = data.frame(z)
z$Environment= "Familia"
z$Idioma= "Ingles"
names(z)[1] = "Uso"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new_2 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)




# Work/School
x = xtabs(~ U...Home.WorkUse_Triqui, survey)
x = data.frame(x)
x$Environment= "Trabajo"
x$Idioma= "Triqui"
names(x)[1] = "Uso"


y = xtabs(~ U...Home.WorkUse_Spanish, survey)
y = data.frame(y)
y$Environment= "Trabajo"
y$Idioma= "Español"
names(y)[1] = "Uso"


z = xtabs(~ U...Home.WorkUse_English, survey)
z = data.frame(z)
z$Environment= "Trabajo"
z$Idioma= "Ingles"
names(z)[1] = "Uso"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new_3 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)




# Yourself
x = xtabs(~ U...YourselfUse_Triqui, survey)
x = data.frame(x)
x$Environment= "Yo mismo"
x$Idioma= "Triqui"
names(x)[1] = "Uso"


y = xtabs(~ U...YourselfUse_Spanish, survey)
y = data.frame(y)
y$Environment= "Yo mismo"
y$Idioma= "Español"
names(y)[1] = "Uso"


z = xtabs(~ U...YourselfUse_English, survey)
z = data.frame(z)
z$Environment= "Yo mismo"
z$Idioma= "Ingles"
names(z)[1] = "Uso"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new_4 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)

new_4 = subset(new_4, Uso != "")


lang_use = rbind(new, new_2, new_3, new_4)


lang_use$Uso = factor(lang_use$Uso, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

#lang_use$Environment = factor(lang_use$Uso, levels = c("Yo mismo", "Familia", "Amigos", "Trabajo"))

png("lang-use.png", units="px", width=2000, height=1000, res=300)
ggplot(lang_use, aes(x=Uso, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~factor(Environment, levels=c("Yo mismo", "Familia", "Amigos", "Trabajo"))) +
  labs(x = "Frecuencia de uso", y = "Participantes") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()

# Do the same for understanding, speaking, and writing
# Redo location graphs with language names in Spanish

# Interactions with gender, age, location, years speaking English

#### Skills by gender for each language ####

names(survey)[4] = "Genero"

# Comprehension
x = xtabs(~ S...Understand_Triqui + Genero, survey)
x = data.frame(x)
x$Skill= "Entienden"
x$Idioma= "Triqui"
names(x)[1] = "Nivel"


y = xtabs(~ S...Understand_Spanish + Genero, survey)
y = data.frame(y)
y$Skill= "Entienden"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ S...Understand_English + Genero, survey)
z = data.frame(z)
z$Skill= "Entienden"
z$Idioma= "Ingles"
names(z)[1] = "Nivel"

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


# Speaking
x = xtabs(~ S...SpeakSkill_Triqui + Genero, survey)
x = data.frame(x)
x$Skill= "Hablan"
x$Idioma= "Triqui"
names(x)[1] = "Nivel"


y = xtabs(~ S...SpeakSkill_Spanish + Genero, survey)
y = data.frame(y)
y$Skill= "Hablan"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ S...SpeakSkill_English + Genero, survey)
z = data.frame(z)
z$Skill= "Hablan"
z$Idioma= "Ingles"
names(z)[1] = "Nivel"

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new2 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)


# Reading
x = xtabs(~ S...Read_Triqui + Genero, survey)
x = data.frame(x)
x$Skill= "Leen"
x$Idioma= "Triqui"
names(x)[1] = "Nivel"


y = xtabs(~ S...Read_Spanish + Genero, survey)
y = data.frame(y)
y$Skill= "Leen"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ S...Read_English + Genero, survey)
z = data.frame(z)
z$Skill= "Leen"
z$Idioma= "Ingles"
names(z)[1] = "Nivel"

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new3 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)



# Writing
x = xtabs(~ Write_Triqui + Genero, survey)
x = data.frame(x)
x$Skill= "Escriben"
x$Idioma= "Triqui"
names(x)[1] = "Nivel"


y = xtabs(~ S...Write_Spanish + Genero, survey)
y = data.frame(y)
y$Skill= "Escriben"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ Write_English + Genero, survey)
z = data.frame(z)
z$Skill= "Escriben"
z$Idioma= "Ingles"
names(z)[1] = "Nivel"

#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new4 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)





gender_skill = rbind(new, new2, new3, new4)
gender_skill$Nivel = factor(gender_skill$Nivel, levels = c("mal", "regular", "bien", "excelente"))
gender_skill$Idioma = factor(gender_skill$Idioma, levels = c("Triqui", "Español", "Ingles"))
gender_skill$Skill = factor(gender_skill$Skill, levels = c("Entienden", "Hablan", "Leen", "Escriben"))

png("gender-skills.png", units="px", width=2000, height=1000, res=300)
ggplot(gender_skill, aes(x=Nivel, y=Freq, fill = Skill)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(Idioma~Genero) + labs(x = "Nivel", y = "Participantes") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()



#### Skills for each language ####
# Speaking
x = xtabs(~ S...Understand_Triqui, survey)
x = data.frame(x)
x$Skill= "Hablan"
x$Idioma= "Triqui"
names(x)[1] = "Nivel"



y = xtabs(~ S...Understand_Spanish, survey)
y = data.frame(y)
y$Skill= "Hablan"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ S...Understand_English, survey)
z = data.frame(z)
z$Skill= "Hablan"
z$Idioma= "English"
names(z)[1] = "Nivel"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)





y = xtabs(~ S...Understand_Spanish, survey)
y = data.frame(y)
y$Skill= "Hablan"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ S...Understand_English, survey)
z = data.frame(z)
z$Skill= "Hablan"
z$Idioma= "English"
names(z)[1] = "Nivel"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)



#Understand
x = xtabs(~ S...Understand_Triqui, survey)
x = data.frame(x)
x$Skill= "Entienden"
x$Idioma= "Triqui"
names(x)[1] = "Nivel"



y = xtabs(~ S...Understand_Spanish, survey)
y = data.frame(y)
y$Skill= "Entienden"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ S...Understand_English, survey)
z = data.frame(z)
z$Skill= "Entienden"
z$Idioma= "English"
names(z)[1] = "Nivel"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new_2 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)



#Read
x = xtabs(~ S...Read_Triqui, survey)
x = data.frame(x)
x$Skill= "Leen"
x$Idioma= "Triqui"
names(x)[1] = "Nivel"



y = xtabs(~ S...Read_Spanish, survey)
y = data.frame(y)
y$Skill= "Leen"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ S...Read_English, survey)
z = data.frame(z)
z$Skill= "Leen"
z$Idioma= "English"
names(z)[1] = "Nivel"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new_3 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)



#Write

x = xtabs(~ Write_Triqui, survey)
x = data.frame(x)
x$Skill= "Escriben"
x$Idioma= "Triqui"
names(x)[1] = "Nivel"



y = xtabs(~ S...Write_Spanish, survey)
y = data.frame(y)
y$Skill= "Escriben"
y$Idioma= "Español"
names(y)[1] = "Nivel"


z = xtabs(~ Write_English, survey)
z = data.frame(z)
z$Skill= "Escriben"
z$Idioma= "English"
names(z)[1] = "Nivel"


#put all data frames into list
df_list <- list(x, y, z)

#merge all data frames in list
new_4 = Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
rm(df_list, x, y, z)



lang_skill = rbind(new, new_2, new_3, new_4)
lang_skill$Nivel = factor(lang_skill$Nivel, levels = c("mal", "regular", "bien", "excelente"))

png("lang-skills.png", units="px", width=2000, height=1000, res=300)
ggplot(lang_skill, aes(x=Nivel, y=Freq, fill = Idioma)) +
  geom_bar(stat="identity", position=position_dodge2(preserve = "single")) + facet_grid(~factor(Skill, levels=c("Entienden", "Hablan", "Leen", "Escriben"))) + labs(x = "Nivel", y = "Participantes") + theme_bw()+ 
  theme(axis.text.x=element_text(angle=40,hjust=1))
dev.off()



# if language use at work correlates with language use anywhere else

survey$U...Home.WorkUse_Triqui <- factor(survey$FriendsUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$YourselfUse_Triqui <- factor(survey$YourselfUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

x = data.frame(xtabs(~ YourselfUse_Triqui + FriendsUse_Triqui, survey))
x = subset(x, x$FriendsUse_Triqui != "")

png("yourself-friends.png", units="px", width=1600, height=1000, res=300)
ggballoonplot(x, x = "YourselfUse_Triqui", y = "FriendsUse_Triqui", size = "Freq",
              fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "¿Con qué frecuencia hablas triqui?", y = "¿Con qué frecuencia tus amigos hablan triqui?") + theme_bw()
dev.off()




#### Triqui - work and other environments ####

survey$U...Home.WorkUse_Triqui <- factor(survey$U...Home.WorkUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...YourselfUse_Triqui <- factor(survey$U...YourselfUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...FamilyUse_Triqui <- factor(survey$U...FamilyUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...FriendsUse_Triqui <- factor(survey$U...FriendsUse_Triqui, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

t1.df = data.frame(xtabs(~ U...Home.WorkUse_Triqui + U...YourselfUse_Triqui, survey))


png("home-yourself-Triqui.png", units="px", width=1600, height=1000, res=300)
t1 = ggballoonplot(t1.df, x = "U...Home.WorkUse_Triqui", y = "U...YourselfUse_Triqui", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Familia") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

t2.df = data.frame(xtabs(~ U...Home.WorkUse_Triqui + U...FamilyUse_Triqui, survey))


png("home-family-Triqui.png", units="px", width=1600, height=1000, res=300)
t2 = ggballoonplot(t2.df, x = "U...Home.WorkUse_Triqui", y = "U...FamilyUse_Triqui", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Familia") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

t3.df = data.frame(xtabs(~ U...Home.WorkUse_Triqui + U...FriendsUse_Triqui, survey))

png("home-friends-Triqui.png", units="px", width=1600, height=1000, res=300)
t3 = ggballoonplot(t3.df, x = "U...Home.WorkUse_Triqui", y = "U...FriendsUse_Triqui", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Amigos") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

install.packages("gridExtra")
library("gridExtra")  


png("Triqui-work-other-environments.png", units="px", width=3000, height=1500, res=300)
grid.arrange(t1,t2,t3, ncol = 3, top = textGrob("Uso del triqui en el trabajo, el hogar y otros entornos"))
dev.off()




#### Spanish - work and other environments ####

survey$U...Home.WorkUse_Spanish <- factor(survey$U...Home.WorkUse_Spanish, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...YourselfUse_Spanish <- factor(survey$U...YourselfUse_Spanish, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...FamilyUse_Spanish <- factor(survey$U...FamilyUse_Spanish, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...FriendsUse_Spanish <- factor(survey$U...FriendsUse_Spanish, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

t1.df = data.frame(xtabs(~ U...Home.WorkUse_Spanish + U...YourselfUse_Spanish, survey))


png("home-yourself-Spanish.png", units="px", width=1600, height=1000, res=300)
t1 = ggballoonplot(t1.df, x = "U...Home.WorkUse_Spanish", y = "U...YourselfUse_Spanish", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Familia") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

t2.df = data.frame(xtabs(~ U...Home.WorkUse_Spanish + U...FamilyUse_Spanish, survey))


png("home-family-Spanish.png", units="px", width=1600, height=1000, res=300)
t2 = ggballoonplot(t2.df, x = "U...Home.WorkUse_Spanish", y = "U...FamilyUse_Spanish", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Familia") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

t3.df = data.frame(xtabs(~ U...Home.WorkUse_Spanish + U...FriendsUse_Spanish, survey))

png("home-friends-Spanish.png", units="px", width=1600, height=1000, res=300)
t3 = ggballoonplot(t3.df, x = "U...Home.WorkUse_Spanish", y = "U...FriendsUse_Spanish", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Amigos") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

install.packages("gridExtra")
library("gridExtra")  


png("Spanish-work-other-environments.png", units="px", width=3000, height=1500, res=300)
grid.arrange(t1,t2,t3, ncol = 3, top = textGrob("Uso del español en el trabajo, el hogar y otros entornos"))
dev.off()



#### English - work and other environments ####

survey$U...Home.WorkUse_English <- factor(survey$U...Home.WorkUse_English, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...YourselfUse_English <- factor(survey$U...YourselfUse_English, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...FamilyUse_English <- factor(survey$U...FamilyUse_English, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))
survey$U...FriendsUse_English <- factor(survey$U...FriendsUse_English, levels = c("nunca", "casi nunca", "a veces", "casi siempre", "siempre"))

t1.df = data.frame(xtabs(~ U...Home.WorkUse_English + U...YourselfUse_English, survey))


png("home-yourself-English.png", units="px", width=1600, height=1000, res=300)
t1 = ggballoonplot(t1.df, x = "U...Home.WorkUse_English", y = "U...YourselfUse_English", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Familia") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

t2.df = data.frame(xtabs(~ U...Home.WorkUse_English + U...FamilyUse_English, survey))


png("home-family-English.png", units="px", width=1600, height=1000, res=300)
t2 = ggballoonplot(t2.df, x = "U...Home.WorkUse_English", y = "U...FamilyUse_English", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Familia") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

t3.df = data.frame(xtabs(~ U...Home.WorkUse_English + U...FriendsUse_English, survey))

png("home-friends-English.png", units="px", width=1600, height=1000, res=300)
t3 = ggballoonplot(t3.df, x = "U...Home.WorkUse_English", y = "U...FriendsUse_English", fill = "Freq") +
  scale_fill_viridis_c(option = "C") +
  labs(x = "Casa/Trabajo", y = "Amigos") + theme_bw()+ theme(legend.position="none",axis.text.x=element_text(angle=40,hjust=1))
dev.off()

install.packages("gridExtra")
library("gridExtra")  
library(grid)

png("English-work-other-environments.png", units="px", width=3000, height=1500, res=300)
grid.arrange(t1,t2,t3, ncol = 3, top = textGrob("Uso del inglés en el trabajo, el hogar y otros entornos"))
dev.off()
