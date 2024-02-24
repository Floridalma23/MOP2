library(tidyverse)
library(agricolae)
library(readxl)
library(jtools)

datos <- read_excel("datos/Datos23035.xlsx",
                    sheet = "datos")
etiquetas <- read_excel("datos/Datos23035.xlsx",
                        sheet = "etiquetas")

datos <- datos |>
  mutate(
    Diámetro = (DiámetroH + DiámetroV)/2
  )

datos <- datos |>
  select(-c(5,6))

datos_largos <- datos |>
  pivot_longer(
    4:6,
    names_to = "Variable",
    values_to = "Valor"
  )

variables <- unique(datos_largos$Variable)

tcalc <- qt(0.975, 15)

stats_vars <- datos_largos |>
  group_by(Semana, Tratamiento, Variable) |>
  summarise(
    Media = mean(Valor),
    Desv = sd(Valor),
    eem = tcalc*Desv/sqrt(n()),
    Min = Media - eem,
    Max = Media + eem
  )

semanas <- unique(datos$Semana)

grupos <- data.frame(
  Tratamiento = "x",
  Semana = 0,
  Variable = "x",
  Grupos = "x"
)

for(i in seq_along(semanas)){
  for(j in seq_along(variables)){
    datos_largos_filtrados <- datos_largos |>
      filter(Variable == variables[j] & Semana == semanas[i])
    mod1 <- aov(Valor ~ Tratamiento, datos_largos_filtrados)
    grps_mod1 <- LSD.test(mod1, "Tratamiento")$groups
    grps_mod1 <- grps_mod1 |>
      rownames_to_column(var = "Tratamiento") |>
      select(-2) |>
      mutate(
        Variable = variables[j],
        Semana = semanas[i]
      ) |>
      rename(Grupos = groups)
    grupos <- rbind(grupos, grps_mod1)
  }
}

grupos <- grupos |>
  filter(Semana != 0)

stats_vars <- left_join(stats_vars, grupos)

for(i in seq_along(variables)){
  datos_largos_filtrados <- datos_largos |>
    filter(Variable == variables[i])
  stats_vars_filtrados <- stats_vars |>
    filter(Variable == variables[i])
  nombre1 <- paste0("graficos/",variables[i],"_box.png")
  nombre2 <- paste0("graficos/",variables[i],"_evol.png")
  nombre3 <- paste0("graficos/",variables[i],"_barras.png")
  etiqueta <- paste0(etiquetas[i,2])
  grafico1 <- ggplot(datos_largos_filtrados, aes(x = factor(Semana), y = Valor, fill = Tratamiento)) +
    geom_boxplot() +
    theme_apa() +
    labs(x = "Semana", y = etiqueta)
  
  grafico2 <-  ggplot(stats_vars_filtrados, aes(x = Semana, y = Media, col = Tratamiento)) +
    geom_point(aes(col = Tratamiento)) +
    geom_line(aes(col = Tratamiento)) +
    geom_errorbar(aes(ymin=Min, ymax=Max),
                  width=.3) +
    theme_apa() +
    scale_x_continuous(breaks = seq(min(datos$Semana), max(datos$Semana), 1) ) +
    labs(x = "Semana", y = etiqueta)
  grafico3 <- ggplot(stats_vars_filtrados, aes(x = factor(Semana), Media, fill = Tratamiento)) +
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=Min, ymax=Max),
                  width=.3,                    
                  position=position_dodge(.9)) +
    scale_fill_grey(start = 0.3, end = .8)  +
    theme_apa() +
    ylab(etiqueta) +
    xlab("Semana")
  maximo = max(stats_vars_filtrados$Media) + max(stats_vars_filtrados$eem) + 1
  grafico3 <- grafico3 + geom_text(data = stats_vars_filtrados, aes(x = Semana, y = maximo, label = Grupos), 
                      position = position_dodge(width = .75))
  ggsave(nombre1, grafico1, width = 8, height = 4)
  ggsave(nombre2, grafico2, width = 8, height = 4)
  ggsave(nombre3, grafico3, width = 8, height = 4)
}
