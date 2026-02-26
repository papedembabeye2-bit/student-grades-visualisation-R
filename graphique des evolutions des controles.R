le_plus_grand <- calcul_note_CP_Mauve %>% arrange(desc(`Moyenne General`))


library(dplyr)
library(tidyr)
library(ggplot2)

donnees_long <- le_plus_grand %>%
  pivot_longer(
    cols = c(`1er Contrôle`, `2eme contrôle`, `Moyenne General`),
    names_to = "Type",
    values_to = "Note"
  )

# Extraire la moyenne générale par élève
moyennes <- donnees_long %>%
  filter(Type == "Moyenne General") %>%
  select(`PRENOMS - NOMS2`, Moyenne = Note)

# Joindre la moyenne à toutes les lignes
donnees_long <- donnees_long %>%
  left_join(moyennes, by = "PRENOMS - NOMS2") %>%
  mutate(
    Groupe = case_when(
      Moyenne >= 6 & Moyenne < 7 ~ "Entre 6 et 7",
      Moyenne >= 8 & Moyenne < 9 ~ "Entre 8 et 9",
      Moyenne >= 9 ~ "9 et plus",
      TRUE ~ "Autres"
    )
  )

# Graphique par groupe
ggplot(donnees_long, aes(x = Type, y = Note, group = `PRENOMS - NOMS2`, color = Groupe)) +
  geom_line(alpha = 0.6) +
  geom_point() +
  labs(x = "Évaluations", y = "Note", title = "Évolution des notes par groupe de moyenne générale") +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Calcul de la moyenne générale de la classe
moyenne_classe <- le_plus_grand %>%
  summarise(MoyenneClasse = mean(`Moyenne General`, na.rm = TRUE)) %>%
  pull(MoyenneClasse)

# Diagramme en bandes des moyennes générales par élève
ggplot(le_plus_grand, aes(x = reorder(`PRENOMS - NOMS2`, `Moyenne General`), 
                          y = `Moyenne General`)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = moyenne_classe, color = "red", linetype = "dashed", size = 1) +
  coord_flip() +
  labs(x = "Élèves", y = "Moyenne générale", 
       title = "Classement des élèves avec moyenne de la classe en référence") +
  theme_minimal()

