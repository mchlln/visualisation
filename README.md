# Fouille Extraction Visualisation

## Ressources

- [Jeu de donnée: data.gouv](https://www.data.gouv.fr/datasets/donnees-sur-la-localisation-et-lacces-de-la-population-aux-equipements/)
- [Légende des équipements utilisés](https://www.insee.fr/fr/metadonnees/source/operation/s2216/presentation)
- [Données sur les dépenses culturelles](https://www.data.gouv.fr/datasets/depenses-culturelles-des-communes/)
- [Données sur les dépenses en santé](https://www.data.gouv.fr/datasets/open-damir-base-complete-sur-les-depenses-dassurance-maladie-interregimes)

## Idées:

- Identifier les zones les plus isolées, les plus denses en équipement
- Voir s'il y a une corrélation entre l'éloignement à des services de santé et les dépenses en santé des habitants.
- Distance avec les équipements et rapport avec le salaire moyen des habitants
- Voir s'il y a une liaison entre la présence d'établissements d'éduction supérieure et de certains types d'équipements
- Est-ce que les communes qui ont peu d'établissements culturels dépensent moins en budget culturel par habitant que celles avec beaucoup d'établissments culturels
- Est-ce que les dépenses en culture augmentent plus on se trouve proche d'un équipement culturel
- Quel équipement fait le plus grimper le prix de l'immobilier
- Types d'équipement en fonction de la densité : est-ce qu'à partir d'une certaine densité, on retrouve certains équipements plus proches
- Quel est le sport le plus accessible selon l'endroit de vie (terrains de foot + présents à la campagne/périphérie, salle de sport en centre ville)

## Technique

- R
- PostGreSQL pour nos données : peut être distant, possibilité de faire des opérations SQL
- package arrow pour gérer les .parquet
- dplyr pour faire un workflow lazy (execution seulement quand on le demande et directement sur disque)
- Shiny pour faire de l'interractif