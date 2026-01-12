
# Study 1
## individual characteristics influence a person's funcitoning, risk for trauma exposure, likelihood of joining the military, and risk for post-traumatic stress
## Joining the military influences risk of trauma exposure, as well as functioning directly
dag_1 <-
  
  dagify('PF' ~ 'PTS' + 'Tr' + 'Ind' + 'Mil',
         'PTS' ~ 'Tr' + 'Ind' + 'Mil',
         'Mil' ~ 'Ind',
         'Tr' ~ 'Mil' +  'Ind',
         exposure = 'PTS', 
         outcome = 'PF')


coordinates(dag_1) <- list(
  x = c(T = 0, PTS =  .1, PF = 1, Ind = -.5, Tr = -.5, Mil = -.25),
  y = c(T = 1, PTS = -.29, PF = 0, Ind = 1, Tr = -1, Mil = 0)
)

(plot_dag_1 <-
    ggdag(dag_1) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG: General ')
)

## What to adjust for to get the total effect of post-traumatic stress on functioning?
dag_1 |> adjustmentSets()



# Study 2 -----------------------------------------------------------------


## For Military OSI
dag_1 <-
  
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI' + 'Ind',
         'PTSD' ~ 'Combat' + 'Ind',
         'MI' ~ 'Combat' + 'Ind',
         'OSI' ~ 'Combat' + 'Ind',
         'Combat' ~ 'Ind',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_1) <- list(
  x = c(MI = 0, PTSD =  0, PF = 1, Combat = -1, OSI = 0, Ind = -1),
  y = c(MI = 1, PTSD = -1, PF = 0, Combat = 0, OSI = 0, Ind = 1)
)

(plot_dag_1 <-
    ggdag(dag_1) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 1')
)

#ggsave(here::here('output/dags/dag-1.jpeg'), plot = plot_dag_1)
(dag_1_adj <- 
    ggdag_adjustment_set(dag_1, 
                         text_size = 3,
                         shadow = T,
                         text_col = 'black',
    ) + 
    theme_dag() +
    ggsci::scale_color_bmj() + 
    labs(title = 'Adjustment Sets: DAG 1')
)

#ggsave(here::here('output/dags/dag-1-adj.jpeg'), plot = dag_1_adj)



# DAG 2a: Account for Participation ----------------------------------------
dag_2a <-
  
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI' + 'Ind',
         'PTSD' ~ 'Combat' + 'Ind',
         'MI' ~ 'Combat' + 'Ind',
         'OSI' ~ 'Combat' + 'Ind',
         'Combat' ~ 'Ind',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_2a) <- list(
  x = c(MI = 0, PTSD =  0, PF = 1, Combat = -1, OSI = 0, Ind = -1),
  y = c(MI = 1, PTSD = -1, PF = 0, Combat = 0, OSI = 0, Ind = 1)
)

(plot_dag_2a <-
    ggdag(dag_2a) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 2a')
)



dag_2a %>% adjust_for('Particpate') %>% adjustmentSets()



(dag_2a_adj <- 
    dag_2a %>% adjust_for('Particpate') %>%
       ggdag_adjustment_set(dag_1, 
                            text_size = 3,
                            shadow = T,
                            text_col = 'black',
       ) + 
       theme_dag() +
       ggsci::scale_color_bmj() + 
       labs(title = 'Adjustment Sets: DAG 1')
    )
  

  #ggsave(here::here('output/dags/dag-2a.jpeg'), plot = plot_dag_2a)






