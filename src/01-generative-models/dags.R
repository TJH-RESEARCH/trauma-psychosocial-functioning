library(tidyverse)
library(ggdag)
library(dagitty)

dag_1 <-
  
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI',
         'PTSD' ~ 'Combat',
         'MI' ~ 'Combat',
         'OSI' ~ 'Combat',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_1) <- list(
  x = c(MI = 0, PTSD =  0, PF = 1, Combat = -1, OSI = 0),
  y = c(MI = 1, PTSD = -1, PF = 0, Combat = 0, OSI = 0)
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


# DAG 2: Role of Basic Demographics --------------------------------------------
dag_2 <- 
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI' + 'Era' + 'Gender' + 'Race',
         'PTSD' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'MI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'OSI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'Combat' ~ 'Era' + 'Gender' + 'Race',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_2) <- list(
  x = c(Combat = 0, Era = .64, Race = .32, Gender = .31, MI = .5, OSI = .5, PTSD =  .5, PF = 1),
  y = c(Combat = 0,  Era =  -.43, Race = -.40, Gender =  .36, MI = 1, OSI = 0, PTSD = -1, PF = 0)
)

(plot_dag_2 <-
    ggdag(dag_2) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 2')
)

#ggsave(here::here('output/dags/dag-2.jpeg'), plot = plot_dag_2)

adjustmentSets(dag_2)
(dag_2_adj <- 
    ggdag_adjustment_set(dag_2, 
                         text_size = 3,
                         shadow = T,
                         text_col = 'black',
    ) + 
    theme_dag() +
    ggsci::scale_color_bmj() + 
    labs(title = 'Adjustment Sets: DAG 2')
)

#ggsave(here::here('output/dags/dag-2-adj.jpeg'), plot = dag_2_adj)


# -------------------------------------------------------------------------
# DAG 3: Thinking Widely About other Confounds ---------------------------------
dag_3 <- 
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI' + 'Era' + 'Gender' + 'Race' + 'Education' + 'Rank' + 'Income' + 'Marital' + 'n_child' + 'MH',
         'PTSD' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'MI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'OSI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'Combat' ~ 'Branch' + 'Era' + 'Gender' + 'Race' + 'MOS',
         'MOS' ~ 'Branch' + 'Era' + 'Gender' + 'Race' + 'Education' + 'Income' + 'Marital' + 'n_child',
         'Education' ~ 'Rank' + 'Income' + 'Gender' + 'Race',
         'MH' ~ 'Era' + 'Gender' + 'Race' + 'Education' + 'Rank' + 'Income' + 'Marital' + 'n_child',
         
         'Income' ~~ 'Rank' + 'Education',
         'n_child' ~ 'Marital',
         
         exposure = 'MI', 
         outcome = 'PF')



(plot_dag_3 <-
    ggdag(dag_3) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 3')
)

#ggsave(here::here('output/dags/dag-3.jpeg'), plot = plot_dag_3)

adjustmentSets(dag_3)

(dag_3_adj <- 
    ggdag_adjustment_set(dag_3, 
                         text_size = 3,
                         shadow = T,
                         text_col = 'black',
    ) + 
    theme_dag() +
    ggsci::scale_color_bmj() + 
    labs(title = 'Adjustment Sets: DAG 3')
)

#ggsave(here::here('output/dags/dag-3-adj.jpeg'), plot = dag_3_adj)



# DAG 4: Account for Participation ----------------------------------------
dag_4 <- 
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI' + 'Era' + 'Gender' + 'Race' + 'Education' + 'Rank' + 'Income' + 'Marital' + 'n_child' + 'MH',
         'PTSD' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'MI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'OSI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'Combat' ~ 'Branch' + 'Era' + 'Gender' + 'Race' + 'MOS',
         'MOS' ~ 'Branch' + 'Era' + 'Gender' + 'Race' + 'Education' + 'Income' + 'Marital' + 'n_child',
         'Education' ~ 'Rank' + 'Income' + 'Gender' + 'Race',
         'MH' ~ 'Era' + 'Gender' + 'Race' + 'Education' + 'Rank' + 'Income' + 'Marital' + 'n_child',
         
         'Income' ~~ 'Rank' + 'Education',
         'n_child' ~ 'Marital',
         
         'Particpate' ~ 'Branch' + 'Era' + 'MOS' + 'Gender' + 'Race' + 'Marital' + 'n_child' +  'Rank' + 'Age' + 'Income' + 'Education',
         
         exposure = 'MI', 
         outcome = 'PF')



(plot_dag_4 <-
    ggdag(dag_4) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 4) +
    theme_dag() +
    labs(title = 'DAG 4')
)

#ggsave(here::here('output/dags/dag-4.jpeg'), plot = plot_dag_4)

dag_4 %>% adjust_for('Particpate') %>% adjustmentSets()



(dag_4_adj <- 
    dag_4 %>% adjust_for('Particpate') %>%
    ggdag_adjustment_set(text_size = 3,
                         shadow = T,
                         text_col = 'black',
    ) + 
    theme_dag() +
    ggsci::scale_color_bmj() + 
    labs(title = 'Adjustment Sets: DAG 4')
)

#ggsave(here::here('output/dags/dag-4-adj.jpeg'), plot = dag_4_adj)








