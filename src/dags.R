library(tidyverse)
library(ggdag)
library(dagitty)


dag_1 <- 
  dagify('PF' ~ 'MI',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_1) <- list(
  x = c(MI = 0, PF = 1),
  y = c(MI = 0, PF = 0)
)

(plot_1_adj <-
    ggdag(dag_1) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 1')
)

#ggsave(here::here('output/dags/dag-1.jpeg'), plot = dag_1_adj)



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

dag_2 <- 
  
  dagify('PF' ~ 'MI' + 'PTSD',
         'PTSD' ~ 'Combat',
         'MI' ~ 'Combat',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_2) <- list(
  x = c(MI = 0, PTSD =  0, PF = 1, Combat = -1),
  y = c(MI = 1, PTSD = -1, PF = 0, Combat = 0)
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
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

dag_3 <- 
  
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI',
         'PTSD' ~ 'Combat',
         'MI' ~ 'Combat',
         'OSI' ~ 'Combat',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_3) <- list(
  x = c(MI = 0, PTSD =  0, PF = 1, Combat = -1, OSI = 0),
  y = c(MI = 1, PTSD = -1, PF = 0, Combat = 0, OSI = 0)
)

(plot_dag_3 <-
    ggdag(dag_3) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 3')
)

#ggsave(here::here('output/dags/dag-3.jpeg'), plot = plot_dag_3)


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


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


dag_4 <- 
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI',
         'PTSD' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'MI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'OSI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'Combat' ~ 'Era' + 'Gender' + 'Race',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_4) <- list(
  x = c(Combat = 0, Era = .64, Race = .32, Gender = .31, MI = .5, OSI = .5, PTSD =  .5, PF = 1),
  y = c(Combat = 0,  Era =  -.43, Race = -.40, Gender =  .36, MI = 1, OSI = 0, PTSD = -1, PF = 0)
)

(plot_dag_4 <-
    ggdag(dag_4) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 4')
)

#ggsave(here::here('output/dags/dag-3.jpeg'), plot = plot_dag_3)


(dag_4_adj <- 
    ggdag_adjustment_set(dag_4, 
                         text_size = 3,
                         shadow = T,
                         text_col = 'black',
    ) + 
    theme_dag() +
    ggsci::scale_color_bmj() + 
    labs(title = 'Adjustment Sets: DAG 4')
)

#ggsave(here::here('output/dags/dag-4-adj.jpeg'), plot = dag_5_adj)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


dag_5 <- 
  dagify('PF' ~ 'MI' + 'PTSD',
         'PTSD' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'MI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'Combat' ~ 'Era' + 'Gender' + 'Race',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_5) <- list(
  x = c(Combat = 0, Era = .64, Race = .32, Gender = .31, MI = .5, PTSD =  .5, PF = 1),
  y = c(Combat = 0,  Era =  -.43, Race = -.40, Gender =  .36, MI = 1, PTSD = -1, PF = 0)
)

(plot_dag_5 <-
    ggdag(dag_5) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 5')
)

#ggsave(here::here('output/dags/dag-3.jpeg'), plot = plot_dag_3)


(dag_5_adj <- 
    ggdag_adjustment_set(dag_5, 
                         text_size = 3,
                         shadow = T,
                         text_col = 'black',
    ) + 
    theme_dag() +
    ggsci::scale_color_bmj() + 
    labs(title = 'Adjustment Sets: DAG 5')
)

#ggsave(here::here('output/dags/dag-4-adj.jpeg'), plot = dag_4_adj)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

dag_6 <- 
  dagify('PF' ~ 'MI' + 'PTSD' + 'OSI' + 'Combat' + 'Era' + 'Gender' + 'Race',
         'PTSD' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'MI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'OSI' ~ 'Combat' + 'Era' + 'Gender' + 'Race',
         'Combat' ~ 'Era' + 'Gender' + 'Race',
         exposure = 'MI', 
         outcome = 'PF')


coordinates(dag_6) <- list(
  x = c(Combat = 0, Era = .64, Race = .32, Gender = .31, MI = .5,  OSI = .5, PTSD =  .5, PF = 1),
  y = c(Combat = 0,  Era =  -.43, Race = -.40, Gender =  .36, MI = 1,  OSI = 0, PTSD = -1, PF = 0)
)

(plot_dag_6 <-
    ggdag(dag_6) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 5')
)

#ggsave(here::here('output/dags/dag-3.jpeg'), plot = plot_dag_3)


(dag_6_adj <- 
    ggdag_adjustment_set(dag_6, 
                         text_size = 3,
                         shadow = T,
                         text_col = 'black',
    ) + 
    theme_dag() +
    ggsci::scale_color_bmj() + 
    labs(title = 'Adjustment Sets: DAG 5')
)

#ggsave(here::here('output/dags/dag-4-adj.jpeg'), plot = dag_4_adj)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


dag_7 <- 
  dagify(
         
         
         'Era' ~ 'Age',
         'Rank' ~~ 'Education',
         
         'Income' ~~ 'Rank' + 'Education',
         'n_child' ~ 'Marital',
         
         'MOS' ~    'Branch' + 'Gender' + 'Race' + 'Marital' + 'n_child',
         
         'Combat' ~ 'Branch' + 'Gender' + 'Race' + 'Marital' + 'n_child' + 'Era' + 'MOS' + 'Rank',
         
         'Leadership' ~~ 'Cohesion',
         
         'PTSD' ~ 'Combat' + 'Era' + 'Gender' + 'Race' + 'MOS' + 'Cohesion' + 'Leadership',
         'MI' ~ 'Combat' + 'Era' + 'Gender' + 'Race' + 'Cohesion' + 'Leadership' + 'MOS',
         'OSI' ~ 'Combat' + 'Era' + 'Gender' + 'Race' + 'MOS',
         
         
         'PF' ~ 'MI' + 'PTSD' + 'OSI' + 'Combat' + 'Era' + 'Gender' + 'Race' + 'Education' + 'Rank' + 'Income' + 'Marital' + 'n_child' + 'non-OHI MH',
         
         'Particpate' ~ 'Branch' + 'Era' + 'MOS' + 'Gender' + 'Race' + 'Marital' + 'n_child' +  'Rank' + 'Age' + 'Income' + 'Education',
         
         exposure = 'MI', 
         outcome = 'PF')



(plot_dag_7 <-
    ggdag(dag_7) +
    geom_dag_point(fill = 'white', color = 'white') +
    geom_dag_edges() +
    geom_dag_text(color = 'black', size = 3) +
    theme_dag() +
    labs(title = 'DAG 7')
)

#ggsave(here::here('output/dags/dag-7.jpeg'), plot = plot_dag_7)


(dag_7_adj <-  
    ggdag_adjustment_set(dag_7, 
                         text_size = 3,
                         shadow = T,
                         text_col = 'black',
    ) + 
    theme_dag() +
    ggsci::scale_color_bmj() + 
    labs(title = 'Adjustment Sets: DAG 7')
)

adjustmentSets(dag_7)

#ggsave(here::here('output/dags/dag-7-adj.jpeg'), plot = dag_7_adj)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------




plot_dag_2
plot_dag_3
plot_dag_4
plot_dag_5

dag_2_adj
dag_3_adj
dag_4_adj
dag_5_adj

