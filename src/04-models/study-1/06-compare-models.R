

# STUDY 1 -----------------------------------------------------------------
loo_model_1 <- loo(model_1_hurdle)
loo_improper_1 <- loo(model_1_improper)
#loo_vague_1 <- loo(model_1_vague) # not sampling properly

loo_comp_1 <- loo_compare(loo_model_1, 
                          #loo_vague_1, # not sampling properly
                          loo_improper_1)

    
