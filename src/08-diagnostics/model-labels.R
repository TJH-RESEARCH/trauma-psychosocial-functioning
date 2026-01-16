
# Helpful function to label diagnostics with model information

get_model_labels <-
  function(model_name){

    # Detect the labels given the R object name
    model_num <- 
      case_when(
        str_detect(model_name, "_1") ~ "Sample 1",
        str_detect(model_name, "_2") ~ "Sample 2",
        str_detect(model_name, "_3") ~ "Sample 3",
        .default = NA
      )
    
    model_type <- 
      case_when(
        str_detect(model_name, "pcl") ~ "PCL",
        str_detect(model_name, "mios") ~ "MIOS",
        str_detect(model_name, "adjust") ~ "Adjusted",
        str_detect(model_name, "interact") ~ "Interaction",
        .default = "Hurdle"
      )
    
    model_prior <- 
      case_when(
        str_detect(model_name, "improper") ~ "Improper",
        str_detect(model_name, "vague") ~ "Vague",
        !str_detect(model_name, "improper") & !str_detect(model_name, "vague") ~ "Weakly Informative",
        .default = NA
      )
    
    model_controls <- 
      case_when(
        str_detect(model_name, "_bi") ~ "Bivariate",
        .default = "Controls"
      )
    
    model_labels <- 
      list(
      model_num = model_num, 
      model_type = model_type, 
      model_prior = model_prior,
      model_controls = model_controls)
    
    return(model_labels)
    
  }

