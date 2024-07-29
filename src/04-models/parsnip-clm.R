
str(ordinal::clm)

library(parsnip)

# Register model, modes, and arguments -----------------------------------------------------------
set_new_model(model = 'clm')
set_model_mode(model = "clm", mode = "classification")
set_model_engine(
  "clm", 
  mode = "classification", 
  eng = "clm"
)
set_dependency("clm", eng = "clm", pkg = "ordinal")

parsnip::show_model_info('clm')


# Set arguments -----------------------------------------------------------
set_model_arg(
  model = "clm",
  eng = "clm",
  parsnip = "link",
  original = "link",
  func = list(pkg = 'ordinal', fun = 'link'),
  has_submodel = FALSE
)

set_model_arg(
  model = "clm",
  eng = "clm",
  parsnip = "threshold",
  original = "threshold",
  func = list(pkg = 'ordinal', fun = 'threshold'),
  has_submodel = FALSE
)


# Create function ----------------------------------------------------------
clm <-
  function(mode = "classification",  sub_classes = NULL) {
    # Check for correct mode
    if (mode  != "classification") {
      rlang::abort("`mode` should be 'classification'")
    }
    
    # Capture the arguments in quosures
    args <- list(sub_classes = rlang::enquo(sub_classes))
    
    # Save some empty slots for future parts of the specification
    new_model_spec(
      "clm",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

parsnip::show_model_info('clm')





# Add a mit module -----------------------------------------------------------------
set_fit(
  model = "clm",
  eng = "clm",
  mode = "classification",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "ordinal", fun = "clm"),
    defaults = list()
  )
)

set_encoding(
  model = "clm",
  eng = "clm",
  mode = "classification",
  options = list(
    predictor_indicators = "traditional",
    compute_intercept = FALSE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

parsnip::show_model_info('clm')




# Add Modules for Prediction ----------------------------------------------


class_info <- 
  list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      # These lists should be of the form:
      # {predict.mda argument name} = {values provided from parsnip objects}
      list(
        # We don't want the first two arguments evaluated right now
        # since they don't exist yet. `type` is a simple object that
        # doesn't need to have its evaluation deferred. 
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "class"
      )
  )

prob_info <-
  pred_value_template(
    post = function(x, object) {
      tibble::as_tibble(x)
    },
    func = c(fun = "predict"),
    # Now everything else is put into the `args` slot
    object = quote(object$fit),
    newdata = quote(new_data),
    type = "posterior"
  )

set_pred(
  model = "clm",
  eng = "clm",
  mode = "classification",
  type = "class",
  value = class_info
)


# Test --------------------------------------------------------------------

parsnip::show_model_info('clm')

#clm(mode = 'classification') %>% 
#set_engine(engine = 'clm') %>% 
#  fit(bipf_category ~ mios_scaled + pc_ptsd_positive_screen_scaled + service_era_post_911_scaled + service_era_persian_gulf_scaled + sex_female_scaled + race_black_scaled + race_white_scaled,
#      data = data_scaled)
    



