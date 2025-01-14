box::use(box / deps_,
  box / help_,
  box / ops[...],
)

.on_load <- function(ns) {
  deps_$need(
    "foreach"
  )
}


# set runloop value
set_parallel_type <- function(is_parallel) {

  if (is_parallel) choice <- foreach::`%dopar%`
  if (!is_parallel) choice <- foreach::`%do%`

  return(choice)
}
