calc_num_fish_aged <- function(samples,
                               fns = here::here("data-output",
                                                c("can-ft-num-fish-aged.csv",
                                                  "can-ss-num-fish-aged.csv",
                                                  "can-jv-num-fish-aged.csv")),
                               write_files = TRUE){

  lst <- map2(samples, fns, ~{
    tab <- .x |>
      group_by(year) |>
      summarize(num_fish = sum(!is.na(age))) |>
      ungroup() |>
      filter(num_fish > 0)
    if(write_files){
      # Need the `as.data.frame` below to ensure no quotes around the headers
      write_csv(as.data.frame(tab), .y, quote = "none")
    }
    tab
  })

  if(!write_files){
    lst
  }else{
    invisible()
  }
}