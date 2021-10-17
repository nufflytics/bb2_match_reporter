library(plumber)

#* @apiTitle Plumber Example API

#* Echo back the input
#* @param msg The message to echo
#* @get /leaderboard
function(msg = "") {
   plumber::include_rmd("chaos.Rmd")
}