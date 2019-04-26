#' @export
launch_stnmApp <- function(.app = "ShippingTransitNetworkMaker",
                             .use_browser = TRUE) {
  dir <- system.file("shinyApps", .app, package = "COREmaritime")
  
  shiny::runApp(appDir = dir, launch.browser = .use_browser)
}
