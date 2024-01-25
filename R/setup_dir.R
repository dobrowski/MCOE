
#' @title MCOE

#' Set up initial project directory
#'
#' @export

setup_dir <- function() {
    dir.create("data")
    dir.create("figs")
    }


#' The inverse of _in_ where the list is excluded
#'
#' @param x Item or list to look at
#' @param table List to make sure it is not part of
#'
#' @export


`%notin%` <- function(x,table) {
    !`%in%`(x,table)
}

#' Rounding the way most people do it with .5 roudning up
#'
#' @param x number to be rounded
#'
#' @param digits how many digits to round to
#'
#' @export


round2 = function(x, digits=2) {
    posneg = sign(x)
    z = abs(x)*10^digits
    z = z + 0.5
    z = trunc(z)
    z = z/10^digits
    z*posneg
}




#' Adds the MCOE logo
#'
#' @export


    mcoe_logo <- function() {
        magick::image_read(system.file("logo/MCOE_logo.png", package = "MCOE"))
    }





#' Given a CDS code it provides the logo
#'
#' @param cds.number County-District-School code from California Department of Education
#'
#' @export


    mcoe_logo_location <- function(cds.number) {

        options(scipen = 9999)

        target <-       mry.dist %>%
            dplyr::filter(cds == as.character(cds.number)) %>%
            dplyr::select(logoname)

        logo.location <- target[1,1] %>%
            as.character()

    #    print(logo.location)

        system.file(logo.location, package = "MCOE")

  #      magick::image_read(system.file(logo.location, package = "MCOE"))
    }



    #'Given a CDS code, adds the district logo
    #'
    #' @param cds.number County-District-School code from California Department of Education
    #'
    #' @export


    mcoe_d_logo <- function(cds.number) {

        mcoe_logo_location(cds.number)

              magick::image_read( mcoe_logo_location(cds.number)  )
    }






#' Modifies ggplots to use a common theme
#'
#' @export

    mcoe_theme <- list(ggthemes::theme_hc(),
                     ggthemes::scale_fill_few() ,
                     ggplot2::theme(plot.title.position = "plot"),
                     ggplot2::labs(x = "",
                          y = "",
                          fill ="") )





    #' Given a CDS code, it gives the name of the district
    #'
    #' @param cds.number County-District-School code from California Department of Education
    #'
    #' @export

    mcoe_name <-  function(cds.number) {

        options(scipen = 9999)

         target <-       mry.dist %>%
            dplyr::filter(cds == as.character(cds.number)) %>%
            dplyr::select(District)

            target[1,1] %>%
            as.character()

    }



    #' Connects to the MCOE SQL tables
    #'
    #' @export

    mcoe_sql_con <-  function() {

      DBI::dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = "sql-prod-02.mcoe.monterey.k12.ca.us",
                       Database = "Ed_Services_Data",
                       UID      = keyring::key_get("sql username"),
                       PWD      = keyring::key_get("sql password")
      )
    }





    #' When using the MCOE SQL tables, it will merge codebook descriptions from CDE
    #'
    #' @param df local dataframe you want to append labels to
    #'
    #' @param tablename which SQL table is the data from
    #' @param field which field in the table do you want the labels for
    #'
    #' @export

    left_join_codebook <-  function(df, tablename, field) {

        sheet_id <- "https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0"

        codebook <- googlesheets4::read_sheet(sheet_id,
                               col_types = "ccccccD")

        codebook.short <- codebook %>%
          dplyr::filter(table == tablename,
                 field_name == field) %>%
          dplyr::select(variable,definition)

        dplyr::left_join(df, codebook.short, by = setNames( "variable", field))


    }



    #' Graphing function to make lollipop bar graphs
    #'
    #' @param df dataframe source to graph
    #'
    #' @param y_var variable for Y
    #' @param x_var variable for X
    #' @param colorme color to make the bars
    #'
    #' @export


    lollipop <- function(df, y_var, x_var, colorme) {
      ggplot2::ggplot(df, aes( y = {{y_var}}/100,
                               x =forcats::fct_reorder({{x_var}},{{y_var}}) ,
                               label = scales::percent({{y_var}}/100, accuracy = .1))) +
        geom_segment( aes(x=forcats::fct_reorder({{x_var}}, {{y_var}}/100),
                          xend=forcats::fct_reorder({{x_var}}, {{y_var}}/100),
                          y=0,
                          yend={{y_var}}/100),
                      color=colorme,
                      size =2 ) +
        geom_point( color=colorme, size=5, alpha=0.6) +
        coord_flip() +
        geom_text(size = 3, color = "black") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
        theme_hc() +
        mcoe_theme
    }
