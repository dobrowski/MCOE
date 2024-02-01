
#' @title MCOE



#' The inverse of _in_ where the list is excluded
#'
#' @param x Item or list to look at
#' @param table List to make sure it is not part of
#'
#' @export
#'
#' @returns `%notin%` returns a 'list' of class `logical` indicating absence of passed items
#' @examples
#'
#' fruit <- c("apple", "banana", "pear", "pineapple")
#' "kiwi" %notin% fruit



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
#'
#' @returns `round2` returns a truncated `numeric` with decimal places equal to `digits` argument
#'
#' @examples
#' x <- 1.2345
#' round2(x, digits = 2)



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
#'
#' @returns `mcoe_logo` an object of class `magick-image` from [magick::image_read()] which is an image file of the MCOE logo
#'
#' @examples
#' mcoe_logo()



    mcoe_logo <- function() {
        magick::image_read(system.file("logo/MCOE_logo.png", package = "MCOE"))
    }





#' Given a CDS code it provides the logo
#'
#' @param cds.number County-District-School code from California Department of Education
#'
#' @export
#'
#' @importFrom dplyr .data
#'
#' @returns `mcoe_logo_location` returns a `string` containing a file location the district logo
#'
#' @examples
#' mcoe_logo_location("27659870000000")


    mcoe_logo_location <- function(cds.number) {

        old <- options()
        options(scipen = 9999)
        on.exit(options(old))

        target <-       mry.dist |>
            dplyr::filter(.data$cds == as.character(cds.number)) |>
            dplyr::select(.data$logoname)

        logo.location <- target[1,1] |>
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
    #'
    #'
    #' @returns `mcoe_d_logo` an object of class `magick-image` from [magick::image_read()] which is an image file of the district logo
    #'
    #' @examples
    #' try(mcoe_d_logo("27659870000000"))


    mcoe_d_logo <- function(cds.number) {

        mcoe_logo_location(cds.number)

              magick::image_read( mcoe_logo_location(cds.number)  )
    }






#' Modifies ggplots to use a common theme
#'
#' @export
#'
#' @returns a `list` of [ggplot] elements to add to a graph
#'
#' @examples
#' library(dplyr)
#' df.example <- dplyr::tribble(~name, ~rate, "Nina",32,"David",65)
#' g <- ggplot2::ggplot(df.example, ggplot2::aes(name, rate)) + ggplot2::geom_col()
#' g + mcoe_theme

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
    #'
    #' @importFrom dplyr .data
    #'
    #' @returns a `string` of the district name
    #'
    #' @examples
    #' mcoe_name("27661590000000")

    mcoe_name <-  function(cds.number) {

        old <- options()
        options(scipen = 9999)
        on.exit(options(old))

         target <-       mry.dist |>
            dplyr::filter(.data$cds == as.character(cds.number)) |>
            dplyr::select(.data$District)

            target[1,1] |>
            as.character()

    }



    #' Connects to the MCOE 'SQL' tables
    #'
    #' @export
    #'
    #' @returns a database connection in the form of [DBI::dbConnect]
    #'
    #' @examples
    #' try(mcoe_sql_con())

    mcoe_sql_con <-  function() {

      DBI::dbConnect(odbc::odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = "sql-prod-02.mcoe.monterey.k12.ca.us",
                       Database = "Ed_Services_Data",
                       UID      = keyring::key_get("sql username"),
                       PWD      = keyring::key_get("sql password")
      )
    }





    #' When using the MCOE 'SQL' tables, it will merge codebook descriptions from CDE
    #'
    #' @param df local dataframe you want to append labels to
    #'
    #' @param tablename which 'SQL' table is the data from
    #' @param field which field in the table do you want the labels for
    #'
    #'
    #' @importFrom dplyr .data
    #'
    #' @export
    #'
    #'
    #' @returns An `tibble` of the same type as `.data`. The output has an one column names `definition` which contains labels for the passed argument `field`.
    #' @examples
    #' try(
    #' sbac.filtered <- tbl(con, "CAASPP") |>
    #'  head(100) |>
    #'  collect() |>
    #'  left_join_codebook("CAASPP", "Subgroup_ID")
    #' )

    left_join_codebook <-  function(df, tablename, field) {

        sheet_id <- "https://docs.google.com/spreadsheets/d/1_EmvLQq-cUe8Ist_WYGJFgdjD27dajpfSbEd_djvbsc/edit#gid=0"

        codebook <- googlesheets4::read_sheet(sheet_id,
                               col_types = "ccccccD")

        codebook.short <- codebook |>
          dplyr::filter(table == tablename,
                 .data$field_name == field) |>
          dplyr::select(.data$variable,.data$definition)

        dplyr::left_join(df, codebook.short, by = stats::setNames( "variable", field))


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
    #'
    #'
    #' @returns a `ggplot` class graph with horiztontal bars with circles at the end of the bars
    #' @examples
    #' library(dplyr)
    #' df.example <- dplyr::tribble(~name, ~rate, "Nina",32,"David",65)
    #' lollipop(df = df.example, x_var = name, y_var = rate, colorme = "pink" )


    lollipop <- function(df, y_var, x_var, colorme) {
      ggplot2::ggplot(df, ggplot2::aes( y = {{y_var}}/100,
                               x =forcats::fct_reorder({{x_var}},{{y_var}}) ,
                               label = scales::percent({{y_var}}/100, accuracy = .1))) +
            ggplot2::geom_segment( ggplot2::aes(x=forcats::fct_reorder({{x_var}}, {{y_var}}/100),
                          xend=forcats::fct_reorder({{x_var}}, {{y_var}}/100),
                          y=0,
                          yend={{y_var}}/100),
                      color=colorme,
                      size =2 ) +
            ggplot2::geom_point( color=colorme, size=5, alpha=0.6) +
            ggplot2::coord_flip() +
            ggplot2::geom_text(size = 3, color = "black") +
            ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        #  facet_grid(facets = vars(`Student Group`), scales = "free" ) +
        ggthemes::theme_hc() +
        mcoe_theme
    }
