
#' @title MCOE
#'
#'
#' @export

setup_dir <- function()
{dir.create("data")
    dir.create("figs")
    }


#' @export


`%notin%` <- function(x,table) !`%in%`(x,table)


#' @export


round2 = function(x, digits=2) {
    posneg = sign(x)
    z = abs(x)*10^digits
    z = z + 0.5
    z = trunc(z)
    z = z/10^digits
    z*posneg
}




#' @export


    mcoe_logo <- function() {
        magick::image_read(system.file("logo/MCOE_logo.png", package = "MCOE"))
    }





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



    #' @export


    mcoe_d_logo <- function(cds.number) {

        mcoe_logo_location(cds.number)

              magick::image_read( mcoe_logo_location(cds.number)  )
    }






#' @export

    mcoe_theme <- list(ggthemes::theme_hc(),
                     ggthemes::scale_fill_few() ,
                     #           geom_text(size = 2, position = position_dodge(width = 1)),
                     ggplot2::theme(plot.title.position = "plot"),
                     ggplot2::labs(x = "",
                          y = "",
                          fill ="") )





    #' @export

    mcoe_name <-  function(cds.number) {

        options(scipen = 9999)

         target <-       mry.dist %>%
            dplyr::filter(cds == as.character(cds.number)) %>%
            dplyr::select(District)

            target[1,1] %>%
            as.character()

    }



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



