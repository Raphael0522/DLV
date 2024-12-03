#' Title Create Sassy report object by using titles and footnotes from DLV
#'
#' @param env_list An object that must contain study root, program name, program side.
#' @param output_name TLF output name.
#' @param output_type Output file type. Refer to output_type in create_report function in reporter.
#' @param headers_list Page titles and table titles.
#' @param footers_vector Page footers and table footers.
#' @param tbl Table data.
#' @param margin_list Margins for the report (top, bottom, left, right).
#' @return An report object.
#'
#' @import reporter
#' @importFrom purrr reduce
#' @export crt_rpt
#'
# @examples
crt_rpt <- function(env_list, output_name, output_type, headers_list, footers_vector, tbl, margin_list = NULL){

  if (is.null(margin_list)){
    t_margin_list <- c(top = 0.9525, bottom = 0.9525, left = 1.905, right = 1.905)
  }else {
    t_margin_list <- margin_list
  }

  # Create report spec. Assign output file name, location, type and font.
  rpt <- reporter::create_report(orientation = "landscape",
                                 file.path(env_list$pgm_location, paste0(env_list$pgm_name, {{output_name}})),
                                 output_type = {{output_type}}, font = "Courier", units = "cm") %>%
    # Assign font size
    reporter::options_fixed(font_size = 9) %>%
    reporter::set_margins(  top = t_margin_list[["top"]], bottom = t_margin_list[["bottom"]],
                            left = t_margin_list[["left"]], right = t_margin_list[["right"]]) %>%
    # Assign page header from dlv file
    reporter::page_header(headers_list$first_line[[1]], headers_list$first_line[[2]], width = 20) %>%
    # Assign title from dlv file
    reporter::titles( headers_list$second_line[[1]], headers_list$second_line[[2]], headers_list$second_line[[3]],
                      columns = 3, header = TRUE, bold = FALSE,
                      font_size = 9, blank_row = "none") %>%
    # Add footnotes from dlv file and a solid line on the top of the borders
    reporter::footnotes(footers_vector, borders = "top") %>%
    # Add page footer
    reporter::page_footer(paste0("Date ", Sys.time()), "Astellas", "Page [pg] of [tpg]") |>

    reporter::titles(as_vector(headers_list$titles), bold = FALSE, font_size = 9, align = "center", header = TRUE, blank_row = "below")

  rpt <- rpt %>%
    # Add table content
    reporter::add_content(tbl)

  rpt$user_line_count <- 48
  rpt$user_line_size <- 153

  rpt
}

