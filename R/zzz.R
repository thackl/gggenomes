.onAttach <- function(libname, pkg) {
  pkg_ver <- paste0(pkg, " v", utils::packageDescription(pkg, fields="Version"))
  citfile <- file.path(system.file(package = pkg), "CITATION")
  cit <- format(utils::readCitationFile(citfile), style="text")
  packageStartupMessage(paste0(pkg_ver, "\n\nIf you use '", pkg, "' in published research, please cite:\n\n", cit))
}

register_scalebar_theme_elements <- function() {
  style <- scalebar_style()

  ggplot2::register_theme_elements(
    gggenomes.scalebar.line = style$line,
    gggenomes.scalebar.text = style$text,
    gggenomes.scalebar.tick.height = style$tick_height,
    gggenomes.scalebar.label.offset = style$label_offset,

    element_tree = list(
      gggenomes.scalebar.line =
        ggplot2::el_def("element_line", "line"),

      gggenomes.scalebar.text =
        ggplot2::el_def("element_text", "text"),

      gggenomes.scalebar.tick.height =
        ggplot2::el_def("unit"),

      gggenomes.scalebar.label.offset =
        ggplot2::el_def("unit")
    )
  )
}

.onLoad <- function(libname, pkgname) {
  register_scalebar_theme_elements()
}
