#' @title Insert base64.
#'
#' @description Insert a file in base64 encoding.
#'
#' @param name [\code{character}]\cr Name of file which should be inserted.
#'
#' @return File in base64 encoding.
#'
#' @examples insert_base64(name = read_example("text.png"))
#'
#' @export
insert_base64 = function(name) {
  # check input:
  assert_character(x = name, len = 1, any.missing = FALSE)

  # check if package base64enc is available:
  if(!requireNamespace("base64enc", quietly = TRUE)) {
    warning(paste0('Install package base64enc to use this function.'))
  }

  if(file.exists(name)) {
    return(paste0('<file name="', basename(name), '" encoding="base64">',
                  base64enc::base64encode(name),
                  '</file>'))
  } else {
    warning(paste0('File ', name, ' does not exist.'))
  }
}

#' @title Insert png in text.
#'
#' @description Insert png in text.
#'
#' @param name [\code{character}]\cr Name of file which should be inserted.
#' @param width [\code{number}]\cr Width of png. NULL to use original width.
#' Default is NULL.
#' @param height [\code{number}]\cr Height of png. NULL to use original height.
#' Default is NULL.
#'
#' @return png inserted in text.
#'
#' @examples insert_png(name = read_example("text.png"))
#'
#' @export
insert_png = function(name, width = NULL, height = NULL) {
  # check input:
  assert_character(x = name, len = 1, any.missing = FALSE)
  assert_number(x = width, lower = 0, finite = TRUE, null.ok = TRUE)
  assert_number(x = height, lower = 0, finite = TRUE, null.ok = TRUE)

  # check if package png is available:
  if(!requireNamespace("png", quietly = TRUE)) {
    warning(paste0('Install package png to use this function.'))
  }

  if(file.exists(name)) {
    # determine width and height of image:
    if(is.null(width) || is.null(height)) {
      tmp = png::readPNG(name)
      dims = dim(tmp)
      if(is.null(width)) width = dims[2]
      if(is.null(height)) height = dims[1]
    }
    # return text with png for Moodle:
    return(paste0('<img src="@@PLUGINFILE@@/', basename(name), '" alt="" role="presentation"',
                  'class="img-responsive atto_image_button_text-bottom" width="', width,
                  '" height="', height, '"><br>'))
  } else {
    warning(paste0('File ', name, ' does not exist.'))
  }
}

#' @title Read base64 from html file.
#'
#' @description Read all base64 from html.
#'
#' @param name [\code{character}]\cr Name of html file which should be read.
#'
#' @return base64 as list.
#'
#' @examples
#' read_base64_html(name = read_example("Graphics.html"))
#'
#' @export
read_base64_html = function(name) {
  # check input:
  assert_character(x = name, len = 1, any.missing = FALSE)

  if(file.exists(name)) {
    # read html:
    tmp = readLines(name)

    # find base64:
    tmp = tmp[stringr::str_detect(string = tmp, pattern = "data:image/png;base64")]
    images = vector(mode = "list", length = length(tmp))
    for(i in seq_along(tmp)) {
      tmpi = stringr::str_split(tmp[i], ",")[[1]][2]
      tmpi = stringr::str_split(tmpi, '\"')[[1]][1]
      images[[i]] = tmpi
    }
    # return images as base64:
    return(images)
  } else {
    warning(paste0('File ', name, ' does not exist.'))
  }
}

#' @title Write base64 to png.
#'
#' @description Write base64 to png.
#'
#' @param images [\code{list}]\cr List of images as base64.
#' @param names [\code{character}]\cr Names of images.
#' @param force [\code{flag}]\cr TRUE to force writing. Default is FALSE.
#'
#' @return Nothing returned but png created.
#'
#' @examples
#' \dontrun{
#' base64_to_png(images = read_base64_html(name = read_example("Graphics.html")),
#'               names = "text.png")
#' }
#'
#' @export
base64_to_png = function(images, names, force = FALSE) {
  # check input:
  assert_list(x = images, types = "character", any.missing = FALSE, min.len = 1)
  assert_character(x = names, any.missing = FALSE, len = length(images))

  # check if package base64enc is available:
  if(!requireNamespace("base64enc", quietly = TRUE)) {
    warning(paste0('Install package base64enc to use this function.'))
  }

  for(i in seq_along(images)) {
    n = paste0(names[i])
    if(!file.exists(n) || force) {
      # write png:
      con = file(description = n, open = "wb")
      base64enc::base64decode(what = images[[i]], output = con)
      close(con)
    } else {
      warning(paste0('File ', n, ' already exists. Use force = TRUE to ',
                     'force writing.'))
    }
  }
}

#' @title Render tikz in Rmd to png.
#'
#' @description Render tikz chunks in Rmd-file to png.
#'
#' @param Rmd [\code{character}]\cr Path to Rmd file with tikz code. See the
#' example for more information.
#' @param names [\code{character}]\cr Names of images.
#' @param force [\code{flag}]\cr TRUE to force writing. Default is FALSE.
#'
#' @return Nothing returned but png created.
#'
#' @details It might be necessary to install further R packages and magick to
#' be able to use this function.
#'
#' Looking at the file Graphics.Rmd might be useful.
#'
#' @examples
#' \dontrun{
#' render_tikz(Rmd = read_example(name = "Graphics.Rmd"),
#'             names = c("text1.png", "text2.png"), force = FALSE)
#' }
#'
#' @export
render_tikz = function(Rmd = "Graphics.Rmd", names = "graphics.png", force = FALSE) {
  # check input:
  assert_character(x = Rmd, any.missing = FALSE, len = 1)
  assert_character(x = names, any.missing = FALSE, min.len = 1)
  assert_flag(x = force)

  # check if package rmarkdown is available:
  if(!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning(paste0('Install package rmarkdown to use this function.'))
  }
  # check if package magick is available:
  if(!requireNamespace("magick", quietly = TRUE)) {
    warning(paste0('Install package magick to use this function.'))
  }
  # check if package pdftools is available:
  if(!requireNamespace("magick", quietly = TRUE)) {
    warning(paste0('Install package pdftools to use this function.'))
  }

  dir = tempdir()
  tryCatch(rmarkdown::render(input = Rmd, output_dir = dir), error = function(e)
    stop(paste0("Rendering ", Rmd, "failed due to ", e)))
  lf = list.files(paste0(dir, "/Graphics_files/figure-html"), pattern = "\\.png$")
  if(length(names) != length(lf)) {
    warning("Wrong number of names. Using default names instead.")
    names = paste0("graphics", 1:length(lf), ".png")
  }

  for(i in seq_along(lf)) {
    file.copy(from = paste0(dir, "/Graphics_files/figure-html/", lf[i]),
              to = names[i], overwrite = force)
  }
}
