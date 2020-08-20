.check_unrar <- function(quiet) {
  installed_unrar <- system(
    "which unrar",
    intern = FALSE,
    ignore.stdout = quiet
  )
  installed_unrar == 0
}

#' Extract files from `rar` archives (only works on Linux)
#'
#' This function extract files from a `.rar` file
#'
#' @param file a file path to a `file.rar`
#' @param dest_dir path to extract files. Defaults to `dirname(file)`.
#' @param overwrite logical, use `overwrite = TRUE` to overwrite
#' existing files.
#' @param quiet Hide printed output, messages, warnings, and errors
#' (TRUE, the default), or display them as they occur?
#' @return character vector with files path
#' @details This function has the side effect of extract files in a
#' directory named (`dest_dir/basename(file)`) when `dest_dir` is not `NULL`.
#' @export
#' @note This function is a wrapper to call `unrar` command. It is required you
#' have `unrar` Linux library installed. You can install it with
#' `apt install unrar`.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   rarfile_url <- "https://ndownloader.figshare.com/files/13366451"
#'   dest_file <- tempfile(fileext = ".rar")
#'   download.file(rarfile_url, dest_file, mode = "wb")
#'   extracted_files <- unrar(dest_file)
#'   extracted_files
#' }
#' }
#' @importFrom checkmate assert_file_exists test_path_for_output assert_os
#' assert_true
#' @importFrom assertthat assert_that has_extension
#' @importFrom fs path_ext_remove dir_ls

unrar <- function(
                  file,
                  dest_dir = dirname(file),
                  overwrite = FALSE,
                  quiet = TRUE) {
  file <- fs::path_real(file)

  # input checks
  checkmate::assert_file_exists(file)
  assertthat::assert_that(
    assertthat::has_extension(file, "rar")
  )

  # create dir to extract at base dir from rar file
  dir_extract_rar <- fs::path_ext_remove(file)

  if (!is.null(dest_dir)) {
    dest_dir <- fs::path_real(dest_dir)
    checkmate::assert_directory_exists(dest_dir)
    dir_extract_rar <- fs::path(dest_dir, basename(dir_extract_rar))
    # checkmate::assert_path_for_output(dir_extract_rar)
  }

  # build appropriate call to unrar
  if (!fs::dir_exists(dir_extract_rar)) {
    fs::dir_create(dir_extract_rar)
    checkmate::assert_path_for_output(dir_extract_rar, overwrite = TRUE)
    cmd <- paste0("unrar e ", file, " ", dir_extract_rar)
  } else {
    # check if exists files in pre-existent dir
    if (length(dir(dir_extract_rar, all.files = TRUE)) > 0 && overwrite) {
      cmd <- paste0("unrar e -o+ ", file, " ", dir_extract_rar)
    } else {
      stop("There are files in the folder. Use overwrite = TRUE to
           overwrite existing files.")
    }
  }

  # decompress
  if (checkmate::test_os("linux")) {
    # capture.output(out_call_unrar <- system(cmd,intern = TRUE),file = "NUL")
    # if(quiet){

    # check unrar is available
    is_unrar_installed <- .check_unrar(quiet)
    if (checkmate::test_false(is_unrar_installed)) {
      message("This function require the unrar tool installed.")
      message("You can install it typing on terminal 'apt install unrar'.")
      return("")
    }


    out_call_unrar <- system(cmd, intern = FALSE, ignore.stdout = quiet)

    # } #else {
    # out_call_unrar <- system(cmd, intern = FALSE)
    # }

    if (out_call_unrar != 0) {
      # print output console
      system(cmd, intern = TRUE)
      stop("\n unrar process returned error \n")
      # sudo apt install unrar
    }
    # list of files
    extracted_files <- fs::dir_ls(dir_extract_rar, type = "file", recurse = TRUE)
    return(extracted_files)
  }

  message("This function works only on linux systems.")
  return("")
}
