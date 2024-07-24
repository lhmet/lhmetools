# Check if unrar is installed on linux
.check_unrar <- function(quiet) {

  if(checkmate::test_os("linux")){
    installed_unrar <- system(
      "which unrar",
      intern = FALSE,
      ignore.stdout = quiet
    )

    is_unrar_installed <- installed_unrar == 0

    if (checkmate::test_false(is_unrar_installed)) {
      stop(
        "This function require the unrar tool installed. \n",
        "You can install it typing on terminal 'apt install unrar'."
      )
    }
   return(is_unrar_installed)
  }

  if(checkmate::test_os("windows")){
    path_7zip <- .check_7Zip()
    installed_7zip <- file.exists(path_7zip)

    if (checkmate::test_false(installed_7zip)) {
      stop(
        "This function require the 7Zip tool installed. \n",
        "You can install it using the installr package: \n
        'installr::install.7zip()'."
      )
    }
    return(installed_7zip)
  }

  stop("This function is only suported for linux and windows.")

}

# Check if  7-Zip is installed on windows
.check_7Zip <- function() {
  executableName <- "C:\\Program Files (x86)\\7-Zip\\7z.exe"

  if (file.exists(executableName)) {
    return(executableName)
  }

  # other executable file names and ideas go here ...
  stop(
    "failed to find 7zip. \n",
    "you can install it using installr::"
  )
}

# call system
.run_process <- function(executable, arguments, quiet) {
  cmd <- paste(executable, arguments)
  cat(cmd, "\n")

  # print(cmd)


  exit_code <- system(cmd,
    intern = FALSE,
    ignore.stdout = quiet,
    ignore.stderr = FALSE,
    wait = TRUE,
    input = NULL,
    show.output.on.console = TRUE,
    invisible = FALSE
  )


  if (exit_code != 0) {
    message("This function require the 7-zip tool installed.")
    message("You can install it typing on console 'installr::install.7zip'.")
    stop("Process returned error")
  }
  return(exit_code)
}

unrar_7zip <- function(file.rar, out.dir, overwrite, quiet = TRUE) {
  # file.rar = rarfile; out.dir = dest_dir; overwrite = TRUE; quiet = TRUE
  # based on https://github.com/swish-climate-impact-assessment/awaptools/blob/master/R/ZipFunctions.R
  #z7path <- normalizePath(.check_7Zip(), "/")
  z7path <- fs::path(.check_7Zip())
  checkmate::assert_file_exists(z7path)
  file.rar <- fs::path(file.rar)
  checkmate::assert_file_exists(file.rar)
  out.dir <- fs::path(out.dir)
  checkmate::assert_directory_exists(out.dir)

  #"C:/Program Files (x86)/7-Zip/7z.exe" e "C:/Users/bitev/AppData/Local/Temp/RtmpWQCvGd/file40c4506833ec.rar" "-oC:/Users/bitev/AppData/Local/Temp/RtmpWQCvGd/file40c4506833ec"
  # args <- system(
  #   paste(
  #   shQuote(z7path),
  #   "e",
  #   shQuote(file.rar),
  #   " -y"
  #   )
  #   )

  # args <- paste0(
  #   "e ",
  #   "\"", file.rar, "\" ",
  #   "\"-o", out.dir, "\" ",
  #   ""
  # )
  args <- paste(
    "e",
    shQuote(file.rar),
    shQuote(paste0("-o", out.dir))
    )
  args_overwrite <- paste(args, "-y")
  arguments <- ifelse(overwrite, args_overwrite, args)
  exec <- shQuote(z7path)
  out_call_7z <- .run_process(exec, arguments, quiet)

  # return extracted files
  extracted_files <- fs::dir_ls(out.dir, type = "file", recurse = TRUE)

  #normalizePath(extracted_files, winslash = "\\")
  extracted_files
}





check_rar_file <- function(file.rar) {
  file <- fs::path_real(file.rar)
  # input checks

  assertthat::assert_that(
    assertthat::has_extension(file, "rar")
  )

  checkmate::assert_file_exists(file)
}


unrar_linux <- function(file.rar, out.dir, overwrite, quiet = TRUE) {
  # file.rar = dest_file; out.dir = NULL; overwrite = TRUE; quiet = TRUE
  .check_unrar(quiet)

  cmd_extract <- paste0("unrar e ", file.rar, " ", out.dir)
  cmd_overwrite <- paste0("unrar e -o+ ", file.rar, " ", out.dir)
  cmd <- ifelse(overwrite, cmd_overwrite, cmd_extract)
  out_call_unrar <- system(cmd, intern = FALSE, ignore.stdout = quiet)

  # if there is a problem, call again to show the error
  if (out_call_unrar != 0) {
    # print output console
    system(cmd, intern = TRUE)
    stop("\n unrar process returned error \n")
  }
  # return extracted files
  extracted_files <- fs::dir_ls(out.dir, type = "file", recurse = TRUE)
  extracted_files
}


unrar_file <- function(file.rar, out.dir, overwrite, quiet = TRUE) {

  if(checkmate::test_os("windows")){
    return(unrar_7zip(file.rar, out.dir, overwrite, quiet))
  }

  if(checkmate::test_os("linux")){
    return(unrar_linux(file.rar, out.dir, overwrite, quiet))
  }

 stop("This function is only suported for linux and windows.")

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
#'
#'   rarfile_url <- paste0("https://www.dropbox.com/scl/fi/63eimy6j2ok1q3vf11zs4/",
#'   "some-file.rar?rlkey=jhveob9ysl1ivy3alnt68c61g&st=dvt43nb1&dl=1")
#'
#'   dest_file <- file.path(tempdir(), strsplit(basename(rarfile_url), "\\?")[[1]][[1]])
#'   #dest_file <- tempfile(fileext = ".rar")
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
                  dest_dir = fs::path_ext_remove(file),
                  overwrite = FALSE,
                  quiet = TRUE) {
  # file <- dest_file; dest_dir <- "~/Downloads"; overwrite = TRUE; quiet = TRUE
  # file <- rarfile; dest_dir <- fs::path_ext_remove(file); overwrite = TRUE; quiet = TRUE
  check_rar_file(file)

  subdir2extract <- basename(fs::path_ext_remove(file))

  if(is.null(dest_dir)) dest_dir <- tempdir()

  # se o diretorio já existe --------------------------------------------------
  if(fs::dir_exists(dest_dir)){ # e.g. ~/Downloads
    subdir2extract <- fs::path(dest_dir, subdir2extract)
    if(!dir.exists(subdir2extract)) {
      # cria
      fs::dir_create(subdir2extract)
      # extrai
      out <- unrar_file(file, subdir2extract, overwrite, quiet)
      return(out)

    } else {
      if(overwrite) {
        out <- unrar_file(file, subdir2extract, overwrite, quiet)
        return(out)
      } else {
        stop("There are files in the folder. Use overwrite = TRUE to
           overwrite existing files.")
      }
    }


  } else {
  # se o diretorio não existir ()------------------------------------------------
    # mas o dir um nível antes existir
    checkmate::assert_directory_exists(fs::path_dir(dest_dir))
    fs::dir_create(dest_dir)
    if(checkmate::test_os("windows")){
      file <- normalizePath(file, "\\")
      dest_dir <- normalizePath(dest_dir, "\\")
    }
    out <- unrar_file(file, dest_dir, overwrite, quiet)
    #out <- unrar_file(normalizePath(file), dest_dir, overwrite, quiet)
    return(out)
  }

  message("This is a dark hole")
  return("")
}
