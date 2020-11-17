context("extract rar file")

.example_file <- function() {
  rarfile_url <- "https://ndownloader.figshare.com/files/13366451"
  dest_file <- tempfile(fileext = ".rar")
  download.file(rarfile_url, dest_file, mode = "wb")
  return(dest_file)
}

rarfile <- .example_file()

.count_files <- function(extract_files) {
  # extract_files = ""
  extract_files <- as.character(extract_files)
  extract_dir <- unique(dirname(extract_files))
  # checkmate::check_character(extract_dir)
  # dir_list <- suppressWarnings(
  #   fs::dir_ls(extract_dir,
  #              type = "any",
  #              fail = FALSE)
  #   )
  dir_list <- fs::dir_ls(extract_dir,
    type = "any",
    fail = FALSE
  )
  length(dir_list)
}

test_that("test extraction of rar file in the folder of compressed file", {

  # delete dir resulting from previous extraction of rar file
  rar_dir <- fs::path_ext_remove(rarfile)
  if (fs::dir_exists(rar_dir)) {
    fs::dir_delete(rar_dir)
  }

  output <- unrar(
    rarfile,
    dest_dir = rar_dir
  )

  nfiles <- .count_files(output)

  is_unrar_installed <- ifelse(
    checkmate::check_os("windows"),
    file.exists(.check_7Zip()),
    .check_unrar(quiet = TRUE)
  )
  if (checkmate::test_false(is_unrar_installed)) {
    expect_equal(nfiles, 0)
    return(NULL)
  }

  # cleanup
  fs::dir_delete(rar_dir)

  expect_gt(nfiles, 0)
})

test_that("test extraction of rar file in a arbitraty folder", {
  # rm -rf /tmp/R*
  tmpd <- fs::path_temp() # differs from .example_file() dir
  tmpd <- fs::path(tmpd, "temp")
  fs::dir_create(path = tmpd)
  # dir.exists(tmpd)
  output <- unrar(
    file = rarfile,
    dest_dir = tmpd
  )
  # if (!checkmate::test_os("linux")) {
  #   expect_error(basename(output))
  #   return(NULL)
  # }

  nfiles <- .count_files(output)

  is_unrar_installed <- ifelse(
    checkmate::check_os("windows"),
    file.exists(.check_7Zip()),
    .check_unrar(quiet = TRUE)
  )


  if (checkmate::test_false(is_unrar_installed)) {
    expect_equal(nfiles, 0)
    return(NULL)
  }
  expect_gt(nfiles, 0)
})


test_that("test try overwriting a pre-existent non empty folder", {
  tmpd <- tempdir()
  unrar(
    file = rarfile,
    dest_dir = tmpd,
    overwrite = TRUE
  )

  expect_error(
    unrar(
      file = rarfile,
      dest_dir = tmpd
    )
  )
})


test_that("test empty output folder", {
  expect_error(
    unrar(
      file = rarfile,
      dest_dir = ""
    )
  )
  #fs::dir_delete(dest_dir)
})

test_that("test folder instead of a file as input", {
  expect_error(
    unrar(
      file = system.file(package = "lhmetools"),
      dest_dir = NULL
    )
  )
})


test_that("test for a inexistent file", {
  expect_error(
    unrar(file = "")
  )
})
