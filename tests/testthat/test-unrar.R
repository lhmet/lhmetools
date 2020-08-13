context("extract rar file")

example_rar_file <- function(){
  rarfile_url <- "https://ndownloader.figshare.com/files/13366451"
  dest_file <- tempfile(fileext = ".rar")
  download.file(rarfile_url, dest_file, mode = 'wb')
  return(dest_file)
}

rarfile <- example_rar_file()

test_that("test extraction of rar file in the folder of compressed file", {

  # delete dir resulting from previous extraction of rar file
  rar_dir <- fs::path_ext_remove(rarfile)
  if(fs::dir_exists(rar_dir)){
    fs::dir_delete(rar_dir)
  }

  output <- unrar(
    rarfile,
    dest_dir = NULL
  )

  if(!checkmate::test_os("linux")){
    expect_error(basename(output))
    return(NULL)
  }

  output <- length(fs::dir_ls(basename(output), type = "any"))
  # cleanup
  fs::dir_delete(rar_dir)

  expect_gt(output, 0)
})

test_that("test extraction of rar file in a arbitraty folder", {
  # rm -rf /tmp/R*
  tmpd <- tempdir() # differs from example_rar_file() dir
  output <- extract_rar(
    rar_file = rarfile,
    dest_dir = tmpd
  )
  if(!checkmate::test_os("linux")){
    expect_error(basename(output))
    return(NULL)
  }
  output <- length(fs::dir_ls(basename(output), type = "any"))
  if(checkmate::test_os("windows")){

  }
  expect_gt(output, 0)
})


test_that("test try overwriting a pre-existent non empty folder", {
  tmpd <- tempdir()
  extract_rar(
    rar_file = rarfile,
    dest_dir = tmpd,
    overwrite = TRUE
  )

  expect_error(
    extract_rar(
      rar_file = rarfile
    ),
      dest_dir = tmpd
  )
})


test_that("test wrong output folder", {
  expect_error(
    extract_rar(
      rar_file = rarfile,
      dest_dir = "/someplace"
    )
  )
})

test_that("test folder instead of a file as input", {
  expect_error(
    extract_rar(
      rar_file = system.file(package = "lhmetools"),
      dest_dir = NULL
    )
  )
})


test_that("test for a inexistent file", {
  expect_error(
    extract_rar(rar_file = "")
  )
})


