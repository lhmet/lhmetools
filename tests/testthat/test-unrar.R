context("extract rar file")

.example_file <- function(){
  rarfile_url <- "https://ndownloader.figshare.com/files/13366451"
  dest_file <- tempfile(fileext = ".rar")
  download.file(rarfile_url, dest_file, mode = 'wb')
  return(dest_file)
}

rarfile <- .example_file()

.count_files <- function(extract_files){
  length(fs::dir_ls(unique(dirname(extract_files)), type = "any"))
}

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

  nfiles <- .count_files(output)
  # cleanup
  fs::dir_delete(rar_dir)

  expect_gt(nfiles, 0)
})

test_that("test extraction of rar file in a arbitraty folder", {
  # rm -rf /tmp/R*
  tmpd <- fs::path_temp() # differs from .example_file() dir
  tmpd <- fs::path(tmpd, 'temp')
  fs::dir_create(path = tmpd)
  #dir.exists(tmpd)
  output <- unrar(
    file = rarfile,
    dest_dir = tmpd
  )
  if(!checkmate::test_os("linux")){
    expect_error(basename(output))
    return(NULL)
  }
  nfiles <- .count_files(output)
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
      file = rarfile
    ),
      dest_dir = tmpd
  )
})


test_that("test wrong output folder", {
  expect_error(
    unrar(
      file = rarfile,
      dest_dir = "/someplace"
    )
  )
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


