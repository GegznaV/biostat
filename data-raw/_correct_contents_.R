# [tags] Rename, Correct contents

# Function 1.A --------------------------------------------------------------

correct_contents2 <- function(FILE){
    # Read
    x0 <- readLines(con = FILE)
    x <- x0

    # Correct
    x <- gsub("BioStat", "biostat", x, fixed = TRUE, ignore.case = FALSE)


    # Writte
    if (any(x0 != x))    {
        cat(FILE, sep = "\n")
        writeLines(x, con = FILE)
    }
}

# function 1.B ------------------------------------------------------------

rename2 <- function(FILE){
    x <- FILE
    stop("Do not need this operation", call. = FALSE)
    # Correct filenames
    x <- gsub("extract_and_print", "print_container", x, fixed = TRUE)

    NEW_FILE <- x
    # Rename files if needed
    if (FILE != NEW_FILE)    {
        cat(sprintf("Files renamed: %30s   >>>   %-30s", FILE, NEW_FILE), sep = "\n")
        file.rename(from = FILE, to = NEW_FILE)
    }
}

# Function 2 --------------------------------------------------------------

apply_content_corrections <- function(x){
    Start <-  Sys.time()
    current_wd <- getwd()
    # Reset wd on exit
    on.exit(setwd(current_wd))

    # Change wd
    setwd(x)
    AllFiles <- dir()
    FILES <- as.list(AllFiles[grepl("(.*\\.R$)|(.*\\.Rmd$)|(.*\\.html$)|(.*\\.Rd$)", AllFiles)])

    # lapply(FILES, correct_contents)
    lapply(FILES, correct_contents2)
    # lapply(FILES, rename2)

    # open_wd()
    spMisc::printDuration(Start, returnString = TRUE)
}

# Function 3 --------------------------------------------------------------

require(spHelper)

# List all directories of interest

# directories  <- as.list(
#     c(
#         "D:/Dokumentai/R/biostat",
#
#         paste0('D:\\Dokumentai\\R\\knitrContainer\\',
#                c("R","vignettes","inst\\doc", "tests\\testthat"),"\\")
#
#     )
# )

# directories  <- as.list("D:\\Dokumentai\\R\\Spektroskopija\\PAP_RK_2014\\")
directories  <- list.dirs()[!grepl(".git|.Rproj.", list.dirs())]


# Apply corrections
stop("This script can be harmful!!!", call. = FALSE)
lapply(directories, apply_content_corrections)

