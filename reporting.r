library(knitr)
library(dplR)
library(brew)
library(tinytex)
library(readr)


brew2pdf <- function(input, output_dir, envir = parent.frame(), ...) {
  
  brew_file_name <- basename(input)
  rnw_file_name <- sub(".brew$", ".Rnw", brew_file_name)
  tex_file_name <- sub(".brew$", ".tex", brew_file_name)
  pdf_file_name <- sub(".brew$", ".pdf", brew_file_name)
  
  rnw_file_path <- file.path(output_dir, rnw_file_name)
  tex_file_path <- file.path(output_dir, tex_file_name)
  pdf_file_path <- file.path(output_dir, pdf_file_name)
  
  brew(input, output = rnw_file_path, envir = envir)
  
  # when using figures knitr expects compilation to be done from the output directory
  # so unfortunately we need to temporarily change the working directory
  orig_wd <- setwd(output_dir)
  # return to the original working directory on function exit (including exit via error)
  on.exit(setwd(orig_wd))
  knit2pdf(rnw_file_name, output = tex_file_name, envir = envir)
}


custom_sanitize <- function(string) {
  
  string_split <- strsplit(string, "\n")[[1]]
  
  output_list <- c()
  for (x in string_split) {
    
    x <- enc2utf8(toString(x))
    
    # replace any ampersands so they are caught by filter
    x <- gsub("\u26", "&", x)
    x <- gsub("\uff06", "&", x)
    x <- gsub("\ufe60", "&", x)
    
    # latexify seems to be a more robust function for doing this but requires a couple of fixes
    x <- dplR::latexify(x, doublebackslash = TRUE)
    
    # fix latexify quote changes
    x <- gsub("\\\\textquotesingle", "'", x)
    x <- gsub("\\\\textquotedbl", '"', x)
    
    # fix latexify guillemet changes
    x <- gsub("\\\\guillemotleft", "$\\\\ll$", x)
    x <- gsub("\\\\guillemotright", "$\\\\gg$", x)
    
    # fix latexify beta symbol
    x <- gsub("\\\\ss", "$\\\\beta$", x)
    
    # normal less/greater than and equal sign
    x <- gsub("\u2264", "$\\\\leq$", x)
    x <- gsub("\u2265", "$\\\\geq$", x)
    
    # slanted less/greater than and equal sign
    x <- gsub("\u2a7d", "$\\\\leqslant$", x)
    x <- gsub("\u2a7e", "$\\\\geqslant$", x)
    
    # greek letters
    x <- gsub("\u03b1", "$\\\\alpha$", x)
    x <- gsub("\u03b2", "$\\\\beta$", x)
    x <- gsub("\u03b3", "$\\\\gamma$", x)
    x <- gsub("\u03b4", "$\\\\delta$", x)
    x <- gsub("\u03b5", "$\\\\epsilon$", x)
    x <- gsub("\u03b6", "$\\\\zeta$", x)
    x <- gsub("\u03b7", "$\\\\eta$", x)
    x <- gsub("\u03b8", "$\\\\theta$", x)
    x <- gsub("\u03b9", "$\\\\iota$", x)
    x <- gsub("\u03ba", "$\\\\kappa$", x)
    x <- gsub("\u03bb", "$\\\\lambda$", x)
    x <- gsub("\u03bc", "$\\\\mu$", x)
    x <- gsub("\u03bd", "$\\\\nu$", x)
    x <- gsub("\u03be", "$\\\\xi$", x)
    x <- gsub("\u03bf", "$o$", x) # x <- gsub("\u03bf", "$\\\\omicron$", x) # Doesn't exist in latex?
    x <- gsub("\u03c0", "$\\\\pi$", x)
    x <- gsub("\u03c1", "$\\\\rho$", x)
    x <- gsub("\u03c2", "$\\\\varsigma$", x)
    x <- gsub("\u03c3", "$\\\\sigma$", x)
    x <- gsub("\u03c4", "$\\\\tau$", x)
    x <- gsub("\u03c5", "$\\\\upsilon$", x)
    x <- gsub("\u03c6", "$\\\\phi$", x)
    x <- gsub("\u03c7", "$\\\\chi$", x)
    x <- gsub("\u03c8", "$\\\\psi$", x)
    x <- gsub("\u03c9", "$\\\\omega$", x)
    
    x <- gsub("\r\n", "\\\\n", x, fixed=TRUE)
    x <- gsub("\n", "\\\\n\\\\n", x, fixed=TRUE)
    
    # remove any other unicode character 
    x <- gsub("[^\x20-\x7E]", "{\\\\textbigcircle}", x)
    
    x <- gsub("\\\\", "\\", x, fixed=TRUE)
    
    output_list <- append(output_list, x)
  }
  
  output <- paste(output_list, collapse="\n\n")
  
  return(output)
  
}

generate_report <- function(text_file, input = "./report_template.brew",
                       output_dir = ".", ...) {
  string <- read_file(text_file)
  
  string <- custom_sanitize(string)
  
  eval_envir <- list2env(list(string = string))
  brew2pdf(input, output_dir, envir = eval_envir, ...)
}


generate_report("./block_of_unicode.txt")
# generate_report("./newline_test.txt")