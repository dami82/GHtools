##  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### ~~~~~~~~~~~~~ GHtools ~~~~~~~~~~~~~
##  ...the ultimate R GitHub package...
#   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Damiano Fantini
# 2023-May-29

#' Manage Project Files and Repositories on GitHub using R.
#'
#' GitHub is one of the best-known Web Services for software development and 
#' version control using Git. It supports access control, bug tracking, 
#' software feature requests, task management and much more. 
#' It is very easy to interact with GitHub from a Bash/Unix terminal 
#' using git. Here, we provide a collection of functions to access 
#' GitHub from an R environment and perform some simple basic operations. 
#'  
#'
#' @references
#' \enumerate{
#'   \item{\strong{GitHub Website}: \url{https://github.com/} }
#'   \item{\strong{GitHub Wiki Page}: \url{https://en.wikipedia.org/wiki/GitHub} }
#'   \item{\strong{Get started with Git}: \url{https://www.w3schools.com/git/git_getstarted.asp?remote=github} }
#'  }
#'
#' @docType package
#' @name GHtools-package
NULL




#' Clone a GitHub Repository.
#'
#' Download a local copy of a GitHub repository. 
#' 
#'
#' @param repo string, GitHub repository to clone (e.g., 'dami82/mutSignatures')
#' @param user string (optional). Identifier of the Github user 
#' attempting the repo cloning. Defualts to NULL (no user ID).
#' @param token string (optional). Authentication Token for the user  
#' attempting the repo cloning. Defualts to NULL (no token).
#' @param branch string (optional). Name of the single branch to download. 
#' Defaults to NULL (if so, all branches are retrieved).
#' @param dest path to a local folder where to save the downloaded files. 
#' Defaults to '.' (install in a subfolder in the local directory).
#'
#' @return code returned by the 'git clone' command
#' 
#'
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#'
#'
#' @export
git_clone_repo <- function(repo, user = NULL, token = NULL, 
                           branch = NULL, dest = NULL) {

  my_exe <- paste0('git clone https://github.com/', repo)
  
  if (!is.null(user) && !is.null(token)) {
    
    stopifnot(is.character(user) && !is.na(user) && length(user) == 1,
              is.character(token) && !is.na(token) && length(token) == 1)
    
    my_exe <- paste0('git clone https://', user, ':', token, 
                     '@github.com/', repo, '.git')
    
  }
  
  if (!is.null(branch)) {
    stopifnot(is.character(branch), !is.na(branch), length(branch) == 1)
    my_exe <- paste0(my_exe, ' --branch ', branch, ' --single-branch')
  }
  
  if (!is.null(branch)) {
    stopifnot(is.character(branch), !is.na(branch), length(branch) == 1)
    my_exe <- paste0(my_exe, ' --branch ', branch, ' --single-branch')
  }

  if (!is.null(dest)) {
    stopifnot(is.character(dest), !is.na(dest), 
              length(dest) == 1, dir.exists(dest))
    my_exe <- paste0(my_exe, ' ', dest)
  }
  
  y <- tryCatch({system(command = my_exe)}, error = function(e) { e } )
  return(y)
}


#' Check Active GitHub Branch.
#'
#' Check which is the Active Branch of a 
#' local installation of a GitHub repository. 
#' 
#' @param git_folder path to a local folder where to a GitHub repo
#' has been cloned/installed. Defaults to '.' (local directory).
#'
#' @return string corresponding to the active branch.
#' 
#'
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#'
#'
#' @export
git_which_active_branch <- function(git_folder = '.') {
  
  curwd <- getwd()
  zz <- try({setwd(git_folder)}, silent = TRUE)
  
  # check which branch is currently selected
  cur_branch <- system('git rev-parse --abbrev-ref HEAD', intern = TRUE)
  zz <- try({setwd(curwd)}, silent = TRUE)
  
  return(cur_branch)
}


#' Checkout into a GitHub Repo Branch.
#'
#' Checkout into a specific branch of a local copy of a GitHub repository. 
#' 
#'
#' @param git_folder path to a local folder where to a GitHub repo
#' has been cloned/installed. Defaults to '.' (local directory).
#' @param branch string corresponding to the branch to checkout into. 
#' @param force logical (defaults to TRUE). If branch does not exist, create
#' one with this name.
#' @param verbose logical (defaults to TRUE). Shall information be printed 
#' to console. 
#'
#' @return NULL
#' 
#'
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#'
#'
#' @export
git_checkout_branch <- function(git_folder = '.', branch = 'master', 
                                force = TRUE, verbose = TRUE) {
  
  curwd <- getwd()
  zz <- try({setwd(git_folder)}, silent = TRUE)
  
  # check which branch is currently selected
  cur_branch <- system('git rev-parse --abbrev-ref HEAD', intern = TRUE)

  # Check if branch exists
  exe_1 <- paste0('git show-ref --verify --quiet refs/heads/', branch)
  chk1 <- suppressMessages(suppressWarnings(
    system(exe_1, intern = TRUE, ignore.stderr = TRUE)))
  
  if (is.null(attributes(chk1)$status)) {
    
    # branch exists... cheking in
    exe_2 <- paste0('git checkout ', branch)
    chk2 <- suppressMessages(suppressWarnings(
      system(exe_2, intern = TRUE, ignore.stderr = TRUE)))
    
  } else if (attributes(chk1)$status == 1 && force) {

    # create branch and check in
    if(verbose)
      message(paste0('Creating the \'', branch, '\' branch.'))
      
    exe_2 <- paste0('git branch ', branch)
    chk2 <- suppressMessages(suppressWarnings(
      system(exe_2, intern = TRUE, ignore.stderr = TRUE)))
    
    # then cheking in
    exe_3 <- paste0('git checkout ', branch)
    chk3 <- suppressMessages(suppressWarnings(
      system(exe_3, intern = TRUE, ignore.stderr = TRUE)))
    
  } else {
    stop('Unexpected exception!')
  }
  
  # check which branch is currently selected
  cur_branch <- system('git rev-parse --abbrev-ref HEAD', intern = TRUE)
  zz <- try({setwd(curwd)}, silent = TRUE)
  
  if (verbose)
    message(paste0('Checked out in the \'', branch, '\' branch.'))  
  
  return(NULL)
}


#' Commit and Push to GitHub.
#'
#' Add and Commit File Changes and then Push all modifications
#' remotely to the GitHub repository. 
#' 
#' @param git_folder path to a local folder where to a GitHub repo
#' has been cloned/installed. Defaults to '.' (local directory).
#' @param branch string corresponding to the branch to commit and push
#' to GitHub (defaults to 'master').
#' @param commit_message string, commit message describind the file changes.
#' @param user string (optional). Identifier of the Github user 
#' attempting the repo cloning. Defualts to NULL (no user ID).
#' @param token string (optional). Authentication Token for the user  
#' attempting the repo cloning. Defualts to NULL (no token).
#' @param files character vector of filenames (paths to files) to be added to
#' the commit. Defaults to NULL (all files in the `git_folder` will be added).
#' @param force logical (defaults to TRUE). The -f tag is attached
#' to the 'git push' command.
#' 
#'
#' @return NULL
#' 
#'
#' @author Damiano Fantini, \email{damiano.fantini@@gmail.com}
#'
#'
#' @export
git_commit_and_push <- function(git_folder = '.', branch = 'master',
                                commit_message = NULL,
                                user = NULL, token = NULL,
                                files = NULL, force = TRUE) {
  
  stopifnot(!is.null(user), !is.null(token), 
            is.character(user), length(user) == 1, !is.na(user), 
            is.character(token), length(token) == 1, !is.na(token))
  
  curwd <- getwd()
  zz <- try({setwd(git_folder)}, silent = TRUE)
  
  # Make sure to add access tokens using this format: 
  
  #system('git remote show origin')
  #system('git config --get remote.origin.url')
  rmt_ori <- try({system('git config --get remote.origin.url', intern = TRUE)}, 
                 silent = TRUE)
  rmt_repo <- sub('https://.*github.com/([^/]+/[^/]+)$', '\\1', rmt_ori)
  rmt_repo <- sub('\\.git$', '', rmt_repo)
  
  exe0 <- paste0('git remote set-url origin https://', 
                 user, ':', token, 
                 '@github.com/', rmt_repo, '.git')
  zz <- try({system(exe0, intern = TRUE, ignore.stderr = TRUE)}, 
                 silent = TRUE)
  
  # Add files
  if (is.null(files)) {
    zz <- try({system(command = 'git add .', intern = TRUE,
                      ignore.stderr = TRUE)}, 
              silent = TRUE)
  } else {
    for (fi in files) {
      if (file.exists(fi)) {
        zz <- try({system(command = paste0('git add ', fi), 
                          ignore.stderr = TRUE, intern = TRUE)}, 
                  silent = TRUE)
      }
    }
  }
  
  # Commit w/ message
  if (is.null(commit_message) || !is.character(commit_message) || 
      length(commit_message) != 1 || nchar(commit_message) < 1) {

    commit_message <- paste0('Updated files in the remote \'', 
                             branch, '\' branch. Code modified on ', 
                             Sys.Date(), ' by \'', user, '\'.')    
  }
  zz <- try({system(command = paste0('git commit -m "', commit_message, '"'), 
                    ignore.stderr = TRUE, intern = TRUE)}, 
            silent = TRUE)
  
  # Commit w/ message
  zz <- try({system(command = paste0('git push -uf origin ', branch))}, 
            silent = TRUE)
  zz <- try({setwd(curwd)}, silent = TRUE)
  
  return(NULL)
}  


