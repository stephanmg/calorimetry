################################################################################
#' get_git_info_from_repository
#' 
#' This function is used to create the git branch information for the UI
################################################################################
get_git_information_from_repository <- function() {
      repo <- git2r::repository(".")
      head_ref <- git2r::repository_head(repo)
      current_branch <- NULL
      tag_name <- NULL

      if (git2r::is_branch(head_ref)) {
         current_branch <- head_ref$name
      }

      if (inherits(head_ref, "git_commit")) {
         commit_id <- head_ref$sha
         all_tags <- git2r::tags(repo)
         for (tag in all_tags) {
            if (identical(tag$target, head_ref)) {
               tag_name <- tag$name
               break
            }
         }
      }

      current_tag <- tag_name
      current_commit_id <- substring(git2r::commits(repo)[[1]]$sha, 1, 16)

	  version_info <- NULL

      if (is.null(current_branch)) {
         if (is.null(current_tag)) {
            version_info <- paste("Current commit ID:", current_commit_id)
         } else {
            version_info <- paste("Current version: ", current_tag)
         }
      }  else {
         version_info <- paste("Current branch: ", current_branch, "@ commit: ", current_commit_id)
      }
	return(version_info)
}
