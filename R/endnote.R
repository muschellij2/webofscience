#' @title Find API Key for EndNote
#'
#' @description Determines if \code{option(ws_endnote_key)} or
#' \code{option(ws_endnote_key_filename)} is set.
#' If not, it stops and returns an error.  If so, returns the value.
#'
#' @inheritParams ws_api_key
#' @export
#' @examples
#' res = ws_endnote_key(error = FALSE)
ws_endnote_key = function(api_key = NULL, error = TRUE) {
  x = ws_list_apis()
  x = x$endnote
  api_key = ws_api_key(
    api_key = api_key,
    error = error,
    key_option = x$key_option,
    sys_env = x$sys_env,
    key_filename_option = x$key_filename_option,
    api_name = x$api_name)
  return(api_key)
}

#' @rdname ws_endnote_key
#' @export
#' @examples
#' ws_have_endnote_key()
ws_have_endnote_key = function(api_key = NULL) {
  api_key = ws_endnote_key(api_key = api_key, error = FALSE)
  !is.null(api_key)
}


#' @rdname ws_set_key_option
#' @export
ws_set_endnote_key = function(api_key) {
  x = ws_list_apis()
  x = x$endnote
  ws_set_key_option(api_key = api_key, key_option = x$key_option)
}


#' @rdname ws_endpoints
#' @export
ws_endnote_endpoints = function() {

  L = list(
    user_space_attachment_size = list(
      verb = "GET",
      endpoint = "/user_space/attachment_size",
      description = "Retrieve the current total space of the all user's attachments"
    ),
    user_space_attachments = list(
      verb = "GET",
      endpoint = "/user_space/attachments",
      description = "Retrieve the total space available for a user"
    ),
    reference_types = list(
      verb = "GET",
      endpoint = "/reference/types",
      description = "Retrieves the full collection of all reference types"
    ),
    user_space = list(
      verb = "GET",
      endpoint = "/user_space",
      description = "Retrieve user's space of the library"
    ),
    user_space_references = list(
      verb = "GET",
      endpoint = "/user_space/references",
      description = "Retrieve the amount of references that a user currently has, the amount of references a user can have in the library, and the amount of references that a user can still add."
    ),
    references_guid_file = list(
      verb = "POST",
      endpoint = "/references/{guid}/file",
      description = "Create a file attached to this reference"
    ),
    reference_get = list(
      verb = "GET",
      endpoint = "/reference",
      description = "Retrieve the reference specified in header x-enws-guid"
    ),
    reference_put = list(
      verb = "PUT",
      endpoint = "/reference",
      description = "Change a set of a reference bibliographic content fields"
    ),
    reference_post = list(
      verb = "POST",
      endpoint = "/reference",
      description = "Creates a reference from a set of its fields"
    ),
    references_guid_get = list(
      verb = "GET",
      endpoint = "/references/{guid}",
      description = "Retreive the set of fields of a specific reference"
    ),
    references_guid_put = list(
      verb = "PUT",
      endpoint = "/references/{guid}",
      description = "Change a set of this reference bibliographic content fields"
    ),
    root = list(
      verb = "GET",
      endpoint = "/",
      description = "Retrieve the user's Library object."
    )
  )
  return(L)
}
