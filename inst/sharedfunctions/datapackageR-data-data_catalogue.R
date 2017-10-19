#' @title data_catalogue
#' @name data_catalogue
#' @description \code{\link{list}} representing a collection of metadata
#' for stored or othwhise used files/objects as a reference for data
#' verification.
#' @seealso \code{\link{list}} \code{\link[digest]{digest}}
#' @format \code{\link{list}} with entries named according to file names.
#' \code{default_compression_algo} and \code{default_hashing_algo} are set as
#' \code{\link{attributes}} and represent default values for the corresponding
#' per-entry values below.
#' Each entry is a \code{\link{list}} with the following fields:
#' \describe{
#'   \item{\code{File}:}{\code{\link{basename}}s of original data files as
#'     \code{\link{character}} objects. Giving rise to objects of the same names
#'     and are stored (zipped and with the corresponding extension) in the
#'     \code{extdata} directory of the package.}
#'   \item{\code{Hashing.Algo}:}{Cryptographic hashing algorithms as understood
#'     by \code{\link[digest]{digest}} and represented by
#'     \code{\link{character}} objects. Used to test data integrity (see below).}
#'   \item{\code{Hash.Uncompressed} & \code{Hash.Compressed}:}{
#'     \code{\link{character}} representations of cryptographic hashes of the
#'     plain and zip-compressed version of integrated data files. Algorithm used
#'     depends on \code{Hashing.Algo}.}
#'   \item{\code{Parsing,Function}:}{Name (\code{\link{character}}) of the
#'     \code{R} function used to produce the corresponding \code{R} object from
#'     its original data file.}
#'   \item{\code{Parsing.Options}:}{Options to \code{File.Reading.Function}
#'     stored as a \code{\link{list}}.}
#'   \item{\code{Package.Dependencies}:}{Package dependencies
#'     required by \code{Parsing.Function}. Encoded as a
#'     \code{\link{character}} vector.}
#'   \item{\code{Git.Ignore}:}{\code{\link{logical}}, indicating whether
#'     the file is to be listed in \file{.gitignore} of the top package
#'     directory. This option is intended for tracking analysis of
#'     non-distibutable data sets.}
#'   \item{\code{R.Buildgnore}:}{\code{\link{logical}}, indicating whether
#'     the file is to be listed in \file{.Rbuildignore} of the top package
#'     directory. This option is intended for tracking analysis of
#'     non-distibutable data sets.}}
#' @keywords datasets
#' @docType data
NULL
