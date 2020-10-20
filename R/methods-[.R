# ==== LongTable Class

#' [ LongTable Method
#'
#' Single bracket subsetting for a LongTable object. See subset for more details.
#'
#' This function is endomorphic, it always returns a LongTable object.
#'
#' @examples
#' data(merckDrugCombosSmall)
#'
#' # Character
#' merckDrugCombosSmall['CAOV3', 'ABT-888']
#'
#' # Numeric
#' merckDrugCombosSmall[1, c(1, 2)]
#'
#' # Logical
#' merckDrugCombosSmall[rowData(merckDrugCombosSmall)$cell_line1 == 'A2058', ]
#'
#' # Call
#' merckDrugCombosSmall[.(cell_line1 == 'A2058'),
#'  .(drug1 == 'Dasatinib' & drug2 != '5-FU')]
#'
#' @param x [`LongTable`] The object to subset.
#' @param i [`character`], [`numeric`], [`logical`] or [`call`]
#'  Character: pass in a character vector of drug names, which will subset the
#'      object on all row id columns matching the vector. This parameter also
#'      supports valid R regex query strings which will match on the colnames
#'      of `x`. For convenience, * is converted to .* automatically. Colon
#'      can be to denote a specific part of the colnames string to query.
#'
#'  Numeric or Logical: these select based on the rowKey from the `rowData`
#'      method for the `LongTable`.
#'
#'  Call: Accepts valid query statements to the `data.table` i parameter as
#'      a call object. We have provided the function .() to conveniently
#'      convert raw R statements into a call for use in this function.
#'
#' @param j [`character`], [`numeric`], [`logical`] or [`call`]
#'  Character: pass in a character vector of drug names, which will subset the
#'      object on all drug id columns matching the vector. This parameter also
#'      supports regex queries. Colon can be to denote a specific part of the
#'      colnames string to query.
#'
#'  Numeric or Logical: these select based on the rowID from the `rowData`
#'      method for the `LongTable`.
#'
#'  Call: Accepts valid query statements to the `data.table` i parameter as
#'      a call object. We have provided the function .() to conveniently
#'      convert raw R statements into a call for use in this function.
#' @param assays [`character`] Names of assays which should be kept in the
#'   `LongTable` after subsetting.
#' @param ... Included to ensure drop can only be set by name.
#' @param drop [`logical`] Included for compatibility with the '[' primitive,
#'   it defaults to FALSE and changing it does nothing.
#'
#' @export
setMethod('[', signature('LongTable'), function(x, i, j, assays, ..., drop=FALSE) {
    eval(substitute(subset(x, i, j, assays)))
})