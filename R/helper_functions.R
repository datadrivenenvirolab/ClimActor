# Data-Driven Lab
# Helper functions for package

# Helper function to check for names column
#
# @param dataset
# @param col
.col_check <- function(dataset, col) {
  if (!any(grepl(col, tolower(names(dataset))))){
    cat(paste0("No \"" , col, "\"",
               "column is detected in the dataset.",
               " Would you like to specify a column to rename?"))
        ans <- readline(prompt = "Rename column? (Y/N): ")
        while (toupper(ans) != "Y" & toupper(ans) != "N"){
          ans <- readline(prompt = "Please input either Y/N:")
        }
        if (toupper(ans) == "Y"){
          ans2 <- readline(prompt = "Please input column name to be renamed:")
          names(dataset)[grepl(tolower(ans2), tolower(names(dataset)))] <- col
        } else if (toupper(ans) == "N"){
          stop()
          on.exit(assign("to.stop", T), add = T)
        }
  }
  # Check for similar versions of "col"
  col.names <- gsub("[[:punct:]]", "", tolower(names(dataset)))
  if (any(grepl(gsub("[[:punct:]]", "", col), col.names))){
    names(dataset)[grepl(gsub("[[:punct:]]", "", col), col.names)] <- col
    assign(paste0(col, "name"),
           names(dataset)[grepl(gsub("[[:punct:]]", "", col), col.names)])
  }
  return(dataset)
}

# Coerce location names to handle special characters

# @param locations Column with names of location (city, country, etc.)
# @return A vector of locations names with the special character replaced with
# closest equivalent
# @example coerce_locations_names(df$name)
.coerce_location_names <- function(locations) {
  locations <- gsub("[\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c6\u0100\u0102\u0104\u01cd\u01de\u01e0\u01e2\u01fa\u01fc\u0200\u0202\u1ea4\u1ea6\u1ea8\u1eb6\u0226\u1e00\u1ea0\u1ea2]", "A", locations)
  locations <- gsub("[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e6\u0101\u0103\u0105\u01ce\u01df\u01e1\u01e3\u01fb\u01fd\u0201\u0203\u1ea5\u1ea7\u1ea9\u1eb7\u0227\u1e01\u1ea1\u1ea3\u1e9a]", "a", locations)
  locations <- gsub("[\u1e02\u1e04\u1e06\u0243]", "B", locations)
  locations <- gsub("[\u1e03\u1e05\u1e07\u0180\u1d6c\u1d80\u0253]", "b", locations)
  locations <- gsub("[\u00c7\u0106\u0108\u010a\u010c]", "C", locations)
  locations <- gsub("[\u00e7\u0107\u0109\u010b\u010d]", "c", locations)
  locations <- gsub("[\u00d0\u010e\u0110]", "D", locations)
  locations <- gsub("[\u00f0\u010f\u0111]", "d", locations)
  locations <- gsub("[\u00c8\u00c9\u00ca\u00cb\u0112\u0114\u0116\u0118\u011a\u0204\u0206]", "E", locations)
  locations <- gsub("[\u00e8\u00e9\u00ea\u00eb\u0113\u0115\u0117\u0119\u011b\u0205\u0207]", "e", locations)
  locations <- gsub("[\u0191]", "F", locations)
  locations <- gsub("[\u0192]", "f", locations)
  locations <- gsub("[\u011c\u011e\u0120\u0122\u01e4\u01e6]", "G", locations)
  locations <- gsub("[\u011d\u011f\u0121\u0123\u01e5\u01e7]", "g", locations)
  locations <- gsub("[\u0124\u0126]", "H", locations)
  locations <- gsub("[\u0125\u0127]", "h", locations)
  locations <- gsub("[\u00cc\u00cd\u00ce\u00cf\u0128\u012a\u012c\u012e\u0130\u0132\u01cf\u0208\u020a]", "I", locations)
  locations <- gsub("[\u00ec\u00ed\u00ee\u00ef\u0129\u012b\u012d\u012f\u0131\u0133\u01d0\u0209\u020b]", "i", locations)
  locations <- gsub("[\u0134\u0248]", "J", locations)
  locations <- gsub("[\u0135\u01f0\u0249]", "j", locations)
  locations <- gsub("[\u0136]", "K", locations)
  locations <- gsub("[\u0137\u0138]", "k", locations)
  locations <- gsub("[\u0139\u013b\u013d\u013f\u0141]", "L", locations)
  locations <- gsub("[\u013a\u013c\u013e\u0140\u0142\u019a]", "l", locations)
  locations <- gsub("[\u00d1\u0143\u0145\u0147\u014a\u01f8]", "N", locations)
  locations <- gsub("[\u00f1\u0144\u0146\u0148\u0149\u014b\u01f9]", "n", locations)
  locations <- gsub("[\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8\u014c\u014e\u0150\u0152\u019f\u01a0\u01a2\u01d1\u01ea\u01ec\u020c\u020e\u022a\u022c\u022e\u0230]", "O", locations)
  locations <- gsub("[\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u014f\u0151\u0153\u01a1\u01a3\u01d2\u01eb\u01ed\u020d\u020f\u022b\u022d\u022f\u0231]", "o", locations)
  locations <- gsub("[\u0154\u0156\u0158\u0210\u0212\u024c]", "R", locations)
  locations <- gsub("[\u0155\u0157\u0159\u0211\u0213\u024d]", "r", locations)
  locations <- gsub("[\u015a\u015c\u015e\u0160\u0218]", "S", locations)
  locations <- gsub("[\u015b\u015d\u015f\u0161\u0219]", "s", locations)
  locations <- gsub("[\u0162\u0164\u0166\u01ac\u01ae]", "T", locations)
  locations <- gsub("[\u0163\u0165\u0167\u01ab\u01ad]", "t", locations)
  locations <- gsub("[\u00d9\u00da\u00db\u00dc\u0168\u016a\u016c\u016e\u0170\u0172\u01d3\u01d5\u01d7\u01d9\u01db\u0214\u0216]", "U", locations)
  locations <- gsub("[\u00f9\u00fa\u00fb\u00fc\u0169\u016b\u016d\u016f\u0171\u0173\u01d4\u01d6\u01d8\u01da\u01dc\u0215\u0217]", "u", locations)
  locations <- gsub("[\u0174]", "W", locations)
  locations <- gsub("[\u0175]", "w", locations)
  locations <- gsub("[\u00dd\u0176\u0178\u024e]", "Y", locations)
  locations <- gsub("[\u00fd\u00ff\u0177\u024f]", "y", locations)
  locations <- gsub("[\u0179\u017b\u017d\u1e92\u1e94\u1e90\u0179\u01b5]", "Z", locations)
  locations <- gsub("[\u017a\u017c\u017e\u1e93\u1e95\u1e91\u0290\u0291\u0225\u01b6]", "z", locations)

  return(locations)
}


# Helper function to help check and convert the encoding of the column specified
# @param column Column to check and convert the encoding for
# @return column with the encoding (hopefully) converted
.check_and_convert <- function(col){
  # Check if there exists a hidden environment to create hidden variables for
  # If not, then create one
  if (!exists(".pkgenv")){
    .pkgenv <- new.env(parent = emptyenv())
  }
  if (!exists("foreign", envir = .pkgenv)){
    assign("foreign", names(which(!unlist(l10n_info()[2:3]))),
           envir = .pkgenv)
    assign("native", names(which(unlist(l10n_info()[2:3]))),
           envir = .pkgenv)
  }
  if (.pkgenv$native == "UTF-8"){
    col <- iconv(col, from = "UTF-8", to = "latin1")
    col <- iconv(col, from = "latin1", to = "UTF-8")
  } else if (.pkgenv$native == "Latin-1"){
    col <- iconv(col, from = "latin1", to = "UTF-8")
    col <- iconv(col, from = "UTF-8", to = "UTF-8")
  }
  return(col)
}
