#: Method for text analysis-------------------------

#' Extract n-grams from text with basic filtering and grouping
#'
#' @title Extract n-grams from text
#' @description Tokenise text into n-grams (unigrams, bigrams, etc.) per group, remove
#'   tokens containing digits or stop words, and return a compact data.table of tokens.
#'
#' @param df data.frame or data.table containing the input rows. Must include the columns named
#'   in `grouping_cols` and `text_col`. Note: the function calls `data.table::setDT(df)` and
#'   therefore will convert and mutate `df` by reference; pass `data.table::copy(df)` if you
#'   want to avoid in-place modification.
#' @param ngrams integer scalar or integer vector. If scalar (e.g. `2L`) it is interpreted
#'   as `1:ngrams` (i.e. all n from 1 to that value). If a vector (e.g. `c(1L,2L)`), those
#'   exact n values are used. Values are coerced to integers.
#' @param grouping_cols character vector of column names to keep as grouping keys.
#' These columns are preserved in the output and used to group token rows.
#' @param text_col character scalar. Name of the text column containing sentences to tokenise
#'   (default: `"sentence"`). NA values in the text column are handled as empty input to the
#'   tokenizer (they will not produce tokens).
#' @param min_nchar integer scalar. Minimum number of characters a token must have to be kept.
#'   Defaults to `2L`. Tokens shorter than this are removed.
#' @param stop_words NULL or character vector of words to treat as stop words. If `NULL`
#'   the function uses `tidytext::stop_words$word`. Matching is case-insensitive: all
#'   stop words are lower-cased before comparison.
#'
#' @return A data.table with columns:
#'   - the `grouping_cols` (in the same order as provided),
#'   - `token` (character scalar; multi-word tokens have internal spaces replaced with `_`),
#'   - `ngram` (integer scalar).
#'   Returns an empty data.table with those columns if no tokens survive filtering.
#'   The function throws an error if required packages are missing or if required columns
#'   are not present in `df`.
#'
#' @details This function produces n-grams per row of `df`, preserves the grouping columns,
#'   filters tokens that contain digits or any stop word, and normalises multi-word tokens by
#'   replacing spaces with underscores. It is intended for downstream term-frequency and
#'   TF–IDF computations on grouped text.
#'
#' Implementation notes:
#' - Steps:
#'   1. Validate required namespaces (`tokenizers`, `tidytext`, `stringr`) are available.
#'   2. Convert `df` to a `data.table` in-place (fast, but mutates `df`).
#'   3. Compute the set of `ngram` sizes to generate (either `1:ngrams` when scalar or the
#'      provided vector).
#'   4. For each `n`, call `tokenizers::tokenize_ngrams()` to obtain a list-column of tokens,
#'      then unnest to one token per row.
#'   5. Remove tokens shorter than `min_nchar`, tokens containing digits, or tokens that
#'      contain any stop word component (checked per token word).
#'   6. Replace internal spaces with underscores for multi-word tokens and return a data.table.
#' - Assumptions and preconditions:
#'   - `df` contains the named grouping and text columns.
#'   - `tokenizers::tokenize_ngrams()` returns a list of character vectors per row.
#' - Edge-case handling and failure modes:
#'   - If required packages are not installed the function stops with an informative message.
#'   - If required columns are missing the function stops listing the missing names.
#'   - If no tokens survive filtering the function returns an empty data.table with the
#'     expected column set rather than `NULL`.
#' - Trade-offs:
#'   - The function uses `data.table` for speed and memory efficiency; this also means the
#'     input `df` is modified by reference which can be surprising — callers should copy if
#'     needed.
#' - Dependencies: `tokenizers`, `tidytext`, `stringr`, `data.table`.
#'
#' @implementation Uses `tokenizers::tokenize_ngrams()` for token generation and `data.table`
#'   operations for efficient unnesting and filtering. Stop words are lower-cased and matched
#'   at the component-word level; digits are detected using a simple `grepl("[0-9]", ...)`.
#'
#' @examples
#' # minimal example
#' df <- data.frame(
#'   year_pair_start = c("1900", "1910"),
#'   type = c("new", "old"),
#'   sentence = c("Rationality matters", "A new rational method"),
#'   stringsAsFactors = FALSE
#' )
#' # extract unigrams and bigrams
#' if (requireNamespace("tokenizers", quietly = TRUE)) {
#'   out <- extract_ngrams(df, ngrams = 2L, grouping_cols = c("year_pair_start", "type"))
#'   head(out)
#' }
#'
#' # edge-case: tokens with digits are removed
#' df2 <- data.frame(
#'   year_pair_start = "2000",
#'   type = "new",
#'   sentence = "Model 42 predicts 8 outcomes",
#'   stringsAsFactors = FALSE
#' )
#' if (requireNamespace("tokenizers", quietly = TRUE)) {
#'   extract_ngrams(df2, ngrams = 1L)
#' }
#'
#' @keywords internal
#' @seealso tokenizers::tokenize_ngrams, tidytext::stop_words
#' @author Your Name
extract_ngrams <- function(
  df,
  ngrams = 2L,
  grouping_cols = NULL,
  text_col = "text",
  min_nchar = 2L,
  stop_words = NULL
) {
  # Ensure required packages are available before doing any in-place changes
  if (!requireNamespace("tokenizers", quietly = TRUE)) {
    stop("Please install the 'tokenizers' package.")
  }
  if (!requireNamespace("tidytext", quietly = TRUE)) {
    stop("Please install the 'tidytext' package.")
  }
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("Please install the 'stringr' package.")
  }

  # Convert to data.table in-place for performance. This mutates `df` by reference.
  # If the caller wants to preserve the original, they should call data.table::copy(df).
  data.table::setDT(df)

  # Default stop words from tidytext if none provided; normalise to lower-case
  if (is.null(stop_words)) {
    stop_words <- unique(tidytext::stop_words$word)
  }
  stop_words <- tolower(stop_words)

  # Validate required columns exist
  required_cols <- unique(c(
    grouping_cols,
    text_col
  ))
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in df: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Normalize ngrams input: if scalar -> 1:max, else use provided vector
  if (length(ngrams) == 1L) {
    ng_range <- seq_len(as.integer(ngrams))
  } else {
    ng_range <- as.integer(ngrams)
  }

  # For each requested ngram size generate a temporary data.table with a list-column `token`
  token_list <- lapply(ng_range, function(n) {
    # copy only needed columns to keep memory use low (grouping + text)
    tmp <- df[, c(grouping_cols, text_col), with = FALSE]

    # tokenizers::tokenize_ngrams returns a list of character vectors (one element per row).
    # Use get() to refer to the text column by name inside data.table's environment.
    tmp[,
      token := tokenizers::tokenize_ngrams(
        get(text_col),
        n = as.integer(n),
        lowercase = TRUE,
      )
    ]

    # remove the original text column (we only need grouping + token list)
    tmp[, (text_col) := NULL]

    # record the ngram size for later filtering/analysis
    tmp[, ngram := as.integer(n)]

    tmp
  })

  # Stack all ngram tables into one
  tokens <- data.table::rbindlist(token_list, use.names = TRUE, fill = TRUE)

  # Unnest the list-column of tokens into one token per row, preserving grouping + ngram
  # `unlist()` flattens each element. The by= ensures grouping columns remain as columns.
  tokens <- tokens[, .(token = unlist(token)), by = c(grouping_cols, "ngram")]

  # Filter out tokens that are too short
  tokens <- tokens[nchar(token) >= as.integer(min_nchar), ]

  # faster vectorised splitting (stringi is much quicker than base strsplit)
  word_columns <- stringr::str_c("word_", seq_len(max(ng_range)))
  tokens[, (word_columns) := tstrsplit(token, " ", fixed = TRUE)]

  # detect digits in any component word (NA -> FALSE)
  tokens[,
    has_digit := Reduce(
      `|`,
      lapply(.SD, function(x) {
        xi <- as.character(x)
        !is.na(xi) & grepl("[0-9]", xi)
      })
    ),
    .SDcols = word_columns
  ]

  # detect stop words in any component word (stop_words assumed lower-case)
  tokens[,
    has_stop := Reduce(
      `|`,
      lapply(.SD, function(x) {
        xi <- as.character(x)
        !is.na(xi) & (xi %in% stop_words)
      })
    ),
    .SDcols = word_columns
  ]

  # Keep only tokens without digits and without stop words
  tokens <- tokens[!has_digit & !has_stop]

  # Keep only the requested output columns; replace spaces with underscores in multi-word tokens
  tokens <- tokens[, c(grouping_cols, "token", "ngram"), with = FALSE]
  tokens[, token := stringr::str_replace_all(token, " ", "_")]

  # Return the result as a data.table (tokens[] ensures printing semantics in interactive use)
  tokens[]
}

#' Compute TF–IDF (optionally weighted) for tokens per document
#'
#' @title Compute TF–IDF for tokenized data
#' @description
#' Calculate term-frequency (TF), inverse document frequency (IDF) and TF–IDF
#' for tokens across one or more document identifiers. Supports optional
#' per-occurrence numeric `weight_col` to produce weighted TF values.
#'
#' @param df data.frame or data.table. Token-level table where each row
#'   represents a token occurrence. The function coercively copies `df` to a
#'   data.table and does not mutate the caller's object.
#' @param token_col character scalar. Name of the column containing token
#'   strings (single-word or multi-word tokens). Default: `"token"`.
#'   Must be present in `df`. Token values are treated as character.
#' @param document_col character scalar or character vector. Column name(s)
#'   that together identify a document (e.g. `"doc_id"` or `c("year","id")`).
#'   Values are coerced to character and concatenated with a `"\r"` separator
#'   internally when multiple columns are provided. Returned result restores
#'   the original document columns.
#' @param weight_col character scalar or NULL. Optional column name in `df`
#'   containing numeric per-occurrence weights (e.g. token importance). If
#'   provided, values are coerced to numeric, `NA`s are replaced with `0`
#'   (with a single warning), and absolute values are used. Default: `NULL`
#'   (each token occurrence counts as weight = 1).
#'
#' @return A data.table with one row per (token, document) pair and these
#'   columns (names may vary slightly when multiple `document_col` are used):
#'   - token (character): the token (name preserved as `token_col`).
#'   - <document_col(s)>: restored document identifier columns (character or integer-like).
#'   - corpus_tf (integer): total number of occurrences of `token` across the corpus
#'     (unweighted count of rows that contained the token).
#'   - nb_doc_word (numeric): total weight of all tokens in the given document.
#'   - df (integer): number of distinct documents containing the token.
#'   - idf (numeric): inverse document frequency computed as `log(total_docs / df)`.
#'   - tf or weighted_tf (numeric): per-document term frequency; named `weighted_tf`
#'     when `weight_col` was provided (equals token_doc_weight / nb_doc_word).
#'   - tf_idf (numeric): product of `tf` (or `weighted_tf`) and `idf`.
#'
#'   The function returns a `data.table` (visible in examples). If a division by
#'   zero occurs for `nb_doc_word == 0` the resulting `tf` will be `NaN` for that row.
#'
#' @details
#' High-level behaviour:
#' - The input `df` is copied and coerced to `data.table` for efficient grouping.
#' - If multiple `document_col` values are supplied they are combined into a
#'   temporary composite key (separator `"\r"`) for grouping and later split back
#'   into original columns in the result.
#' - When `weight_col` is given the function builds a positive weight `.weight`
#'   from `abs(weight_col)` and treats `NA` as `0` (with a warning). Otherwise
#'   each occurrence contributes weight `1.0`.
#' - Corpus-level frequency `corpus_tf` is computed as the (unweighted) count
#'   of occurrences of each token in `df`.
#' - Per-document weighted sums and document totals are computed and used to
#'   derive `tf`, `idf`, and `tf_idf` where `idf = log(total_docs / df)`.
#'
#' Implementation notes:
#' - Steps implemented in code:
#'   1. Validate presence of `token_col` and `document_col` and coerce types.
#'   2. Create a safe temporary composite document key when multiple document
#'      columns are provided; choose a helper name that does not collide.
#'   3. Standardise the token column to the name `token` internally to simplify
#'      data.table expressions, restoring the original name before returning.
#'   4. Compute `corpus_tf` by token, then compute per-(token,document)
#'      weighted sums (`token_doc_weight`) and per-document totals (`nb_doc_word`).
#'   5. Compute `tf = token_doc_weight / nb_doc_word` (may be NaN if denominator 0),
#'      `df` (number of documents containing token), `idf = log(total_docs / df)`,
#'      and `tf_idf = tf * idf`.
#'   6. Clean helper columns, restore original column names, and return the table.
#'
#' - Assumptions and preconditions:
#'   * `token_col` and all `document_col` names exist in `df`.
#'   * If provided, `weight_col` is coercible to numeric.
#'   * `token_col` must be distinct from every `document_col`.
#'
#' - Edge cases and failure modes:
#'   * Missing required columns triggers an informative `stop()`.
#'   * `weight_col` NA values are replaced with `0` (warning); negative weights
#'     are made positive via `abs()`.
#'   * If `nb_doc_word == 0` for a document the computed `tf` will be `NaN`;
#'     callers should filter these rows if needed.
#'   * If multiple `document_col` are provided, the composite separator `"\r"`
#'     is used; unusual document values containing `"\r"` may lead to unexpected
#'     splitting but this separator was chosen to minimise collisions.
#'
#' - Trade-offs:
#'   * Uses `data.table` for speed and memory efficiency. The function copies the
#'     input to avoid mutating caller data at the cost of an extra allocation.
#'
#' @implementation
#' The implementation relies on grouping operations in `data.table`:
#' compute corpus counts, aggregate per-(token,document) weights, merge
#' document totals, derive TF/IDF, and restore original column names. Weighting
#' is optional and sign-agnostic (absolute values taken).
#'
#' @examples
#' # normal case: simple corpus, unweighted
#' df <- data.frame(
#'   doc_id = c("a", "a", "b", "b", "b"),
#'   token = c("x", "y", "x", "x", "z"),
#'   stringsAsFactors = FALSE
#' )
#' compute_tf_idf(df, document_col = "doc_id", token_col = "token")
#'
#' # weighted example: per-occurrence weights (NA treated as 0)
#' df2 <- data.frame(
#'   doc = c("d1", "d1", "d2"),
#'   token = c("t", "t", "t"),
#'   w = c(2, NA, 1)
#' )
#' compute_tf_idf(df2, document_col = "doc", token_col = "token", weight_col = "w")
#'
#' @keywords internal
#' @author Your Name
compute_tf_idf <- function(
  df,
  token_col = "token",
  document_col = "document",
  weight_col = NULL
) {
  # Coerce to data.table and operate on a copy to avoid mutating caller data.
  df <- data.table::as.data.table(df)
  df <- data.table::copy(df)

  # Ensure inputs are character vectors
  document_col <- as.character(document_col)
  token_col <- as.character(token_col)

  # Validate required columns exist; provide a clear error if not.
  if (!all(document_col %in% colnames(df))) {
    stop(
      "Input must contain document column(s): ",
      paste(document_col, collapse = ", "),
      call. = FALSE
    )
  }
  if (!token_col %in% colnames(df)) {
    stop("Input must contain token column: ", token_col, call. = FALSE)
  }

  # Capture original classes / metadata for document columns so we can restore types later
  orig_classes <- vapply(
    document_col,
    function(col) {
      cl <- class(df[[col]])[1L]
      if (is.null(cl)) "character" else cl
    },
    character(1),
    USE.NAMES = TRUE
  )
  orig_levels <- lapply(document_col, function(col) {
    if (is.factor(df[[col]])) levels(df[[col]]) else NULL
  })
  names(orig_levels) <- document_col
  orig_tzone <- lapply(document_col, function(col) {
    if (inherits(df[[col]], "POSIXt")) attr(df[[col]], "tzone") else NULL
  })
  names(orig_tzone) <- document_col

  # Validate weight column if provided
  if (!is.null(weight_col)) {
    weight_col <- as.character(weight_col)
    if (!weight_col %in% colnames(df)) {
      stop("weight_col '", weight_col, "' not found in input", call. = FALSE)
    }
    # coerce to numeric and replace NA with 0 (warn once)
    if (!is.numeric(df[[weight_col]])) {
      df[, (weight_col) := as.numeric(.SD[[1]]), .SDcols = weight_col]
    }
    if (any(is.na(df[[weight_col]]))) {
      warning("NA values found in weight_col; treating as 0 for weighting")
      df[is.na(get(weight_col)), (weight_col) := 0]
    }
    df[, .weight := abs(df[[weight_col]])] # use absolute value to avoid negative weights
  } else {
    # default unweighted behaviour: weight = 1 per token occurrence (as before)
    df[, .weight := 1.0]
  }

  # Prevent collision: token_col must not be one of the document grouping columns
  if (token_col %in% document_col) {
    stop("`token_col` must be distinct from `document_col`", call. = FALSE)
  }

  # Create a temporary composite document key when multiple document columns are provided.
  # Use a name unlikely to collide with existing columns.
  doc_key <- ".document_tmp_key"
  i <- 1L
  while (doc_key %in% colnames(df)) {
    doc_key <- paste0(".document_tmp_key", i)
    i <- i + 1L
  }

  # Build composite key from the original values coerced to character for safe concatenation.
  if (length(document_col) == 1L) {
    # keep a character representation in doc_key but remember original class to restore later
    df[, (doc_key) := as.character(.SD[[1]]), .SDcols = document_col]
  } else {
    df[,
      (doc_key) := do.call(paste, c(lapply(.SD, as.character), sep = "\r")),
      .SDcols = document_col
    ]
  }

  # Temporarily standardise token column name to `token` to simplify expressions.
  if (token_col != "token") {
    data.table::setnames(df, token_col, "token")
    token_was_renamed <- TRUE
  } else {
    token_was_renamed <- FALSE
  }

  # corpus_tf: total (unweighted) count of appearances of token across all rows
  df[, corpus_tf := .N, by = "token"]

  # Compute per-document total weight (nb_doc_word) and per token-document weighted sum
  doc_totals <- df[, .(nb_doc_word = sum(.weight)), by = doc_key]
  token_doc <- df[,
    .(token_doc_weight = sum(.weight)),
    by = c("token", doc_key, "corpus_tf")
  ]

  # merge nb_doc_word into token_doc to compute tf
  token_doc <- merge(
    token_doc,
    doc_totals,
    by = doc_key,
    sort = FALSE,
    all.x = TRUE
  )

  # tf: weighted token frequency within document (token_doc_weight / nb_doc_word)
  # guard against division by zero (nb_doc_word == 0) — produce NaN which can be filtered downstream
  token_doc[, tf := token_doc_weight / nb_doc_word]

  # Document frequency: number of distinct documents containing each token
  df_dt <- token_doc[, .(df = .N), by = token]

  # total distinct documents
  total_docs <- uniqueN(token_doc[[doc_key]])

  # merge df into token_doc
  token_doc <- merge(token_doc, df_dt, by = "token", all.x = TRUE, sort = FALSE)

  # IDF and TF–IDF (natural log)
  token_doc[, idf := log(total_docs / df)]
  token_doc[, tf_idf := tf * idf]

  # Restore token original name
  if (token_was_renamed) {
    data.table::setnames(token_doc, "token", token_col)
  }

  # Restore original document columns with original types
  if (length(document_col) == 1L) {
    # single column: convert doc_key back to original class
    col <- document_col[1]
    typ <- orig_classes[col]
    if (typ == "integer") {
      token_doc[, (col) := as.integer(get(doc_key))]
    } else if (typ %in% c("numeric", "double")) {
      token_doc[, (col) := as.numeric(get(doc_key))]
    } else if (typ == "logical") {
      token_doc[, (col) := as.logical(get(doc_key))]
    } else if (typ == "factor") {
      token_doc[, (col) := factor(get(doc_key), levels = orig_levels[[col]])]
    } else if (typ == "Date") {
      token_doc[, (col) := as.Date(get(doc_key))]
    } else if (typ %in% c("POSIXct", "POSIXt")) {
      tz <- orig_tzone[[col]]
      token_doc[, (col) := as.POSIXct(get(doc_key), tz = tz)]
    } else {
      # fallback: character
      token_doc[, (col) := as.character(get(doc_key))]
    }
    token_doc[, (doc_key) := NULL]
  } else {
    # multiple columns: split then coerce each to its original class
    token_doc[,
      (document_col) := data.table::tstrsplit(get(doc_key), "\r", fixed = TRUE)
    ]
    token_doc[, (doc_key) := NULL]

    for (col in document_col) {
      typ <- orig_classes[col]
      if (typ == "integer") {
        token_doc[, (col) := as.integer(get(col))]
      } else if (typ %in% c("numeric", "double")) {
        token_doc[, (col) := as.numeric(get(col))]
      } else if (typ == "logical") {
        token_doc[, (col) := as.logical(get(col))]
      } else if (typ == "factor") {
        token_doc[, (col) := factor(get(col), levels = orig_levels[[col]])]
      } else if (typ == "Date") {
        token_doc[, (col) := as.Date(get(col))]
      } else if (typ %in% c("POSIXct", "POSIXt")) {
        tz <- orig_tzone[[col]]
        token_doc[, (col) := as.POSIXct(get(col), tz = tz)]
      } else {
        token_doc[, (col) := as.character(get(col))]
      }
    }
  }

  # Remove helper weight column from result if present
  token_doc[, c("token_doc_weight") := NULL]

  if (!is.null(weight_col)) {
    data.table::setnames(token_doc, "tf", "weighted_tf")
  }

  # Return the data.table with restored column types
  token_doc[]
}
