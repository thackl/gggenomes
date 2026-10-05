#' Simulate genomes for gggenomes
#'
#' Simulates prokaryotic-like gene arrangements with realistic gene lengths,
#' intergenic gaps and overlaps, occasional large intergenic regions, and
#' strand runs. Bins can optionally be fragmented into multiple sequences.
#'
#' Simulation proceeds hierarchically:
#'
#' ```
#'   sim_genomes()
#'       |
#'       +-- for each bin
#'       |     |
#'       |     +-- sim_gene_lengths()
#'       |     |      `-- log-normal gene lengths
#'       |     |
#'       |     +-- sim_gap_lengths()
#'       |     |      +-- short overlaps
#'       |     |      +-- ordinary intergenic gaps
#'       |     |      `-- occasional large gaps
#'       |     |
#'       |     +-- sim_strands()
#'       |     |      `-- strand runs via a Markov process
#'       |     |
#'       |     `-- assemble genes along one continuous sequence
#'       |
#'       `-- fragment_genomes()
#'              |
#'              +-- sample relative sequence sizes from rexp()
#'              +-- convert to target breakpoints
#'              +-- merge overlapping gene intervals
#'              +-- move breakpoints out of occupied intervals
#'              +-- remove duplicate / zero-length fragments
#'              `-- remap seq_id, start and end
#' ```
#'
#' @param n_genes Number of genes per bin. Scalar or vector of length `n_bins`.
#'   If `NULL`, complete genes are generated until `bin_sizes` is filled.
#' @param bin_sizes Desired bin sizes in bp. Scalar or vector of length
#'   `n_bins`. If `NULL`, bin sizes emerge from the simulated genes and gaps.
#' @param n_bins Number of bins. If `NULL`, inferred from the longest of
#'   `n_genes`, `bin_sizes` and `n_seqs`.
#' @param n_seqs Number of sequences per bin. Scalar or vector of length
#'   `n_bins`.
#' @param gene_median Median gene length in bp.
#' @param gene_sigma Standard deviation on the log scale for gene lengths.
#' @param gene_min,gene_max Minimum and maximum gene lengths.
#' @param overlap_prob Probability that adjacent genes overlap.
#' @param overlap_mean Mean size of overlaps in bp.
#' @param gap_median Median ordinary intergenic gap in bp.
#' @param gap_sigma Standard deviation on the log scale for ordinary gaps.
#' @param large_gap_prob Probability that a positive gap is an unusually large
#'   intergenic region.
#' @param large_gap_median Median size of large gaps.
#' @param large_gap_sigma Standard deviation on the log scale for large gaps.
#' @param strand_switch_prob Probability of switching strand between adjacent
#'   genes.
#' @param seed Optional random seed.
#'
#' @return A list with `genes` and `seqs` tibbles.
#' @export
#' @examples
#' # three genomes with 20 genes each
#' x <- sim_genomes(n_genes = 20, n_bins = 3, seed = 42)
#' gggenomes(genes = x$genes, seqs = x$seqs) +
#'   geom_seq() + geom_bin_label() +
#'   geom_gene(aes(fill = strand))
#'
#' # genomes of fixed size, fragmented into a varying number of contigs
#' x <- sim_genomes(
#'   bin_sizes = c(60e3, 40e3), n_seqs = c(5, 2), seed = 42
#' )
#' gggenomes(genes = x$genes, seqs = x$seqs) +
#'   geom_seq() + geom_seq_label() + geom_bin_label() +
#'   geom_gene()
#'
#' # tweak the gene model: long genes, frequent overlaps and strand switches
#' x <- sim_genomes(
#'   n_genes = 30, gene_median = 2000, overlap_prob = .5,
#'   strand_switch_prob = .5, seed = 42
#' )
#' gggenomes(genes = x$genes, seqs = x$seqs) +
#'   geom_seq() + geom_gene(aes(fill = strand), position = "strand")
sim_genomes <- function(
    n_genes = NULL,
    bin_sizes = NULL,
    n_bins = NULL,
    n_seqs = 1,
    gene_median = 850,
    gene_sigma = .65,
    gene_min = 90,
    gene_max = 10000,
    overlap_prob = .12,
    overlap_mean = 20,
    gap_median = 70,
    gap_sigma = 1,
    large_gap_prob = .02,
    large_gap_median = 2000,
    large_gap_sigma = 1,
    strand_switch_prob = .2,
    seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(n_genes) && is.null(bin_sizes)) {
    stop("At least one of `n_genes` and `bin_sizes` must be supplied.")
  }

  # Infer the number of bins from the longest per-bin argument.
  n_bins <- n_bins %||% max(lengths(list(n_genes, bin_sizes, n_seqs)), 1L)

  n_genes <- recycle_arg(n_genes, n_bins)
  bin_sizes <- recycle_arg(bin_sizes, n_bins)
  n_seqs <- recycle_arg(n_seqs, n_bins)
  bin_ids <- make_bin_ids(n_bins)

  bins <- lapply(seq_len(n_bins), function(i) {

    ng <- if (is.null(n_genes)) NULL else n_genes[i]
    bs <- if (is.null(bin_sizes)) NULL else bin_sizes[i]

    bin_id <- bin_ids[i]

    # -----------------------------------------------------------------------
    # Simulate gene and gap lengths
    #
    # With only a bin size, generate the natural gene/gap process until
    # another complete gene would exceed the requested size.
    #
    # With a fixed number of genes, generate exactly that many genes. If a
    # bin size is also specified, positive intergenic gaps are subsequently
    # rescaled to satisfy the requested total size while preserving gene
    # lengths and overlaps.
    # -----------------------------------------------------------------------

    if (is.null(ng)) {

      sim <- sim_until_size(
        bs,
        gene_median = gene_median,
        gene_sigma = gene_sigma,
        gene_min = gene_min,
        gene_max = gene_max,
        overlap_prob = overlap_prob,
        overlap_mean = overlap_mean,
        gap_median = gap_median,
        gap_sigma = gap_sigma,
        large_gap_prob = large_gap_prob,
        large_gap_median = large_gap_median,
        large_gap_sigma = large_gap_sigma
      )

      lengths <- sim$lengths
      gaps <- sim$gaps
      ng <- length(lengths)

    } else {

      lengths <- sim_gene_lengths(
        ng,
        median = gene_median,
        sigma = gene_sigma,
        min = gene_min,
        max = gene_max
      )

      gaps <- sim_gap_lengths(
        max(ng - 1L, 0L),
        overlap_prob = overlap_prob,
        overlap_mean = overlap_mean,
        gap_median = gap_median,
        gap_sigma = gap_sigma,
        large_gap_prob = large_gap_prob,
        large_gap_median = large_gap_median,
        large_gap_sigma = large_gap_sigma
      )
    }

    # -----------------------------------------------------------------------
    # Constrain the simulated arrangement to a requested bin size.
    #
    # Gene lengths and overlaps are preserved. Only positive gaps are scaled.
    # -----------------------------------------------------------------------

    if (!is.null(bs) && !is.null(n_genes)) {

      occupied <- sum(lengths) + sum(gaps[gaps < 0])
      available <- bs - occupied

      if (available < 0) {
        stop(
          "Genes and overlaps exceed requested `bin_sizes` for bin ",
          bin_id, "."
        )
      }

      positive <- gaps > 0

      if (any(positive)) {

        gaps[positive] <- round(
          gaps[positive] * available / sum(gaps[positive])
        )

        # Correct rounding so the resulting bin has exactly the requested
        # length.
        delta <- bs - (sum(lengths) + sum(gaps))
        j <- which(positive)[1]
        gaps[j] <- gaps[j] + delta

      } else if (available > 0 && length(gaps)) {

        # There are no positive gaps to scale. Put the remaining sequence
        # into one intergenic region.
        gaps[1] <- gaps[1] + available

      } else if (available > 0 && ng == 1) {

        # A single gene has no internal gap. Remaining sequence becomes
        # terminal sequence after the gene and requires no coordinate change.
      }
    }

    # -----------------------------------------------------------------------
    # Assemble genes along one continuous sequence.
    # -----------------------------------------------------------------------

    strands <- sim_strands(
      ng,
      switch_prob = strand_switch_prob
    )

    if (ng == 0) {

      genes <- tibble::tibble(
        bin_id = character(),
        seq_id = character(),
        start = integer(),
        end = integer(),
        strand = character(),
        gene_id = character(),
        type = character()
      )

      size <- bs %||% 0L

    } else {

      starts <- c(
        1L,
        head(cumsum(lengths + c(gaps, 0L)), -1L) + 1L
      )

      ends <- starts + lengths - 1L

      # If bin size is specified it may include terminal non-coding sequence.
      size <- bs %||% max(ends)

      genes <- tibble::tibble(
        bin_id = bin_id,
        seq_id = bin_id,
        start = as.integer(starts),
        end = as.integer(ends),
        strand = strands,
        gene_id = paste0(bin_id, "_", seq_len(ng)),
        type = "CDS"
      )
    }

    list(
      genes = genes,
      seqs = tibble::tibble(
        bin_id = bin_id,
        seq_id = bin_id,
        length = as.integer(size)
      )
    )
  })

  x <- list(
    genes = dplyr::bind_rows(lapply(bins, `[[`, "genes")),
    seqs = dplyr::bind_rows(lapply(bins, `[[`, "seqs"))
  )

  # Fragmentation is deliberately a separate operation. sim_genomes() calls it
  # for convenience, while fragment_genomes() can also be used independently to
  # re-fragment the same simulated genes with different seeds or n_seqs.
  fragment_genomes(
    x$genes,
    x$seqs,
    n_seqs = n_seqs
  )
}


# ---------------------------------------------------------------------------
# Component generators
# ---------------------------------------------------------------------------

sim_gene_lengths <- function(
    n,
    median = 850,
    sigma = .65,
    min = 90,
    max = 10000) {

  if (n == 0) {
    return(integer())
  }

  x <- round(stats::rlnorm(n, log(median), sigma))

  as.integer(
    pmax(min, pmin(max, x))
  )
}


sim_gap_lengths <- function(
    n,
    overlap_prob = .12,
    overlap_mean = 20,
    gap_median = 70,
    gap_sigma = 1,
    large_gap_prob = .02,
    large_gap_median = 2000,
    large_gap_sigma = 1) {

  if (n == 0) {
    return(integer())
  }

  overlap <- stats::runif(n) < overlap_prob
  large <- !overlap & stats::runif(n) < large_gap_prob

  # Ordinary positive intergenic gaps.
  gaps <- round(
    stats::rlnorm(n, log(gap_median), gap_sigma)
  )

  # Occasional unusually large intergenic regions.
  gaps[large] <- round(
    stats::rlnorm(
      sum(large),
      log(large_gap_median),
      large_gap_sigma
    )
  )

  # Overlapping genes are represented by negative gaps.
  gaps[overlap] <- -round(
    stats::rexp(
      sum(overlap),
      rate = 1 / overlap_mean
    )
  )

  as.integer(gaps)
}


sim_strands <- function(
    n,
    switch_prob = .2) {

  if (n == 0) {
    return(character())
  }

  first <- sample(c(0L, 1L), 1L)

  if (n == 1) {
    return(if (first == 0L) "+" else "-")
  }

  switch <- stats::runif(n - 1L) < switch_prob

  state <- cumsum(
    c(first, switch)
  ) %% 2L

  ifelse(state == 0L, "+", "-")
}


# ---------------------------------------------------------------------------
# Size-constrained simulation
# ---------------------------------------------------------------------------

sim_until_size <- function(
    size,
    gene_median = 850,
    gene_sigma = .65,
    gene_min = 90,
    gene_max = 10000,
    overlap_prob = .12,
    overlap_mean = 20,
    gap_median = 70,
    gap_sigma = 1,
    large_gap_prob = .02,
    large_gap_median = 2000,
    large_gap_sigma = 1) {

  if (size <= 0) {
    return(list(
      lengths = integer(),
      gaps = integer()
    ))
  }

  lengths <- integer()
  gaps <- integer()
  pos <- 0

  # Generate in batches rather than repeatedly calling the RNG for individual
  # genes. This estimate controls only the batch size; it does not determine
  # the resulting number of genes.
  #
  # Use the mean of the underlying log-normal gene-length distribution rather
  # than its median to obtain a more useful initial batch-size estimate.
  mean_gene <- exp(
    log(gene_median) + gene_sigma^2 / 2
  )

  estimated_n <- size / (mean_gene + gap_median)

  batch_size <- max(
    100L,
    ceiling(estimated_n * 1.1)
  )

  repeat {

    new_lengths <- sim_gene_lengths(
      batch_size,
      median = gene_median,
      sigma = gene_sigma,
      min = gene_min,
      max = gene_max
    )

    # Generate one potential preceding gap for every gene. For the first gene
    # in the entire bin, the first gap is ignored.
    new_gaps <- sim_gap_lengths(
      batch_size,
      overlap_prob = overlap_prob,
      overlap_mean = overlap_mean,
      gap_median = gap_median,
      gap_sigma = gap_sigma,
      large_gap_prob = large_gap_prob,
      large_gap_median = large_gap_median,
      large_gap_sigma = large_gap_sigma
    )

    step <- new_lengths + new_gaps

    if (!length(lengths)) {
      step[1] <- new_lengths[1]
    }

    positions <- pos + cumsum(step)
    keep <- positions <= size

    if (any(keep)) {

      k <- max(which(keep))

      if (!length(lengths)) {

        lengths <- new_lengths[seq_len(k)]

        if (k > 1L) {
          gaps <- new_gaps[2:k]
        }

      } else {

        lengths <- c(
          lengths,
          new_lengths[seq_len(k)]
        )

        gaps <- c(
          gaps,
          new_gaps[seq_len(k)]
        )
      }

      pos <- positions[k]
    }

    # The first gene that would extend beyond the requested bin terminates the
    # simulation. Remaining sequence becomes terminal non-coding sequence.
    if (!all(keep)) {
      break
    }
  }

  list(
    lengths = lengths,
    gaps = gaps
  )
}


# ---------------------------------------------------------------------------
# Fragmentation
# ---------------------------------------------------------------------------

#' Fragment simulated genomes into sequences
#'
#' Fragmentation is independent of gene simulation. Target sequence sizes are
#' drawn from an exponential distribution and normalized to the bin size.
#'
#' Gene coordinates are expected relative to the complete bin, i.e. each bin
#' should consist of a single sequence, as returned by `sim_genomes(n_seqs =
#' 1)`. Re-fragmenting already fragmented bins is not supported.
#'
#' Target breakpoints that fall inside genes are moved to the nearest valid
#' intergenic coordinate. With inclusive gene coordinates, the valid positions
#' immediately surrounding an occupied interval `[start, end]` are
#' `start - 1` and `end`: a breakpoint at `end` leaves the complete gene on the
#' left-hand sequence, while a breakpoint at `start - 1` leaves it on the
#' right-hand sequence.
#'
#' Overlapping genes are first merged into occupied intervals so that
#' breakpoint relocation cannot move a breakpoint from one gene into another.
#'
#' Gene-free sequences are retained. Duplicate breakpoints, which would produce
#' zero-length sequences, are removed.
#'
#' @param genes Gene table.
#' @param seqs Sequence table describing the unfragmented bins.
#' @param n_seqs Number of sequences per bin.
#' @param seed Optional random seed, allowing the same genes to be repeatedly
#'   fragmented into reproducible alternative assemblies.
#'
#' @return A list containing `genes` and `seqs`.
#' @rdname sim_genomes
#' @export
#' @examples
#' # simulate two complete genomes, then fragment them into contigs
#' x <- sim_genomes(n_genes = 40, n_bins = 2, seed = 42)
#' y <- fragment_genomes(x$genes, x$seqs, n_seqs = 6, seed = 42)
#' gggenomes(genes = y$genes, seqs = y$seqs) +
#'   geom_seq() + geom_seq_label() + geom_bin_label() +
#'   geom_gene()
#'
#' # the same genes fragmented into two alternative assemblies
#' library(patchwork)
#' a <- fragment_genomes(x$genes, x$seqs, n_seqs = 4, seed = 43)
#' b <- fragment_genomes(x$genes, x$seqs, n_seqs = 4, seed = 44)
#' gggenomes(genes = a$genes, seqs = a$seqs) + geom_seq() + geom_gene() +
#'   gggenomes(genes = b$genes, seqs = b$seqs) + geom_seq() + geom_gene() +
#'   plot_layout(ncol = 1)
fragment_genomes <- function(
    genes,
    seqs,
    n_seqs = 1,
    seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  bins <- unique(seqs$bin_id)
  n_seqs <- recycle_arg(n_seqs, length(bins))

  out <- lapply(seq_along(bins), function(i) {

    bin <- bins[i]

    s <- seqs[
      seqs$bin_id == bin,
      ,
      drop = FALSE
    ]

    g <- genes[
      genes$bin_id == bin,
      ,
      drop = FALSE
    ]

    size <- sum(s$length)
    ns <- n_seqs[i]

    if (ns <= 1L || size <= 1L) {

      seq_id <- paste0(bin, "_1")

      s <- tibble::tibble(
        bin_id = bin,
        seq_id = seq_id,
        length = as.integer(size)
      )

      if (nrow(g)) {
        g$seq_id <- seq_id
      }

      return(list(
        genes = g,
        seqs = s
      ))
    }

    # -----------------------------------------------------------------------
    # Draw unequal relative sequence sizes.
    #
    # rexp() naturally produces a mixture of larger and smaller fragments.
    # Normalizing the weights ensures that their target lengths sum exactly
    # to the original bin size.
    # -----------------------------------------------------------------------

    weights <- stats::rexp(ns)
    target <- round(
      cumsum(weights / sum(weights) * size)
    )

    target <- target[-length(target)]

    # -----------------------------------------------------------------------
    # Move targets that fall inside genes to the nearest intergenic position.
    #
    # Occupied intervals are merged once, after which all breakpoint lookups
    # are performed vectorially using findInterval().
    # -----------------------------------------------------------------------

    breaks <- move_breakpoints(
      target,
      genes = g,
      size = size
    )

    # Breakpoints may collapse onto the same intergenic coordinate when nearby
    # targets fall within the same gene or overlapping gene cluster. Remove
    # duplicates rather than producing zero-length sequences.
    breaks <- sort(
      unique(as.integer(breaks))
    )

    breaks <- breaks[
      breaks > 0L &
      breaks < size
    ]

    boundaries <- c(
      0L,
      breaks,
      size
    )

    new_seqs <- tibble::tibble(
      bin_id = bin,
      seq_id = paste0(
        bin,
        "_",
        seq_len(length(boundaries) - 1L)
      ),
      length = as.integer(diff(boundaries))
    )

    # -----------------------------------------------------------------------
    # Remap genes from bin-relative to sequence-relative coordinates.
    # -----------------------------------------------------------------------

    if (nrow(g) > 0) {

      # start - 1 represents the boundary immediately before each gene.
      seq_no <- findInterval(
        g$start - 1L,
        breaks
      ) + 1L

      offsets <- boundaries[seq_no]

      g$seq_id <- new_seqs$seq_id[seq_no]
      g$start <- g$start - offsets
      g$end <- g$end - offsets
    }

    list(
      genes = g,
      seqs = new_seqs
    )
  })

  list(
    genes = dplyr::bind_rows(
      lapply(out, `[[`, "genes")
    ),
    seqs = dplyr::bind_rows(
      lapply(out, `[[`, "seqs")
    )
  )
}


# ---------------------------------------------------------------------------
# Merge occupied gene intervals
# ---------------------------------------------------------------------------

merge_gene_intervals <- function(
    start,
    end) {

  if (!length(start)) {
    return(
      tibble::tibble(
        start = integer(),
        end = integer()
      )
    )
  }

  o <- order(start, end)

  start <- start[o]
  end <- end[o]

  # A new occupied interval begins whenever the next gene starts beyond the
  # furthest coordinate covered by all preceding genes. This merges arbitrary
  # chains of overlapping genes, not just directly adjacent pairs.
  max_end <- cummax(end)

  new_interval <- c(
    TRUE,
    start[-1L] > max_end[-length(max_end)]
  )

  group <- cumsum(new_interval)

  tibble::tibble(
    start = start,
    end = end,
    group = group
  ) |>
    dplyr::summarise(
      start = min(start),
      end = max(end),
      .by = group
    ) |>
    dplyr::select(-group)
}


# ---------------------------------------------------------------------------
# Move breakpoints out of occupied gene intervals
# ---------------------------------------------------------------------------

move_breakpoints <- function(
    x,
    genes,
    size) {

  if (!length(x) || nrow(genes) == 0) {
    return(x)
  }

  occupied <- merge_gene_intervals(
    genes$start,
    genes$end
  )

  # For each target, identify the last occupied interval whose start is at or
  # before the target.
  i <- findInterval(
    x,
    occupied$start
  )

  # pmax() provides a valid index for targets preceding the first gene; these
  # are subsequently excluded by i > 0.
  ii <- pmax(i, 1L)

  inside <- i > 0L &
    x < occupied$end[ii]

  if (!any(inside)) {
    return(x)
  }

  ii <- i[inside]

  # Coordinates are inclusive. For an occupied interval [start, end], valid
  # sequence boundaries immediately outside it are:
  #
  #     start - 1 | [ start ........ end ] | end
  #               ^                       ^
  #            before gene             after gene
  #
  # A boundary at `end` places the complete occupied interval on the left-hand
  # sequence. A boundary at `start - 1` places it on the right-hand sequence.
  left <- occupied$start[ii] - 1L
  right <- occupied$end[ii]

  # Boundaries at 0 or at the complete bin size do not create useful internal
  # fragments.
  left[left <= 0L] <- NA_integer_
  right[right >= size] <- NA_integer_

  dl <- abs(x[inside] - left)
  dr <- abs(right - x[inside])

  dl[is.na(dl)] <- Inf
  dr[is.na(dr)] <- Inf

  # Resolve ties towards the left boundary for deterministic behaviour.
  x[inside] <- ifelse(
    dl <= dr,
    left,
    right
  )

  x
}


# ---------------------------------------------------------------------------
# Utilities
# ---------------------------------------------------------------------------

recycle_arg <- function(
    x,
    n) {

  if (is.null(x)) {
    return(NULL)
  }

  if (!length(x) %in% c(1L, n)) {
    stop(
      "`", deparse(substitute(x)), "` must have length 1 or match the ",
      "number of bins (", n, "), not ", length(x), "."
    )
  }

  rep(
    x,
    length.out = n
  )
}


make_bin_ids <- function(n) {
  vapply(seq_len(n), function(i) {
    id <- character()

    while (i > 0) {
      i <- i - 1L
      id <- c(LETTERS[i %% 26L + 1L], id)
      i <- i %/% 26L
    }

    paste0(id, collapse = "")
  }, character(1))
}