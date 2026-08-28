expected_fa <- data.frame(tribble(
  ~seq_id,   ~seq_desc,                                                              ~length,
  "Z92525.1", "Prochlorococcus marinus ppeC gene for phycoerythrin class III gamma chain", 1111,
  "U44977.1", "Prochlorococcus marinus DnaA (dnaA) gene, complete cds"                   , 5304,
  "AF354644.1", "Prochlorococcus marinus psb operon, complete sequence"                  , 2567
))

test_that("get seq len from fasta", {
  expect_equal(data.frame(read_seq_len("../data/seq-len/sequence.fasta")), expected_fa)
})

expected_gff <- mutate(expected_fa, seq_desc = NA_character_)

test_that("get seq len from gff", {
  expect_equal(data.frame(read_seq_len("../data/seq-len/sequence.gff3")), expected_gff)
})

expected_gb <- mutate(expected_gff, seq_id = str_replace(str_remove(seq_id,".1"),"U","PMU"))

test_that("get seq len from gb", {
  expect_equal(data.frame(read_seq_len("../data/seq-len/sequence.gb")), expected_gb)
})
