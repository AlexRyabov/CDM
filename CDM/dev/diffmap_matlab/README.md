# Diffusion map: comparison with MATLAB

Developer scripts and reference data used to check `diffmap()` against the
original MATLAB implementation. This folder is excluded from the package build
(`.Rbuildignore`). Run the scripts with this folder as the working directory.

- `save_random_matrix.R` creates `random_matrix_20x10_rowsum1.csv`.
- `load_random_matrix_dm.R` runs `diffmap()` in R and writes `random_matrix_dm.csv`.
- `load_random_matrix_dm.m` is a standalone MATLAB port that writes
  `random_matrix_dm_matlab.csv`.
- `load_random_matrix_dm_matlab_diffmap.m` runs the original MATLAB DM toolbox.
- `dune_example.R` is a visual example with the `vegan` dune data.

The automated regression test `tests/testthat/test-diffmap.R` uses copies of
`random_matrix_20x10_rowsum1.csv` and `random_matrix_dm_matlab.csv` from
`tests/testthat/fixtures/`.
