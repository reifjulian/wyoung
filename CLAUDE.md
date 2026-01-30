# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**wyoung** is a Stata command (`.ado` file) that controls the family-wise error rate (FWER) when performing multiple hypothesis tests, using the free step-down resampling methodology of Westfall and Young (1993). It also computes Bonferroni-Holm and Sidak-Holm adjusted p-values. Current version: 2.0.

## Running Tests

Tests are run via Stata in the `test/` directory:

```bash
powershell.exe -Command "Start-Process -FilePath 'C:\Program Files\Stata19\StataMP-64.exe' -ArgumentList '/e do wyoung_tests.do' -WorkingDirectory 'test' -Wait -NoNewWindow"
```

Output is written to `test/wyoung_tests.log`. The test suite uses Stata's `cscript` framework and validates results against reference datasets in `test/compare/` using the `cf` (compare files) command. Error cases are tested with `rcof` (return code of).

## Running Simulations

```bash
powershell.exe -Command "Start-Process -FilePath 'C:\Program Files\Stata19\StataMP-64.exe' -ArgumentList '/e do wyoung_simulations.do' -WorkingDirectory 'documentation/simulations' -Wait -NoNewWindow"
```

Simulations test 11 scenarios (normal, subgroup, lognormal, correlated, cluster, lincom, nlcom, multiplefamilyp, permute, permutestrata, permutecluster) and output results to `documentation/simulations/output/` and `documentation/simulations/tables/`.

## Repository Structure

- `src/wyoung.ado` - Main command implementation (~800 lines, 3 Stata programs)
- `src/wyoung.sthlp` - Stata help file (SMCL format)
- `test/wyoung_tests.do` - Comprehensive test suite
- `test/compare/` - Reference datasets for test validation
- `documentation/` - LaTeX source and PDF documentation (also synced with Overleaf)
- `stata.toc` and `wyoung.pkg` - Stata package distribution metadata (files installed from `src/`)

## Architecture

The `.ado` file contains three programs:

1. **`wyoung`** (main) - Supports two syntax variants:
   - **Syntax 1**: Multiple outcomes with one model template using `OUTCOMEVAR` placeholder (e.g., `wyoung mpg turn, cmd(regress OUTCOMEVAR x) familyp(x)`)
   - **Syntax 2**: Multiple distinct models passed as separate strings in `cmd()`

2. **`_wyoung_shuffle`** - Default permutation program for stratified cluster-level shuffling

3. **`_wyoung_fvexpandnobase`** - Helper for factor variable expansion

### Algorithm Flow

The main program follows these steps:
1. Parse syntax and validate options (extensive error checking)
2. Estimate K initial models, extract unadjusted p-values (tries `lincom`, falls back to `nlcom`, then t-stat, then normal approximation)
3. Bootstrap/permutation loop (N replications): resample data, re-estimate models, compute resampled p-values, enforce monotonicity via successive minima
4. Compute Westfall-Young adjusted p-values with monotonicity enforcement via successive maxima
5. Compute Bonferroni-Holm and Sidak-Holm adjusted p-values

### Key Coding Patterns

- Requires Stata version 13+ (`version 13`)
- Uses Mata for string operations (e.g., `mata: st_local("cmd", strtrim(...))`)
- Heavy use of `tempvar`, `tempname`, `tempfile` for scratch data
- `preserve`/`restore` blocks for safe data manipulation during resampling
- Hypothesis count K = num_subgroups x num_familyp x num_outcomes x num_controls
- The `OUTCOMEVAR` and `CONTROLVARS` placeholders in `cmd()` are substituted at runtime

## Package Distribution

- **GitHub install**: `net install wyoung, from("https://raw.githubusercontent.com/reifjulian/wyoung/master") replace`
- **SSC install**: `ssc install wyoung, replace`
- The `wyoung.pkg` file specifies which files from `src/` are installed
