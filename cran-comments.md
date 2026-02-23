## Test environments
- Local: Ubuntu 24.04.4 LTS, R 4.5.2
- GitHub Actions own workflows: 
  - macos-release, windows-release/oldrel4, ubuntu-release
- R-hub v2 via GitHub Actions: 
  - ubuntu-release, ubuntu-next


## R CMD check results
0 errors | 0 warnings | 0 notes

## Additional comments
- The package passes cleanly on R-release across Windows, macOS, and Linux.
- R-devel checks fail only due to temporary unavailability of Bioconductor package ‘IRanges’ for Bioconductor 3.23.
- The package includes fallbacks that gracefully skip IRanges-dependent functionality when it is unavailable.