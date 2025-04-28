# Comments on the submission of the `datamedios` package

## Results from `R CMD check`

-   **Checks performed**: All checks passed without errors on Windows, macOS, and Linux systems. GitHub Actions were used to validate across different platforms.
-   **Warnings**: No significant warnings were found during the checks.

## Reverse dependency checks (`revdep`)

-   **Results**: No compatibility issues with other packages were found during reverse dependency checks.

## Other details

-   The package was thoroughly tested on various devices and environments, including using GitHub Actions, without any issues.
-   All examples, tests, and documentation were validated successfully to ensure compliance with CRAN policies.

## Additional comments for CRAN

The package facilitates automated extraction and processing of news articles from Chilean media outlets (initially focusing on BioBio.cl and now in Emol.com) and is specifically designed to handle Spanish-language content.

Since the package is aimed at Spanish-speaking users who work with Chilean media data, the function names, parameters, and documentation are written in Spanish. This ensures clarity and accessibility for the intended audience, as the use of the native language is essential for proper understanding and usability.

### Use of \dontrun in examples

Several functions in the package use `\dontrun{}` in their examples for the following reasons:

1. **External API dependencies**: Functions like `agregar_datos_unicos()` interact with external APIs that may not be available during CRAN checks.
2. **Database modifications**: Some functions write data to external databases, which is not appropriate for automated testing.
3. **Network requirements**: The package requires internet access to scrape news websites, which cannot be guaranteed in the CRAN check environment.

These examples are fully functional for users but are wrapped in `\dontrun{}` to prevent issues during CRAN's automated checking process.

The package was developed by:

-   Exequiel Trujillo (Maintainer, [exequiel.trujillo\@ug.uchile.cl](mailto:exequiel.trujillo@ug.uchile.cl))
-   Ismael Aguayo ([ismael.aguayo\@ug.uchile.cl](mailto:ismael.aguayo@ug.uchile.cl))
-   Klaus Lehmann ([klehmann\@fen.uchile.cl](mailto:klehmann@fen.uchile.cl))

We conducted thorough testing of the package on multiple devices and environments, including GitHub Actions, without encountering any issues. All examples, tests, and documentation were validated successfully to ensure compliance with CRAN policies.

If there are any adjustments or additional requirements needed to meet CRAN standards, please let me know. I am committed to making any necessary changes promptly.

Thank you for considering `datamedios` for CRAN. I look forward to your feedback.

## R CMD check results

Duration: 1m 57.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
