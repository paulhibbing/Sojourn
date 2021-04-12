## Resubmission:
This is a resubmission with the following changes:

  * Fixed invalid URLs per email from Uwe Ligges.
  * Ensured conditional use of the Sojourn.Data package, per
    email from Prof Brian Ridley.
  * Made technical bug fixes (major increment) and an additional
    minor function enhancement.
    
## Test environments

* local Windows 10 install, R 4.0.4
* ubuntu 14.04.5 (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 2 NOTES

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'

  Suggests or Enhances not in mainstream repositories:
    Sojourn.Data
    
* Package suggested but not available for checking: 'Sojourn.Data'
    
The referenced package was recently archived due to issues with
LazyDataCompression. I am also the maintainer of that
package, and I am working on fixing the issue. As noted above,
this resubmission includes updates to ensure the affected package
is only referenced conditionally. Per CRAN Repository Policy, I
have also added to the description field in DESCRIPTION to
highlight an alternative means of accessing the affected
package while I am working to get it resubmitted to CRAN.
    
## Reverse dependencies

There are no reverse dependencies.
