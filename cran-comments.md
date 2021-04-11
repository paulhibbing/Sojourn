## Test environments

* local Windows 10 install, R 4.0.4
* ubuntu 14.04.5 (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'

  Suggests or Enhances not in mainstream repositories:
    Sojourn.Data
    
The referenced package (Sojourn.Data) was recently archived
due to issues with LazyDataCompression. I am working on
fixing that. For this package (Sojourn) I have fixed the
code to use Sojourn.Data conditionally.
    
## Reverse dependencies

There are no reverse dependencies.
