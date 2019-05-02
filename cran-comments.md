## Suggests Sojourn.Data

This package's key functions require the use of large
    objects (e.g. neural networks) that have been stored
    in a separate package, per the CRAN policy regarding
    "consideration [of] a separate data-only package which
    can be updated only rarely." The reviewer should be
    advised that the separate package (i.e., Sojourn.Data)
    has been submitted to CRAN at the same time as this package.

## Test environments

* local Windows 10 install, R 3.5.0
* ubuntu 14.04.5 (on travis-ci), R 3.5.3
* win-builder (devel and release)

## R CMD check results

0 ERRORs | 0 WARNINGs | 4 NOTEs

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Paul R. Hibbing <paulhibbing@gmail.com>'

  New submission

  Possibly mis-spelled words in DESCRIPTION:
    AGread (23:5)
    Accelerometer (3:55)
    ActiGraph (3:45, 23:17)
    Ellingson (18:5, 19:57)
    Freedson (17:23)
    GJ (18:47, 20:22)
    Hibbing (19:45)
    IJ (18:31)
    Keadle (16:55)
    LD (18:15, 19:67)
    Lyden (16:46)
    Schwabacher (18:19)
    Staudenmayer (17:5)
    Welk (18:42, 20:17)
    accelerometer (16:5)
    activPAL (23:62)
    activpalProcessing (23:38)

This is the first submission of the Sojourn package. The possibly mis-spelled
    words are package names, sensor names, or author names.
    
** running examples for arch 'i386' ... [36s] NOTE
    Examples with CPU or elapsed time > 10s
                         user system elapsed
    apply_youth_sojourn 20.54   0.74   22.44

This example is wrapped in \donttest{}

** running examples for arch 'x64' ... [37s] NOTE
    Examples with CPU or elapsed time > 10s
                         user system elapsed
    apply_youth_sojourn 23.35   0.76   24.65

This example is wrapped in \donttest{}

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
