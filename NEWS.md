# cmdfun 1.0.2
* Vignette now directs users to cmdfun website: snystrom.github.io/cmdfun
* Vignette alteration fixes bug where forcing vignette rebuild writes & cleans
up empty dir to userspace if it doesn't exist to allow rebuild to complete
without error
* exports `cmd_list_keep_named` and `cmd_list_drop_named` which are less
abstracted than `cmd_list_keep`/`cmd_list_drop` for the simple operation of
dropping list items by name.

# cmdfun 1.0.1
* Spell check fixes

# cmdfun 1.0.0
* CRAN Release Candidate

# cmdfun 0.2.01
* fixed error in .Rbuildignore causing failing R CMD CHECK

# cmdfun 0.2.00
* Release candidate for CRAN submission

# cmdfun 0.1.92.9000

* Updated README and `vignette("cmdfun")` to have more user-friendly explanations

# cmdfun 0.1.91.9000

* Changed version numbering scheme to fix my terrible mistakes. This is what I
get for implementing a versioning scheme just before bed.

# cmdfun 0.1.10.9000

* Fixed `cmd_help_parse_flags()` so it now detects short (-) and long (--) flag names

# cmdfun 0.1.9005

* Added a `NEWS.md` file to track changes to the package.
* Renamed `cmd_help_parse_flags()` `processx` argument to `split_newline`
* Updated maintainer email
* Copied README text over to `cmdfun` vignette so pkg will have a vignette
