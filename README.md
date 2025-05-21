[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/mold_lib.json)](https://alire.ada.dev/crates/mold_lib.html)
[![Alire CI/CD](https://img.shields.io/endpoint?url=https://alire-crate-ci.ada.dev/badges/mold_lib.json)](https://alire-crate-ci.ada.dev/crates/mold_lib.html)
![unit-test](https://github.com/rocher/mold_lib/actions/workflows/unit-test.yml/badge.svg)
[![codecov](https://codecov.io/gh/rocher/mold_lib/branch/main/graph/badge.svg?token=LB83SI4I0Y)](https://codecov.io/gh/rocher/mold_lib/tree/main)
[![GitHub release](https://img.shields.io/github/release/rocher/mold_lib.svg)](https://github.com/rocher/mold_lib/releases/latest)
[![License](https://img.shields.io/github/license/rocher/mold_lib.svg?color=blue)](https://github.com/rocher/mold_lib/blob/master/LICENSE)

<img src="Ada_Mold_Lib.png" width="222" />

## Welcome to **Mold**

> **MOLD**: *Meta-variable Operations for Lean Development*

Mold is a Template Processor, or Template Engine, to work with repository
templates. The main motivation of Mold is to have repositories in GitHub used
as template repositories to create new, customized repositories for other
users.

Main features supported in Mold include

  * variable replacement in mold files (*.mold)

  * for a given directory, variable replacement recursively for all mold files
    in all subdirectories

  * variable replacement in file names

  * inclusion of other templates

  * definition of variables with a simple TOML file

  * predefined and customized text filters to easily apply text
    transformations

Variable replacement can be specified as *normal*, *optional* or *mandatory*.
For example, the variable `foo = "bar"` can be specified with `{{foo}}`,
`{{?foo}}` (optional) or `{{#foo}}` (mandatory). The difference is the handling
of errors when an undefined variable is encountered.

All mold files must end with the extension `.mold`, for example
`README.md.mold`. Destination files (with variables replaced) have the same
name without the mold extension: `README.md`. This simplifies the work done in
large subdirectories with few templates.

Text filters can be used to apply text transformations, for example
`{{foo/W5}}` truncates the contents of the variable `foo` to a maximum width
of 5 characters.

This crate contains the Ada library and unit tests. For a CLI tool, please
take a look at the crate `mold`.

## Reference Guide

Please visit [Mold documentation](https://rocher.github.io/mold) for more
information.

---
## License
MIT (c) 2023-2025 Francesc Rocher
