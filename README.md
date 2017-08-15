# Deprecations

[![Build Status](https://travis-ci.org/JuliaComputing/Deprecations.jl.svg?branch=master)](https://travis-ci.org/JuliaComputing/Deprecations.jl)
[![codecov.io](http://codecov.io/github/JuliaComputing/Deprecations.jl/coverage.svg?branch=master)](http://codecov.io/github/JuliaComputing/Deprecations.jl?branch=master)

This package contains a database of deprecated syntax and functions and how to automatically upgrade them. This database can be found in the src/database.jl
repository. In addition, it contains a number of utilities to aid in this task. The main user-facing entrypoints are the
`edit_text` and `edit_file` functions, which take a text snippet or a file name respectively and applies all upgrades it knows how to.
For the `edit_file` function, this is done in-place, so make sure not to use this on a file that you have not committed to version control.
Ideally, however, you should not need to use this package directly, as it does not have sufficient context to determine which julia/package versions
your file supports. Instead, consider using [FemtoCleaner](https://github.com/Keno/FemtoCleaner.jl), or an IDE with integrated support for using
the information in this package.

# Adding additional deprecations

There are currently two ways of adding deprecations to this package. However, they are both rather ad-hoc and if
you can think of a better way, please open a PR. The two ways are:

1. Manually using the CSTParser API. As an example see the `ObsoleteVersionCheck` deprecations. This kind of check
   can register itself to any CSTParser AST node and will get a callback whenever that node is encountered. This
   callback then determines whether or not the deprecation applies, and if so what the appropriate replacement is.
2. By using the AST matcher utilities in this package. Most other deprecations follow this pattern. To use it, you simply
   provide two strings: The pattern to match, and the pattern to replace. Both must be vaild, parseable julia expressions.
   At any point you may insert a `$NAME` or `$NAME...` expression, which represents a wildcard match of one (or several for `...`)
   expressions at that position in the parse tree. Similarly, using the same name in the replacement expression will splice
   the matched values in at that location in the parse tree. Appending an `!` to the name in the replacement expression,
   will cause it to ignore surrounding whitespace. Otherwise whitespace from the replacement expression is added in addition
   to any whitespace that may be matched by one of the template variables.
