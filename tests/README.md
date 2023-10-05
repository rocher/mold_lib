# Testing

### Profiling with valgrind

This is a basic process:

```sh
   # prepare the environment
   $ git clean -dfx suite/
   $ alr clean

   # build the project
   $ alr build --validation
   $ git clean -dfx suite/

   #
   $ valgrind --tool=callgrind ./bin/mold_lib_tests
   $ kcachegrind callgrind.out.*
```

### Code coverage with `gnatcov`

To have a code coverage of the `mold-lib` library when running the unis tests
in this crate, it is necessary to exclude the project `mold_lib_tests` from
the *projects of interests*, as well as other dependencies, which is done
with:

```sh
   # prepare the environment
   git clean -dfx suite/
   alr clean

   # instrument the code and build the project
   alr gnatcov instrument --level=stmt --dump-trigger=atexit --projects mold_lib --no-subprojects
   alr build --development -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full

   # run the unit tests
   ./bin/mold_lib_tests > test.log 2>&1

   # summarize gnatcov information
   alr gnatcov coverage --annotate=xcov --output-dir gnatcov_out --level=stmt --projects mold_lib --no-subprojects *srctrace
```

Remember that:

  1. the parameters `-P mold_lib_tests` are automatically send by `alr
     gnatcov`.
  2. the github actions upload the results of gnatcov to
     [codecov.io](https://codecov.io/rocher/mold_lib)
