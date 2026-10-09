## DUT Configuration for the WardRV

ACT4 configuration (riscv-arch-test 4.1.0, Sail 0.13.1) :

- `test_config.yaml` : ACT framework configuration
- `wardrv.yaml`      : UDB configuration (I 2.1, Zicsr 2.0, Sm 1.13.0, MXLEN 32)
- `sail.json`        : Sail reference model configuration
- `rvmodel_macros.h` : DUT specific macros (halt, console)
- `link.ld`          : linker script (code at 0x4000)

`rvtest_config.h` and `rvtest_config.svh` are generated from `wardrv.yaml` by the framework.

To build the ELFs, run from `esw/compliance_act4` :

```
$ make full
```

which calls the framework with :

```
$ make CONFIG_FILES=<path>/config/wardrv/test_config.yaml EXTENSIONS=I,Zicsr
```

If EXTENSIONS is left blank, the tests of all the extensions of the UDB config are built.
