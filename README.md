This repositore contains a snapshot of the MAL implementation detached from proprietary interfaces.

To ue the code generator on examples/Empty.fmal to produce out/empty.cs do:
```
    nuget restore
    mkdir out
    build.bat
    run-debug.bat --compile Empty.fmal empty false
```