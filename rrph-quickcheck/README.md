# Tests for the placeholder feature of HRR

This package provides test cases for the placeholder feature of HRR with QuickCheck and [dragen](https://github.com/OctopiChalmers/dragen),
to test the new placeholder feature implemented in <https://github.com/khibino/haskell-relational-record/pull/73>.

Originally copied from rr-quickcheck, the generators in this package generate more various queries thanks to dragen.

## Note for the dependent packages.

Currently, [the dragen package uploaded on Hackage](http://hackage.haskell.org/package/dragen) can't be built with some recent version of GHC due to its too strict version constraints (See <https://github.com/OctopiChalmers/dragen/issues/2>).  
This problem has already been fixed in the upstream, so specify the commit SHA on your `stack.yaml` or `project.cabal`.

Here is (the part of) an example `stack.yaml`:

```yaml
resolver: lts-13.26
extra-deps:
  - http-conduit-2.3.0
  - HDBC-sqlite3-2.3.3.1@sha256:5025fd94d02b9b3f0b8b8233796dd9a85a0b3dda6503c6e671e3eddbc51cb4d4
  - QuickCheck-GenT-0.2.0@sha256:d11b7b4a3f7648895eff66376a00f763821ee955ce42a37fb918213a8eae88d6
  - git: https://github.com/OctopiChalmers/dragen.git
    commit: 78122dc8d54916b97b64a4a7efbf745ab71930b6

# ...
```
