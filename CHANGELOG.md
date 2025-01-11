# Changelog

## Unreleased

* feat: Show signatures of store paths (thanks @caspervk, PR: [#107])
* fix: Fix default nix-tree invocation not working when one of the default directories is not a store path (thanks @sg-qwt, @GrigorenkoPV, issue #[106], pr #[109], #[110])

[#107]: https://github.com/utdemir/nix-tree/pull/107
[#106]: https://github.com/utdemir/nix-tree/issues/106
[#109]: https://github.com/utdemir/nix-tree/pull/109
[#110]: https://github.com/utdemir/nix-tree/pull/110

## 0.5.0 - 2024-10-06:

* feat: Add `--file` to run nix-tree against attributes from a file. (thanks @blackheaven, PR: [#103])
* fix: Fix a spurious warning when running `nix-tree` as an untrusted user (thanks @Schweber & @bryango, issue [#99], PR: [#100])
* chore: Add an example for running `nix-tree` on a NixOS configuration flake reference (thanks @thenbe, PR: [#97])

[#103]: https://github.com/utdemir/nix-tree/pull/103
[#99]: https://github.com/utdemir/nix-tree/issues/99
[#100]: https://github.com/utdemir/nix-tree/pull/100
[#97]: https://github.com/utdemir/nix-tree/pull/97

## 0.4.1 - 2024-04-07:

* fix: Restore terminal state properly after exiting (thanks @Atemu, issue: [#88])

[#88]: https://github.com/utdemir/nix-tree/issues/88

## 0.4.0 - 2024-01-21:

* feat: Allow passing `--store` for alternative nix store (e.g. remote binary cache) (thanks @bryango, PR: [#79][])
* feat: Graphviz export via `--dot` flag (thanks @SomeoneSerge, issue: [#59][])

[#79]: https://github.com/utdemir/nix-tree/pull/79
[#59]: https://github.com/utdemir/nix-tree/issues/59

## 0.3.3 - 2024-01-17:

* chore: Update 'brick' & 'optparse-applicative' library to work with the newer Stackage snapshot (thanks @ncfavier, PR: [#78][]) 

[#78]: https://github.com/utdemir/nix-tree/issues/78

## 0.3.2 - 2023-11-28:

* fix: Support new path-info syntax introduced in nix 2.19 (thanks @SuperSandro2000, @GrigorenkoPV9, issue: [#67][], PR: [#68][])

[#67]: https://github.com/utdemir/nix-tree/issues/67
[#68]: https://github.com/utdemir/nix-tree/issues/68

## 0.3.1 - 2022-12-10:

* fix: Update 'brick' library (thanks @ncfavier, PR: [#47][]) 

[#47]: https://github.com/utdemir/nix-tree/issues/47

## 0.3.0 - 2022-11-26:

* feat: Improved help text.
* feat: Allow passing '--impure' flag to Nix (issue: [#40][]) 
* fix: Use -O1 instead of -O2 to save on compile times.

[#40]: https://github.com/utdemir/nix-tree/issues/40

## 0.2.1 - 2022-10-24:

* fix: Fix excessive memory use when using why-depends on large dependency graphs (issue: [#31][])

[#31]: https://github.com/utdemir/nix-tree/issues/31

## 0.2.0 - 2022-01-02:

* feat: Support passing flake references (issue: [#27][])
  * Try `nix-tree nixpkgs#hello'
* change: `nix-tree` now requires an explicit `--derivation` flag to work on store derivations rather than their outputs.
  * This is to be more consistent `nix path-info`.
* fix: Fix timeout when running external command for the 'yank' functionality (issue: [#25][])

[#27]: https://github.com/utdemir/nix-tree/issues/27

## 0.1.9 - 2021-11-08:

* fix: Automatically enable the required 'nix-command' experimental feature on Nix >= 2.4
* fix: Do not refresh screen periodically unless necessary

## 0.1.8 - 2021-09-06:

* fix: Reduce idle CPU use (issue: [#22][])
* fix: Put a timeout on yank command (issue: [#25][])
* fix: Various performance improvements

[#22]: https://github.com/utdemir/nix-tree/issues/22
[#25]: https://github.com/utdemir/nix-tree/issues/25

## 0.1.7 - 2021-03-28

* feat: Ability to yank selected store path to clipboard (shortcut: 'y')
* fix: Try to respect terminals color scheme (thanks @kanashimia, PR: [#20][])

[#20]: https://github.com/utdemir/nix-tree/issues/20

## 0.1.6 - 2021-03-12

* feat: Support non standard Nix store locations (issue: [#17][])
* feat: Horizontal scrolling on why-depends window (issue: [#18][])

[#17]: https://github.com/utdemir/nix-tree/issues/17
[#18]: https://github.com/utdemir/nix-tree/issues/18

## 0.1.5 - 2021-03-03

* feat: Add sort order toggle.
* fix: Search now does not contain false-positives.

## 0.1.4 - 2021-02-23

* feat: Make search case insensitive.
* fix: Can close the help modal now.

## 0.1.3.1 - 2021-01-07

* fix: Correctly pass --derivation flag to Nix >= 2.4

## 0.1.3.0 - 2020-12-22

* feat: Ability to work on .drv files.

## 0.1.2.0 - 2020-09-15

* feat: Add --version flag
* fix: Can search the letter 'q' without closing the search modal. (issue: [#21][])

[#21]: https://github.com/utdemir/nix-tree/issues/21

## 0.1.1.0 - 2020-08-23

* feat: Seach functionality.
