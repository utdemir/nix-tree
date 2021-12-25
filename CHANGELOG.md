# Changelog

## Unreleased

* feat: Support passing flake references (issue: [#27][])
  * Try `nix-tree nixpkgs#hello'
* change: `nix-tree` now requires an explicit `--derivation` flag to work on store derivations rather than their outputs.
  * This is to be more consistent `nix path-info`.

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
