## Nix Setup

1. If you haven't already, [install Nix](https://nixos.org/download/)
   * On POSIX systems, this is usually `curl -L https://nixos.org/nix/install | sh`
2. Run `nix flake show --allow-import-from-derivation` to verify that the flake can be read correctly by nix.
3. Build libraries: `nix build`

If you have trouble, refer to the [Nix Reference Manual](https://hydra.nixos.org/build/275163694/download/1/manual/introduction.html) for additional detail.

## Haskell Setup

Because this project uses nix, there's no need to install Stack or Cabal on your system, you can run it in a shell.

1. Init a shell
   * Via nix-shell: `nix-shell`
   * Via nix command:

        `nix develop -c {your shell interpreter}`
2. Build libraries: `cabal build`
If you have trouble, refer to the [Reflex-VTY GitHub Repo](https://github.com/reflex-frp/reflex-vty) for additional detail.

## Run

* Via Cabal command

    In the shell of the project, we start the **Reflex-VTY** app with: `cabal run`
* Via Nix command
    1. Check the **Reflex-VTY** flake with:

        `nix flake show allow-import-from-derivation`

    2. Build the **Reflex-VTY** app with: `nix build`

    3. Run the **Reflex-VTY** app with: `nix run`

To close you can press `Esc` or `Ctrl` keys.

## Credits and Licenses

* This project includes code derived from [Reflex-FRP/Reflex-VTY](https://github.com/reflex-frp/reflex-vty), licensed under the [BSD-3-Clause License](https://github.com/reflex-frp/reflex-vty/blob/develop/LICENSE)  
* Additional modifications and original code in this repository are licensed under the [BSD-3-Clause License](./LICENSE).
