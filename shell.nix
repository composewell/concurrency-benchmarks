# nix-shell --argstr c2nix "--benchmark --flag fusion-plugin"
{ pkgsCommit ? "5272327b81ed355bbed5659b8d303cf2979b6953"
, streamlyCommit ? "03cf686ca7e3fe1093b064c9763de8b684cce0f4"
, compiler ? "default", c2nix ? "--benchmark" }:
let

  nixpkgs = let
    src = builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs.git";
      ref = pkgsCommit;
    };
  in import src { };

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default" then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

  mkPkgGit = super: gitSrc: ref: name:
    let
      src = builtins.fetchGit {
        url = gitSrc;
        ref = ref;
      };
    in super.callCabal2nix name src { };

  overiddenPackages = haskellPackages.override {
    overrides = _: super: {
      # fusion-plugin-types is required as a few package sets don't have it.
      fusion-plugin-types =
        super.callHackageDirect
          { pkg = "fusion-plugin-types";
            ver = "0.1.0";
            sha256 = "17211b80p4zqisghs2j8flm4dj788f9glx2id6nh8f223q4cigc9";
          } {};
      streamly = let
        src = "https://github.com/composewell/streamly.git";
        ref = streamlyCommit;
      in mkPkgGit super src ref "streamly";
    };
  };

  drv = let
    orig =
      overiddenPackages.callCabal2nixWithOptions "concurrency-benchmarks" ./.
      c2nix { };
  in orig.overrideAttrs (oldAttrs: { src = null; });

in drv.env
