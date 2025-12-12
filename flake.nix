{
  description = "Dev shell with .NET 9 and dependencies";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs = { self, nixpkgs }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };

    dotnetPkg =
      with pkgs.dotnetCorePackages;
      combinePackages [ sdk_10_0 ];

    deps = with pkgs; [
      zlib
      zlib.dev
      openssl
      dotnetPkg
      fontconfig
    ];

    libPath = pkgs.lib.makeLibraryPath (
      [
        pkgs.stdenv.cc.cc
        pkgs.fontconfig
        pkgs.libxrandr
        pkgs.xorg.libICE
        pkgs.xorg.libX11
        pkgs.xorg.libSM
      ] ++ deps
    );

  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = [
        pkgs.git
      ] ++ deps;

      LD_LIBRARY_PATH = libPath;
      NIX_LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath ([ pkgs.stdenv.cc.cc ] ++ deps);
      NIX_LD = "${pkgs.stdenv.cc.libc_bin}/bin/ld.so";

      shellHook = ''
        echo $SHELL
        export DOTNET_ROOT="${dotnetPkg}"
        export DOTNET_CLI_TELEMETRY_OPTOUT=1
      '';
    };
  };
}
