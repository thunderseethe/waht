{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    buildInputs = with pkgs; [ clang emscripten llvmPackages.libclang cmake ];
    LIBCLANG_PATH="${pkgs.llvmPackages.libclang.lib}/lib";
    shellHook = ''
      export PATH="~/rs/rust-analyzer/target/release:$PATH"
    '';
}
