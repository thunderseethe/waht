{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
    buildInputs = with pkgs; [ clang emscripten llvmPackages.libclang cmake ];
    LIBCLANG_PATH="${pkgs.llvmPackages.libclang.lib}/lib";
}
