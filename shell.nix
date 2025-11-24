{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    # PDF tools
    poppler_utils  # pdftotext, pdftoppm
    
    # Typst for document creation
    typst
    
    # Haskell for question 6
    ghc
    haskellPackages.cabal-install
    
    # Utilities
    imagemagick
  ];
  
  shellHook = ''
    echo "╔════════════════════════════════════════════════╗"
    echo "║  Ambiente Lógica - Pronto para usar!          ║"
    echo "╚════════════════════════════════════════════════╝"
    echo ""
    echo "Ferramentas disponíveis:"
    echo "  • pdftotext, pdftoppm (manipulação de PDFs)"
    echo "  • typst (geração de documentos)"
    echo "  • ghc, cabal (Haskell)"
    echo ""
    echo "Diretórios do projeto: Atividade_01, Atividade_02"
    echo ""
  '';
}
