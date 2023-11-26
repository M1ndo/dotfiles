$pdflatex = 'lualatex %O %S; lualatex -shell-escape -interaction=nonstopmode %S; (exit 0)';
$cleanup_mode = 1;
