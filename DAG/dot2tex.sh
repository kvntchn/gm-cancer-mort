cd "~/GM/Modeling"
dot -Txdot dag.gv | dot2tex --figonly -tmath -ftikz --preproc dag.gv | dot2tex > output.tex
# sed -i.tex "s|{article}|[12pt]{standalone}|g" output.tex
cat output.tex | %{$_ -replace "{article}","[12pt]{standalone}"} > output_standalone.tex
# sed -i.tex "s|\enlargethispage{100cm}| |g" output.tex
cat output_standalone.tex | %{$_ -replace "\\enlargethispage{100cm}","%\enlargethispage{100cm}"} > output.tex
Remove-Item -path output_standalone.tex
pdflatex output.tex
