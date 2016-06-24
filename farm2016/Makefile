all: paper.pdf paper/paper.html

paper/paper.html: paper.scrbl
	raco scribble --html --dest paper paper.scrbl

paper.pdf: paper.scrbl
	raco scribble --pdf paper.scrbl
