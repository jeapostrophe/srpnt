%.wav: %.bin
	ffmpeg -y -f s8 -ar 44100 -ac 2 -i $^ -ac 1 $@

%.m4a: %.wav
	ffmpeg -y -i $^ -c:a libfdk_aac -b:a 64k $@
