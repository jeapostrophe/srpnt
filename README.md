srpnt - synthesized, random pulse, noise, and triangle (wip)

--

To try it out...

racket -t srpnt/studio.rkt -- srpnt/dev.rkt

Then open up srpnt/dev.rkt and change the "audio" binding to
experiment with different things. You can also make new instruments in
srpnt/nestration/instruments.rkt and test them out in dev.rkt. You
probably will also want to look at srpnt/nestration.rkt for the
definition of "styles".

If you want to see some saved examples, check out
[Examples](examples/README.md).

--

Samples are expected to be 1-channel, unsigned 8-byte PCM data. You
can create that format from an arbitrary input with ffmpeg:

ffmpeg -i INPUT -map_channel 0.0.0 -f u8 OUTPUT_LEFT.raw
ffmpeg -i INPUT -map_channel 0.0.1 -f u8 OUTPUT_RIGHT.raw
OR
ffmpeg -i INPUT -ac 1 -f u8 OUTPUT.raw

--

If you want to turn srpnt's raw output back into something you can
plan with a normal player, ffmpeg can do that too:

FILE=song ; ffmpeg -f u8 -i ${FILE}.l.raw -f u8 -i ${FILE}.r.raw -filter_complex "[0:a][1:a]amerge[aout]" -map "[aout]" ${FILE}.wav

--

TODO:

* merge files
      96     334    3191 ./srpnt/dev.rkt
      96     334    3191 ./srpnt/gui.rkt
      27      57     531 ./srpnt/player.rkt
      98     319    3298 ./srpnt/studio-x.rkt
      43     107    1303 ./srpnt/studio.rkt
* implement new model