Each example below is created by running

```sh
  racket -t srpnt/studio.rkt -- -s tmp.m4a srpnt/dev.rkt
```

and moving a different `audio` definition to the top of the `or` form.

--

I recommend listening to the examples in this order. All files are
produced by srpnt in raw 8-bit audio form and then converted to AAC
with ffmpeg.

# Basic Waveforms

The RP2A03 has four basic channels: pulse, triangle, noise, and
sample. The sample channel is not included in these examples.

The first channel is the pulse wave. It has volume control and a
duty cycle parameter for the width of the rectangle. Each of the
following examples are a C scale with different duty cycles.

* [This is a 1/8 width.](basic-pulse-1.m4a)
* [This is a 1/4 width.](basic-pulse-2.m4a)
* [This is a 1/2 width.](basic-pulse-3.m4a)
* [This is a 3/4 width](basic-pulse-4.m4a) (notice that it sounds the
same as 1/4 width)

The second is the triangle wave. It lacks volume control or parameters
other than frequency. This is an example of it playing a C scale:

* [Default](basic-triangle.m4a)

The third is a noise channel. It has volume control and a "mode"
setting that creates a metallic sounding wave.

* [Normal mode](basic-noise-normal.m4a)
* ["Metal" mode](basic-noise-metal.m4a)

# Instruments

We have a small DSL for describing different instruments. Below are
some samples of the different instruments we've implemented.

The pulse channels supports frequency and volume control, so we can
produce vibrato, tremolo, and use ADSR to build different sounding
constant frequency notes.

* [Vibrato](pulse-vibrato.m4a)
* [Tremolo](pulse-tremolo.m4a)
* [An instrument that "plucks"](pulse-adsr-plucky.m4a)
* [An instrument with a "natural" decay](pulse-adsr-natural.m4a)

The triangle channel supports frequency control only, but we can
simulate a tremolo-like effect by turning it off very quickly.

* [Vibrato](triangle-vibrato.m4a)
* [Tremolo-ish](triangle-tremoloish.m4a)
* [A "plucky"-ish instrument](triangle-adsr-plucky.m4a)

The noise channel also has volume control and a wide range of
different refresh frequencies. We use it to build a drum kit.

* [A hihat](noise-hihat.m4a)
* [A bass drum](noise-bass.m4a)
* [A snare drum](noise-snare.m4a)

# Musical Patterns

The examples above include scales for the harmonic instruments, which
are derived algorithmically through a simple model of music theory.

We have a tiny DSL for writing down drum measures, as well.

* [A straight rock beat](drums-straight-rock.m4a)
* [A "double time" beat](drums-double-time.m4a)
* [A heavy metal beat](drums-heavy-metal.m4a)

# Bithoven

The most exciting part of srpnt is the Bithoven composer. We include a
number of samples with explanations for each.

* [audio](bithoven-major180.m4a) and [sheet music](bithoven-major180.pdf)

This is composition number
159526143626800468123169814346150250253428891440038894278294580069522599233708729162444490949628638
played with NEStration 3148729328697556916. The NEStration style is
specialized to use a diatonic major scale and a tempo of 180 bpm. The
composition uses a ABABCABCAB structure and the CAFG chord
progression.

* [audio](bithoven-harmonic-minor120.m4a) and [sheet music](bithoven-harmonic-minor120.pdf)

This is composition number
49460740888706885790347319921703009205952968322810492028727289302293397538115189299366981890531865
played NEStration 22580309652954949. The style is specialized to a
harmonic minor with a tempo of 120 bpm. The composition uses a ABACABA
(symmetric rondo) structure with a CFGF chord progression.

* [audio](bithoven-natural-minor100.m4a) and [sheet music](bithoven-natural-minor100.pdf)

This is composition number
151082340486227048364359446585186219936648607538270884717246070010340145820201505988133216403205486
played with NEStration 1076478016002637264. The style uses a natural
minor with a tempo of 100 bpm. It uses a ABCBA (arch) structure with a
CAFG chord progression.

* [audio](bithoven-melodic-minor160.m4a) and [sheet music](bithoven-melodic-minor160.pdf)

This is composition number
112689913599145029760399753369564077417508307352863072009096706091575107074490822974755157820327328
played with NEStration 1947063280882832403. The style uses a melodic
minor, no drums, and a tempo of 160 bpm. It uses a ABAC structure with
a CECG chord progression.

* [audio](bithoven-all.m4a) and [sheet music](bithoven-all.pdf)

This is composition number
105370697650089816574974105282759653499238341156204192774604369084467049004939613509001127484518264
played with NEStration 13606271740551781396585046. The style is
totally unbounded. It uses a ABABCAB structure and the CECG chord
progression.

