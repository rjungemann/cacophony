class Wavetable extends Chugen {
  SndBuf buffers[];
  0.0 => float interp;

  fun float tick (float in) {
    interp * (buffers.size() - 1) => float interpolation;
    Math.floor(interpolation) $ int => int lowI;
    (lowI + 1) $ int => int highI;
    (highI - interpolation) * 2.0 - 1.0 => float swing;
    Math.sqrt(0.5 * (1.0 + swing)) => float lowPercent;
    Math.sqrt(0.5 * (1.0 - swing)) => float highPercent;
    buffers[lowI] @=> SndBuf @ low;
    buffers[highI] @=> SndBuf @ high;
    low.valueAt((in * low.samples()) $ int) => float lowValue;
    high.valueAt((in * high.samples()) $ int) => float highValue;
    return lowValue * lowPercent + highValue * highPercent;
  }
}

SndBuf buf1;
me.dir() + "AKWF_cello/AKWF_cello_0001.wav" => buf1.read;
SndBuf buf2;
me.dir() + "AKWF_cello/AKWF_cello_0002.wav" => buf2.read;
SndBuf buf3;
me.dir() + "AKWF_cello/AKWF_cello_0003.wav" => buf3.read;
SndBuf buf4;
me.dir() + "AKWF_cello/AKWF_cello_0004.wav" => buf4.read;
SndBuf buf5;
me.dir() + "AKWF_cello/AKWF_cello_0005.wav" => buf5.read;
SndBuf buf6;
me.dir() + "AKWF_cello/AKWF_cello_0006.wav" => buf6.read;
SndBuf buf7;
me.dir() + "AKWF_cello/AKWF_cello_0007.wav" => buf7.read;
SndBuf buf8;
me.dir() + "AKWF_cello/AKWF_cello_0008.wav" => buf8.read;
[buf1, buf2, buf3, buf4, buf5, buf6, buf6, buf8] @=> SndBuf buffers[];

Phasor phasor;
55.0 => phasor.freq;
Wavetable wavetable;
buffers @=> wavetable.buffers;
Gain gain;
0.1 => gain.gain;

0.0 => float n;
fun void morph () {
  while (true) {
    ((Math.sin(n) + 1.0) * 0.5) => wavetable.interp;
    n + 0.01 => n;
    5::ms => now;
  }
}

phasor => wavetable => gain => dac;
spork ~ morph();
4000::ms => now;
