class Wavetable {
  SndBuf buffers[];
  Gain gains[100]; // TODO: Do not hard-code this.
  Gain master;

  fun void setup (SndBuf bufs[]) {
    bufs @=> buffers;
    for (0 => int i; i < buffers.size(); i++) {
      Gain gain;
      gains[i] @=> gain;
      buffers[i] => gain => master;
    }
  }

  fun float connect (UGen u) {
    master => u;
  }

  fun void setGain (float f) {
    f => master.gain;
  }

  fun void setPhase (float p) {
    for (0 => int i; i < buffers.size(); i++) {
      p => buffers[i].phase;
    }
  }

  fun void setFreq (float f) {
    for (0 => int i; i < buffers.size(); i++) {
      f => buffers[i].freq;
    }
  }

  fun float interpolate (float n) {
    n * buffers.size() => float interp;
    Math.floor(interp) $ int => int lowI;
    (lowI + 1) $ int => int highI;
    (highI - interp) * 2.0 - 1.0 => float percent;
    Math.sqrt(0.5 * (1.0 + percent)) => float lowPercent;
    Math.sqrt(0.5 * (1.0 - percent)) => float highPercent;
    for (0 => int i; i < buffers.size(); i++) {
      0.0 => gains[i].gain;
    }

    <<< interp, lowI, highI, percent, lowPercent, highPercent >>>;

    lowPercent => gains[lowI].gain;
    highPercent => gains[highI].gain;
  }
}

SndBuf buf1;
me.dir() + "AKWF_cello/AKWF_cello_0001.wav" => buf1.read;
1 => buf1.loop;
SndBuf buf2;
me.dir() + "AKWF_cello/AKWF_cello_0002.wav" => buf2.read;
1 => buf2.loop;
SndBuf buf3;
me.dir() + "AKWF_cello/AKWF_cello_0003.wav" => buf3.read;
1 => buf3.loop;
SndBuf buf4;
me.dir() + "AKWF_cello/AKWF_cello_0004.wav" => buf4.read;
1 => buf4.loop;
SndBuf buffers[];
[buf1, buf2, buf3, buf4] @=> buffers;

Wavetable wavetable;

0.0 => float i;
fun void interp () {
  while (true) {
    wavetable.interpolate((Math.sin(i * 2.0) + 1.0) * 0.5);
    i + 0.01 => i;
    5::ms => now;
  }
}

wavetable.setup(buffers);
wavetable.connect(dac);
wavetable.setGain(0.05);
wavetable.setFreq(55.0);
spork ~ interp();
4000::ms => now;
