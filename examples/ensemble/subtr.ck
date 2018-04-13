public class Subtr {
  float filtermult;
  float filteroffset;
  SndBuf shape;
  Gain oscgain;
  ADSR ampenv;
  ADSR filterenv;
  LPF lpf;

  1 => shape.loop;
  shape => oscgain => lpf => ampenv;

  1500.0 => filtermult;
  200.0 => filteroffset;

  // TODO: Allow to only be called once.
  public void setup(string path) {
    path => shape.read;
    spork ~ envdrive();
  }

  public void connect(UGen ugen) {
    ampenv => ugen;
  }

  public void keyOn() {
    ampenv.keyOn();
    filterenv.keyOn();
  }

  public void keyOff() {
    ampenv.keyOff();
    filterenv.keyOff();
  }

  fun void envdrive () {
    while (true) {
      filterenv.last() * filtermult + filteroffset => lpf.freq;
      5::ms => now;
    }
  }
}

/* Subtr s; */
/* s.setup(me.dir() + "AKWF_0001.wav"); */
/* s.connect(dac); */
/* 36.0 => Std.mtof => s.shape.freq; */
/* 0.5 => s.oscgain.gain; */
/* s.ampenv.set(1::ms, 350::ms, 0.2, 500::ms); */
/* s.filterenv.set(15::ms, 550::ms, 0.2, 400::ms); */
/* s.keyOn(); */
/* 100::ms => now; */
/* s.keyOff(); */
/* 1000::ms => now; */
