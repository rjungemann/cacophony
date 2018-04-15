public class Subtr extends Chubgraph {
  float spread;
  float filtermult;
  float filteroffset;
  SndBuf shape1;
  SndBuf shape2;
  Gain oscgain1;
  Gain oscgain2;
  ADSR ampenv;
  ADSR filterenv;
  LPF lpf;

  inlet => shape1 => oscgain1;
  inlet => shape2 => oscgain2;
  oscgain1 => lpf;
  oscgain2 => lpf;
  lpf => ampenv => outlet;

  0.07 => spread;
  1 => shape1.loop;
  1 => shape2.loop;
  0.5 => oscgain1.gain;
  0.5 => oscgain2.gain;
  1500.0 => filtermult;
  200.0 => filteroffset;

  fun void read (string s) {
    s => shape1.read;
    s => shape2.read;
  }

  fun void freq (float f) {
    f => Std.ftom => float m;
    m + spread => Std.mtof => shape1.freq;
    m - spread => Std.mtof => shape2.freq;
  }

  fun void keyOn () {
    filterenv.keyOn();
    ampenv.keyOn();
  }

  fun void keyOff () {
    filterenv.keyOff();
    ampenv.keyOff();
  }

  fun void envdrive () {
    while (true) {
      filterenv.last() * filtermult + filteroffset => lpf.freq;
      1::ms => now;
    }
  }

  spork ~ envdrive();
}
