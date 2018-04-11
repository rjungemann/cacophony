BlitSaw bs1;
BlitSaw bs2;
LPF lpf;
ADSR amp;
ADSR filt;
Gain og;
Gain fg;
amp.set(1::ms, 350::ms, 0.2, 500::ms);
filt.set(15::ms, 550::ms, 0.2, 400::ms);

0.6 => og.gain;

bs1 => og;
bs2 => og;
og => amp => lpf => dac;
filt => fg => blackhole;

fun void envdrive () {
  while (true) {
    fg.last() * 1500.0 + 200.0 => lpf.freq;
    5::ms => now;
  }
}

fun void playanote (int nnum) {
  int mstodie;
  nnum + 0.07 => Std.mtof => bs1.freq;
  nnum - 0.07 => Std.mtof => bs2.freq;
  <<< "On", nnum >>>;
  amp.keyOn();
  filt.keyOn();
  Std.rand2(150, 300) => mstodie;
  mstodie * 1::ms => now;
  <<< "Off", nnum >>>;
  amp.keyOff();
  filt.keyOff();
}

spork ~envdrive();
for (int l; l < 32; l++) {
  spork ~ playanote(Std.rand2(30,60));
  800::ms => now;
}
