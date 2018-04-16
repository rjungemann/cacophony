SndBuf @ buffers[100];
for (0 => int i; i < 100; i++) {
  SndBuf buf;
  Std.itoa(i + 1) => string s;
  4 - s.length() => int l;
  for (0 => int j; j < l; j++) {
    "0" + s => s;
  }
  me.dir() + "AKWF_0001/AKWF_" + s + ".wav" => buf.read;
  buf @=> buffers[i];
}

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
    n + 0.00025 => n;
    0.5::ms => now;
  }
}

phasor => wavetable => gain => dac;
spork ~ morph();
8000::ms => now;
