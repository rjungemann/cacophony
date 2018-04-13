OscIn oin;
OscMsg msg;
13698 => oin.port;
oin.addAddress("/lead,i");

Subtr s;
s.setup(me.dir() + "AKWF_0096.wav");
s.connect(dac);
36.0 => Std.mtof => s.shape.freq;
0.5 => s.oscgain.gain;
s.ampenv.set(1::ms, 350::ms, 0.2, 500::ms);
s.filterenv.set(15::ms, 550::ms, 0.2, 400::ms);
2000 => s.filtermult;
400 => s.filteroffset;

while (true) {
  oin => now;
  while (oin.recv(msg)) {
    msg.getInt(0) => Std.mtof => s.shape.freq;
    s.keyOn();
    50::ms => now;
    s.keyOff();
  }
}
