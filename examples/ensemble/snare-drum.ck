OscIn oin;
OscMsg msg;
13698 => oin.port;
oin.addAddress("/snare-drum");

me.dir() + "SNARE1.aif" => string filename;
SndBuf buf;
filename => buf.read;
buf => dac;
0 => buf.loop;
0 => buf.rate;

while (true) {
  oin => now;
  while (oin.recv(msg)) {
    0 => buf.pos;
    0.5 => buf.gain;
    1.0 => buf.rate;
  }
}
