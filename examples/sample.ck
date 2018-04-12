// Load buffer
me.dir() + "SNARE1.aif" => string filename;
SndBuf buf => dac;
0 => buf.loop;
0 => buf.rate;

// Play buffer
0 => buf.pos;
0.5 => buf.gain;
1.0 => buf.rate;
1000::ms => now;
