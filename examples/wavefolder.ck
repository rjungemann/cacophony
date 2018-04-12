class Wavefolder extends Chugen {
  0.1 => float threshold;
  fun float tick(float in) {
    if (in > threshold)
      threshold - (in - threshold) => in;
    else if (in < -threshold)
      -threshold + ( -threshold - in) => in;
    return in;
  }
}

Wavefolder folder;
adc => folder => dac;
2000::ms => now;
