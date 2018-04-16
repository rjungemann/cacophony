public class Wavetable extends Chugen {
  SndBuf @ buffers[];
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
    (in * low.samples()) $ int => int lowSampleI;
    (in * high.samples()) $ int => int highSampleI;
    low.valueAt(lowSampleI) => float lowValue;
    high.valueAt(highSampleI) => float highValue;
    return lowValue * lowPercent + highValue * highPercent;
  }
}
