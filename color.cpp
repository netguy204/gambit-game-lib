#include "color.h"

extern "C" {
#include "spectra.h"
}

#include <math.h>

#define TEMP_ENTRIES 25
static const float base_temp = 0;
static const float step = 500;
static const float max_temp = base_temp + step * (TEMP_ENTRIES-1);
static float temp_table[TEMP_ENTRIES][3];

static float lerp(float t, float a, float b) {
  return a + t * (b - a);
}

void color_for_temp(float temp, float* color) {
  if(temp < base_temp || temp > max_temp) {
    color[0] = 1.0;
    color[1] = 0.0;
    color[2] = 1.0;
    return;
  }

  int lower_idx = (int)floorf((temp - base_temp) / step);
  int upper_idx = (int)ceilf((temp - base_temp) / step);
  if(lower_idx == upper_idx) {
    color[0] = temp_table[lower_idx][0];
    color[1] = temp_table[lower_idx][1];
    color[2] = temp_table[lower_idx][2];
    return;
  }

  float t = (temp - (base_temp + lower_idx * step)) / step;
  color[0] = lerp(t, temp_table[lower_idx][0], temp_table[upper_idx][0]);
  color[1] = lerp(t, temp_table[lower_idx][1], temp_table[upper_idx][1]);
  color[2] = lerp(t, temp_table[lower_idx][2], temp_table[upper_idx][2]);
}


void color_init() {
  double x, y, z, r, g, b;
  struct colourSystem* cs = &SMPTEsystem;

  for(int ii = 0; ii < TEMP_ENTRIES; ++ii) {
    double temp = base_temp + step * ii;
    bbTemp = temp;
    spectrum_to_xyz(bb_spectrum, &x, &y, &z);
    xyz_to_rgb(cs, x, y, z, &r, &g, &b);
    constrain_rgb(&r, &g, &b);
    norm_rgb(&r, &g, &b);

    temp_table[ii][0] = r;
    temp_table[ii][1] = g;
    temp_table[ii][2] = b;
  }
}
