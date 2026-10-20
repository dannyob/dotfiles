// Brief blue sheen that sweeps across the window when it gains focus.
// Tunable via `var` lines in focus-shine.pipeline.

import kitty_custom_shader_types;

static const float3 shine_color = float3(0.02f, 0.18f, 1.0f); // linear RGB
static const float strength = 0.35f;
static const float band_width = 0.12f;
static const float tilt = 0.6f; // how diagonal the sweep is

public float4
fragment_main(float4 color, KittyTextures t, KittyCustomShaderData d) {
    float p = t.animation_progress;
    float aspect = float(d.viewport_size_pixels.x) / float(d.viewport_size_pixels.y);

    // Diagonal coordinate, 0 at bottom-left and about 1 at top-right.
    float u = (t.pos.x * aspect + t.pos.y * tilt) / (aspect + tilt);

    // Band centre travels from just off the left edge to just off the right.
    float centre = -band_width * 2.0f + p * (1.0f + band_width * 4.0f);
    float x = (u - centre) / band_width;
    float band = exp(-x * x);

    // Fade in and out over the animation so it starts and ends invisible.
    float envelope = sin(p * 3.14159265f);

    // Additive, so it reads on dark backgrounds.
    return float4(color.rgb + shine_color * band * envelope * strength, color.a);
}
