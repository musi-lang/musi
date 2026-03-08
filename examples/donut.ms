// donut.ms -- Spinning ASCII donut in Musi
// Based on the famous donut.c by Andy Sloane
// https://www.a1k0n.net/2011/07/20/donut-math.html

import { sin, cos } from "musi:math";
import { rune_to_string } from "musi:string";

const SCREEN_WIDTH: Int := 40;
const SCREEN_HEIGHT: Int := 22;
const BUFFER_SIZE: Int := SCREEN_WIDTH * SCREEN_HEIGHT;

// Precomputed constants
const THETA_SPACING: Float := 0.07;
const PHI_SPACING: Float := 0.02;
const R1: Float := 1.0;
const R2: Float := 2.0;
const K2: Float := 5.0;
const K1: Float := int_to_float(SCREEN_WIDTH) * K2 * 3.0 / (8.0 * (R1 + R2));

// Luminance characters
const LUMA_CHARS: String := ".,-~:;=!*#$@";

fn char_at(s: String, i: Int): String => string_slice(s, i, i + 1);

fn render_frame(a: Float, b: Float): Unit => (
    // Use a string as output buffer (space-filled)
    var output: String := "";
    var i := 0;
    while i < BUFFER_SIZE loop (
        output <- string_concat(output, " ");
        i <- i + 1
    );

    // Z-buffer as parallel string of digits (0-9)
    var zbuf: String := "";
    i <- 0;
    while i < BUFFER_SIZE loop (
        zbuf <- string_concat(zbuf, "0");
        i <- i + 1
    );

    const sin_a := sin(a);
    const cos_a := cos(a);
    const sin_b := sin(b);
    const cos_b := cos(b);

    var theta: Float := 0.0;
    while theta < 6.28 loop (
        const sin_theta := sin(theta);
        const cos_theta := cos(theta);

        var phi: Float := 0.0;
        while phi < 6.28 loop (
            const sin_phi := sin(phi);
            const cos_phi := cos(phi);

            const circle_x := R2 + R1 * cos_theta;
            const circle_y := R1 * sin_theta;

            const x := circle_x * (cos_b * cos_phi + sin_a * sin_b * sin_phi) - circle_y * cos_a * sin_b;
            const y := circle_x * (sin_b * cos_phi - sin_a * cos_b * sin_phi) + circle_y * cos_a * cos_b;
            const z := K2 + cos_a * circle_x * sin_phi + circle_y * sin_a;
            const ooz := 1.0 / z;

            const xp := float_to_int(int_to_float(SCREEN_WIDTH) / 2.0 + K1 * ooz * x);
            const yp := float_to_int(int_to_float(SCREEN_HEIGHT) / 2.0 - K1 * ooz * y / 2.0);

            const l := cos_phi * cos_theta * sin_b - cos_a * cos_theta * sin_phi - sin_a * sin_theta + cos_b * (cos_a * sin_theta - cos_theta * sin_a * sin_phi);

            if xp >= 0 and xp < SCREEN_WIDTH and yp >= 0 and yp < SCREEN_HEIGHT then (
                const idx := yp * SCREEN_WIDTH + xp;
                const z_digit := float_to_int(ooz * 10.0) % 10;

                // Get current z value
                const cur_z_str := char_at(zbuf, idx);
                const cur_z := if string_contains(cur_z_str, "0") then 0
                    else if string_contains(cur_z_str, "1") then 1
                    else if string_contains(cur_z_str, "2") then 2
                    else if string_contains(cur_z_str, "3") then 3
                    else if string_contains(cur_z_str, "4") then 4
                    else if string_contains(cur_z_str, "5") then 5
                    else if string_contains(cur_z_str, "6") then 6
                    else if string_contains(cur_z_str, "7") then 7
                    else if string_contains(cur_z_str, "8") then 8
                    else 9;

                if z_digit > cur_z then (
                    // Update z-buffer (replace char at idx)
                    const z_char := int_to_string(z_digit);
                    zbuf <- string_concat(
                        string_concat(string_slice(zbuf, 0, idx), z_char),
                        string_slice(zbuf, idx + 1, BUFFER_SIZE)
                    );

                    // Get luminance character
                    const luma_idx := if l > 0.0 then float_to_int(l * 8.0) % 12 else 0;
                    const ch := char_at(LUMA_CHARS, luma_idx);

                    // Update output buffer
                    output <- string_concat(
                        string_concat(string_slice(output, 0, idx), ch),
                        string_slice(output, idx + 1, BUFFER_SIZE)
                    )
                ) else ()
            ) else ();

            phi <- phi + PHI_SPACING
        );
        theta <- theta + THETA_SPACING
    );

    // Clear screen and move cursor home
    write("\x1b[H");

    // Print frame line by line
    var row := 0;
    while row < SCREEN_HEIGHT loop (
        writeln(string_slice(output, row * SCREEN_WIDTH, (row + 1) * SCREEN_WIDTH));
        row <- row + 1
    )
);

// Main animation loop
write("\x1b[?25l");  // Hide cursor
write("\x1b[2J");    // Clear screen

var a: Float := 0.0;
var b: Float := 0.0;

while .True loop (
    render_frame(a, b);
    a <- a + 0.07;
    b <- b + 0.03
);
