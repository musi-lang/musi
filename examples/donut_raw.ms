import { sin, cos } from "musi:math";

const (W, H) := (80, 24);
const N := W * H;

const (THETA_STEP, PHI_STEP) := (0.07, 0.02);

const (R1, R2) := (1.0, 2.0);

const K2 := 5.0;
const K1 := int_to_float(W) * K2 * 3.0 / (8.0 * (R1 + R2));

const LUM := ".,-~:;=!*#$@";

fn chr(s: String, i: Int): String => string_slice(s, i, i + 1);

fn donut(A: Float, B: Float): Unit => (
    var (buf, zbuf) := ("", "");

    var i := 0;
    while i < N loop (
        buf <- string_concat(buf, " ");
        zbuf <- string_concat(zbuf, "0");
        i <- i + 1
    );

    const (sA, cA) := (sin(A), cos(A));
    const (sB, cB) := (sin(B), cos(B));

    var t := 0.0;
    while t < 6.28 loop (
        const (sT, cT) := (sin(t), cos(t));

        var p := 0.0;
        while p < 6.28 loop (
            const (sP, cP) := (sin(p), cos(p));

            const (cx, cy) := (R2 + R1 * cT, R1 * sT);

            const x := cx * (cB * cP + sA * sB * sP) - cy * cA * sB;
            const y := cx * (sB * cP - sA * cB * sP) + cy * cA * cB;
            const z := K2 + cA * cx * sP + cy * sA;

            const ooz := 1.0 / z;

            const xp := float_to_int(int_to_float(W) / 2.0 + K1 * ooz * x);
            const yp := float_to_int(int_to_float(H) / 2.0 - K1 * ooz * y);

            const lum :=
                cP * cT * sB - cA * cT * sP - sA * sT + cB * (cA * sT - cT * sA * sP);

            if xp >= 0 and xp < W and yp >= 0 and yp < H then (
                const idx := yp * W + xp;

                const zd := float_to_int(ooz * 10.0);

                const zcur_s := chr(zbuf, idx);
                const zcur :=
                    if string_contains(zcur_s, "0") then 0
                    else if string_contains(zcur_s, "1") then 1
                    else if string_contains(zcur_s, "2") then 2
                    else if string_contains(zcur_s, "3") then 3
                    else if string_contains(zcur_s, "4") then 4
                    else if string_contains(zcur_s, "5") then 5
                    else if string_contains(zcur_s, "6") then 6
                    else if string_contains(zcur_s, "7") then 7
                    else if string_contains(zcur_s, "8") then 8
                    else 9;

                if zd > zcur then (
                    zbuf <- string_concat(
                        string_concat(string_slice(zbuf, 0, idx), int_to_string(zd)),
                        string_slice(zbuf, idx + 1, N)
                    );

                    const lidx := if lum > 0.0 then float_to_int(lum * 8.0) else 0;
                    const ch := chr(LUM, lidx);

                    buf <- string_concat(
                        string_concat(string_slice(buf, 0, idx), ch),
                        string_slice(buf, idx + 1, N)
                    )
                )
            );

            p <- p + PHI_STEP
        );

        t <- t + THETA_STEP
    );

    write("\x1b[H");

    var r := 0;
    while r < H loop (
        writeln(string_slice(buf, r * W, (r + 1) * W));
        r <- r + 1
    )
);

write("\x1b[?25l");
write("\x1b[2J");

var (A, B) := (0.0, 0.0);
while .True loop (
    donut(A, B);
    (A, B) <- (A + 0.07, B + 0.03)
);
