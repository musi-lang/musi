auto receipt_total(int latte_cents, int croissant_cents) -> int {
    return latte_cents + croissant_cents;
}

auto total = receipt_total(450, 120);
