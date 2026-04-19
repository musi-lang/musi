let deliveryFee (distanceKm : Int) : Int := match distanceKm (
| 0 => 0
| _ => 45
);

deliveryFee(3);
