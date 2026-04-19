enum TrafficLight {
    Red,
    Yellow,
    Green,
}

fn can_walk(light: TrafficLight) -> bool {
    matches!(light, TrafficLight::Green)
}
