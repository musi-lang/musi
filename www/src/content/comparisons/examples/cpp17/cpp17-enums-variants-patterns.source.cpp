enum class TrafficLight { Red, Yellow, Green };

auto can_walk(TrafficLight light) -> bool {
    return light == TrafficLight::Green;
}
