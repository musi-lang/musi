enum TrafficLight { Red, Yellow, Green }

static bool CanWalk(TrafficLight light) =>
    light is TrafficLight.Green;
