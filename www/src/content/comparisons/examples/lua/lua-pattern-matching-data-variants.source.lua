local TrafficLight = { red = "red", yellow = "yellow", green = "green" }

local function can_walk(light)
  return light == TrafficLight.green
end
