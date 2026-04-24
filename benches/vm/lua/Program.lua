local DEFAULT_ROUNDS = 5
local DEFAULT_ITERATIONS = 100000
local DEFAULT_WARMUP_ITERATIONS = 10000
local SMOKE_ROUNDS = 2
local SMOKE_ITERATIONS = 10000
local SMOKE_WARMUP_ITERATIONS = 1000

local sink_value = 0
local sink_object = nil

local function parse_phase(raw)
  raw = string.lower(raw)
  if raw == "hot" or raw == "cold" or raw == "both" then
    return raw
  end
  error("unknown phase: " .. raw)
end

local function parse_positive_int(raw, flag)
  local parsed = tonumber(raw)
  if parsed == nil or parsed <= 0 or parsed % 1 ~= 0 then
    error(flag .. " expects positive integer")
  end
  return parsed
end

local function parse_non_negative_int(raw, flag)
  local parsed = tonumber(raw)
  if parsed == nil or parsed < 0 or parsed % 1 ~= 0 then
    error(flag .. " expects non-negative integer")
  end
  return parsed
end

local function require_value(args, index, flag)
  local value = args[index]
  if value == nil then
    error("missing value for " .. flag)
  end
  return value
end

local function parse_settings(args)
  local settings = {
    is_smoke = false,
    rounds = DEFAULT_ROUNDS,
    iterations = DEFAULT_ITERATIONS,
    warmup_iterations = DEFAULT_WARMUP_ITERATIONS,
    phase = "both",
    workload = "all",
  }
  local index = 1
  while index <= #args do
    local current = args[index]
    local lowered = string.lower(current)
    if lowered == "--smoke" then
      settings.is_smoke = true
    elseif lowered == "--rounds" then
      index = index + 1
      settings.rounds = parse_positive_int(require_value(args, index, current), current)
    elseif lowered == "--iterations" then
      index = index + 1
      settings.iterations = parse_positive_int(require_value(args, index, current), current)
    elseif lowered == "--warmup-iterations" then
      index = index + 1
      settings.warmup_iterations = parse_non_negative_int(require_value(args, index, current), current)
    elseif lowered == "--phase" then
      index = index + 1
      settings.phase = parse_phase(require_value(args, index, current))
    elseif lowered == "--workload" then
      index = index + 1
      settings.workload = require_value(args, index, current)
    elseif lowered == "--profile" then
      index = index + 1
      local profile = require_value(args, index, current)
      if string.lower(profile) ~= "native" then
        error("Lua supports only native profile")
      end
    else
      error("unknown argument: " .. current)
    end
    index = index + 1
  end
  if settings.is_smoke then
    settings.rounds = SMOKE_ROUNDS
    settings.iterations = SMOKE_ITERATIONS
    settings.warmup_iterations = SMOKE_WARMUP_ITERATIONS
  end
  return settings
end

local function includes(settings, workload)
  return settings.workload == "all" or string.lower(settings.workload) == string.lower(workload)
end

local function includes_hot(phase)
  return phase == "hot" or phase == "both"
end

local function includes_cold(phase)
  return phase == "cold" or phase == "both"
end

local function consume(value)
  sink_value = (sink_value + value) % 2147483647
end

local function measure_hot(workload, rounds, iterations, warmup_iterations)
  for _ = 1, warmup_iterations do
    consume(workload.run())
  end

  local best_seconds = math.huge
  for _ = 1, rounds do
    local started = os.clock()
    for _ = 1, iterations do
      consume(workload.run())
    end
    local elapsed = os.clock() - started
    if elapsed < best_seconds then
      best_seconds = elapsed
    end
  end
  return best_seconds * 1000000000.0 / iterations
end

local function measure_cold(workload, iterations)
  local started = os.clock()
  for _ = 1, iterations do
    consume(workload.run())
  end
  return (os.clock() - started) * 1000000000.0 / iterations
end

local function new_small_module()
  return { base = 41, offset = 1 }
end

local shared_module = new_small_module()
local shared_grid = { { 1, 2 }, { 3, 4 } }

local function module_answer(module)
  return module.base + module.offset
end

local function init_small_module()
  return module_answer(shared_module)
end

local function construct_small_module()
  return module_answer(new_small_module())
end

local function sum(n, acc)
  if n == 0 then
    return acc
  end
  return sum(n - 1, acc + n)
end

local function scalar_recursive_sum()
  return sum(200, 0)
end

local function apply(fn, value)
  return fn(value)
end

local function make_adder(base)
  return function(value)
    return value + base
  end
end

local function closure_capture()
  return apply(make_adder(41), 1)
end

local function sequence_index_mutation()
  local grid = shared_grid
  grid[1][2] = 42
  grid[2][1] = grid[1][2] + 1
  return grid[1][2] + grid[2][1]
end

local function some(value)
  return { tag = 1, value = value }
end

local function data_match_option()
  local selected = some(41)
  if selected.tag == 1 then
    return selected.value + 1
  end
  return 0
end

local function handle_read_line(value_clause, read_line)
  local resumed = read_line(function(value)
    return value
  end)
  return value_clause(resumed)
end

local function effect_resume_equivalent()
  return handle_read_line(function(value)
    return value + 1
  end, function(_resume)
    return 41
  end)
end

local function sequence_return_alloc()
  local values = { 0, 1, 2, 3, 4, 5, 6, 7 }
  sink_object = values
  return values[8] + #values
end

local function sequence_return_forced_gc()
  local result = sequence_return_alloc()
  collectgarbage("collect")
  return result
end

local workloads = {
  { name = "init_small_module", run = init_small_module },
  { name = "scalar_recursive_sum", run = scalar_recursive_sum },
  { name = "closure_capture", run = closure_capture },
  { name = "sequence_index_mutation", run = sequence_index_mutation },
  { name = "data_match_option", run = data_match_option },
  { name = "effect_resume_equivalent", run = effect_resume_equivalent },
  { name = "sequence_return_alloc", run = sequence_return_alloc },
  { name = "sequence_return_forced_gc", run = sequence_return_forced_gc },
  { name = "construct_small_module", run = construct_small_module },
}

local settings = parse_settings(arg)
print(string.format("Lua VM baselines (%s)", settings.is_smoke and "smoke" or "full"))
print("Runtime: " .. _VERSION)
print(string.format(
  "Config: profile=native phase=%s workload=%s rounds=%d iterations=%d warmup=%d\n",
  settings.phase,
  settings.workload,
  settings.rounds,
  settings.iterations,
  settings.warmup_iterations
))

for _, workload in ipairs(workloads) do
  if includes(settings, workload.name) then
    if includes_cold(settings.phase) then
      local ns = measure_cold(workload, settings.iterations)
      print(string.format("lua/native/cold/%-36s %12.1f ns/op", workload.name, ns))
    end
    if includes_hot(settings.phase) then
      local ns = measure_hot(workload, settings.rounds, settings.iterations, settings.warmup_iterations)
      print(string.format("lua/native/hot/%-36s %12.1f ns/op", workload.name, ns))
    end
  end
end

if sink_object == false then
  print(sink_value)
end
