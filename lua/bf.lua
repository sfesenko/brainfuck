local function Tape()
  local pos = 1
  local data = {}

  local get = function()
    return data[pos] and data[pos] or 0
  end

  local function add(n)
    data[pos] = get() + n
  end


  local function move(n)
    pos = pos + n
  end


  local inc = function()
    add (1)
  end

  local function dec()
    add (-1)
  end

  local function forth()
    move (1)
  end

  local function back()
    move (-1)
  end

  local function put(x)
    data[pos] = x
  end

  return {
    get = get,
    inc = inc,
    dec = dec,
    forth = forth,
    back = back,
    put = put
  }
end

local function Printer(quiet)

  local function print(n)
      io.write(string.char(n))
      io.flush()
  end


  return {
    print = print,
  }
end

local function Brainfuck(text, p)

  local function inc(tape) tape.inc() end
  local function dec(tape) tape.dec() end
  local function forth(tape) tape.forth() end
  local function back(tape) tape.back() end
  local function print(tape) p.print(tape.get()) end
  local function loop(ops)
    return function (tape)
      while tape.get() > 0 do
        for j = 1, #ops do
          ops[j](tape)
        end
      end
    end
  end
  local codes = {
    ["+"] = inc,
    ["-"] = dec,
    [">"] = forth,
    ["<"] = back,
    ["."] = print,
  }
  local function parse(source, i)
    local res = {}
    while i <= source:len() do
      local c = source:sub(i, i)
      local op = codes[c]
      if op == nil then
        if c == "]" then
          break
        elseif c == "[" then
          op, i = parse(source, i+1)
          op = loop(op)
        end
      end
      res[#res + 1] = op
      i = i + 1
    end
    return res, i
  end

  local function _run(program, tape)
    for i = 1, #program do
      program[i](tape)
    end
  end

  local function run()
    local src = parse(text, 1)
    -- show(src)
    _run(src, Tape())
  end

  return {
    run = run
  }
end

(function(arg)
    local f = io.open(arg[1])
    local text = f:read("*a")
    f:close()
    local p = Printer()

    Brainfuck(text, p).run()
end)(arg)
