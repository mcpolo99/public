-- pathlib.lua (Cheat Engine Autorun)
-- Windows pathlib-like helper for Cheat Engine Lua
local GLOBAL_NAME = "Path"

local Path = { } -- having stuff here breaks the lua script for some reason..

Path.version = "pathlib 2.4.7(CE)" 
Path.debug=false --Set for debug help during run of path
local about = {
    author ="https://github.com/mcpolo99",
    link = "https://github.com/mcpolo99/public/tree/main/lua",
    purpose = "used primarly for CE tables"
}

local eqcount1 = 21

-- expose globally
_G[GLOBAL_NAME] = Path

pcall (function()
  -- Enable access to blocked metatables.
  -- Don't worry, this module doesn't change anything in them.
  local debmeta = require "debug".getmetatable
  if debmeta then getmetatable = debmeta end
end)

---- Have this at the top so all functions know what to call 
local function log( ...)
    if Path.debug == false then return end
    local args = {...}
    for i = 1, #args do
        if args[i] == nil then
            args[i] = "nil"
        else
            args[i] = tostring(args[i])
        end
    end
    local msg = table.concat(args, " ")
    print(string.format("[%s] %s","LOG", msg))
end

function Path.about()
    log("local: about")
    print("[",Path.version,"]")
    for k, v in pairs(about) do
        print(("  %-8s : %s"):format(k, v))
    end
end


--------------------------------------------------
-- helpers
--------------------------------------------------
-- Helper: auto-convert string to Path
local function assertself(p)
    log("local: assertself")
    if getmetatable(p) == Path then
        return p
    elseif type(p) == "string" then
        return Path(p)
    else
        error("Expected Path or string, got " .. type(p))
    end
end
-- Wrapper to auto-assert self
local function wrap_method(fn)
    log("wrap_method")
    return function(self, ...)
        -- if dot-call: first argument is a string or nil, not a Path object
        if getmetatable(self) ~= Path then
            -- treat self as the first real argument, and nil as self
            return fn(assertself(self), ...)
        else
            -- normal colon-call
            self = assertself(self)
            return fn(self, ...)
        end
    end
end
local function trim_trailing_sep(p)
    log("local: trim_trailing_sep")
    return p:gsub("\\+$", "")
end

local function normalize(p)
    log("local: normalize")
    p = tostring(p) -- convert to string.
    p = p:gsub("/+$", "\\") -- Replace: Trailing forwardslashes
    p = p:gsub("/+", "\\") -- Replace: forwardslashes along the string
    p = p:gsub("^/+", "") --Remove: Leading forwardslashes
    p = p:gsub("\\+", "\\") --Remove: duplicates of '\' 
    p = p:gsub("\\+$", "") --Remove: Trailing backslashes
    p = p:gsub("^\\+", "") --Remove: Leading backslashes
    return p
end


local function tableToString(t, indent)
    log("local: tableToString")
    indent = indent or ""
    local s = "{\n"
    for k, v in pairs(t) do
        local key = tostring(k)
        local value
        if type(v) == "table" then
            value = tableToString(v, indent .. "  ")
        else
            value = tostring(v)
        end
        s = s .. indent .. "  " .. key .. " = " .. value .. "\n"
    end
    s = s .. indent .. "}"
    return s
end



--------------------------------------------------
-- metamethods
--[`View online doc`](https://www.lua.org/manual/5.4/manual.html#2.4)
--------------------------------------------------

function Path:__tostring()
    log(":__tostring")
    return self.path
end

--the the addition (+) operation
function Path:__add(other)
    log(":__add")
    return
end
-- the subtraction (-) operation
function Path:__sub(other)
    log(":__sub")
    return
end
-- the multiplication (*) operation.
function Path:__mul(other)
    log(":__mul")
    return
end

-- __concat: the concatenation (..) operation. 
function Path:__concat(other)
    log(":__concat")
    --could be used as join?? like joining multiple path or path with string.
    return
end

-- the length (#) operation
function Path:__len(other)
    log(":__len")
    return #self.path
end

--the equal (==) operation
--the equal (~=) operation
function Path:__eq(other)
    log(":__eq")
    return
end
--__lt: the less than (<) operation
function Path:__lt(other)
    log(":__lt")
    return
end
--__le: the less equal (<=) operation.
--__le: the less equal (>=) operation.
function Path:__le(other)
    log(":__le")
    return
end


--The call operation func(args)
function Path:__call(other)
    log(":__call")
    return self.path
end



-- implement Path / "subfolder" syntax

-- Path(file_path):parent() / version / ".lua"

Path.__div = function(self, other)
    log(":__div")
    -- if left-hand side is a string, convert to Path
    if type(self) == "string" then
        self = Path(self)
    end

    -- if right-hand side is a string, normalize automatically
    return self:join(other)
end


--------------------------------------------------
-- core operations
--------------------------------------------------

-- Path(file_path):parent() / version / ".lua"
-- allows joining ".lua" not as a path but as a suffix
function Path:join(other)
    log(":join")

    local base = self.path
    local part = normalize(other or "")

    -- If joining a suffix (".lua"), append to filename instead
    if part:match("^%.%w+$") then
        return self:with_suffix(part)
    end

    part = part:gsub("^\\+", "")
    return Path(base .. "\\" .. part)
end


-- TODO: get full absolute path
function Path:resolve()
    log(":resolve")
    local abs = self.path
    local f = io.popen('cd /d "' .. abs .. '" 2>nul && cd')
    if f then
        abs = f:read("*l") or abs
        f:close()
    end
    return Path(abs)
end
--------------------------------------------------
-- filesystem queries
--------------------------------------------------

function Path:exists()
    log(":exists")
    local f = io.open(self.path, "r") --read
    if f then
        f:close();
        return true
    end
    return false
end

--TODO: Add assert ?
function Path:is_file()
    log(":is_file")
    local f = io.open(self.path, "rb") --readbyte
    if f then
        f:close();
        return true
    end
    return false
end

function Path:is_dir()
    log(":is_dir")

    -- must exist
    local f = io.open(self.path, "r")
    if f then
        f:close()
        return false -- it's a file
    end

    -- directory check
    local d = io.open(self.path .. "\\*", "r")
    if d then
        d:close()
        return true
    end

    return false
end


--------------------------------------------------
-- read / write
--------------------------------------------------

function Path:read_file()
    log(":read_file")
    local f = assert(io.open(self.path, "r"), "Cannot read file")
    local data = f:read("*a")
    f:close()
    return data
end

function Path:write_file(text)
    log(":write_file")
    local f = assert(io.open(self.path, "w"), "Cannot write file")
    f:write(text)
    f:close()
    return -- self.path
end

-- Static helper: write text to a destination path
function Path.write_file_dst(text, dst)
    log(":write_file_dst")
    assert(type(text) == "string", "text must be a string")

    -- allow string or Path
    if type(dst) == "string" then
        dst = Path(dst)
    end

    assert(getmetatable(dst) == Path, "dst must be a Path or string")

    -- ensure parent directory exists
    local parent = dst:parent()
    if parent and not parent:is_dir() then
        parent:mkdir(true)
    end

    local f = assert(io.open(dst.path, "w"), "Cannot write file: " .. dst.path)
    f:write(text)
    f:close()

    return dst
end

--------------------------------------------------
-- directory operations
--------------------------------------------------
-- TODO: support for different arch
function Path:mkdir(parents)
    log(":mkdir")
    if parents then
        os.execute('mkdir "' .. self.path .. '" >nul 2>nul')
    else
        os.execute('mkdir "' .. self.path .. '"')
    end
    return self
end

function Path:iterdir()
    log(":iterdir")
    assert(self:is_dir(), "Not a directory")
    local items = {}
    local p = self.path

    local cmd = io.popen('dir /b "' .. p .. '"')
    for line in cmd:lines() do
        items[#items + 1] = Path(p .. "\\" .. line)
    end
    cmd:close()
    return items
end

--------------------------------------------------
-- pathlib-like properties
--------------------------------------------------
function Path:filename()
    log(":filename")
    if self:is_dir() then return nil end
    return self.path:match("([^\\]+)$")
end

function Path:filenamext()
    log(":filenemext")
    local name = self:filename()
    if name then
        return name:gsub("(%..+)$", "")
    end
end
function Path:filepath()
    log(":filepath")
    return self.path
end

function Path:parent()
    log(":parent")
    return Path(self.path:match("^(.*)\\[^\\]+$") or self.path)
end

function Path:suffix() -- did not working
    log(":suffix")
    return self.path:match("(%.%w+)$")
end

function Path:stem()
    log(":stem")
    local n = self:filename()
    return n and n:gsub("%.%w+$", "")
end

function Path:with_suffix(new_suffix)
    log(":with_suffix")
    assert(new_suffix:sub(1, 1) == ".", "Suffix must start with '.'")
    return self:parent() / (self:stem() .. new_suffix)
end
--- print(p2:replace_suffix([[.testa]]).path)
function Path:replace_suffix(new_suffix)
    log(":replace_suffix")
    assert(type(new_suffix) == "string", "suffix must be a string")
    assert(new_suffix == "" or new_suffix:sub(1, 1) == ".",
           "suffix must start with '.' or be empty")

    local parent = self:parent()
    local stem = self:stem() or self:filename()

    assert(stem, "cannot replace suffix on directory")

    return parent / (stem .. new_suffix)
end

function Path:parts()
    log(":parts")
    local t = {}
    for part in self.path:gmatch("[^\\]+") do
        t[#t + 1] = part
    end
    return t
end

--------------------------------------------------
-- glob (basic)
--------------------------------------------------

function Path:glob(pattern)
    log(":glob")
    assert(self:is_dir(), "Not a directory")
    local results = {}
    local p = trim_trailing_sep(self.path)

    local cmd = io.popen('dir /b "' .. p .. '\\' .. pattern .. '"')
    for line in cmd:lines() do
        results[#results + 1] = Path(p .. "\\" .. line)
    end
    cmd:close()

    return results
end

--------------------------------------------------
-- debug info
--------------------------------------------------
-- if i want to use p.info() i need to do like:
-- p = Path("whatever path")
-- print(p.info(p))
function Path:info()
    log(":info")

    local info =  {
        path = self.path or "",
        parent = tostring(self:parent()),
        exists = tostring(self:exists()),
        is_file =tostring(self:is_file()),
        is_dir = tostring(self:is_dir()),
        filename = tostring(self:filename()),
        stem = tostring(self:stem()),
        suffix = tostring(self:suffix())
    }
    print(tableToString(info))
    return info
end

local function isNumArray(t)
    return type(t) == "table" and t[1] ~= nil
end
local function getItemsCount(t)
    if type(t) ~= "table" then return 0 end

    -- array-style
    if isNumArray(t) then
        return #t
    else
        -- map-style
        local count = 0
        for _ in pairs(t) do
            count = count + 1
        end
        return count
    end
end

-- Converts a Lua script file into a flat string
local eqcount = eqcount1
local function convert_lua_to_flat_string_multiline(file_path,bool1)

    local function scan_comment(line, in_long_comment)
        local i = 1
        local len = #line
        local in_string = nil

        while i <= len do
            local c = line:sub(i, i)
            local n = line:sub(i, i + 1)

            if in_string then
                if c == in_string then
                    in_string = nil
                end
            else
                if c == "'" or c == '"' then
                    in_string = c

                elseif not in_long_comment and n == "--" then
                    -- long comment?
                    local eqs = line:match("^%-%-%[(=*)%[", i)
                    if eqs then
                        return "long_start", i, #eqs
                    end

                    -- single-line comment
                    return "line", i
                end
            end

            i = i + 1
        end

        return "none"
    end
    local function count_parens_outside_strings(line, paren_level, in_long_string)
        local i = 1
        local len = #line
        local in_string = in_long_string or nil  -- track long string marker: e.g., "=+", nil if not inside
        while i <= len do
            local c = line:sub(i,i)
            
            if in_string then
                -- inside string
                if type(in_string) == "string" then
                    -- normal '...' or "..."
                    if c == in_string then
                        -- check not escaped
                        local backslashes = 0
                        local j = i-1
                        while j > 0 and line:sub(j,j) == "\\" do
                            backslashes = backslashes + 1
                            j = j - 1
                        end
                        if backslashes % 2 == 0 then
                            in_string = nil -- string closed
                        end
                    end
                else
                    -- long string: in_string = number of '=' signs
                    local eq_count = 0
                    while line:sub(i,i) == "=" do
                        eq_count = eq_count + 1
                        i = i + 1
                    end
                    if line:sub(i,i) == "]" and eq_count == in_string then
                        in_string = nil
                    end
                end
            else
                -- not in string
                if c == "'" or c == '"' then
                    in_string = c
                elseif c == "[" then
                    -- check for long string start
                    local j = i + 1
                    local eq_count = 0
                    while line:sub(j,j) == "=" do
                        eq_count = eq_count + 1
                        j = j + 1
                    end
                    if line:sub(j,j) == "[" then
                        -- start long string
                        in_string = eq_count  -- store number of '='
                        i = j  -- skip past opening [
                    end
                elseif c == "(" then
                    paren_level = paren_level + 1
                elseif c == ")" then
                    paren_level = paren_level - 1
                end
            end
            i = i + 1
        end
        return paren_level, in_string
    end
    local function split_comment_outside_strings(line)
        local paren = 0
        local in_string = nil
        local i = 1

        while i <= #line do
            local c = line:sub(i,i)
            local n = line:sub(i,i+1)

            if in_string then
                if c == in_string then
                    in_string = nil
                end
            else
                if c == "'" or c == '"' then
                    in_string = c
                elseif n == "--" then
                    return line:sub(1, i-1), line:sub(i+2)
                end
            end
            i = i + 1
        end

        return line, nil
    end
    local function detect_long_comment_start(line)
        local _, _, eqs = line:find("%-%-%[(=*)%[")
        if eqs then
            return #eqs
        end
        return nil
    end
    local function detect_long_comment_end(line, eq_count)
        local closing = "]" .. string.rep("=", eq_count) .. "]"
        return line:find(closing, 1, true) ~= nil
    end
    
    --if type == nil then type = 0 end

    local f = assert(io.open(file_path, "r"), "Cannot open file: " .. file_path)
    local filename = file_path:match("^.+[\\/](.+)%.%w+$") or file_path:match("^(.+)%.%w+$") or file_path
    local content = f:read("*a")
    f:close()

    content = content:gsub("\r\n", "\n"):gsub("\r", "\n")

    local lines = {}
    local paren_level = 0
    local in_string = nil  -- tracks if inside '..."', '...' or long string
    local in_long_comment = nil -- stores eq count or nil


    for line in content:gmatch("[^\n]+") do
        line = line:match("^%s*(.-)%s*$")

        if line ~= "" then
            local skip_line = false

            local kind, pos, eqs = scan_comment(line, in_long_comment)

            -- already inside long comment
            if in_long_comment then
                table.insert(lines, line)
                if detect_long_comment_end(line, in_long_comment) then
                    in_long_comment = nil
                end
                skip_line = true

            -- starting a long comment
            elseif kind == "long_start" then
                in_long_comment = eqs
                table.insert(lines, line)
                skip_line = true

            -- normal single-line comment
            elseif kind == "line" then
                local code = line:sub(1, pos - 1)
                local comment = line:sub(pos + 2)

                comment = "--[==[" .. comment .. "--]==]"
                line = code ~= "" and (code .. comment) or comment
            end

            if not skip_line then
                paren_level, in_string =
                    count_parens_outside_strings(line, paren_level, in_string)

                local skip_semicolon = (
                    paren_level > 0 or
                    in_string or
                    line:match("[{(%[]$") or
                    line:match("=$") or
                    line:match("\"$") or
                    line:match("^else$") or
                    line:match("^elseif%s+") or
                    line:match("^then$") or
                    line:match("^do$") or
                    line:match("^repeat$")
                )

                if (not skip_semicolon 
                    and not line:match(";$")
                    and not line:match(",$")) then
                    line = line .. ";"
                end

                table.insert(lines, line)
            end
        end
    end



    -- join lines
    local str1 = nil
    if bool1 == true then
        print("true")
        str1 = table.concat(lines, "\n")
    else
        print("false")
        str1 = table.concat(lines, " ")
    end
    local version = str1:match('version%s*=%s*"([^"]+)"') or "sample1.0"

    local libname = version:gsub("[%. %(%) ]", "_")

    local eq = string.rep("=", eqcount)

    local parts = {
        "local ", libname,
        "=[" .. eq .. "[", str1, "]" .. eq .. "]; load(", libname, ")();"

    }


    local str2 = table.concat(parts)

    local exportpath = Path(file_path):parent() / (libname .. "_exported") / ".lua"
    print(libname)
    print(exportpath:write_file(str2))

    return str2
end

function Path:exportLua(other)
    assert(self:is_file(),"path provided is not a file, check the path is correct")
    convert_lua_to_flat_string_multiline(self(),other)
    return 
end

--------------------------------------------------
-- constructor
--------------------------------------------------
--- when creating the Path populate data such path, parent, filename, is_file etc??
function Path.new(p)
    log(".new")
    assert(type(p) == "string", "Path must be a string")
    return setmetatable({ path = normalize(p) }, Path)
end

setmetatable(Path, {
    __call = function(_, p)
        return Path.new(p)
    end
})

--------------------------------------------------
-- index and wrapper
--------------------------------------------------
Path.__index = Path
local old_index = Path.__index

function Path:__index(key)
    log(":__index")
    log("index called for: "..key)
    return old_index[key]  -- fallback to normal methods
end



-- Wrap all methods in Path table automatically
for k, v in pairs(Path) do
    
    if type(v) == "function"
        and k ~= "new"
        and not k:match("^__") then
        log("func: ",k)
        Path[k] = wrap_method(v)
    end
end

log("debug has been enabled.")
print("[Pathlib] Loaded " .. Path.version)


return Path

--[[
    to export this library load pathlib like it is then 
    local p = Path([[G:\lua\pathlib.lua]])
    p:exportLua()
    done ! 
--]]


