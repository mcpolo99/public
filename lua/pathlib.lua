--------------------------------------------------------------------------------
-- pathlib.lua (Cheat Engine Autorun)
--
-- A Windows-focused pathlib-like helper for Cheat Engine Lua.
--
-- Features:
--   - Object-oriented path handling
--   - Operator overloading (/ .. () #)
--   - File & directory queries
--   - Read / write helpers
--   - Glob & iteration
--   - Lua source exporter (flattened loader)
--
-- Usage:
--   local p = Path("C:\\Temp") / "file.txt"
--   p:write_text("hello")
-- Path.debug=true
-- local p = Path([[S:\02Development\01Source\lua\pathlib.lua]])
-- print(tostring(p:exportLua()))
--
-- Author: https://github.com/mcpolo99
-- Repo:   https://github.com/mcpolo99/public/tree/main/lua
--------------------------------------------------------------------------------

---@class Path
---@field path string Full normalized path string
---@field version string Library version
---@field debug boolean Enable debug logging
---@field _is_dir boolean|nil Cached directory check
---@operator div(Path|string):Path  # Path / "segment"
---@operator concat(Path|string):Path  # Path .. "segment"
---@operator len:integer  # #Path returns length of path string
---@operator call:Path|string  # Path() returns string, Path("segment") joins
---@operator eq:boolean  # Path == Path
---@operator lt:boolean  # Path < Path
---@operator le:boolean  # Path <= Path
---@overload fun(...:string):Path
---@see Path.new
---@see Path.write_text
---@see Path.read_file
---@see Path.glob
---@see Path.iterdir
---@see Path:info
---@see Path:exportLua
--[[
Pathlib for Cheat Engine Lua (Windows)
Object-oriented path manipulation, file/directory queries, read/write helpers, globbing, and more.

Example:
    local p = Path("C:\\Temp") / "file.txt"
    p:write_text("hello")
    print(p:read_file())
    for _, f in ipairs(p:parent():glob("*.txt")) do
        print(f)
    end
]]
local GLOBAL_NAME = "Path"

--- Global Path constructor
--- Usage:
---   Path("C:\\Temp")
---   Path("C:\\", "Temp", "file.txt")
---@overload fun(...:string):Path
local Path = { }

--- Library version string
---@type string
Path.version = "pathlib 2.4.11(CE)"

--- Enable debug logging
---@type boolean
Path.debug = false

--- Print library metadata and version info
---@type table<string, string>
local about = {
    author ="https://github.com/mcpolo99",
    link = "https://github.com/mcpolo99/public/tree/main/lua",
    purpose = "used primarly for CE tables"
}

local eqcount1 = 23

-- expose globally
_G[GLOBAL_NAME] = Path

pcall (function()
  -- Enable access to blocked metatables.
  -- Don't worry, this module doesn't change anything in them.
  local debmeta = require "debug".getmetatable
  if debmeta then getmetatable = debmeta end
end)

-- internal logging with stack depth
local log_depth = 0
--- Print debug output if Path.debug is enabled
---@vararg any
local function log( ... )
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

-- global/local error handler
local function log_exceptions(err_msg)
    log("local: log_exceptions")
    -- forward/log anywhere you want
    error("[Path exception] " .. err_msg)
    -- optionally return the message to propagate
    return err_msg
end

--- Print library metadata and version info
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
---@param p Path|string
---@return Path
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

---@param fn fun(self:Path, ...):any
---@return fun(self:Path|any, ...):any
local function wrap_method(fn)
    log("wrap_method")
    return function(self, ...)
        if getmetatable(self) ~= Path then
            return fn(assertself(self), ...)
        else
            self = assertself(self)
            return fn(self, ...)
        end
    end
end

---@param p string
---@return string
local function trim_trailing_sep(p)
    log("local: trim_trailing_sep")
    return p:gsub("\\+$", "")
end

---@param p string
---@return string
local function normalize(p)
    log("local: normalize")
    p = tostring(p)
    local unc = p:match("^\\\\") and "\\" or ""
    p = p:gsub("/", "\\")
    p = p:gsub("\\+", "\\")
    if not p:match("^%a:\\$") then
        p = p:gsub("\\+$", "")
    end
    if unc == "" then
        p = p:gsub("^\\+", "")
    end
    return unc .. p
end

---@param t table
---@param indent number?
---@param noChildren boolean?
---@return string
function serializeTable(t, indent, noChildren)
  indent = indent or 0
  local sp = string.rep("  ", indent)
  local s = "{\n"
  local isArray = true

  -- detect if table is array-like
  local count = 0
  for k,v in pairs(t) do
    count = count + 1
    if type(k) ~= "number" then isArray = false end
  end

  local idx = 1
  for k,v in pairs(t) do
    local key
    if isArray then
      key = ""  -- array keys not written
    else
      key = type(k) == "string" and '["'..k..'"]' or "["..k.."]"
      key = key.." = "
    end

    local val
    if type(v) == "table" then
      if noChildren then
        val = "{...}"  -- indicate child table exists but don't serialize
      else
        val = serializeTable(v, indent + 1)
      end
    elseif type(v) == "string" then
      val = '"'..v..'"'
    elseif type(v) == "boolean" then
      val = v and "true" or "false"
    elseif type(v) == "number" then
      val = tostring(v)
    else
      val = 'nil'
    end

    s = s .. sp .. "  " .. key .. val .. ",\n"
    idx = idx + 1
  end

  s = s .. sp .. "}"
  return s
end

---@param t table
---@return boolean
function isNumArray(t)
    return type(t) == "table" and t[1] ~= nil
end

---@param t table
---@return integer
function getItemsCount(t)
    if type(t) ~= "table" then return 0 end
    if isNumArray(t) then
        return #t
    else
        local count = 0
        for _ in pairs(t) do
            count = count + 1
        end
        return count
    end
end


--------------------------------------------------
-- metamethods
--------------------------------------------------

--- Convert Path to string
---@return string
function Path:__tostring()
    log(":__tostring")
    return self.path
end

--- Addition operator (not standard, returns length + other)
---@param other number
---@return number
function Path:__add(other)
    log(":__add")
    return (#self.path) + other
end

--- Subtraction operator (not standard, returns length - other)
---@param other number
---@return number
function Path:__sub(other)
    log(":__sub")
    return (#self.path) - other
end

--- Multiplication operator (not implemented)
---@param other any
---@return nil
function Path:__mul(other)
    log(":__mul")
    return
end

--- Path concatenation using `..` (__concat)
--- Example:
---   Path("C:\\Temp") .. "file.txt"
---@param other string|Path
---@return Path
function Path:__concat(other)
    if type(other) == "string" then
        other = normalize(other):gsub("^\\+", "")
    elseif getmetatable(other) == Path then
        other = trim_trailing_sep(other.path)
    else
        error("Cannot concatenate Path with " .. type(other))
    end
    return Path(trim_trailing_sep(self.path) .. "\\" .. other)
end

--- Length operator (#) (__len)
--- Returns character length of path string
---@return integer
function Path:__len()
    log(":__len")
    return #self.path
end

--- Equality operator (==)
--- compare the path, useful??
---@param other Path
---@return boolean
function Path:__eq(other)
    log(":__eq")
    return self.path == other.path
end

--- Less than operator (<)
---@param other Path
---@return boolean
function Path:__lt(other)
    log(":__lt")
    return self.path < other.path
end

--- Less or equal operator (<=)
---@param other Path
---@return boolean
function Path:__le(other)
    log(":__le")
    return self.path <= other.path
end

--- Callable Path
--- () __call
--- - No argument → returns raw path string
--- - With argument → joins path
---@param other string|nil
---@return string|Path
function Path:__call(other)
    log(":__call")
    if other == nil then
        return self.path
    end
    return self:join(other)
end

--- Path division operator
--- Enables: Path("C:\\") / "Temp" / "file.txt"
--- Example: Path(file_path):parent() / version / ".lua"
---@param other string|Path
---@return Path
Path.__div = function(self, other)
    log(":__div")

    self = assertself(self)
    -- if type(self) == "string" then
    --     self = Path(self)
    -- end
    return self:join(other)
end

--------------------------------------------------
-- core operations
--------------------------------------------------

--- Join another path segment
--- Example: Path(file_path):parent() / version / ".lua"
--- Special case: ".lua" is treated as a suffix, not a directory
---@param other string
---@return Path
function Path:join(other)
    log(":join")
    local base = self.path
    local part = normalize(other or "")
    if part:match("^%.%w+$") then
        return self:with_suffix(part)
    end
    part = part:gsub("^\\+", "")
    return Path(base .. "\\" .. part)
end

--- Resolve absolute path using Windows shell (best-effort, not canonical)
---@return Path
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

--- Check if path exists (file or directory)
---@return boolean
function Path:exists()
    log(":exists")
    return self:is_file() or self:is_dir()
end

--- Check if path is a file
---@return boolean
function Path:is_file()
    log(":is_file")
    local f = io.open(self.path, "rb")
    if f then
        f:close()
        return true
    end
    return false
end

--- Check if path is a directory
---@return boolean
function Path:is_dir()
    log(":is_dir")
    if self._is_dir ~= nil then
        return self._is_dir
    end
    local p = self.path:gsub("\\+$", "")
    local cmd = io.popen('if exist "' .. p .. '\\*" (echo 1) else (echo 0)')
    local res = cmd:read("*l")
    cmd:close()
    self._is_dir = (res == "1")
    return self._is_dir
end

--------------------------------------------------
--  write operations
--------------------------------------------------

-- Internal: Write text to file (overwrites)
---@param self Path
---@param text string
---@return Path
local function __write_file_impl(p, text)
    log("local: __write_file_impl")
    assert(type(text) == "string", "text must be a string")
    p = assertself(p)
    
    local parent = p:parent()
    if parent and not parent:is_dir() then
        parent:mkdir(true)
    end

    local f, err = io.open(p.path, "w")

    if not f then
        log_exceptions("Cannot open file for writing: " .. (err or p.path))
        return nil
    end
    local success, handler_err = xpcall(function()
        f:write(text)
    end, log_exceptions)
    f:close()
    if not success then return nil end
    return p
end

--- Write text to file (overwrites)
--- Creates parent directories if needed
---@param text string
---@return Path self
function Path:write_file(text)
    log(":write_file")
    return __write_file_impl(self, text)
end

--- Alias for write_file
---@param text string
---@return Path
function Path:write_text(text)
    log(":write_text")
    return __write_file_impl(self, text)
end

--- Static helper: write text to path
---@param p string|Path
---@param text string
---@return Path
function Path.write_text(p, text)
    log(".write_text")
    return __write_file_impl(p, text)
end

--- Alias for write_text
Path.write_file = Path.write_text

--------------------------------------------------
--  Read operations
--------------------------------------------------

-- Internal: Read file contents
---@param self Path
---@return string
local function __read_file_impl(p)
    log("local: read_file_impl")
    p = assertself(p)

    -- try to open the file safely
    local f, err = io.open(p.path, "r")
    if not f then
        -- use your exception handler for consistency
        log_exceptions("Cannot open file: " .. (err or p.path))
        return nil
    end

    local data
    -- read file safely
    local success, handler_err = xpcall(function()
        data = assert(f:read("*a"), "Failed to read file")
    end, log_exceptions)

    f:close()  -- always close file

    if not success then
        -- optionally re-throw or just return nil
        return nil
    end

    return data
end

--- Read entire file contents
---@return string
function Path:read_file()
    log(":read_file")
    return __read_file_impl(self)
end

--- Static helper: read file contents
---@param p string|Path
---@return string
function Path.read_file(p)
    log(".read_file")
    return __read_file_impl(p)
end

--- Alias for read_file
Path.read_text = Path.read_file

--------------------------------------------------
-- directory operations
--------------------------------------------------

--- Create directory
---@param parents boolean|nil  Create parent directories if true
---@return Path self
function Path:mkdir(parents)
    log(":mkdir")
    if parents then
        os.execute('mkdir "' .. self.path .. '" >nul 2>nul')
    else
        os.execute('mkdir "' .. self.path .. '"')
    end
    return self
end

--- Iterate directory contents (non-recursive)
---@return Path[]  Array of Path objects
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

--- File name (nil if directory)
---@return string|nil
function Path:filename()
    log(":filename")
    if self:is_dir() then return nil end
    return self.path:match("([^\\]+)$")
end

--- File name without extension
---@return string|nil
function Path:filenamext()
    log(":filenemext")
    local name = self:filename()
    if name then
        return name:gsub("(%..+)$", "")
    end
end

--- Full path string
---@return string
function Path:filepath()
    log(":filepath")
    return self.path
end

--- Parent directory
---@return Path
function Path:parent()
    log(":parent")
    return Path(self.path:match("^(.*)\\[^\\]+$") or self.path)
end

--- File extension (including dot)
---@return string|nil
function Path:suffix()
    log(":suffix")
    return self.path:match("(%.%w+)$")
end

--- File name without suffix
---@return string|nil
function Path:stem()
    log(":stem")
    local n = self:filename()
    return n and n:gsub("%.%w+$", "")
end

--- Return a new Path with replaced suffix
--- Handles both adding/replacing/removing extension
---@param new_suffix string  Must start with "." or be empty to remove extension
---@return Path
function Path:with_suffix(new_suffix)
    log(":with_suffix")

    assert(type(new_suffix) == "string", "suffix must be a string")
    assert(new_suffix == "" or new_suffix:sub(1,1) == ".",
           "suffix must start with '.' or be empty")

    local parent = self:parent()
    local stem = self:stem() or self:filename()
    assert(stem, "cannot replace suffix on a directory")

    return parent / (stem .. new_suffix)
end
-- Alias: replace_suffix uses the same implementation
Path.replace_suffix = Path.with_suffix 


--- Return all path parts
---@return string[]
function Path:parts()
    log(":parts")
    local t = {}
    for part in self.path:gmatch("[^\\]+") do
        t[#t + 1] = part
    end
    return t
end

--- Glob files inside directory (Windows dir-based)
--- Example: Path("C:\\Temp"):glob("*.lua")
---@param pattern string
---@return Path[]
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

--- Print detailed path info (for debugging)
---@return table
function Path:info()
    log(":info")
    local info =  {
        path = self.path or "",
        parent = tostring(self:parent()),
        exists = tostring(self:exists()),
        is_file = tostring(self:is_file()),
        is_dir = tostring(self:is_dir()),
        filename = tostring(self:filename()),
        stem = tostring(self:stem()),
        suffix = tostring(self:suffix())
    }
    local s = serializeTable(info)
    log(s)

    return s
end


-- Converts a Lua script file into a flat string
local eqcount = eqcount1

--- Export Lua file into a flattened loader script
--- Converts source into a single loadable string and writes: <version>_exported.lua
--- TODO content as input, eg no path as input
--- todo content as output , eg no write of file er 
---@param multiline boolean|nil  Preserve newlines if true
---@param p Path
---@param bool1 boolean|nil  Preserve newlines if true
---@param text string? optional string to convert to exported?
---@return string
local function __convert_lua_to_flat_string_multiline(content,bool1)

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
    -- WE are provided with a Path and not a string! handle it 
    -- assert(getmetatable(p) == Path, "path must be Path or string")

    -- -- local f, err = io.open(p.path, "r")
    -- local content = p:read_file() 
    -- --assert(f, err or "Cannot read file")
    -- local file_path = p.path

    -- --local filename = file_path:match("^.+[\\/](.+)%.%w+$") or file_path:match("^(.+)%.%w+$") or p.path
    -- local filename = p:filename()
    -- local content = assert(f:read("*a"), "Failed to read file")
    -- f:close()
    assert(type(content)=="string","[Path] Content is not string!")

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
    if lines then
        local str1 = nil
        if bool1 == true then
            log("bool1 = true")
            str1 = table.concat(lines, "\n")
        else
            log("bool1 = false")
            str1 = table.concat(lines, " ")
        end
        local version = str1:match('version%s*=%s*"([^"]+)"') or (Path(os.tmpname()):filenamext())

        local libname = version:gsub("[%. %(%) ]", "_")

        local eq = string.rep("=", eqcount)

        local parts = {
            "local ", libname,
            "=[" .. eq .. "[", str1, "]" .. eq .. "]; load(", libname, ")();"

        }

        local str2 = table.concat(parts)
    
        -- local exportpath = Path(file_path):parent() / (libname .. "_exported") / ".lua"
        -- log(libname)
        -- log(exportpath:write_file(str2))
        
        return str2
    end

    return log_exceptions("__convert_lua_to_flat_string_multiline: No lines parsed")
end





--- Internal export helper
---@param p Path|string Source path
---@param other any Optional conversion flag
---@param arg string|table|Path|nil Content OR destination
---@return Path
local function __exportLua(p, other, arg)
    -- normalize p
    p = assertself(p)

    local content
    local savePath

    ----------------------------------------------------------------
    -- ARG DISPATCH
    ----------------------------------------------------------------
    if arg ~= nil then
        local t = type(arg)

        -- CASE 1: arg is destination path
        if t == "string" or getmetatable(arg) == Path then
            savePath = assertself(arg)
            content = __convert_lua_to_flat_string_multiline(
                p:read_file(), other
            )

        -- CASE 2: arg is content
        elseif t == "table" then
            content = __convert_lua_to_flat_string_multiline(
                serializeTable(arg), other
            )

        elseif t == "string" then
            content = __convert_lua_to_flat_string_multiline(arg, other)

        else
            return log_exceptions("Invalid 3rd argument to exportLua")
        end

    -- CASE 3: no arg → read from p, export next to it
    else
        content = __convert_lua_to_flat_string_multiline(
            p:read_file(), other
        )
    end

    ----------------------------------------------------------------
    -- TEMP FILE (SAFE WRITE)
    ----------------------------------------------------------------
    local tmp = Path(os.tmpname())
    tmp:write_file(content)

    ----------------------------------------------------------------
    -- FINAL DESTINATION
    ----------------------------------------------------------------


    local exportPath

    if savePath then
        exportPath = savePath
    else
        exportPath = p:parent() / (p:stem() .. "_exported.lua")
    end

    exportPath:write_file(tmp:read_file())
    os.remove(tmp.path)

    log("Exported Lua to:", exportPath.path)
    return exportPath
end

--------------------------------------------------
-- Export this Path (file) as a flattened Lua loader script
--------------------------------------------------

--- Instance method: export this Path (file)
---@param other boolean|nil Preserve newlines if true
---@param text string|table|nil Optional content to export instead of reading from file
---@return string exported file path
function Path:exportLua(other, text)
    log(":exportLua")

    -- if no text provided, we expect self to be a file
    if text == nil then
        assert(self:is_file(), "Path provided is not a file")
        assert(self:exists(), "File does not exist: " .. self.path)
    end

    -- call the internal helper

    local exported_path = __exportLua(self, other, text)
    return exported_path
end

--------------------------------------------------
-- Static method: exportLua with Path or string input
--------------------------------------------------

--- Static function: export a Path or string to flattened Lua
---@param p Path|string Path to read from or save to
---@param other boolean|nil Preserve newlines if true
---@param text string|table|nil Optional content to export instead of reading from file
---@return string exported file path
function Path.exportLua(p, other, arg)
    log("static: .exportLuaContent")

    -- call the internal helper
    local exported_path = __exportLua(p, other, arg)
    return exported_path
end

--- Create a new Path instance
--- All arguments are joined and normalized using Windows separators.
---@param ... string  One or more path segments
---@return Path
function Path.new(...)
    
    local args = {...}
    log("static .new, no args: '"..#args.."\'")
    assert(#args > 0, "Path.new requires at least one argument")
    local parts = {}
    
    for i, v in ipairs(args) do
        assert(type(v) == "string", "All Path arguments must be strings")
        parts[#parts + 1] = normalize(v)
    end
    local full_path = table.concat(parts, "\\")
    return setmetatable({ path = full_path }, Path)
end

setmetatable(Path, {

    __call = function(_, ...)
        return Path.new(...)
    end
})

--------------------------------------------------
-- index and wrapper
--------------------------------------------------
Path.__index = Path
local old_index = Path.__index

function Path:__index(key)
    log(":__index, calling \'"..key.."\'")
    return old_index[key]
end

-- Wrap all methods in Path table automatically
-- for k, v in pairs(Path) do
--     if type(v) == "function"
--         and k ~= "new"
--         and not k:match("^__") then
--         log("func: ",k)
--         Path[k] = wrap_method(v)
--     end
-- end

log("debug has been enabled.")
print("[Pathlib] Loaded " .. Path.version)
--- Test function for Path library
--- Run this to validate all major Path operations
function test_pathlib()

    print("\n=== Pathlib Test Start ===")

    local temp_dir = Path(os.getenv("TEMP") or "C:\\Temp") / "pathlib_test"
    local pathlib_path = Path([[S:\02Development\01Source\lua\pathlib.lua]])

    temp_dir:mkdir(true)
    print("------------------------------------- Temp test directory:", temp_dir(), "-------------------------------------")

    -- 1. Path construction
    local p1 = Path("C:\\Temp") / "file.txt"
    assert(getmetatable(p1) == Path, "p1 must be a Path")
    assert(tostring(p1):match("C:\\Temp\\file.txt"), "Path construction failed")
    print("------------------------------------- Constructor test passed:", p1(), "-------------------------------------")

    -- 2. Parent / parts / stem / suffix
    assert(p1:parent():filepath() == "C:\\Temp", "Parent path failed")
    assert(p1:stem() == "file", "Stem failed")
    assert(p1:suffix() == ".txt", "Suffix failed")
    local parts = p1:parts()
    assert(parts[#parts] == "file.txt", "Parts failed")
    print("------------------------------------- Parent / parts / stem / suffix tests passed -------------------------------------")

    -- 3. Suffix manipulation
    local p2 = p1:with_suffix(".lua")
    assert(p2:suffix() == ".lua", "with_suffix failed")
    local p3 = p1:replace_suffix("")
    assert(p3:suffix() == nil, "replace_suffix remove failed")
    print("------------------------------------- Suffix manipulation tests passed -------------------------------------")

    -- 4. Join / concatenation
    local p4 = Path("C:\\Temp"):join("joined.lua")
    assert(p4:suffix() == ".lua", "Join failed")
    local p5 = Path("C:\\Temp") .. "concat.txt"
    assert(p5:filename() == "concat.txt", "Concat operator failed")
    assert(p5() == "C:\\Temp\\concat.txt", "Concat operator failed")
    
    print("------------------------------------- Join / concat tests passed -------------------------------------")

    -- 5. File writing / reading
    local test_text = p4:info()
    local file_path = temp_dir / "test_export_file.txt"
    file_path:write_file(test_text)
    local read_text = file_path:read_file()
    assert(read_text == test_text, "Write/read failed")
    print("------------------------------------- Write / read test 1 passed -------------------------------------")

    -- 5.1 File writing / reading for auto load script
    local test_text2 = "sample_Load = "..test_text
    local file_path2 = temp_dir / "test_export_file_for_load.txt"
    file_path2:write_file(test_text2)
    local read_text2 = file_path2:read_file()
    assert(read_text2 == test_text2, "Write/read failed")
    print("------------------------------------- Write / read test 2 passed -------------------------------------")


    -- 6. Static write/read helpers
    Path.write_text(temp_dir / "static.txt", "Static helper")
    local static_content = Path.read_text(temp_dir / "static.txt")
    assert(static_content == "Static helper", "Static helper failed")
    print("------------------------------------- Static write/read tests passed -------------------------------------")

    -- 7. mkdir / directory operations
    local dir_path = temp_dir / "subdir"
    dir_path:mkdir()
    assert(dir_path:is_dir(), "mkdir failed")
    local iter_items = temp_dir:iterdir()
    assert(getItemsCount(iter_items) > 0, "iterdir failed")
    print(" ------------------------------------- Directory tests passed ------------------------------------- ")

    -- 8. Export normal Lua (instance)
    local export_path = file_path:exportLua(true)
    assert(export_path:exists(), "exportLua failed")
    -- export loadable :

    print(" ------------------------------------- exportLua test passed:", export_path(), " ------------------------------------- ")

    ----------------------------------------------------------------
    -- 8.1 Export Lua (static) to custom destination
    ----------------------------------------------------------------
    
    local file_path2 = file_path:parent() / "test_export_static.lua"
    local export_path2 = Path.exportLua(file_path, false, file_path2)
    assert(tostring(file_path2) == tostring(export_path2), "exportLua2 path mismatch")
    assert(export_path2:exists(), "exportLua2 failed")
    print(" ------------------------------------- static exportLua test passed:", export_path2(), " ------------------------------------- ")



    ----------------------------------------------------------------
    -- 8.2 Export Lua from CONTENT (table input)
    ----------------------------------------------------------------
    local sample_export_content = {
        HeadGroup = {
            SubGroup1 = {
                {
                    text = "Cycle Num OR like",
                    locations = {
                        { x = 0, y = 0, z = 0, text = "display 1" },
                        { x = 0, y = 0, z = 0, text = "display 2" },
                    },
                    ["any additional data"] = "",
                },
            },
        },
    }

    local content_export_path = temp_dir / "test_export_content.lua"
    local export_path3 = Path.exportLua(content_export_path, false, sample_export_content)

    assert(export_path3:exists(), "exportLua content failed")

    local exported_text = export_path3:read_file()
    assert(type(exported_text) == "string" and #exported_text > 0,
           "exported content empty")

    print(" ------------------------------------- content exportLua test passed:", export_path3(), " ------------------------------------- ")


    ----------------------------------------------------------------
    -- 8.3 Export Lua from CONTENT (table input)
    ----------------------------------------------------------------

    -- create an isolated environment

    -- sandbox environment
    local sandbox = {
        print = print,
        Path  = Path,
    }

    setmetatable(sandbox, {
        __index = function(_, k)
            error("Access to global '" .. tostring(k) .. "' is denied", 2)
        end
    })

    local lib_path1 = temp_dir / "test_export_pathlib1.lua"
    local exported_path_lib1 = Path.exportLua(pathlib_path(),true,lib_path1)
    assert(exported_path_lib1:exists(), "exportLua failed: "..exported_path_lib1())

    -- load file with environment
    local chunk, err = loadfile(exported_path_lib1.path, "bt", sandbox)
    assert(chunk, "Error running exported pathlib1: " .. tostring(err))

    -- local ok, err = pcall(loadfile(exported_path_lib1.path); print(Path.version))


    print(" ------------------------------------- static exportLua test passed:", exported_path_lib1(), " ------------------------------------- ")

    ----------------------------------------------------------------
    -- 8.4 Export Lua from CONTENT (table input)
    ----------------------------------------------------------------
    -- sandbox environment
    local sandbox = {
        print = print,
        Path  = Path,
    }

    setmetatable(sandbox, {
        __index = function(_, k)
            error("Access to global '" .. tostring(k) .. "' is denied", 2)
        end
    })

    local lib_path2 = temp_dir / "test_export_pathlib2.lua"
    local exported_path_lib2 = Path.exportLua(pathlib_path(),false,lib_path2)
    assert(exported_path_lib2:exists(), "exportLua failed: "..exported_path_lib2())
    --local ok, err = pcall(loadfile(exported_path_lib2.path))
    --assert(ok, "Error running exported pathlib: " .. tostring(err))

    local chunk, err = loadfile(exported_path_lib2.path, "bt", sandbox)
    assert(chunk, "Error running exported pathlib2: " .. tostring(err))

    print(" ------------------------------------- static exportLua test passed:", exported_path_lib2(), " ------------------------------------- ")


    




    -- 9. Error handling
    local ok, err = pcall(function()
        Path(123) -- invalid constructor
    end)
    assert(not ok and err:match("All Path arguments must be strings"), "Constructor error test failed")
    print(" ------------------------------------- Error handling test passed ------------------------------------- ")

    -- 10. Cleanup
    for _, f in ipairs(temp_dir:iterdir()) do
       os.remove(f.path)
    end
    os.execute('rmdir /s /q "' .. temp_dir.path .. '"')

    print(" ------------------------------------- Cleanup done ------------------------------------- ")
    print("=== Pathlib Test Completed Successfully ===\n")
end


return Path