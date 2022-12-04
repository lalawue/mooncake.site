//
// Copyright (c) 2022 lalawue
//
// This library is free software; you can redistribute it and/or modify it
// under the terms of the MIT license. See LICENSE for details.
//

// load moocscript browser lib after load fengari-web
function moocscript_web_install_lib() {
	function heredoc(f) {
		return f.toString().split('\n').slice(1, -1).join('\n') + '\n'
	}
	const tmpl_str = heredoc(function(){/*-- auto generated standalone library combined with 'utils', 'parser', 'compiler', 'core', 'class' for moocscript v0.8.20221204
local MoocLib = {  }
do
	js = js or nil
	local ipairs = ipairs
	local Utils = { __tn = 'Utils', __tk = 'class', __st = nil }
	do
		local __st = nil
		local __ct = Utils
		__ct.__ct = __ct
		__ct.isKindOf = function(c, a) return a and c and ((c.__ct == a) or (c.__st and c.__st:isKindOf(a))) or false end
		-- declare class var and methods
		function __ct.printValue(v)
			local tv = type(v)
			if tv == "string" then
				local first = v:sub(1, 1)
				if first == '"' or first == "'" or first == '[' then
					return v
				else 
					return '"' .. v .. '"'
				end
			else 
				return tostring(v)
			end
		end
		function __ct.format(c, p, v, is_table)
			return is_table and (c[v][2] >= p) and Utils.serializeTable(v, p + 1, c) or Utils.printValue(v)
		end
		function __ct.serializeTable(t, p, c)
			local n = 0
			for i, v in next, t do
				n = n + 1
			end
			local ti = 1
			local e = n > 0
			local str = ""
			local _table = Utils.serializeTable
			local _format = Utils.format
			local _srep = string.rep
			c = c or {  }
			p = p or 1
			c[t] = { t, 0 }
			for i, v in next, t do
				local typ_i, typ_v = type(i) == 'table', type(v) == 'table'
				c[i], c[v] = (not c[i] and typ_i) and { i, p } or c[i], (not c[v] and typ_v) and { v, p } or c[v]
				str = str .. _srep('  ', p) .. '[' .. _format(c, p, i, typ_i) .. '] = ' .. _format(c, p, v, typ_v) .. (ti < n and ',' or '') .. '\n'
				ti = ti + 1
			end
			return ('{' .. (e and '\n' or '')) .. str .. (e and _srep('  ', p - 1) or '') .. '}'
		end
		function __ct.split(self, sep, max, regex)
			assert(sep ~= "")
			assert(max == nil or max >= 1)
			local record = {  }
			if self:len() > 0 then
				local plain = not regex
				max = max or -1
				local field, start = 1, 1
				local first, last = self:find(sep, start, plain)
				while first and max ~= 0 do
					record[field] = self:sub(start, first - 1)
					field = field + 1
					start = last + 1
					first, last = self:find(sep, start, plain)
					max = max - 1
				end
				record[field] = self:sub(start)
			else 
				record[1] = ""
			end
			return record
		end
		function __ct.set(tbl)
			local s = {  }
			for _, v in ipairs(tbl) do
				s[v] = true
			end
			return s
		end
		function __ct.seqReduce(tbl, init, func)
			for i, v in ipairs(tbl) do
				init = func(init, i, v)
			end
			return init
		end
		__ct.read_option = _VERSION == "Lua 5.1" and "*a" or "a"
		function __ct.readFile(file_path)
			if js then
				return js.mooc_loadscript(file_path)
			end
			local f, err = io.open(file_path, "rb")
			if not f then
				return nil, err
			end
			local data = f:read(Utils.read_option)
			f:close()
			return data
		end
		function __ct.writeFile(file_path, content)
			local f = io.open(file_path, "wb")
			if not f then
				return 
			end
			f:write(content)
			f:close()
			return true
		end
		function __ct.copy(it)
			local ot = {  }
			for k, v in pairs(it) do
				ot[k] = v
			end
			return ot
		end
		function __ct.suffix(str)
			for i = str:len(), 1, -1 do
				if str:sub(i, i) == '.' then
					return str:sub(i + 1, str:len())
				end
			end
			return ""
		end
		function __ct.debug(str)
			print(str)
		end
		function __ct.dump(t)
			Utils.debug(Utils.serializeTable(t))
		end
		function __ct.posLine(content, lpos)
			assert(type(content) == "string", "Invalid content")
			assert(type(lpos) == "number", "Invalid pos")
			local ln_lnum = 1
			for _ in content:sub(1, lpos):gmatch("\n") do
				ln_lnum = ln_lnum + 1
			end
			local lnum = ln_lnum
			local ln_content = ""
			local lcount = 0
			for line in content:gmatch("([^\n]*\n?)") do
				if lnum == 1 then
					ln_content = line
					break
				end
				lnum = lnum - 1
				lcount = lcount + line:len()
			end
			return { line = ln_lnum, column = lpos - lcount, message = ln_content:gsub('[\n\r]', '') }
		end
		function __ct.errorMessage(content, pos, msg, fname)
			local ct = Utils.posLine(content, pos)
			return string.format("Error: %s\nFile: %s\nLine: %d (Pos: %d)\nSource: %s\n%s", msg, fname or '_', ct.line, pos, ct.message, string.rep(' ', 8) .. ct.message:gsub('[^%s]', ' '):sub(1, math.max(0, ct.column)) .. '^')
		end
		-- declare end
		local __imt = {
			__tostring = function(t) return "<class Utils" .. t.__ins_name .. ">" end,
			__index = function(t, k)
				local v = __ct[k]
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
		}
		setmetatable(__ct, {
			__tostring = function() return "<class Utils>" end,
			__index = function(t, k)
				local v = __st and __st[k]
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
			__call = function(_, ...)
				local t = {}; t.__ins_name = tostring(t):sub(6)
				local ins = setmetatable(t, __imt)
				if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end
				return ins
			end,
		})
	end
	MoocLib.utils = Utils; package.loaded['moocscript.utils'] = MoocLib.utils
end
MoocLib = MoocLib or nil
do
	local Utils = require("moocscript.utils")
	local srep = string.rep
	local math_huge = math.huge
	local unpack = unpack or table.unpack
	local tostring = tostring
	local Token = { Illegal = "illegal", Eof = "eof", Identifier = "identifier", Number = "number", String = "string", StringExprD = "string+d", StringExprS = "string+s", Comment = "comment", SheBang = "shebang", Vararg = "...", SepSemi = ";", SepComma = ",", SepDot = ".", SepColon = ":", SepLabel = "::", SepLparen = "(", SepRparen = ")", SepLbreak = "[", SepRbreak = "]", SepLcurly = "{", SepRcurly = "}", OpAssign = "=", OpMinus = "-", OpWav = "~", OpAdd = "+", OpMul = "*", OpDiv = "/", OpIdiv = "//", OpPow = "^", OpMod = "%", OpBand = "&", OpBor = "|", OpShr = ">>", OpShl = "<<", OpConcat = "..", OpLt = "<", OpLe = "<=", OpGt = ">", OpGe = ">=", OpEq = "==", OpNe = "~=", OpNb = "!=", OpNen = "#", OpAnd = "and", OpOr = "or", OpNot = "not", KwBreak = "break", KwCase = "case", KwClass = "class", KwContinue = "continue", KwDefault = "default", KwDefer = "defer", KwDo = "do", KwElse = "else", KwElseIf = "elseif", KwExport = "export", KwExtension = "extension", KwFalse = "false", KwFn = "fn", KwFor = "for", KwFrom = "from", KwGoto = "goto", KwGuard = "guard", KwIf = "if", KwImport = "import", KwIn = "in", KwLocal = "local", KwNil = "nil", KwPublic = "public", KwRepeat = "repeat", KwReturn = "return", KwStatic = "static", KwStruct = "struct", KwSwitch = "switch", KwTrue = "true", KwUntil = "until", KwWhile = "while" }
	local ReservedWord = { [Token.OpAnd] = Token.OpAnd, [Token.OpOr] = Token.OpOr, [Token.OpNot] = Token.OpNot, [Token.KwBreak] = Token.KwBreak, [Token.KwCase] = Token.KwCase, [Token.KwClass] = Token.KwClass, [Token.KwContinue] = Token.KwContinue, [Token.KwDefault] = Token.KwDefault, [Token.KwDefer] = Token.KwDefer, [Token.KwDo] = Token.KwDo, [Token.KwElse] = Token.KwElse, [Token.KwElseIf] = Token.KwElseIf, [Token.KwExport] = Token.KwExport, [Token.KwExtension] = Token.KwExtension, [Token.KwFalse] = Token.KwFalse, [Token.KwFn] = Token.KwFn, [Token.KwFor] = Token.KwFor, [Token.KwFrom] = Token.KwFrom, [Token.KwGoto] = Token.KwGoto, [Token.KwGuard] = Token.KwGuard, [Token.KwIf] = Token.KwIf, [Token.KwImport] = Token.KwImport, [Token.KwIn] = Token.KwIn, [Token.KwLocal] = Token.KwLocal, [Token.KwNil] = Token.KwNil, [Token.KwPublic] = Token.KwPublic, [Token.KwRepeat] = Token.KwRepeat, [Token.KwReturn] = Token.KwReturn, [Token.KwStatic] = Token.KwStatic, [Token.KwStruct] = Token.KwStruct, [Token.KwSwitch] = Token.KwSwitch, [Token.KwTrue] = Token.KwTrue, [Token.KwUntil] = Token.KwUntil, [Token.KwWhile] = Token.KwWhile }
	local CharSymbol = { [Token.SepSemi] = true, [Token.SepComma] = true, [Token.SepLparen] = true, [Token.SepRparen] = true, [Token.SepRbreak] = true, [Token.SepLcurly] = true, [Token.SepRcurly] = true, [Token.OpAdd] = true, [Token.OpMul] = true, [Token.OpPow] = true, [Token.OpMod] = true, [Token.OpBand] = true }
	local ArithmeticOp = { [Token.OpAdd] = true, [Token.OpMinus] = true, [Token.OpMul] = true, [Token.OpDiv] = true, [Token.OpIdiv] = true, [Token.OpPow] = true, [Token.OpMod] = true }
	local BitwiseOp = { [Token.OpBand] = true, [Token.OpBor] = true, [Token.OpWav] = true, [Token.OpShr] = true, [Token.OpShl] = true }
	local RelationalOp = { [Token.OpEq] = true, [Token.OpNe] = true, [Token.OpNb] = true, [Token.OpLt] = true, [Token.OpLe] = true, [Token.OpGt] = true, [Token.OpGe] = true }
	local LogicalOp = { [Token.OpAnd] = true, [Token.OpOr] = true }
	local CharBlank = { [' '] = true, ['\t'] = true, ['\n'] = true, ['\r'] = true, ['\v'] = true, ['\f'] = true }
	local function unp(a)
		if a then
			return unpack(a)
		end
	end
	local function isBinOp(t)
		return (t == Token.OpConcat) or ArithmeticOp[t] or BitwiseOp[t] or RelationalOp[t] or LogicalOp[t]
	end
	local function isDigit(ch)
		return ch >= '0' and ch <= '9'
	end
	local function isLetter(ch)
		return (ch >= 'a' and ch <= 'z') or (ch >= 'A' and ch <= 'Z')
	end
	local function isHex(ch)
		return (ch >= '0' and ch <= '9') or (ch >= 'a' and ch <= 'f') or (ch >= 'A' and ch <= 'F')
	end
	local _schar = string.char
	local function schar(byte)
		if (byte >= 0 and byte <= 31) or byte == 127 then
			return ' '
		end
		return _schar(byte)
	end
	local QuickStack = { __tn = 'QuickStack', __tk = 'struct' }
	do
		local __ct = QuickStack
		__ct.__ct = __ct
		-- declare struct var and methods
		__ct._array = {  }
		__ct._index = 0
		function __ct:reset()
			self._index = 0
		end
		function __ct:dataOp()
			return self._array, self._index
		end
		function __ct:incTop()
			self._index = self._index + 1
			local t = self._array[self._index] or {  }
			self._array[self._index] = t
			return t
		end
		function __ct:decTop()
			if not (self._index > 0) then
				return 
			end
			local t = self._array[self._index]
			self._index = self._index - 1
			return t
		end
		-- declare end
		local __imt = {
			__tostring = function(t) return "<struct QuickStack" .. t.__ins_name .. ">" end,
			__index = function(t, k)
				local v = rawget(__ct, k)
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
			__newindex = function(t, k, v) if rawget(__ct, k) ~= nil then rawset(t, k, v) end end,
		}
		QuickStack = setmetatable({}, {
			__tostring = function() return "<struct QuickStack>" end,
			__index = function(t, k) local v = rawget(__ct, k); if v ~= nil then rawset(t, k, v); end return v end,
			__newindex = function(t, k, v) if v ~= nil and rawget(__ct, k) ~= nil then rawset(t, k, v) end end,
			__call = function(_, ...)
				local t = {}; t.__ins_name = tostring(t):sub(6)
				local ins = setmetatable(t, __imt)
				if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end
				return ins
			end,
		})
	end
	local GroupMap = { __tn = 'GroupMap', __tk = 'struct' }
	do
		local __ct = GroupMap
		__ct.__ct = __ct
		-- declare struct var and methods
		__ct._gcount = 1
		__ct._imap = {  }
		__ct._atop = 0
		__ct._array = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
		__ct._ret = { 0, 0, 0, 0 }
		function __ct:init(gcount)
			self._gcount = gcount
		end
		function __ct:reset()
			self._atop = 0
			self._imap = {  }
		end
		function __ct:set(key, ...)
			if not (key and select('#', ...) == self._gcount) then
				return 
			end
			local ret = self._ret
			ret[1], ret[2], ret[3], ret[4] = ...
			local base = self._atop
			for i = 1, self._gcount do
				self._array[base + i] = ret[i]
			end
			self._imap[key] = self._atop
			self._atop = self._atop + self._gcount
		end
		function __ct:get(key)
			local base = self._imap[key or self._array]
			if not (key and base) then
				return 
			end
			local ret = self._ret
			for i = 1, self._gcount do
				ret[i] = self._array[base + i]
			end
			return ret[1], ret[2], ret[3], ret[4]
		end
		-- declare end
		local __imt = {
			__tostring = function(t) return "<struct GroupMap" .. t.__ins_name .. ">" end,
			__index = function(t, k)
				local v = rawget(__ct, k)
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
			__newindex = function(t, k, v) if rawget(__ct, k) ~= nil then rawset(t, k, v) end end,
		}
		GroupMap = setmetatable({}, {
			__tostring = function() return "<struct GroupMap>" end,
			__index = function(t, k) local v = rawget(__ct, k); if v ~= nil then rawset(t, k, v); end return v end,
			__newindex = function(t, k, v) if v ~= nil and rawget(__ct, k) ~= nil then rawset(t, k, v) end end,
			__call = function(_, ...)
				local t = {}; t.__ins_name = tostring(t):sub(6)
				local ins = setmetatable(t, __imt)
				if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end
				return ins
			end,
		})
	end
	local Lexer = { __tn = 'Lexer', __tk = 'struct' }
	do
		local __ct = Lexer
		__ct.__ct = __ct
		-- declare struct var and methods
		__ct._chunk = ""
		__ct._pos = 0
		__ct._pmap = GroupMap(4)
		__ct._pstack = {  }
		__ct._ptop = 0
		__ct._err_msg = false
		function __ct:reset(chunk)
			self._chunk = chunk
			self._pos = 0
			self._pmap:reset()
			self._ptop = 0
			self._err_msg = false
		end
		function __ct:savePos()
			self._ptop = self._ptop + 1
			self._pstack[self._ptop] = self._pos
		end
		function __ct:restorePos()
			if self._ptop > 0 then
				self._pos = self._pstack[self._ptop]
				self._ptop = self._ptop - 1
			end
		end
		function __ct:clearPos()
			if self._ptop > 0 then
				self._ptop = self._ptop - 1
			end
		end
		function __ct:peekToken(advance)
			if not (self._pos < self._chunk:len()) then
				return Token.Eof
			end
			local opos = self._pos
			local token, tcontent, tpos, npos = self._pmap:get(self._pos)
			if token then
				if advance then
					self._pos = npos
				end
				return token, tcontent, tpos, opos
			else 
				self:skipSpacesComments()
				npos = self._pos
			end
			token, tcontent, tpos = Token.Illegal, nil, self._pos + 1
			local ch = self:nextChar()
			local __s = ch
			if __s == '#' then
				if '!' == self:peekChar() and self._pos == 1 then
					token = Token.SheBang
					tcontent = "#" .. self:oneLine()
				else 
					token = Token.OpNen
					tcontent = Token.OpNen
				end
			elseif __s == '-' then
				token = Token.OpMinus
				tcontent = Token.OpMinus
			elseif __s == '.' then
				if '.' == self:peekChar() then
					self._pos = self._pos + 1
					if '.' == self:peekChar() then
						self._pos = self._pos + 1
						token = Token.Vararg
						tcontent = Token.Vararg
					else 
						token = Token.OpConcat
						tcontent = Token.OpConcat
					end
				elseif isDigit(self:peekChar()) then
					token = Token.Number
					tcontent = self:oneNumber(ch)
				else 
					token = Token.SepDot
					tcontent = Token.SepDot
				end
			elseif __s == '"' or __s == "'" then
				token, tcontent = self:shortString(ch)
			elseif __s == '[' then
				local nch = self:peekChar()
				if nch == '[' or nch == '=' then
					token = Token.String
					tcontent = self:longString()
				else 
					token = Token.SepLbreak
					tcontent = Token.SepLbreak
				end
			elseif __s == '/' then
				if '/' == self:peekChar() then
					self._pos = self._pos + 1
					token = Token.OpIdiv
					tcontent = Token.OpIdiv
				else 
					token = Token.OpDiv
					tcontent = Token.OpDiv
				end
			elseif __s == '>' then
				local nch = self:peekChar()
				if nch == '>' then
					self._pos = self._pos + 1
					token = Token.OpShr
					tcontent = Token.OpShr
				elseif nch == '=' then
					self._pos = self._pos + 1
					token = Token.OpGe
					tcontent = Token.OpGe
				else 
					token = Token.OpGt
					tcontent = Token.OpGt
				end
			elseif __s == '<' then
				local nch = self:peekChar()
				if nch == '<' then
					self._pos = self._pos + 1
					token = Token.OpShl
					tcontent = Token.OpShl
				elseif nch == '=' then
					self._pos = self._pos + 1
					token = Token.OpLe
					tcontent = Token.OpLe
				else 
					token = Token.OpLt
					tcontent = Token.OpLt
				end
			elseif __s == '=' then
				if '=' == self:peekChar() then
					self._pos = self._pos + 1
					token = Token.OpEq
					tcontent = Token.OpEq
				else 
					token = Token.OpAssign
					tcontent = Token.OpAssign
				end
			elseif __s == '~' then
				if '=' == self:peekChar() then
					self._pos = self._pos + 1
					token = Token.OpNe
					tcontent = Token.OpNe
				else 
					token = Token.OpWav
					tcontent = Token.OpWav
				end
			elseif __s == '!' then
				if '=' == self:peekChar() then
					self._pos = self._pos + 1
					token = Token.OpNb
					tcontent = Token.OpNb
				end
			elseif __s == ':' then
				if ':' == self:peekChar() then
					self._pos = self._pos + 1
					token = Token.SepLabel
					tcontent = Token.SepLabel
				else 
					token = Token.SepColon
					tcontent = Token.SepColon
				end
			else
				if CharSymbol[ch] then
					token = ch
					tcontent = ch
				elseif isDigit(ch) then
					token = Token.Number
					tcontent = self:oneNumber(ch)
				elseif ch == '_' or isLetter(ch) then
					tcontent = self:oneIdentifier()
					if tcontent:len() > 0 then
						token = ReservedWord[tcontent]
						if not token then
							token = Token.Identifier
						end
					end
				elseif self._pos >= self._chunk:len() then
					token = Token.Eof
					tcontent = ""
					self._pos = npos
				end
			end
			self._pmap:set(npos, token, tcontent, tpos, self._pos)
			if not advance then
				self._pos = npos
			end
			return token, tcontent, tpos, opos
		end
		function __ct:nextTokenKind(kind)
			local t, c, p, pp = self:peekToken(true)
			if t ~= kind then
				self._pos = pp
				self._err_msg = string.format("invalid token '%s'%s, when expected '%s'", t, (t == Token.Identifier) and (' ' .. c) or '', kind)
				error("")
			end
			return t, c, p
		end
		function __ct:nextPos()
			local _, _, p = self:peekToken(true)
			return p
		end
		function __ct:oneComment()
			local s, e = self._chunk:find('%[=*%[', self._pos + 1)
			if s == self._pos + 1 then
				local count = e - s - 1
				s, e = self._chunk:find(']' .. srep('=', count) .. ']', e + 1, true)
				if not e then
					self._err_msg = "unfinished long comment near '<eof>'"
					error("")
				end
			else 
				s, e = self._chunk:find('\n', self._pos + 1)
				e = (e and e - 1) or self._chunk:len()
			end
			if e > self._pos then
				self._pos = e
			end
		end
		function __ct:shortString(sep)
			local pos = self._pos
			while true do
				local ch = self:nextChar()
				if ch:len() <= 0 or (CharBlank[ch] and ch ~= ' ') then
					self._err_msg = "unfinished string"
					error("")
				elseif ch == '\\' and self:peekChar():len() > 0 then
					if (sep == '"' or sep == "'") and self:peekChar() == '(' then
						local s, e, prefix = pos, self._pos - 1, ''
						if self:charAt(pos) ~= sep then
							s = pos + 1
							prefix = sep
						end
						local t = sep == '"' and Token.StringExprD or Token.StringExprS
						return t, (s > e) and '' or (prefix .. self._chunk:sub(s, e) .. sep), s
					end
					self._pos = self._pos + 1
				elseif ch == sep then
					local s, e = pos, self._pos
					if self:charAt(pos) == sep then
						return Token.String, self._chunk:sub(s, e), pos
					else 
						s = s + 1
						return Token.String, (s >= e) and '' or (sep .. self._chunk:sub(s, e)), pos + 1
					end
				end
			end
		end
		function __ct:longString()
			local pos = self._pos
			local ecount = 0
			while self:peekChar() == '=' do
				self._pos = self._pos + 1
				ecount = ecount + 1
			end
			if self:peekChar() ~= '[' then
				self._err_msg = "invalid long string delimiter"
				error("")
			end
			local s, e = self._chunk:find(']' .. srep('=', ecount) .. ']', self._pos + 1, true)
			if not e then
				self._err_msg = "unfinished long string near '<eof>'"
				error("")
			end
			self._pos = e
			return self._chunk:sub(pos, e)
		end
		function __ct:oneNumber(pre_char)
			local pos = self._pos
			local ntype = 1
			local ch = self:peekChar()
			if pre_char == '0' and (ch == 'x' or ch == 'X') then
				self._pos = self._pos + 1
				ntype = 0
			elseif pre_char == '.' then
				ntype = 2
			end
			while true do
				ch = self:peekChar()
				if ch:len() <= 0 then
					if ntype == 0 and self._pos - pos > 1 then
						return self._chunk:sub(pos, self._pos)
					elseif ntype == 2 and self._pos > pos then
						return self._chunk:sub(pos, self._pos)
					elseif ntype == 1 or ntype == 3 then
						return self._chunk:sub(pos, self._pos)
					else 
						break
					end
				elseif ch == '_' then
					break
				elseif ntype == 0 then
					if isHex(ch) then
						self._pos = self._pos + 1
					elseif not isLetter(ch) then
						return self._chunk:sub(pos, self._pos)
					else 
						break
					end
				elseif ntype == 1 then
					if isDigit(ch) then
						self._pos = self._pos + 1
					elseif ch == '.' then
						self._pos = self._pos + 1
						ntype = 2
					elseif ch == 'e' or ch == 'E' then
						self._pos = self._pos + 1
						ntype = 3
						local nch = self:nextChar()
						if not (isDigit(nch) or (nch == '-' and isDigit(self:nextChar()))) then
							break
						end
					elseif not isLetter(ch) then
						return self._chunk:sub(pos, self._pos)
					else 
						break
					end
				elseif ntype == 2 then
					if isDigit(ch) then
						self._pos = self._pos + 1
					elseif (ch == 'e' or ch == 'E') then
						self._pos = self._pos + 1
						ntype = 3
						local nch = self:nextChar()
						if not (isDigit(nch) or (nch == '-' and isDigit(self:nextChar()))) then
							break
						end
					elseif not isLetter(ch) and ch ~= '.' then
						return self._chunk:sub(pos, self._pos)
					else 
						break
					end
				else 
					if isDigit(ch) then
						self._pos = self._pos + 1
					elseif not isLetter(ch) then
						return self._chunk:sub(pos, self._pos)
					else 
						break
					end
				end
			end
			self._err_msg = "malformed number"
			error("")
		end
		function __ct:oneIdentifier()
			local pos = self._pos
			while true do
				local ch = self:peekChar()
				if ch == '_' or isDigit(ch) or isLetter(ch) then
					self._pos = self._pos + 1
				else 
					return self._chunk:sub(pos, self._pos)
				end
			end
		end
		function __ct:oneLine()
			local _, e = self._chunk:find('\n', self._pos + 1, true)
			e = (e and e - 1) or self._chunk:len()
			local content = self._chunk:sub(self._pos + 1, e)
			self._pos = e
			return content
		end
		function __ct:skipSpacesComments()
			while true do
				local ch = self:nextChar()
				if CharBlank[ch] then
				elseif ch == '-' and self:peekChar() == '-' then
					self._pos = self._pos + 1
					self:oneComment()
				else 
					self._pos = self._pos - (ch:len() > 0 and 1 or 0)
					break
				end
			end
		end
		function __ct:charAt(i)
			if not (i < self._chunk:len()) then
				return ""
			end
			return schar(self._chunk:byte(i))
		end
		function __ct:peekChar()
			if not (self._pos < self._chunk:len()) then
				return ""
			end
			return schar(self._chunk:byte(self._pos + 1))
		end
		function __ct:nextChar()
			if not (self._pos < self._chunk:len()) then
				return ""
			end
			self._pos = self._pos + 1
			return schar(self._chunk:byte(self._pos))
		end
		function __ct:getLastError()
			return self._err_msg, self._pos
		end
		-- declare end
		local __imt = {
			__tostring = function(t) return "<struct Lexer" .. t.__ins_name .. ">" end,
			__index = function(t, k)
				local v = rawget(__ct, k)
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
			__newindex = function(t, k, v) if rawget(__ct, k) ~= nil then rawset(t, k, v) end end,
		}
		Lexer = setmetatable({}, {
			__tostring = function() return "<struct Lexer>" end,
			__index = function(t, k) local v = rawget(__ct, k); if v ~= nil then rawset(t, k, v); end return v end,
			__newindex = function(t, k, v) if v ~= nil and rawget(__ct, k) ~= nil then rawset(t, k, v) end end,
			__call = function(_, ...)
				local t = {}; t.__ins_name = tostring(t):sub(6)
				local ins = setmetatable(t, __imt)
				if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end
				return ins
			end,
		})
	end
	local Parser = { __tn = 'Parser', __tk = 'struct' }
	do
		local __ct = Parser
		__ct.__ct = __ct
		-- declare struct var and methods
		__ct._sub_mode = false
		__ct._scopes = QuickStack()
		__ct._lo_count = 0
		__ct._df_in = false
		__ct._err_msg = false
		__ct._pos = false
		__ct._blevel = 0
		__ct._fn_list = {  }
		__ct._fn_map = {  }
		__ct._fn_wrap = false
		function __ct:fReset(chunk)
			self._sub_mode = false
			self._scopes:reset()
			self._lo_count = 0
			self._df_in = false
			self._err_msg = false
			self._pos = false
			self._blevel = 0
			Lexer:reset(chunk)
			local t = self._fn_list
			if #t <= 0 then
				t[#t + 1] = self.stExport
				t[#t + 1] = self.stAssign
				t[#t + 1] = self.stFnCall
				t[#t + 1] = self.stIfElse
				t[#t + 1] = self.stGuard
				t[#t + 1] = self.stClassDef
				t[#t + 1] = self.stDo
				t[#t + 1] = self.stSwitch
				t[#t + 1] = self.stFor
				t[#t + 1] = self.stWhile
				t[#t + 1] = self.stRepeat
				t[#t + 1] = self.stImport
				t[#t + 1] = self.stFnDef
				t[#t + 1] = self.stLabel
				t[#t + 1] = self.stDefer
				t[#t + 1] = self.stLoopEnd
				t[#t + 1] = self.stBlockEnd
				t[#t + 1] = self.stShebang
				self._fn_map[Token.Identifier] = function(self)
					return self:stAssign() or self:stFnCall()
				end
				self._fn_map[Token.KwExport] = function(self)
					return self:stExport() or self:stAssign() or self:stFnDef() or self:stClassDef()
				end
				self._fn_map[Token.KwLocal] = function(self)
					return self:stExport() or self:stAssign() or self:stFnDef() or self:stClassDef()
				end
				self._fn_map[Token.SepLparen] = function(self)
					return self:stAssign() or self:stFnCall()
				end
			end
		end
		function __ct:fAsset(exp, err_msg, pos)
			if not exp then
				self._err_msg = err_msg
				if type(pos) == 'number' then
					self._pos = pos
				elseif type(pos) == 'boolean' then
					self._pos = Lexer:nextPos()
				else 
					self._pos = false
				end
				error("")
			end
		end
		function __ct:getLastError()
			local err_msg, pos = Lexer:getLastError()
			pos = math.max(0, self._pos or pos)
			return err_msg or self._err_msg, pos
		end
		function __ct:fnBodyStart()
			local t = self._scopes:incTop()
			t.scope = 'fn'
			t.df = nil
		end
		function __ct:fnBodyEnd()
			local t = self._scopes:decTop()
			return t and t.df or nil
		end
		function __ct:loBodyStart()
			self._lo_count = self._lo_count + 1
			local t = self._scopes:incTop()
			t.scope = 'lo'
			t.index = self._lo_count
		end
		function __ct:loBodyEnd(body)
			local t = self._scopes:decTop()
			if t.co and #body > 0 then
				local ot = body[#body]
				if ot.stype == 'return' or ot.stype == 'break' then
					body[#body] = { stype = 'do', body = { ot } }
				end
				body[#body + 1] = { stype = '::', { etype = "const", value = "__c" .. tostring(t.index), pos = 0 } }
			end
		end
		function __ct:swBodyStart()
			self._lo_count = self._lo_count + 1
			local t = self._scopes:incTop()
			t.scope = 'sw'
			t.index = self._lo_count
		end
		function __ct:swBodyEnd(body)
			local t = self._scopes:decTop()
			if t.br and #body > 0 then
				body.tail = { stype = '::', { etype = "const", value = "__c" .. tostring(t.index), pos = 0 } }
			end
		end
		function __ct:clBodyStart()
			local t = self._scopes:incTop()
			t.scope = 'cl'
			t.cname = nil
			t.sname = nil
			return t
		end
		function __ct:clBodyEnd()
			self._scopes:decTop()
		end
		function __ct:guBodyStart()
			local t = self._scopes:incTop()
			t.scope = 'gu'
			t.term = nil
		end
		function __ct:guBodyEnd()
			return self._scopes:decTop()
		end
		function __ct:isInFn()
			local array, count = self._scopes:dataOp()
			if not (count > 0) then
				return 
			end
			for i = count, 1, -1 do
				local t = array[i]
				if t.scope == 'fn' then
					return t
				end
			end
		end
		function __ct:isCurrentFn()
			local array, count = self._scopes:dataOp()
			if not (count > 0) then
				return 
			end
			local t = array[count]
			return t.scope == 'fn' and t
		end
		function __ct:isInLoop(token)
			local array, count = self._scopes:dataOp()
			if not (count > 0) then
				return 
			end
			for i = count, 1, -1 do
				local t = array[i]
				if t.scope == 'cl' or t.scope == 'fn' then
					return 
				elseif t.scope == 'lo' then
					return t
				elseif t.scope == 'sw' and token == Token.KwBreak then
					return t
				end
			end
		end
		function __ct:isInCls()
			local array, count = self._scopes:dataOp()
			if not (count > 0) then
				return 
			end
			for i = count, 1, -1 do
				local t = array[i]
				if t.scope == 'cl' then
					return t
				end
			end
		end
		function __ct:termInGuard()
			local array, count = self._scopes:dataOp()
			if count > 0 and array[count].scope == 'gu' then
				array[count].term = true
			end
		end
		function __ct:fnWrap()
			return self._fn_wrap
		end
		function __ct:fnMapList(t)
			for i, f in ipairs(self._fn_list) do
				local st = f(self)
				if st then
					self._fn_map[t] = f
					self._fn_wrap = st
					return self.fnWrap
				end
			end
			self._fn_wrap = false
			return self.fnWrap
		end
		function __ct:fParseBlock()
			self._blevel = self._blevel + 1
			local ast = {  }
			repeat
				local t, c, p = Lexer:peekToken()
				local st = (self._fn_map[t] or self:fnMapList(t))(self)
				if st then
					ast[#ast + 1] = st
					if st.stype == "return" then
						self:stSemi(ast, 1)
						t, c, p = Lexer:peekToken()
						if self._blevel == 1 then
							self:fAsset(t == Token.Eof, "'eof' expected after 'return'", p)
						else 
							self:fAsset(t == Token.SepRcurly or t == Token.KwCase or t == Token.KwDefault, "'}', 'case', 'default' expected after 'return'", p)
						end
						break
					end
				end
				st = self:stSemi(ast, math_huge) or st
			until not st or Token.Eof == Lexer:peekToken()
			self._blevel = self._blevel - 1
			if self._blevel == 0 then
				local t, c, p = Lexer:peekToken()
				self:fAsset(Token.Eof == t, "unexpected symbol near '" .. t .. "'", p)
			elseif not self._df_in and #ast > 0 and ast[#ast].stype ~= 'return' then
				local ft = self:isCurrentFn()
				if ft and ft.df then
					ast[#ast + 1] = { stype = 'raw', value = '__dr()' }
				end
			end
			return ast
		end
		function __ct:stSemi(ast, count)
			local i = 0
			while i < count and Token.SepSemi == Lexer:peekToken() do
				Lexer:nextTokenKind(Token.SepSemi)
				i = i + 1
			end
			if i > 0 then
				ast[#ast + 1] = { stype = ';' }
			end
			return i > 0
		end
		function __ct:stShebang()
			local t, c, p = Lexer:peekToken()
			if t == Token.SheBang then
				Lexer:nextTokenKind(t)
				return { stype = "#!", value = c, pos = p }
			end
		end
		function __ct:stExport()
			local t, c, p = Lexer:peekToken()
			if not (t == Token.KwExport or t == Token.KwLocal) then
				return 
			end
			Lexer:savePos()
			Lexer:nextTokenKind(t)
			if t == Token.KwExport and Token.OpMul == Lexer:peekToken() then
				t, c, p = Lexer:nextTokenKind(Token.OpMul)
				Lexer:clearPos()
				return { stype = "ex", attr = t, { etype = '*', value = c, pos = p } }
			end
			if not (Token.Identifier == Lexer:peekToken()) then
				Lexer:restorePos()
				return 
			end
			local nlist = self:etNameList()
			if not (#nlist > 0 and Token.OpAssign ~= Lexer:peekToken()) then
				Lexer:restorePos()
				return 
			end
			Lexer:clearPos()
			return { stype = "ex", attr = t, unp(nlist) }
		end
		function __ct:stAssign()
			Lexer:savePos()
			local attr = nil
			local t, c, p = Lexer:peekToken()
			if t == Token.KwExport or t == Token.KwLocal then
				Lexer:nextTokenKind(t)
				attr = t
			end
			t, c, p = Lexer:peekToken()
			if not (t == Token.Identifier or t == Token.SepLparen) then
				Lexer:restorePos()
				return 
			end
			local vlist = self:etVarList()
			local sub = nil
			t, c, p = Lexer:peekToken(true)
			if t == Token.OpAssign then
			elseif isBinOp(t) and not RelationalOp[t] and Token.OpAssign == Lexer:peekToken(true) then
				sub = t
				self:fAsset(Lexer:charAt(p + t:len()) == Token.OpAssign, "can not keep space between " .. t .. Token.OpAssign, p)
				self:fAsset(#vlist <= 1 or vlist[#vlist].etype == Token.SepDot, "tow much var on equal left")
			else 
				Lexer:restorePos()
				return 
			end
			Lexer:clearPos()
			local elist = self:etExprList("expect exp after assgin", nil, p)
			return { stype = '=', attr = attr, sub = sub, vlist, elist }
		end
		function __ct:stDo()
			if not (Token.KwDo == Lexer:peekToken()) then
				return 
			end
			Lexer:nextTokenKind(Token.KwDo)
			Lexer:nextTokenKind(Token.SepLcurly)
			local body = self:fParseBlock()
			Lexer:nextTokenKind(Token.SepRcurly)
			return { stype = "do", body = body }
		end
		function __ct:stIfElse()
			local t, c, p = Lexer:peekToken()
			if not (t == Token.KwIf) then
				return 
			end
			local out = {  }
			repeat
				local st = { sub = t }
				Lexer:nextTokenKind(t)
				st.cond = (t ~= Token.KwElse) and self:etExpr({  }, "expect condition after " .. t) or nil
				Lexer:nextTokenKind(Token.SepLcurly)
				st.body = self:fParseBlock()
				Lexer:nextTokenKind(Token.SepRcurly)
				out[#out + 1] = st
				t, c, p = Lexer:peekToken()
			until t ~= Token.KwElseIf and t ~= Token.KwElse
			return { stype = 'if', unp(out) }
		end
		function __ct:stGuard()
			if not (Token.KwGuard == Lexer:peekToken()) then
				return 
			end
			Lexer:nextTokenKind(Token.KwGuard)
			local cond = self:etExpr({  }, "expect condition after guard")
			Lexer:nextTokenKind(Token.KwElse)
			local _, _, p = Lexer:nextTokenKind(Token.SepLcurly)
			self:guBodyStart()
			local body = self:fParseBlock()
			self:fAsset(self:guBodyEnd().term, "'guard' body require return/goto/break/continue to transfer control")
			Lexer:nextTokenKind(Token.SepRcurly)
			return { stype = 'guard', cond = cond, body = body }
		end
		function __ct:stSwitch()
			if not (Token.KwSwitch == Lexer:peekToken()) then
				return 
			end
			Lexer:nextTokenKind(Token.KwSwitch)
			local sw_ret = { stype = "switch", cond = self:etExpr({  }, "expect condition after switch") }
			Lexer:nextTokenKind(Token.SepLcurly)
			self:swBodyStart()
			local df_count = 0
			while true do
				local t, c, p = Lexer:peekToken()
				if t == Token.KwCase then
					Lexer:nextTokenKind(t)
					self._sub_mode = 'case'
					local cond = self:etExprList("expect condition after case")
					self._sub_mode = false
					Lexer:nextTokenKind(Token.SepColon)
					local body = self:fParseBlock()
					sw_ret[#sw_ret + 1] = { cond = cond, body = body }
				elseif t == Token.KwDefault then
					if df_count <= 0 then
						df_count = df_count + 1
						Lexer:nextTokenKind(t)
						Lexer:nextTokenKind(Token.SepColon)
						local body = self:fParseBlock()
						sw_ret[#sw_ret + 1] = { body = body }
					else 
						self:fAsset(false, "too much default case in switch statement")
					end
				else 
					break
				end
			end
			self:fAsset(#sw_ret > 0, "switch require 'case' or 'default' in body")
			self:swBodyEnd(sw_ret)
			Lexer:nextTokenKind(Token.SepRcurly)
			return sw_ret
		end
		function __ct:stDefer()
			if not (Token.KwDefer == Lexer:peekToken()) then
				return 
			end
			local ft = self:isInFn()
			self:fAsset(ft, "defer only support function scope")
			self:fAsset(not self._df_in, "defer can not inside another defer")
			ft.df = "local __df={};local __dr=function() local __t=__df; for __i=#__t, 1, -1 do __t[__i]() end;end;"
			Lexer:nextTokenKind(Token.KwDefer)
			Lexer:nextTokenKind(Token.SepLcurly)
			self._df_in = true
			local body = self:fParseBlock()
			self._df_in = false
			Lexer:nextTokenKind(Token.SepRcurly)
			return { stype = 'defer', body = body }
		end
		function __ct:stFor()
			if not (Token.KwFor == Lexer:peekToken()) then
				return 
			end
			Lexer:nextTokenKind(Token.KwFor)
			local name = self:etNameList()
			self:fAsset(name[#name].value ~= Token.Vararg, "invalid name list after for")
			local sub = nil
			if #name == 1 then
				local t, _, p = Lexer:peekToken(true)
				self:fAsset(t == Token.OpAssign or t == Token.KwIn, "invalid token '" .. t .. "', expected '=' or 'in'", p)
				sub = t
			else 
				sub = Lexer:nextTokenKind(Token.KwIn)
			end
			local step = self:etExprList()
			Lexer:nextTokenKind(Token.SepLcurly)
			self:loBodyStart()
			local body = self:fParseBlock()
			self:loBodyEnd(body)
			Lexer:nextTokenKind(Token.SepRcurly)
			return { stype = 'for', sub = sub, name = name, step = step, body = body }
		end
		function __ct:stWhile()
			if not (Token.KwWhile == Lexer:peekToken()) then
				return 
			end
			Lexer:nextTokenKind(Token.KwWhile)
			local cond = self:etExpr({  }, "expect condition after while")
			Lexer:nextTokenKind(Token.SepLcurly)
			self:loBodyStart()
			local body = self:fParseBlock()
			self:loBodyEnd(body)
			Lexer:nextTokenKind(Token.SepRcurly)
			return { stype = 'while', cond = cond, body = body }
		end
		function __ct:stRepeat()
			if not (Token.KwRepeat == Lexer:peekToken()) then
				return 
			end
			Lexer:nextTokenKind(Token.KwRepeat)
			Lexer:nextTokenKind(Token.SepLcurly)
			self:loBodyStart()
			local body = self:fParseBlock()
			self:loBodyEnd(body)
			Lexer:nextTokenKind(Token.SepRcurly)
			Lexer:nextTokenKind(Token.KwUntil)
			local cond = self:etExpr({  }, "expect condition after until")
			return { stype = "repeat", cond = cond, body = body }
		end
		function __ct:stFnCall()
			local t, c, p = Lexer:peekToken()
			if not (t == Token.Identifier or t == Token.SepLparen) then
				return 
			end
			Lexer:savePos()
			local expr = self:etExpr({  })
			if not (expr and #expr > 0 and expr[#expr].etype == '(') then
				Lexer:restorePos()
				return 
			end
			Lexer:clearPos()
			return { stype = '(', expr }
		end
		function __ct:stImport()
			if not (Token.KwImport == Lexer:peekToken()) then
				return 
			end
			Lexer:nextTokenKind(Token.KwImport)
			if Token.String == Lexer:peekToken() then
				local t, c, p = Lexer:nextTokenKind(Token.String)
				return { stype = 'import', lib = { etype = 'const', value = c, pos = p } }
			end
			local vlist = self:etNameList()
			self:fAsset(#vlist > 0 and (vlist[#vlist].value ~= Token.Vararg), "please provide valid var name after import")
			Lexer:nextTokenKind(Token.KwFrom)
			local t, c, p = Lexer:peekToken(true)
			self:fAsset(t == Token.String or t == Token.Identifier, "expect lib type string or variable")
			local out = { stype = "import", lib = { etype = (t == Token.String and 'const' or 'var'), value = c, pos = p }, vlist }
			if not (Token.SepLcurly == Lexer:peekToken()) then
				self:fAsset(#vlist == 1, "import too much var", p)
				return out
			end
			Lexer:nextTokenKind(Token.SepLcurly)
			local plist = {  }
			while Token.Identifier == Lexer:peekToken() do
				plist[#plist + 1] = self:etExpr({  }, "expect sub lib name after from")
				if Token.SepComma == Lexer:peekToken() then
					Lexer:nextTokenKind(Token.SepComma)
					self:fAsset(#vlist > #plist, "from too much sub lib")
				else 
					self:fAsset(#vlist == #plist, "from too few sub lib")
					break
				end
			end
			if #plist > 0 then
				self:fAsset(#vlist == #plist, "import too much/few sub lib")
			end
			out[#out + 1] = plist
			Lexer:nextTokenKind(Token.SepRcurly)
			return out
		end
		function __ct:stFnDef(only_name)
			Lexer:savePos()
			local attr = nil
			local t, c, p = Lexer:peekToken()
			if t == Token.KwLocal or t == Token.KwExport then
				attr = t == Token.KwExport and t or nil
				Lexer:nextTokenKind(t)
			end
			if not (Token.KwFn == Lexer:peekToken()) then
				Lexer:restorePos()
				return 
			end
			Lexer:clearPos()
			Lexer:nextTokenKind(Token.KwFn)
			local name = self:etFnName(only_name)
			Lexer:nextTokenKind(Token.SepLparen)
			local args = self:etNameList()
			Lexer:nextTokenKind(Token.SepRparen)
			Lexer:nextTokenKind(Token.SepLcurly)
			self:fnBodyStart()
			local body = self:fParseBlock()
			Lexer:nextTokenKind(Token.SepRcurly)
			return { stype = "fn", attr = attr, name = name, args = args, body = body, df = self:fnBodyEnd() }
		end
		function __ct:stLabel()
			if not (Token.SepLabel == Lexer:peekToken()) then
				return 
			end
			Lexer:nextTokenKind(Token.SepLabel)
			local t, c, p = Lexer:nextTokenKind(Token.Identifier)
			Lexer:nextTokenKind(Token.SepLabel)
			return { stype = '::', { etype = "const", value = c, pos = p } }
		end
		function __ct:stLoopEnd()
			local t, c, p = Lexer:peekToken()
			if not (t == Token.KwBreak or t == Token.KwContinue) then
				return 
			end
			local lt = self:isInLoop(t)
			self:fAsset(lt, t .. " not in loop" .. (t == Token.KwBreak and " or switch" or ""))
			self:termInGuard()
			Lexer:nextTokenKind(t)
			local nt = Lexer:peekToken()
			self:fAsset(nt == Token.SepRcurly or nt == Token.KwCase or nt == Token.KwDefault, "'}' expected after 'break' or 'continue'")
			if t == Token.KwBreak then
				if lt.scope == 'sw' then
					lt.br = true
					return { stype = 'goto', { etype = "const", value = "__c" .. tostring(lt.index), pos = p } }
				else 
					return { stype = 'break' }
				end
			else 
				lt.co = true
				return { stype = 'goto', { etype = "const", value = "__c" .. tostring(lt.index), pos = p } }
			end
		end
		function __ct:stBlockEnd()
			local t, c, p = Lexer:peekToken()
			if not (t == Token.KwReturn or t == Token.KwGoto) then
				return 
			end
			self:termInGuard()
			if t == Token.KwReturn then
				Lexer:nextTokenKind(t)
				local ft = self:isInFn()
				local list = self:etExprList(false, ft and ft.df and { etype = 'const', value = '__dr()', pos = p } or nil)
				if self._df_in then
					self:fAsset(list == nil, "defer block can not return value", p)
				end
				return { stype = 'return', unp(list) }
			else 
				Lexer:nextTokenKind(t)
				t, c, p = Lexer:nextTokenKind(Token.Identifier)
				return { stype = 'goto', { etype = "const", value = c, pos = p } }
			end
		end
		function __ct:stClassDef()
			Lexer:savePos()
			local attr = nil
			local t, c, p = Lexer:peekToken()
			if t == Token.KwLocal or t == Token.KwExport then
				Lexer:nextTokenKind(t)
				attr = (t == Token.KwExport) and t or nil
			end
			local st, sc, sp = Lexer:peekToken()
			if not (st == Token.KwClass or st == Token.KwStruct or st == Token.KwExtension) then
				Lexer:restorePos()
				return 
			end
			Lexer:clearPos()
			Lexer:nextTokenKind(st)
			t, c, p = Lexer:nextTokenKind(Token.Identifier)
			local out = { stype = st, attr = attr, name = { etype = 'var', value = c, pos = p } }
			local scope = self:clBodyStart()
			scope.cname = c
			if Token.SepColon == Lexer:peekToken() then
				self:fAsset(st ~= Token.KwStruct, "struct can not inherit from super", true)
				Lexer:nextTokenKind(Token.SepColon)
				t, c, p = Lexer:nextTokenKind(Token.Identifier)
				out.super = { etype = 'var', value = c, pos = p }
				scope.sname = c
			end
			Lexer:nextTokenKind(Token.SepLcurly)
			repeat
				t, c, p = Lexer:peekToken()
				local __s = t
				if __s == Token.KwStatic then
					Lexer:nextTokenKind(t)
					self:fAsset(Token.KwFn == Lexer:peekToken(), "expect function definition after " .. t)
					local expr = self:stFnDef(true)
					expr.attr = t
					out[#out + 1] = expr
				elseif __s == Token.KwFn then
					out[#out + 1] = self:stFnDef(true)
				elseif __s == Token.Identifier then
					Lexer:nextTokenKind(t)
					Lexer:nextTokenKind(Token.OpAssign)
					self._sub_mode = 'class'
					out[#out + 1] = { stype = '=', { etype = 'const', value = c, pos = p }, self:etExpr({  }, "expect expr in variable definition") }
					self._sub_mode = false
				elseif __s == Token.SepRcurly then
				else
					self:fAsset(false, "invalid token " .. t .. " in " .. st .. " definition")
				end
			until t == Token.SepRcurly
			self:clBodyEnd()
			Lexer:nextTokenKind(Token.SepRcurly)
			return out
		end
		function __ct:etExpr(out, force_errmsg)
			out.etype = 'exp'
			local to_break = false
			repeat
				local t, c, p = Lexer:peekToken()
				local __s = t
				if __s == Token.KwNil or __s == Token.KwFalse or __s == Token.KwTrue or __s == Token.Vararg or __s == Token.Number or __s == Token.String then
					Lexer:nextTokenKind(t)
					out[#out + 1] = { etype = "const", value = c, pos = p }
				elseif __s == Token.StringExprD or __s == Token.StringExprS then
					Lexer:nextTokenKind(t)
					while true do
						if c:len() > 0 then
							out[#out + 1] = { etype = "const", value = c, pos = p }
							out[#out + 1] = { etype = "binop", value = '..', pos = p }
						end
						out[#out + 1] = { etype = "const", value = "tostring", pos = p }
						self:etPrefixExpr(out)
						t, c, p = Lexer:shortString(t == Token.StringExprD and '"' or "'")
						if t == Token.String then
							if c:len() > 0 then
								out[#out + 1] = { etype = "binop", value = '..', pos = p }
								out[#out + 1] = { etype = "const", value = c, pos = p }
							end
							break
						else 
							out[#out + 1] = { etype = "binop", value = '..', pos = p }
						end
					end
				elseif __s == Token.SepLcurly then
					out[#out + 1] = self:etFnAnonymous() or self:etTableConstructor()
				elseif __s == Token.KwFn then
					out[#out + 1] = self:etFnNoName()
				elseif __s == Token.OpNot or __s == Token.OpNen or __s == Token.OpWav then
					Lexer:nextTokenKind(t)
					local i = #out
					if t == Token.OpWav and i > 0 and (out[i].etype == 'const' or out[i].etype == 'var') then
						out[i + 1] = { etype = "binop", value = c, pos = p }
					else 
						out[i + 1] = { etype = "unop", value = c, pos = p }
					end
					self:etExpr(out, "expect exp after " .. t)
				else
					if t == Token.OpMinus and (#out <= 0 or out[#out].etype == 'binop') then
						Lexer:nextTokenKind(t)
						out[#out + 1] = { etype = "unop", value = c, pos = p }
						self:etExpr(out, "expect exp after " .. t)
					elseif isBinOp(t) then
						self:fAsset(#out > 0, "invalid expr begin with " .. t)
						Lexer:nextTokenKind(t)
						out[#out + 1] = { etype = "binop", value = c, pos = p }
						out.rlop = out.rlop or RelationalOp[t] or LogicalOp[t] or nil
						self:etExpr(out, "expect exp after " .. t)
					else 
						local ncount = #out
						self:etPrefixExpr(out)
						self:etPrefixExprFinish(out)
						if #out <= ncount then
							to_break = true
						end
					end
				end
			until to_break or not isBinOp(Lexer:peekToken())
			if #out > 0 then
				return #out == 1 and out[1] or out
			elseif force_errmsg then
				self:fAsset(false, force_errmsg)
			end
		end
		function __ct:etPrefixExpr(out)
			local o_out = #out
			local t, c, p = Lexer:peekToken()
			local __s = t
			if __s == Token.Identifier then
				Lexer:nextTokenKind(t)
				if #out == o_out and (c == 'Self' or c == 'Super') then
					local cls = self:isInCls()
					if cls then
						if c == 'Self' then
							out[#out + 1] = { etype = "const", value = cls.cname, pos = p }
						else 
							if cls.sname then
								out[#out + 1] = { etype = "const", value = cls.sname, pos = p }
							else 
								out[#out + 1] = { etype = "const", value = cls.cname, pos = p }
								out[#out + 1] = { etype = '.', { etype = 'const', value = '__st', pos = p } }
							end
						end
						return 
					end
				end
				local etype = (#out == 0 or out[#out].etype == 'binop') and 'var' or 'const'
				out[#out + 1] = { etype = etype, value = c, pos = p }
			elseif __s == Token.SepLparen then
				Lexer:nextTokenKind(t)
				local expr = self:etExpr({  }, "expect exp after " .. t)
				Lexer:nextTokenKind(Token.SepRparen)
				out[#out + 1] = { etype = '(', expr }
			end
		end
		function __ct:etPrefixExprFinish(out)
			local to_break = false
			repeat
				local t, c, p = Lexer:peekToken()
				local __s = t
				if __s == Token.SepLbreak then
					self:fAsset(#out > 0, "expect prefix expr before " .. t)
					Lexer:nextTokenKind(t)
					local expr = self:etExpr({  }, "expect exp after " .. t)
					Lexer:nextTokenKind(Token.SepRbreak)
					out[#out + 1] = { etype = '[', expr }
				elseif __s == Token.SepDot then
					self:fAsset(#out > 0, "expect prefix expr before " .. t)
					Lexer:nextTokenKind(t)
					t, c, p = Lexer:peekToken()
					local __s = t
					if __s == Token.Identifier then
						Lexer:nextTokenKind(t)
						out[#out + 1] = { etype = '.', { etype = 'const', value = c, pos = p } }
					elseif __s == Token.String then
						Lexer:nextTokenKind(t)
						out[#out + 1] = { etype = '(', { etype = 'const', value = c, pos = p } }
					elseif __s == Token.SepLcurly then
						local expr = self:etTableConstructor()
						out[#out + 1] = { etype = '(', expr }
					else
						self:fAsset(false, 'expect identifier or [string | table] after ' .. t)
					end
				elseif __s == Token.SepColon then
					self:fAsset(#out > 0, "expect prefix expr before " .. t)
					if self._sub_mode == 'case' and CharBlank[Lexer:charAt(p + 1)] then
						return 
					else 
						Lexer:nextTokenKind(t)
						t, c, p = Lexer:nextTokenKind(Token.Identifier)
						out[#out + 1] = { etype = ':', { etype = 'const', value = c, pos = p } }
						out[#out + 1] = { etype = '(', self:etArgs() }
					end
				elseif __s == Token.SepLparen then
					self:fAsset(#out > 0, "expect prefix expr before " .. t)
					out[#out + 1] = { etype = '(', self:etArgs() }
				else
					to_break = true
				end
			until to_break
		end
		function __ct:etArgs()
			local t, c, p = Lexer:peekToken()
			self:fAsset(t == Token.SepLparen, "expect args begin with " .. Token.SepLparen)
			Lexer:nextTokenKind(t)
			local list = (Token.SepRparen ~= Lexer:peekToken()) and self:etExprList()
			Lexer:nextTokenKind(Token.SepRparen)
			return unp(list)
		end
		function __ct:etFnNoName()
			self:fAsset(self._sub_mode ~= 'class', "can not define function in class/struct/extension variable definition")
			Lexer:nextTokenKind(Token.KwFn)
			Lexer:nextTokenKind(Token.SepLparen)
			local args = self:etNameList()
			Lexer:nextTokenKind(Token.SepRparen)
			Lexer:nextTokenKind(Token.SepLcurly)
			self:fnBodyStart()
			local body = self:fParseBlock()
			Lexer:nextTokenKind(Token.SepRcurly)
			return { etype = 'fn', args = args, body = body, df = self:fnBodyEnd() }
		end
		function __ct:etFnAnonymous()
			Lexer:savePos()
			Lexer:nextTokenKind(Token.SepLcurly)
			local args = self:etNameList()
			if not (Token.KwIn == Lexer:peekToken()) then
				Lexer:restorePos()
				return 
			end
			self:fAsset(self._sub_mode ~= 'class', "can not define function in class/struct/extension variable definition")
			Lexer:clearPos()
			Lexer:nextTokenKind(Token.KwIn)
			self:fnBodyStart()
			local body = self:fParseBlock()
			Lexer:nextTokenKind(Token.SepRcurly)
			return { etype = 'fn', args = args, body = body, df = self:fnBodyEnd() }
		end
		function __ct:etTableConstructor()
			Lexer:nextTokenKind(Token.SepLcurly)
			local expr = { etype = "{" }
			while Token.SepRcurly ~= Lexer:peekToken() do
				expr[#expr + 1] = self:etField()
				local t, c, p = Lexer:peekToken()
				if t == Token.SepComma or t == Token.SepSemi then
					Lexer:nextTokenKind(t)
				else 
					break
				end
			end
			Lexer:nextTokenKind(Token.SepRcurly)
			return expr
		end
		function __ct:etField()
			local t, c, p = Lexer:peekToken()
			local __s = t
			if __s == Token.SepLbreak then
				Lexer:nextTokenKind(t)
				local expr = { bkey = self:etExpr({  }, "expect exp after " .. t) }
				Lexer:nextTokenKind(Token.SepRbreak)
				Lexer:nextTokenKind(Token.OpAssign)
				expr.value = self:etExpr({  }, "expect exp after " .. Token.OpAssign)
				return expr
			elseif __s == Token.OpAssign then
				Lexer:nextTokenKind(t)
				t, c, p = Lexer:nextTokenKind(Token.Identifier)
				return { nkey = { etype = 'var', value = c, pos = p } }
			elseif __s == Token.Identifier or __s == Token.Number or __s == Token.String then
				Lexer:savePos()
				Lexer:nextTokenKind(t)
				if Token.OpAssign == Lexer:peekToken() then
					Lexer:clearPos()
					Lexer:nextTokenKind(Token.OpAssign)
					local expr = { value = self:etExpr({  }, "expect exp after " .. Token.OpAssign) }
					if t == Token.Identifier then
						expr.vkey = { etype = 'const', value = c, pos = p }
					else 
						expr.bkey = { etype = 'const', value = c, pos = p }
					end
					return expr
				else 
					Lexer:restorePos()
				end
			end
			return { value = self:etExpr({  }, "expect exp in table field") }
		end
		function __ct:etNameList()
			local out = {  }
			while true do
				local t, c, p = Lexer:peekToken()
				local __s = t
				if __s == Token.Identifier then
					Lexer:nextTokenKind(t)
					out[#out + 1] = { etype = "const", value = c, pos = p }
				elseif __s == Token.Vararg then
					Lexer:nextTokenKind(t)
					out[#out + 1] = { etype = "const", value = c, pos = p }
					return out
				else
					return out
				end
				if Token.SepComma == Lexer:peekToken() then
					Lexer:nextTokenKind(Token.SepComma)
				else 
					return out
				end
			end
			return out
		end
		function __ct:etExprList(force_msg, extra, pos)
			local out = {  }
			while true do
				local expr = self:etExpr({  })
				out[#out + 1] = expr
				if expr and Token.SepComma == Lexer:peekToken() then
					Lexer:nextTokenKind(Token.SepComma)
				else 
					break
				end
			end
			if extra then
				out[#out + 1] = extra
			end
			if #out > 0 then
				return out
			elseif force_msg then
				self:fAsset(false, force_msg, pos)
			end
		end
		function __ct:etVarList()
			Lexer:savePos()
			local out = {  }
			while true do
				local t, c, p = Lexer:peekToken()
				if not (t == Token.Identifier) then
					break
				end
				local expr = { etype = "exp" }
				self:etPrefixExpr(expr)
				self:etPrefixExprFinish(expr)
				local etype = expr[#expr].etype
				if not (etype == 'var' or etype == "[" or etype == ".") then
					break
				end
				out[#out + 1] = #expr == 1 and expr[1] or expr
				if not (Token.SepComma == Lexer:peekToken()) then
					break
				end
				Lexer:nextTokenKind(Token.SepComma)
			end
			if #out > 0 then
				Lexer:clearPos()
				return out
			else 
				Lexer:restorePos()
			end
		end
		function __ct:etFnName(only_name)
			local t, c, p = Lexer:nextTokenKind(Token.Identifier)
			local out = { etype = "exp", { etype = "var", value = c, pos = p } }
			if not only_name then
				while Token.SepDot == Lexer:peekToken() do
					Lexer:nextTokenKind(Token.SepDot)
					t, c, p = Lexer:nextTokenKind(Token.Identifier)
					out[#out + 1] = { etype = '.', { etype = 'const', value = c, pos = p } }
				end
				if Token.SepColon == Lexer:peekToken() then
					Lexer:nextTokenKind(Token.SepColon)
					t, c, p = Lexer:nextTokenKind(Token.Identifier)
					out[#out + 1] = { etype = ':', { etype = 'const', value = c, pos = p } }
				end
			end
			if #out == 1 then
				return out[1]
			else 
				out[1].etype = 'var'
				return out
			end
		end
		-- declare end
		local __imt = {
			__tostring = function(t) return "<struct Parser" .. t.__ins_name .. ">" end,
			__index = function(t, k)
				local v = rawget(__ct, k)
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
			__newindex = function(t, k, v) if rawget(__ct, k) ~= nil then rawset(t, k, v) end end,
		}
		Parser = setmetatable({}, {
			__tostring = function() return "<struct Parser>" end,
			__index = function(t, k) local v = rawget(__ct, k); if v ~= nil then rawset(t, k, v); end return v end,
			__newindex = function(t, k, v) if v ~= nil and rawget(__ct, k) ~= nil then rawset(t, k, v) end end,
			__call = function(_, ...)
				local t = {}; t.__ins_name = tostring(t):sub(6)
				local ins = setmetatable(t, __imt)
				if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end
				return ins
			end,
		})
	end
	local function parse(content)
		Parser:fReset(content)
		local ret, ast = pcall(Parser.fParseBlock, Parser)
		if ret then
			return true, { content = content, ast = ast }
		else 
			local err_msg, pos = Parser:getLastError()
			return false, { content = content, pos = pos, err_msg = (err_msg or ast) }
		end
	end
	MoocLib.parser = { parse = parse }; package.loaded['moocscript.parser'] = MoocLib.parser
end
MoocLib = MoocLib or nil
do
	local Utils = require("moocscript.utils")
	local assert = assert
	local type = type
	local srep = string.rep
	local ipairs = ipairs
	local mathmax = math.max
	local Out = { __tn = 'Out', __tk = 'class', __st = nil }
	do
		local __st = nil
		local __ct = Out
		__ct.__ct = __ct
		__ct.isKindOf = function(c, a) return a and c and ((c.__ct == a) or (c.__st and c.__st:isKindOf(a))) or false end
		-- declare class var and methods
		__ct._indent = 0
		__ct._changeLine = false
		__ct._output = {  }
		__ct._inline = 0
		function __ct:reset()
			self._indent = 0
			self._changeLine = false
			self._output = {  }
			self._inline = 0
		end
		function __ct:incIndent()
			self._indent = self._indent + 1
		end
		function __ct:decIndent()
			self._indent = self._indent - 1
		end
		function __ct:changeLine()
			self._changeLine = true
		end
		function __ct:pushInline()
			self._inline = self._inline + 1
		end
		function __ct:popInline()
			self._inline = self._inline - 1
		end
		function __ct:append(str, same_line)
			assert(type(str) == "string", "Invalid input")
			local t = self._output
			same_line = same_line or (self._inline > 0)
			if same_line and not self._changeLine then
				local i = mathmax(#t, 1)
				t[i] = (t[i] or "") .. str
			else 
				self._changeLine = false
				t[#t + 1] = (self._indent > 0 and srep("\t", self._indent) or "") .. str
			end
		end
		-- declare end
		local __imt = {
			__tostring = function(t) return "<class Out" .. t.__ins_name .. ">" end,
			__index = function(t, k)
				local v = __ct[k]
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
		}
		setmetatable(__ct, {
			__tostring = function() return "<class Out>" end,
			__index = function(t, k)
				local v = __st and __st[k]
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
			__call = function(_, ...)
				local t = {}; t.__ins_name = tostring(t):sub(6)
				local ins = setmetatable(t, __imt)
				if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end
				return ins
			end,
		})
	end
	local _global_names = Utils.set({ "_G", "fengari", "js", "_VERSION", "_ENV", "assert", "collectgarbage", "coroutine", "debug", "dofile", "error", "getfenv", "getmetatable", "io", "ipairs", "jit", "load", "loadfile", "loadstring", "math", "module", "next", "os", "package", "pairs", "pcall", "print", "rawequal", "rawget", "rawlen", "rawset", "require", "select", "setfenv", "setmetatable", "string", "table", "tonumber", "tostring", "type", "unpack", "xpcall", "nil", "true", "false" })
	local _scope_global = { otype = "gl", vars = _global_names }
	local _scope_proj = { otype = "pj", vars = {  } }
	local Ctx = { __tn = 'Ctx', __tk = 'class', __st = nil }
	do
		local __st = nil
		local __ct = Ctx
		__ct.__ct = __ct
		__ct.isKindOf = function(c, a) return a and c and ((c.__ct == a) or (c.__st and c.__st:isKindOf(a))) or false end
		-- declare class var and methods
		__ct.config = false
		__ct.ast = false
		__ct.content = false
		__ct.scopes = false
		__ct.err_info = false
		__ct.last_pos = 0
		function __ct:reset(config, ast, content)
			self.config = config
			self.ast = ast
			self.content = content
			self.scopes = { index = 3, _scope_global, _scope_proj, { otype = "fi", vars = config.fi_scope or {  } } }
			self.err_info = false
			self.last_pos = 0
		end
		function __ct:pushScope(ot, exp)
			local t = self.scopes
			t.index = t.index + 1
			local tn = t[t.index] or {  }
			tn.otype = ot
			tn.vars = {  }
			tn.exp = exp
			t[t.index] = tn
		end
		function __ct:popScope()
			local t = self.scopes
			t.index = t.index - 1
		end
		function __ct:globalInsert(n)
			local t = self.scopes
			t[2].vars[n] = true
		end
		function __ct:localInsert(n)
			local t = self.scopes
			t[t.index].vars[n] = true
		end
		function __ct:checkName(e, only_check)
			if e and e.etype == 'exp' then
				e = e[1]
			end
			if e and e.etype == 'var' then
				local n = e.value
				local t = self.scopes
				for i = t.index, 1, -1 do
					if t[i].vars[n] or t[i].vars["*"] then
						return true
					end
				end
				if not only_check then
					self:errorPos("undefined variable", e.pos - 1)
				end
			end
		end
		function __ct:errorPos(err_msg, pos)
			if self.err_info then
				return 
			end
			pos = pos or mathmax(0, self.last_pos - 1)
			self.err_info = { err_msg = err_msg, pos = pos }
			error('')
		end
		function __ct:updatePos(pos)
			if type(pos) == "number" and not self.err_info then
				self.last_pos = pos
			end
		end
		-- declare end
		local __imt = {
			__tostring = function(t) return "<class Ctx" .. t.__ins_name .. ">" end,
			__index = function(t, k)
				local v = __ct[k]
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
		}
		setmetatable(__ct, {
			__tostring = function() return "<class Ctx>" end,
			__index = function(t, k)
				local v = __st and __st[k]
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
			__call = function(_, ...)
				local t = {}; t.__ins_name = tostring(t):sub(6)
				local ins = setmetatable(t, __imt)
				if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end
				return ins
			end,
		})
	end
	local _cls_metafns = Utils.set({ "__tostring", "__index", "__newindex", "__call", "__add", "__band", "__bnot", "__bor", "__bxor", "__close", "__concat", "__div", "__eq", "__idiv", "__le", "__len", "__pairs", "__ipairs", "__lt", "__metatable", "__mod", "__mode", "__mul", "__name", "__pow", "__shl", "__shr", "__sub", "__unm" })
	local _map_binop = { ['!='] = '~=' }
	local M = { __tn = 'M', __tk = 'class', __st = nil }
	do
		local __st = nil
		local __ct = M
		__ct.__ct = __ct
		__ct.isKindOf = function(c, a) return a and c and ((c.__ct == a) or (c.__st and c.__st:isKindOf(a))) or false end
		-- declare class var and methods
		__ct.ctx = false
		__ct.out = false
		__ct.exfn = {  }
		__ct.stfn = {  }
		function __ct:reset(ctx, out)
			self.ctx = ctx
			self.out = out
		end
		function __ct:trExpr(t)
			assert(type(t) == "table", "Invalid expr type")
			local ctx = self.ctx
			local out = self.out
			local etype = t.etype
			if etype == "exp" then
				for _, v in ipairs(t) do
					self:trExpr(v)
				end
				return 
			end
			local func = self.exfn[etype]
			if func then
				func(self, t)
				return 
			end
			local __s = etype
			if __s == "var" then
				func = function(_, t)
					ctx:checkName(t)
					out:append(t.value, true)
				end
			elseif __s == "const" then
				func = function(_, t)
					out:append(t.value, true)
				end
			elseif __s == "{" then
				func = self.trEtTblDef
			elseif __s == "fn" then
				func = self.trEtFnOnly
			elseif __s == "(" then
				func = self.trEtPara
			elseif __s == "." then
				func = function(_, t)
					out:append("." .. t[1].value, true)
				end
			elseif __s == ":" then
				func = self.trEtColon
			elseif __s == "[" then
				func = self.trEtSquare
			elseif __s == 'unop' then
				func = function(_, t)
					local v = t.value == 'not' and 'not ' or t.value
					out:append(v, true)
				end
			elseif __s == 'binop' then
				func = function(_, t)
					out:append(' ' .. (_map_binop[t.value] or t.value) .. ' ', true)
				end
			else
				ctx:errorPos("Invalid expr etype near " .. (etype or "unknown"))
				return 
			end
			self.exfn[etype] = func
			func(self, t)
		end
		function __ct:trStatement(ast)
			local ctx = self.ctx
			local out = self.out
			local stfn = self.stfn
			local index = 0
			while true do
				index = index + 1
				if index > #ast then
					break
				end
				local t = ast[index]
				local stype = t.stype
				local func = stfn[stype]
				if t.pos then
					ctx:updatePos(t.pos)
				end
				if func then
					func(self, t)
				else 
					local __s = stype
					if __s == "import" then
						func = self.trStImport
					elseif __s == "fn" then
						func = self.trStFnDef
					elseif __s == "(" then
						func = self.trStCall
					elseif __s == "class" then
						func = self.trStClass
					elseif __s == "struct" then
						func = self.trStStruct
					elseif __s == "extension" then
						func = self.trStExtension
					elseif __s == "ex" then
						func = self.trStExport
					elseif __s == "=" then
						func = self.trAssign
					elseif __s == "return" then
						func = self.trStReturn
					elseif __s == "defer" then
						func = self.trStDefer
					elseif __s == "if" then
						func = self.trStIfElse
					elseif __s == "switch" then
						func = self.trStSwitch
					elseif __s == "guard" then
						func = self.trStGuard
					elseif __s == "break" then
						func = self.trStBreak
					elseif __s == "goto" or __s == "::" then
						func = self.trStGotoLabel
					elseif __s == "for" then
						func = self.trStFor
					elseif __s == "while" then
						func = self.trStWhile
					elseif __s == "repeat" then
						func = self.trStRepeat
					elseif __s == "do" then
						func = self.trStDo
					elseif __s == "#!" then
						func = function(self, t)
							if self.ctx.config.shebang and t.value then
								self.out:append(t.value)
							end
						end
					elseif __s == ';' then
						func = function(self, _)
							t = out._output
							local i = mathmax(#t, 1)
							t[i] = (t[i] or "") .. ';'
						end
					elseif __s == 'raw' then
						func = function(self, t)
							self.out:append(t.value)
						end
					else
						ctx:errorPos("Invalid stype near " .. (stype or "uknown stype"))
						return 
					end
					stfn[stype] = func
					func(self, t)
				end
				out:changeLine()
			end
		end
		function __ct:trEtName(t)
			local ctx = self.ctx
			local n = ''
			if t.etype == 'exp' and #t > 0 then
				local name = ''
				for i, v in ipairs(t) do
					if i == 1 then
						n = v.value
						name = n
					else 
						name = name .. v.etype .. v[1].value
					end
				end
				return name, n
			elseif t.etype then
				return t.value, t.value
			end
		end
		function __ct:trEtPara(t)
			assert(t.etype == "(", "Invalid op (")
			local out = self.out
			out:pushInline()
			out:append("(", true)
			for i, e in ipairs(t) do
				if i > 1 then
					out:append(", ", true)
				end
				self:trExpr(e)
			end
			out:append(")", true)
			out:popInline()
		end
		function __ct:trEtColon(t)
			assert(t.etype == ":", "Invalid op =")
			self.out:append(":", true)
			for i, e in ipairs(t) do
				self:trExpr(e)
			end
		end
		function __ct:trEtSquare(t)
			assert(t.etype == "[", "Invalid op [")
			local out = self.out
			out:pushInline()
			out:append("[", true)
			for _, e in ipairs(t) do
				self:trExpr(e)
			end
			out:append("]", true)
			out:popInline()
		end
		function __ct:trEtTblDef(t)
			assert(t.etype == "{", "Invalid etype table def")
			local ctx = self.ctx
			local out = self.out
			out:append("{ ")
			for i, e in ipairs(t) do
				if e.nkey then
					local value = e.nkey.value
					out:append(value, true)
					out:append(" = ", true)
					out:append(value, true)
				else 
					if e.vkey then
						out:append(e.vkey.value, true)
						out:append(" = ", true)
					elseif e.bkey then
						out:append("[", true)
						self:trExpr(e.bkey)
						out:append("] = ", true)
					end
					self:trExpr(e.value)
				end
				if i < #t then
					out:append(", ")
				end
			end
			out:append(" }")
		end
		function __ct:trEtFnOnly(t)
			assert(t.etype == "fn", "Invalid etype fn def only")
			local ctx = self.ctx
			local out = self.out
			ctx:pushScope("fn", t)
			out:append("function(" .. Utils.seqReduce(t.args, "", function(init, i, v)
				ctx:localInsert(v.value)
				return init .. (i > 1 and ", " or "") .. v.value
			end) .. ")")
			out:incIndent()
			if #t.body > 0 then
				if t.df then
					out:changeLine()
					out:append(t.df)
				end
				out:changeLine()
				self:trStatement(t.body)
			end
			out:decIndent()
			out:append("end")
			ctx:popScope()
		end
		function __ct:trStImport(t)
			assert(t.stype == "import", "Invalid stype import")
			local ctx = self.ctx
			local out = self.out
			if #t <= 0 then
				out:append("require(" .. t.lib.value .. ")")
			elseif #t == 1 then
				local lt = t[1][1]
				if t.lib.etype == 'const' then
					out:append("local " .. lt.value .. " = require(" .. t.lib.value .. ")")
				else 
					ctx:checkName(t.lib)
					out:append("local " .. lt.value .. " = " .. t.lib.value)
				end
				ctx:localInsert(lt.value)
			else 
				local lt = t[1]
				local rt = t[2]
				if #rt <= 0 then
					rt = lt
				end
				out:append(Utils.seqReduce(lt, "local ", function(init, i, v)
					ctx:localInsert(v.value)
					return init .. (i <= 1 and "" or ", ") .. v.value
				end))
				if t.lib.etype == 'const' then
					out:append("do")
					out:incIndent()
					out:append("local __l = require(" .. t.lib.value .. ")")
					out:append(Utils.seqReduce(lt, "", function(init, i, v)
						return init .. (i <= 1 and "" or ", ") .. v.value
					end))
					out:append(" = " .. Utils.seqReduce(rt, "", function(init, i, v)
						return init .. (i <= 1 and "__l." or ", __l.") .. v.value
					end), true)
					out:decIndent()
					out:append("end")
				else 
					ctx:checkName(t.lib)
					local tfirst, tnext = t.lib.value .. ".", ", " .. t.lib.value .. "."
					out:append(" = " .. Utils.seqReduce(rt, "", function(init, i, v)
						return init .. (i <= 1 and tfirst or tnext) .. v.value
					end), true)
				end
			end
		end
		function __ct:trStExport(t)
			assert(t.stype == "ex", "Invalid stype export")
			local ctx = self.ctx
			local out = self.out
			out:pushInline()
			if t.attr == "local" then
				out:append("local ")
				for i, v in ipairs(t) do
					if i > 1 then
						out:append(", ")
					end
					self:trExpr(v)
					ctx:localInsert(v.value)
				end
			elseif t.attr == "export" then
				for i, v in ipairs(t) do
					ctx:globalInsert(v.value)
					out:append(v.value)
					if i < #t then
						out:append(", ")
					end
				end
				out:append(' = ')
				for i, v in ipairs(t) do
					out:append(v.value .. ' or nil')
					if i < #t then
						out:append(", ")
					end
				end
			elseif t.attr == "*" then
				ctx:localInsert("*")
			else 
				ctx:errorPos("Invalid export attr near " .. (t.attr or "unknown"))
			end
			out:popInline()
		end
		function __ct:trAssign(t)
			assert(t.stype == '=', "Invalid stype =")
			local ctx = self.ctx
			local out = self.out
			out:pushInline()
			if t.sub then
				assert(#t[1] == 1 and #t[1] == #t[2], "Invalid assign sub AST")
				local e = t[1][1]
				self:trExpr(e)
				out:append(' = ')
				self:trExpr(e)
				out:append(' ' .. t.sub .. ' ')
				e = t[2][1]
				local sp, ep = '', ''
				if e.rlop then
					sp, ep = '(', ')'
				end
				out:append(sp)
				self:trExpr(e)
				out:append(ep)
			else 
				local e = t[2]
				for _, v in ipairs(e) do
					ctx:checkName(v)
				end
				e = t[1]
				for i, v in ipairs(e) do
					if t.attr == 'export' then
						ctx:globalInsert(v.value)
					elseif t.attr == 'local' or #v <= 0 and not ctx:checkName(v, true) then
						ctx:localInsert(v.value)
						if i == 1 then
							out:append("local ")
						end
					end
					self:trExpr(v)
					if i < #e then
						out:append(", ")
					end
				end
				out:append(" = ")
				e = t[2]
				for i, v in ipairs(e) do
					self:trExpr(v)
					if i < #e then
						out:append(", ")
					end
				end
			end
			out:popInline()
		end
		function __ct:trStFnDef(t)
			assert(t.stype == "fn", "Invalid stype fn")
			local ctx = self.ctx
			local out = self.out
			local attr = (t.attr == "export" and "" or "local ")
			local args = t.args or {  }
			local fname, pname = self:trEtName(t.name)
			if fname == pname then
				if t.attr == "export" or ctx:checkName(t.name, true) then
					attr = ''
					ctx:globalInsert(fname)
				else 
					ctx:localInsert(fname)
				end
			else 
				ctx:checkName(t.name)
			end
			ctx:pushScope("fn", t)
			local mark = self:hasColonDot(t.name)
			if mark then
				if mark == ':' then
					ctx:localInsert("self")
				end
				attr = ""
			end
			out:append(attr .. "function " .. fname .. "(" .. Utils.seqReduce(args, "", function(init, i, v)
				ctx:localInsert(v.value)
				return init .. (i > 1 and ", " or "") .. v.value
			end) .. ")")
			out:incIndent()
			if #t.body > 0 then
				if t.df then
					out:append(t.df)
				end
				out:changeLine()
				self:trStatement(t.body)
			end
			out:decIndent()
			out:append("end")
			ctx:popScope()
		end
		function __ct:trStCall(t)
			assert(t.stype == "(", "Invalid stype fn call")
			local ctx = self.ctx
			local out = self.out
			local n = 0
			out:pushInline()
			for i, e in ipairs(t) do
				if i > n then
					self:trExpr(e)
				end
			end
			out:popInline()
		end
		function __ct:trStIfElse(t)
			assert(t.stype == "if", "Invalid stype if")
			local ctx = self.ctx
			local out = self.out
			for i, e in ipairs(t) do
				out:append(e.sub .. " ")
				if e.sub ~= 'else' then
					out:pushInline()
					self:trExpr(e.cond)
					out:popInline()
					out:append(" then", true)
				end
				ctx:pushScope("if", e)
				out:changeLine()
				out:incIndent()
				self:trStatement(e.body)
				out:decIndent()
				ctx:popScope()
			end
			out:append('end')
		end
		function __ct:trStSwitch(t)
			assert(t.stype == "switch", "Invalid stype switch")
			local ctx = self.ctx
			local out = self.out
			out:append("local __s = ")
			out:pushInline()
			self:trExpr(t.cond)
			out:popInline()
			out:changeLine()
			for i = 1, #t do
				local c = t[i]
				out:pushInline()
				if c.cond then
					if i == 1 then
						out:append("if ")
					else 
						out:append("elseif ")
					end
					local sp, ep, count = nil, nil, #c.cond
					for j, e in ipairs(c.cond) do
						out:append("__s ==")
						if e.rlop then
							sp, ep = ' (', (j == count and ')' or ') or ')
						else 
							sp, ep = ' ', (j == count and '' or ' or ')
						end
						out:append(sp)
						self:trExpr(e)
						out:append(ep)
					end
					out:append(" then")
				else 
					out:append("else")
				end
				out:changeLine()
				ctx:pushScope("if")
				out:popInline()
				out:incIndent()
				self:trStatement(c.body)
				out:decIndent()
				ctx:popScope()
			end
			out:append("end")
			out:changeLine()
			if t.tail then
				self:trStGotoLabel(t.tail)
				out:changeLine()
			end
		end
		function __ct:trStGuard(t)
			assert(t.stype == "guard", "Invalid stype guard")
			local ctx = self.ctx
			local out = self.out
			out:append("if not (")
			out:pushInline()
			self:trExpr(t.cond)
			out:append(") then", true)
			out:popInline()
			out:changeLine()
			out:incIndent()
			ctx:pushScope("gu", t)
			self:trStatement(t.body)
			ctx:popScope()
			out:decIndent()
			out:append("end")
		end
		function __ct:trStFor(t)
			assert(t.stype == "for" and (t.sub == '=' or t.sub == 'in'), "Invalid stype for")
			local ctx = self.ctx
			local out = self.out
			out:pushInline()
			out:append("for ")
			ctx:pushScope("lo", t)
			for i, e in ipairs(t.name) do
				ctx:localInsert(e.value)
				if i > 1 then
					out:append(", ")
				end
				self:trExpr(e)
			end
			out:append(' ' .. t.sub .. ' ')
			for i, e in ipairs(t.step) do
				if i > 1 then
					out:append(", ")
				end
				self:trExpr(e)
			end
			out:append(" do")
			out:popInline()
			out:changeLine()
			out:incIndent()
			self:trStatement(t.body)
			out:decIndent()
			out:append("end")
			ctx:popScope()
		end
		function __ct:trStWhile(t)
			assert(t.stype == "while", "Invalid stype while")
			local ctx = self.ctx
			local out = self.out
			out:append("while ")
			out:pushInline()
			self:trExpr(t.cond)
			out:append(" do")
			out:popInline()
			out:changeLine()
			out:incIndent()
			ctx:pushScope("lo", t)
			self:trStatement(t.body)
			ctx:popScope()
			out:decIndent()
			out:append("end")
		end
		function __ct:trStRepeat(t)
			assert(t.stype == "repeat", "Invalid repeat op")
			local ctx = self.ctx
			local out = self.out
			out:append("repeat")
			out:changeLine()
			out:incIndent()
			ctx:pushScope("lo", t)
			self:trStatement(t.body)
			out:decIndent()
			out:append("until ")
			out:pushInline()
			self:trExpr(t.cond)
			out:popInline()
			ctx:popScope()
		end
		function __ct:trStBreak(t)
			assert(t.stype == "break", "Invalid stype break")
			local ctx = self.ctx
			local out = self.out
			out:append("break")
		end
		function __ct:trStGotoLabel(t)
			assert(t.stype == "goto" or t.stype == "::", "Invalid stype goto")
			local ctx = self.ctx
			local out = self.out
			if t.stype == "goto" then
				out:append("goto " .. t[1].value)
			else 
				out:append("::" .. t[1].value .. "::")
			end
		end
		function __ct:trStReturn(t)
			assert(t.stype == "return", "Invalid stpye return")
			local ctx = self.ctx
			local out = self.out
			out:append("return ")
			out:pushInline()
			for i, e in ipairs(t) do
				if i > 1 then
					out:append(", ")
				end
				self:trExpr(e)
			end
			out:popInline()
		end
		function __ct:trStDefer(t)
			assert(t.stype == "defer", "Invalid stype defer")
			local ctx = self.ctx
			local out = self.out
			out:append("__df[#__df+1] = function()")
			out:changeLine()
			out:incIndent()
			ctx:pushScope("df")
			self:trStatement(t.body)
			ctx:popScope()
			out:decIndent()
			out:append("end")
		end
		function __ct:trStDo(t)
			assert(t.stype == "do", "Invalid stype do end")
			local ctx = self.ctx
			local out = self.out
			out:append("do")
			out:changeLine()
			out:incIndent()
			ctx:pushScope("do")
			self:trStatement(t.body)
			ctx:popScope()
			out:decIndent()
			out:append("end")
		end
		function __ct:trStClass(t)
			assert(t.stype == "class", "Invalid stype class")
			local ctx = self.ctx
			local out = self.out
			local attr = (t.attr == "export") and "" or "local "
			local clsname = t.name.value
			local supertype = t.super and t.super.value
			if t.attr == "export" or ctx:checkName(t.name, true) then
				attr = ''
				ctx:globalInsert(clsname)
			else 
				ctx:localInsert(clsname)
			end
			if supertype then
				ctx:checkName(t.super)
			end
			ctx:updatePos(t.name.pos)
			out:append(attr .. clsname .. " = { __tn = '" .. clsname .. "', __tk = 'class', __st = " .. (supertype or "nil") .. " }")
			out:append("do")
			out:changeLine()
			out:incIndent()
			out:append("local __st = " .. (supertype or "nil"))
			out:append("local __ct = " .. clsname)
			out:append("__ct.__ct = __ct")
			if supertype then
				out:append("assert(type(__st) == 'table' and __st.__ct == __st and __st.__tk == 'class', 'invalid super type')")
			else 
				out:append("__ct.isKindOf = function(c, a) return a and c and ((c.__ct == a) or (c.__st and c.__st:isKindOf(a))) or false end")
			end
			ctx:pushScope("cl")
			local cls_fns, ins_fns = {  }, {  }
			local fn_deinit = self:hlVarAndFns(t, "class", "__ct", ctx, out, cls_fns, ins_fns)
			out:append("local __imt = {")
			out:incIndent()
			if not ins_fns.has_tostring then
				out:append([[__tostring = function(t) return "<class ]] .. clsname .. [[" .. t.__ins_name .. ">" end,]])
			end
			out:append("__index = function(t, k)")
			out:incIndent()
			if ins_fns.has_index then
				out:append("local ok, v = __ct.__ins_index(t, k)")
				out:append("if ok then return v else v = __ct[k] end")
			else 
				out:append("local v = __ct[k]")
			end
			out:append("if v ~= nil then rawset(t, k, v) end")
			out:append("return v")
			out:decIndent()
			out:append("end,")
			if fn_deinit then
				out:append("__gc = function(t) t:deinit() end,")
			end
			for _, e in ipairs(ins_fns) do
				out:append(e.name.value .. " = function(")
				self:hlFnArgsBody(e, false, true)
			end
			out:decIndent()
			out:append("}")
			out:append("setmetatable(__ct, {")
			out:incIndent()
			if not cls_fns.has_tostring then
				out:append('__tostring = function() return "<class ' .. clsname .. '>" end,')
			end
			out:append('__index = function(t, k)')
			out:incIndent()
			if cls_fns.has_index then
				out:append('local ok, v = t.__cls_index(t, k)')
				out:append('if ok then return v else v = __st and __st[k] end')
			else 
				out:append('local v = __st and __st[k]')
			end
			out:append('if v ~= nil then rawset(t, k, v) end')
			out:append('return v')
			out:decIndent()
			out:append('end,')
			out:append("__call = function(_, ...)")
			out:incIndent()
			out:append("local t = {}; t.__ins_name = tostring(t):sub(6)")
			out:append("local ins = setmetatable(t, __imt)")
			out:append("if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end")
			if fn_deinit then
				out:append('if _VERSION == "Lua 5.1" then')
				out:incIndent()
				out:append("rawset(ins, '__gc_proxy', newproxy(true))")
				out:append("getmetatable(ins.__gc_proxy).__gc = function() ins:deinit() end")
				out:decIndent()
				out:append("end")
			end
			out:append("return ins")
			out:decIndent()
			out:append("end,")
			for _, e in ipairs(cls_fns) do
				out:append(e.name.value .. " = function(")
				self:hlFnArgsBody(e, false, true)
			end
			out:decIndent()
			out:append("})")
			out:decIndent()
			out:append("end")
			ctx:popScope()
		end
		function __ct:trStStruct(t)
			assert(t.stype == "struct", "Invalid stype struct")
			local ctx = self.ctx
			local out = self.out
			local attr = (t.attr == "export") and "" or "local "
			local strname = t.name.value
			if t.attr == "export" or ctx:checkName(t.name, true) then
				attr = ''
				ctx:globalInsert(strname)
			else 
				ctx:localInsert(strname)
			end
			ctx:updatePos(t.name.pos)
			out:append(attr .. strname .. " = { __tn = '" .. strname .. "', __tk = 'struct' }")
			out:append("do")
			out:changeLine()
			out:incIndent()
			out:append("local __ct = " .. strname)
			out:append("__ct.__ct = __ct")
			ctx:pushScope("cl")
			local cls_fns, ins_fns = {  }, {  }
			local fn_deinit = self:hlVarAndFns(t, "struct", "__ct", ctx, out, cls_fns, ins_fns)
			out:append("local __imt = {")
			out:incIndent()
			if not ins_fns.has_tostring then
				out:append([[__tostring = function(t) return "<struct ]] .. strname .. [[" .. t.__ins_name .. ">" end,]])
			end
			out:append("__index = function(t, k)")
			out:incIndent()
			out:append("local v = rawget(__ct, k)")
			out:append("if v ~= nil then rawset(t, k, v) end")
			out:append("return v")
			out:decIndent()
			out:append("end,")
			out:append("__newindex = function(t, k, v) if rawget(__ct, k) ~= nil then rawset(t, k, v) end end,")
			if fn_deinit then
				out:append("__gc = function(t) t:deinit() end,")
			end
			for _, e in ipairs(ins_fns) do
				out:append(e.name.value .. " = function(")
				self:hlFnArgsBody(e, false, true)
			end
			out:decIndent()
			out:append("}")
			out:append(strname .. " = setmetatable({}, {")
			out:incIndent()
			if not cls_fns.has_tostring then
				out:append('__tostring = function() return "<struct ' .. strname .. '>" end,')
			end
			out:append('__index = function(t, k) local v = rawget(__ct, k); if v ~= nil then rawset(t, k, v); end return v end,')
			out:append('__newindex = function(t, k, v) if v ~= nil and rawget(__ct, k) ~= nil then rawset(t, k, v) end end,')
			out:append("__call = function(_, ...)")
			out:incIndent()
			out:append("local t = {}; t.__ins_name = tostring(t):sub(6)")
			out:append("local ins = setmetatable(t, __imt)")
			out:append("if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end")
			if fn_deinit then
				out:append('if _VERSION == "Lua 5.1" then')
				out:incIndent()
				out:append("rawset(ins, '__gc_proxy', newproxy(true))")
				out:append("getmetatable(ins.__gc_proxy).__gc = function() ins:deinit() end")
				out:decIndent()
				out:append("end")
			end
			out:append("return ins")
			out:decIndent()
			out:append("end,")
			for _, e in ipairs(cls_fns) do
				out:append(e.name.value .. " = function(")
				self:hlFnArgsBody(e, false, true)
			end
			out:decIndent()
			out:append("})")
			out:decIndent()
			out:append("end")
			ctx:popScope()
		end
		function __ct:trStExtension(t)
			assert(t.stype == "extension", "Invalid stype extension")
			local ctx = self.ctx
			local out = self.out
			local clsname = t.name.value
			local extype = t.super and t.super.value
			ctx:checkName(t.name)
			if extype then
				ctx:checkName(t.super)
			end
			ctx:updatePos(t.name.pos)
			out:append("do")
			out:changeLine()
			out:incIndent()
			out:append("local __et = " .. (extype or "nil"))
			out:append("local __ct = " .. clsname)
			out:append("assert(type(__ct) == 'table' and type(__ct.__ct) == 'table' and (__ct.__tk == 'class' or __ct.__tk == 'struct'), 'invalid extended type')")
			out:append("__ct = __ct.__ct")
			if extype then
				out:append("assert(type(__et) == 'table' and type(__et.__ct) == 'table' and (__et.__tk == 'class' or __et.__tk == 'struct'), 'invalid super type')")
				out:append('for k, v in pairs(__et.__ct) do')
				out:incIndent()
				out:append('if __ct[k] == nil and (k:len() < 2 or (k:sub(1, 2) ~= "__" and k ~= "isKindOf" and k ~= "init" and k ~= "deinit")) then')
				out:incIndent()
				out:append('__ct[k] = v')
				out:decIndent()
				out:append("end")
				out:decIndent()
				out:append("end")
			end
			ctx:pushScope("cl")
			self:hlVarAndFns(t, "extension", "__ct", ctx, out, {  }, {  })
			out:decIndent()
			out:append("end")
			ctx:popScope()
		end
		function __ct:hlVarAndFns(t, cname, sname, ctx, out, cls_fns, ins_fns)
			out:append("-- declare " .. cname .. " var and methods")
			out:changeLine()
			local fn_deinit = false
			for _, s in ipairs(t) do
				local stype = s.stype
				if stype == "=" then
					out:append(sname .. ".")
					out:pushInline()
					self:trExpr(s[1])
					out:append(' = ')
					self:trExpr(s[2])
					out:popInline()
					out:changeLine()
				elseif stype == "fn" then
					local fn_name = s.name.value
					if cname == "extension" and (fn_name == "init" or fn_name == "deinit") then
						ctx:errorPos("extension not support init/deinit", s.name.pos)
					elseif fn_name == "deinit" then
						fn_deinit = true
					end
					local fn_ins = s.attr ~= "static"
					if _cls_metafns[fn_name] then
						if cname == "extension" then
							ctx:errorPos("extension not support metamethod", s.name.pos)
						elseif fn_ins then
							ins_fns[#ins_fns + 1] = s
							if fn_name == "__tostring" then
								ins_fns.has_tostring = true
							elseif fn_name == "__index" or fn_name == "__newindex" then
								if cname == "struct" then
									ctx:errorPos("struct not support " .. fn_name, s.name.pos)
								elseif fn_name == "__index" then
									ins_fns[#ins_fns] = nil
									ins_fns.has_index = true
									s.name.value = "__ins_index"
									out:append("function " .. sname .. "." .. s.name.value .. "(")
									self:hlFnArgsBody(s, false)
								end
							end
						else 
							cls_fns[#cls_fns + 1] = s
							if fn_name == "__tostring" then
								cls_fns.has_tostring = true
							elseif fn_name == "__index" or fn_name == "__newindex" then
								if cname == "struct" then
									ctx:errorPos("struct not support " .. fn_name, s.name.pos)
								elseif fn_name == "__index" then
									cls_fns[#cls_fns] = nil
									cls_fns.has_index = true
									s.name.value = "__cls_index"
									out:append("function " .. sname .. "." .. s.name.value .. "(")
									self:hlFnArgsBody(s, false)
								end
							elseif fn_name == "__call" then
								ctx:errorPos(cname .. " not support static " .. fn_name, s.name.pos)
							end
						end
					else 
						out:append("function " .. sname .. (fn_ins and ":" or ".") .. fn_name .. "(")
						self:hlFnArgsBody(s, fn_ins)
					end
				end
			end
			out:append("-- declare end")
			return fn_deinit
		end
		function __ct:hlFnArgsBody(e, fn_ins, comma_end)
			local ctx = self.ctx
			local out = self.out
			out:pushInline()
			ctx:pushScope("fn", e)
			for i, v in ipairs(e.args) do
				if i > 1 then
					out:append(", ")
				end
				self:trExpr(v)
				ctx:localInsert(v.value)
			end
			if fn_ins then
				ctx:localInsert("self")
			end
			out:append(")")
			out:popInline()
			out:incIndent()
			if #e.body > 0 then
				if e.df then
					out:append(e.df)
				end
				out:changeLine()
				self:trStatement(e.body)
			end
			out:decIndent()
			out:append("end" .. (comma_end and "," or ""))
			out:changeLine()
			ctx:popScope()
		end
		function __ct:hasColonDot(expr)
			if type(expr) == 'table' then
				if expr.etype == 'exp' then
					local count = #expr
					for i = count, 1, -1 do
						local v = expr[i]
						if v.etype == ':' or v.etype == '.' then
							return v.etype
						end
					end
				end
				return (expr.etype == ':' or expr.etype == '.') and expr.etype
			end
			return false
		end
		-- declare end
		local __imt = {
			__tostring = function(t) return "<class M" .. t.__ins_name .. ">" end,
			__index = function(t, k)
				local v = __ct[k]
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
		}
		setmetatable(__ct, {
			__tostring = function() return "<class M>" end,
			__index = function(t, k)
				local v = __st and __st[k]
				if v ~= nil then rawset(t, k, v) end
				return v
			end,
			__call = function(_, ...)
				local t = {}; t.__ins_name = tostring(t):sub(6)
				local ins = setmetatable(t, __imt)
				if type(rawget(__ct,'init')) == 'function' and __ct.init(ins, ...) == false then return nil end
				return ins
			end,
		})
	end
	local function compile(config, data)
		if not (type(data) == "table" and data.ast and data.content) then
			return false, { err_msg = "Invalid data", pos = 0 }
		end
		Ctx:reset(config, data.ast, data.content)
		Out:reset()
		M:reset(Ctx, Out)
		local ret, emsg = pcall(M.trStatement, M, Ctx.ast)
		if not (ret) then
			return false, (Ctx.err_info or { err_msg = emsg, pos = 0 })
		end
		return true, table.concat(Out._output, "\n")
	end
	local function clearproj()
		_scope_proj.vars = {  }
	end
	MoocLib.compiler = { compile = compile, clearproj = clearproj }; package.loaded['moocscript.compiler'] = MoocLib.compiler
end
MoocLib = MoocLib or nil
do
	local Utils = require("moocscript.utils")
	local parse
	do
		local __l = require("moocscript.parser")
		parse = __l.parse
	end
	local compile, clearproj
	do
		local __l = require("moocscript.compiler")
		compile, clearproj = __l.compile, __l.clearproj
	end
	local readFile = Utils.readFile
	local concat, insert, remove = table.concat, table.insert, table.remove
	local unpack, assert = unpack or table.unpack, assert
	local type, error, load, loadstring = type, error, load, loadstring
	local sfmt = string.format
	local srep = string.rep
	local function toAST(config, text)
		local t = type(text)
		if t ~= "string" then
			return nil, "expecting string (got " .. t .. ")"
		end
		config = config or {  }
		local ret, tbl = parse(text)
		if not ret then
			return nil, Utils.errorMessage(tbl.content, tbl.pos, tbl.err_msg, config.fname)
		end
		return tbl
	end
	local function toLua(config, tbl)
		local ret, code = compile(config, tbl)
		if not ret then
			return nil, Utils.errorMessage(tbl.content, code.pos, code.err_msg, config.fname)
		end
		return code
	end
	local dir_spliter = package.config and package.config[1] or '/'
	local tmp_config = {  }
	local function mcLoader(name)
		local name_path = name:gsub("%.", dir_spliter)
		local text, file_path = nil, nil
		for path in package.path:gmatch("[^;]+") do
			local len = path:len()
			path = path:sub(1, len - 4) .. ".mooc"
			file_path = path:gsub("?", name_path)
			text = readFile(file_path)
			if text then
				break
			end
		end
		if not (text) then
			return nil, "Could not find .mooc file"
		end
		tmp_config.fname = file_path
		local res, emsg = toAST(tmp_config, text)
		if not res then
			error(emsg)
		end
		res, emsg = toLua(tmp_config, res)
		if not res then
			error(emsg)
		end
		return (loadstring or load)(res, file_path)
	end
	local function mcLoadString(text, cname, mode, env)
		tmp_config.fname = cname
		local res, emsg = toAST(tmp_config, text)
		if not res then
			return nil, emsg
		end
		res, emsg = toLua(tmp_config, res)
		if not res then
			return nil, emsg
		end
		local f = (loadstring or load)
		return f(res, cname, unpack({ mode = mode, env = env }))
	end
	local function mcLoadBuffer(text, cname)
		tmp_config.fname = cname
		local res, emsg = toAST(tmp_config, text)
		if not res then
			return nil, emsg
		end
		res, emsg = toLua(tmp_config, res)
		if not res then
			return nil, emsg
		end
		return true, res
	end
	local function mcLoadFile(fname, ...)
		local text, err = readFile(fname)
		if not (text) then
			return nil, err
		end
		return mcLoadString(text, fname, ...)
	end
	local function mcDoFile(...)
		local f = assert(mcLoadFile(...))
		return f()
	end
	local function mcRemoveLoader()
		if not (package.mooc_loaded) then
			return 
		end
		local loaders = package.loaders or package.searchers
		for i = #loaders, 1, -1 do
			if package.mooc_loaded == loaders[i] then
				remove(loaders, i)
				package.mooc_loaded = nil
				return true
			end
		end
	end
	local function mcAppendLoader()
		if package.mooc_loaded then
			return 
		end
		local loaders = package.loaders or package.searchers
		insert(loaders, 3, mcLoader)
		package.mooc_loaded = mcLoader
	end
	local moocVersionShort = "0.8.20221204"
	local moocVersionLong = "moocscript v" .. moocVersionShort .. ", " .. (jit and jit.version or _VERSION)
	local function mcVersion()
		return moocVersionLong
	end
	local function mcLoaded()
		return package.mooc_loaded ~= nil
	end
	mcAppendLoader()
	MoocLib.core = { loadbuffer = mcLoadBuffer, loadstring = mcLoadString, loadfile = mcLoadFile, dofile = mcDoFile, removeloader = mcRemoveLoader, appendloader = mcAppendLoader, toAST = toAST, toLua = toLua, clearProj = clearproj, version = mcVersion, loaded = mcLoaded, require = mcLoader }; package.loaded['moocscript.core'] = MoocLib.core
end
MoocLib = MoocLib or nil
do
	local fType = type
	local fAssert = assert
	local fRawSet = rawset
	local sfmt = string.format
	local function newMoocClass(cls_name, super_type)
		if not (fType(cls_name) == "string") then
			return nil
		end
		local cls_type = { __tn = cls_name, __tk = 'class', __st = super_type }
		cls_type.__ct = cls_type
		if super_type then
			assert(type(super_type) == "table" and type(super_type.__ct) == "table" and super_type.__tk == "class")
		else 
			cls_type.isKindOf = function(c, a)
				return a and c and ((c.__ct == a) or (c.__st and c.__st:isKindOf(a))) or false
			end
		end
		local ins_mt = { __tostring = function(t)
			return "<class " .. cls_name .. t.__ins_name .. ">"
		end, __index = function(t, k)
			local v = cls_type[k]
			if v ~= nil then
				fRawSet(t, k, v)
			end
			return v
		end }
		setmetatable(cls_type, { __tostring = function()
			return "<class " .. cls_name .. ">"
		end, __index = function(_, k)
			local v = super_type and super_type[k]
			if v ~= nil then
				fRawSet(cls_type, k, v)
			end
			return v
		end, __call = function(_, ...)
			local t = {  }
			t.__ins_name = tostring(t):sub(6)
			local ins = setmetatable(t, ins_mt)
			if type(ins.init) == 'function' and ins:init(...) == false then
				return nil
			end
			return ins
		end })
		return cls_type
	end
	local function newMoocStruct(cls_name)
		if not (fType(cls_name) == "string") then
			return nil
		end
		local cls_type = { __tn = cls_name, __tk = 'struct' }
		cls_type.__ct = cls_type
		local ins_mt = { __tostring = function(t)
			return "<struct " .. cls_name .. t.__ins_name .. ">"
		end, __index = function(t, k)
			local v = rawget(cls_type, k)
			if v ~= nil then
				rawset(t, k, v)
			end
			return v
		end, __newindex = function(t, k, v)
			if rawget(cls_type, k) ~= nil then
				rawset(t, k, v)
			end
		end }
		return setmetatable({  }, { __tostring = function()
			return "<struct " .. cls_name .. ">"
		end, __index = function(_, k)
			return rawget(cls_type, k)
		end, __newindex = function(_, k, v)
			if v ~= nil and rawget(cls_type, k) ~= nil then
				rawset(cls_type, k, v)
			end
		end, __call = function(_, ...)
			local t = {  }
			t.__ins_name = tostring(t):sub(6)
			local ins = setmetatable(t, ins_mt)
			if type(ins.init) == 'function' and ins:init(...) == false then
				return nil
			end
			return ins
		end }), cls_type
	end
	local function extentMoocClassStruct(cls, ext)
		assert(type(cls) == "table" and type(cls.__ct) == "table" and (cls.__tk == 'class' or cls.__tk == 'struct'))
		local ct = cls.__ct
		if ext then
			assert(type(ext) == "table" and type(ext.__ct) == "table" and (ext.__tk == 'class' or ext.__tk == 'struct'))
			for k, v in pairs(ext.__ct) do
				if ct[k] == nil and (k:len() < 2 or (k:sub(1, 2) ~= "__" and k ~= "__st" and k ~= "isKindOf")) then
					ct[k] = v
				end
			end
		end
		return ct
	end
	MoocLib.class = { newMoocClass = newMoocClass, newMoocStruct = newMoocStruct, extentMoocClassStruct = extentMoocClassStruct }; package.loaded['moocscript.class'] = MoocLib.class
end
return MoocLib
*/});

	// load moocscript-web source
	var res = fengari.load(tmpl_str, "moocscript-web");
	if (typeof(res) == "function") {
		res();
	} else {
		throw new SyntaxError("failed to load moocscript-web");
	}
}

// load moocscript-web api for 'require()'
function moocscript_web_install_api() {
	const f = fengari;
	const L = f.L;
	const l = f.lua;
	const la = f.lauxlib;

	// only load file from same site for 'require()'
	var mooc_loadscript = function(L) {
		var filename = la.luaL_checkstring(L, 1);

		var path = f.to_uristring(filename);
		var xhr = new XMLHttpRequest();
		xhr.open("GET", path, false);
		/*
		Synchronous xhr in main thread always returns a js string.
		Some browsers make console noise if you even attempt to set responseType
		*/

		if (typeof window === "undefined") {
			xhr.responseType = "arraybuffer";
		}

		xhr.send();

		if (xhr.status >= 200 && xhr.status <= 299) {
			if (typeof xhr.response === "string") {
				l.lua_pushstring(L, f.to_luastring(xhr.response));
				return 1;
			} else {
				return new Uint8Array(xhr.response);
			}
		} else {
			return 0;
		}
	}

	// set to `js.mooc_loadscript` in Lua side
	l.lua_getglobal(L, f.to_luastring("js", true));
	l.lua_pushcclosure(L, mooc_loadscript, 0);
	l.lua_setfield(L, -2, f.to_luastring("mooc_loadscript", true));
	l.lua_pop(L, 1);
}

// install listener for <script> tag
function moocscript_web_install_listener() {
	if (typeof document !== 'undefined' && document instanceof HTMLDocument) {
		const f = fengari;
		const L = f.L;
		const l = f.lua;
		const la = f.lauxlib;

		/* Have a document, e.g. we are in main browser window */
		var crossorigin_to_credentials = function(crossorigin) {
			switch (crossorigin) {
				case "anonymous":
					return "omit";

				case "use-credentials":
					return "include";

				default:
					return "same-origin";
			}
		};

		var msghandler = function(L) {
			var ar = new l.lua_Debug();
			if (l.lua_getstack(L, 2, ar)) l.lua_getinfo(L, f.to_luastring("Sl"), ar);
			f.push(L, new ErrorEvent("error", {
				bubbles: true,
				cancelable: true,
				message: f.lua_tojsstring(L, 1),
				error: f.tojs(L, 1),
				filename: ar.short_src ? f.to_jsstring(ar.short_src) : void 0,
				lineno: ar.currentline > 0 ? ar.currentline : void 0
			}));
			return 1;
		};

		var run_mooc_script = function(tag, code, chunkname) {
			{
				// local loadbuffer = package.loaded['moocscript.core'].loadbuffer
				// local ret, lua_code = loadbuffer(mooc_code, chunkname)
				l.lua_getglobal(L, "package");
				l.lua_getfield(L, -1, "loaded");
				l.lua_getfield(L, -1, "moocscript.core");
				l.lua_getfield(L, -1, "loadbuffer");
				l.lua_remove(L, -2);
				l.lua_remove(L, -2);
				l.lua_remove(L, -2);
				l.lua_pushstring(L, code);
				l.lua_pushstring(L, chunkname);
				l.lua_call(L, 2, 2);
				// got result
				const ret = l.lua_toboolean(L, -2)
				code = l.lua_tojsstring(L, -1);
				l.lua_pop(L, 1);
				if (ret) {
					code = f.to_luastring(code);
				} else {
					var filename = tag.src ? tag.src : document.location;
					const lineno = 0;
					var syntaxerror = new SyntaxError(code, filename, lineno);
					e = new ErrorEvent("error", {
						message: msg,
						error: syntaxerror,
						filename: filename,
						lineno: lineno
					});
				}
			}

			var ok = la.luaL_loadbuffer(L, code, null, chunkname);
			var e;

			if (ok === 3) {
				var msg = f.lua_tojsstring(L, -1);
				var filename = tag.src ? tag.src : document.location;
				var lineno = void 0;
				/* TODO: extract out of msg */

				var syntaxerror = new SyntaxError(msg, filename, lineno);
				e = new ErrorEvent("error", {
					message: msg,
					error: syntaxerror,
					filename: filename,
					lineno: lineno
				});
			} else if (ok === 0) {
				/* insert message handler below function */
				var base = l.lua_gettop(L);
				l.lua_pushcfunction(L, msghandler);
				l.lua_insert(L, base);
				/* set document.currentScript.
				   We can't set it normally; but we can create a getter for it, then remove the getter */

				Object.defineProperty(document, 'currentScript', {
					value: tag,
					configurable: true
				});
				ok = l.lua_pcall(L, 0, 0, base);
				/* Remove the currentScript getter installed above; this restores normal behaviour */

				delete document.currentScript;
				/* Remove message handler */

				l.lua_remove(L, base);
				/* Check if normal error that msghandler would have handled */

				if (ok === 2) {
					e = f.checkjs(L, -1);
				}
			}

			if (ok !== 0) {
				if (e === void 0) {
					e = new ErrorEvent("error", {
						message: l.lua_tojsstring(L, -1),
						error: f.tojs(L, -1)
					});
				}

				l.lua_pop(L, 1);

				if (window.dispatchEvent(e)) {
					console.error("uncaught exception", e.error);
				}
			}
		};

		var process_xhr_response = function process_xhr_response(xhr, tag, chunkname) {
			if (xhr.status >= 200 && xhr.status < 300) {
				var code = xhr.response;

				if (typeof code === "string") {
					code = f.to_luastring(xhr.response);
				} else {
					/* is an array buffer */
					code = new Uint8Array(code);
				}
				/* TODO: subresource integrity check? */


				run_mooc_script(tag, code, chunkname);
			} else {
				tag.dispatchEvent(new Event("error"));
			}
		};

		var run_mooc_script_tag = function(tag) {
			if (tag.src) {
				var chunkname = f.to_luastring("@" + tag.src);
				/* JS script tags are async after document has loaded */

				if (document.readyState === "complete" || tag.async) {
					if (typeof fetch === "function") {
						fetch(tag.src, {
							method: "GET",
							credentials: crossorigin_to_credentials(tag.crossorigin),
							redirect: "follow",
							integrity: tag.integrity
						}).then(function(resp) {
							if (resp.ok) {
								return resp.arrayBuffer();
							} else {
								throw new Error("unable to fetch");
							}
						}).then(function(buffer) {
							var code = new Uint8Array(buffer);
							run_mooc_script(tag, code, chunkname);
						}).catch(function(reason) {
							tag.dispatchEvent(new Event("error"));
						});
					} else {
						var xhr = new XMLHttpRequest();
						xhr.open("GET", tag.src, true);
						xhr.responseType = "arraybuffer";

						xhr.onreadystatechange = function() {
							if (xhr.readyState === 4) process_xhr_response(xhr, tag, chunkname);
						};

						xhr.send();
					}
				} else {
					/* Needs to be synchronous: use an XHR */
					var _xhr = new XMLHttpRequest();

					_xhr.open("GET", tag.src, false);

					_xhr.send();

					process_xhr_response(_xhr, tag, chunkname);
				}
			} else {
				var code = f.to_luastring(tag.innerHTML);

				var _chunkname = tag.id ? f.to_luastring("=" + tag.id) : code;

				run_mooc_script(tag, code, _chunkname);
			}
		};

		var contentTypeRegexp = /^(.*?\/.*?)([\t ]*;.*)?$/;
		var luaVersionRegex = /^(\d+)\.(\d+)$/;

		var try_tag = function try_tag(tag) {
			if (tag.tagName !== "SCRIPT") return;
			/* strip off mime type parameters */

			var contentTypeMatch = contentTypeRegexp.exec(tag.type);
			if (!contentTypeMatch) return;
			var mimetype = contentTypeMatch[1];
			if (mimetype !== "application/mooc" && mimetype !== "text/mooc") return;

			if (tag.hasAttribute("lua-version")) {
				var lua_version = luaVersionRegex.exec(tag.getAttribute("lua-version"));
				if (!lua_version || lua_version[1] !== LUA_VERSION_MAJOR || lua_version[2] !== LUA_VERSION_MINOR) return;
			}

			run_mooc_script_tag(tag);
		};

		if (typeof MutationObserver !== 'undefined') {
			/* watch for new script tags added to document */
			new MutationObserver(function(records, observer) {
				for (var i = 0; i < records.length; i++) {
					var record = records[i];

					for (var j = 0; j < record.addedNodes.length; j++) {
						try_tag(record.addedNodes[j]);
					}
				}
			}).observe(document, {
				childList: true,
				subtree: true
			});
		} else if (console.warn) {
			console.warn("fengari-web: MutationObserver not found; lua script tags will not be run when inserted");
		}
		/* the query selector here is slightly liberal,
		   more checks occur in try_tag */


		var selector = 'script[type^="application/mooc"], script[type^="text/mooc"]';
		/* try to run existing script tags */

		Array.prototype.forEach.call(document.querySelectorAll(selector), try_tag);
	}
}

moocscript_web_install_lib()
moocscript_web_install_api()
moocscript_web_install_listener()
