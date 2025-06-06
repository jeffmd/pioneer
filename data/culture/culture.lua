-- Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
-- Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

-- give name, first and last name functions

local utils = require 'utils'

local de = require '.de' -- german
local da = require '.da' -- danish
local es = require '.es' -- spanish
local en = require '.en' -- english
local eu = require '.eu' -- basque
local fi = require '.fi' -- finish
local fil = require '.fil' -- filipino
local fr = require '.fr' -- french
local gd = require '.gd' -- gaelic
local el = require '.el' -- greek
local haw = require '.haw' --hawaiian
local hu = require '.hu' -- hungarian
local hy = require '.hy' -- armenian
local is = require '.is' -- icelandic
local it = require '.it' -- italian
local ja = require '.ja' -- japanese
local nl = require '.nl' -- netherlands
local nb = require '.nb' -- norwegian bokmål
local pl = require '.pl' -- polish
local ro = require '.ro' -- romanian
local ru = require '.ru' -- russian
local se = require '.se' -- northern sami
local sq = require '.sq' -- albanian
local sv = require '.sv' -- swedish
local us = require '.us' -- USA
local tr = require '.tr' -- turkish
local zh = require '.zh' -- chinese/mandarin
local misc = require '.misc' -- Our old namegen / developer's names

--
-- Class: Culture
--

local Culture = {}

Culture.weights = {
	{lang = da,	weight = 1.0},
	{lang = de,	weight = 3.0},
	{lang = el,	weight = 1.0},
	{lang = en,	weight = 6.0},
	{lang = es,	weight = 3.0},
	{lang = eu,	weight = 1.0},
	{lang = fi,	weight = 1.0},
	{lang = fil, weight = 1.0},
	{lang = fr,	weight = 3.0},
	{lang = gd,	weight = 0.2},
	{lang = haw, weight = 0.6},
	{lang = hu,	weight = 1.0},
	{lang = hy,	weight = 0.4},
	{lang = is,	weight = 0.2},
	{lang = it,	weight = 3.0},
	{lang = ja,	weight = 3.0},
	{lang = nb,	weight = 1.0},
	{lang = nl,	weight = 2.0},
	{lang = pl,	weight = 2.0},
	{lang = ro,	weight = 1.0},
	{lang = ru,	weight = 3.0},
	{lang = se,	weight = 0.1},
	{lang = sq,	weight = 0.6},
	{lang = sv,	weight = 1.0},
	{lang = tr,	weight = 1.0},
	{lang = us,	weight = 5.0},
	{lang = zh,	weight = 3.0},
	{lang = misc, weight = 10.0},
}

-- Normalize weights to sum to 1
utils.normWeights(Culture.weights)

-- Map language string to module table
Culture.lookup = {}
for k, v in pairs(Culture.weights) do
	Culture.lookup[v.lang.name] = v.lang
	-- print("* ", k, v.lang.code, v.lang.name, v.lang, v.weight)
end

--
-- Function: FirstName
--
-- Create first name, from specified culture, or default to weighted
-- probability from pre-set list of available cultures. See parameter
-- and return documentation from Culture:FullName()
--
-- > name = Culture:FirstName(isfemale, rand, culture)
--
function Culture:FirstName (isFemale, rand, culture)
	local c = self.lookup[culture] or utils.chooseNormalized(self.weights, rand).lang
	return c:FirstName(isFemale)
end

--
-- Function: Surname
--
-- Create surname, from specified culture, or default to weighted
-- probability from pre-set list of available cultures. See parameter
-- and return documentation from Culture:FullName().
--
-- > name = Culture:Surname(isfemale, rand, culture, make_ascii)
--
function Culture:Surname (isFemale, rand, culture, ascii)
	local c = self.lookup[culture] or utils.chooseNormalized(self.weights, rand).lang
	return c:Surname(isFemale, rand, ascii)
end


--
-- Function: FullName
--
-- Create first and surname strings of matching culture. Both names
-- can be composed of more than one name. Ex. 'Maria Luisa' or 'van
-- der Velden'. Create a full name, where first and last match the
-- same language/culture. If no culture is specified, one is randomly
-- selected according to pre-set weights. Valid input is one of the
-- following (capitalized) strings:
--
-- Albanian American Armenian Basque Chinese Danish Dutch English
-- Filipino Finish French Gaelic German Greek Hawaiian Hungarian
-- Icelandic Italian Japanese Miscellaneous Norwegian Polish
-- Romanian Russian Sami Spanish Swedish Turkish
--
-- > name = Culture:FullName(isfemale, rand, culture)
--
-- Parameters:
--
--   isfemale - whether to generate a male or female name. true for female,
--              false for male
--
--   rand - the <Rand> object to use to generate the name
--
--   culture - optional string
--
-- Return:
--
--   name - a string containing matching first and last name
--
-- Return full name from the same culture/language
function Culture:FullName (isFemale, rand, culture)
	-- if 'culture' given as a string, e.g. "Russian" use that
	local c = self.lookup[culture] or utils.chooseNormalized(self.weights, rand).lang

	-- local debug_code = "(".. c.code .. ") "
	-- return debug_code .. c:FullName(isFemale, rand)
	return c:FullName(isFemale)
end

--
-- Function: Names
--
-- Create the first name and surname of a character separately as two
-- strings and of matching language/culture. If no culture is specified,
-- one is randomly selected according to pre-set weights. Valid input
-- is the same as Culture:Surname.
--
-- > name1, name2 = Culture:Names(isfemale, rand, culture)
--
-- Parameters:
--
--   isfemale - whether to generate a male or female name. true for female,
--              false for male
--
--   rand - the <Rand> object to use to generate the name
--
--   culture - optional string
--
-- Return:
--
--   name1 - a string containing the first name
--
--   name2 - a string containing the surname
--
--
-- Return full name from the same culture/language

function Culture:Names (isFemale, rand, culture)
	-- if 'culture' given as a string, e.g. "Russian" use that
	local c = self.lookup[culture] or utils.chooseNormalized(self.weights, rand).lang
	return c:FirstName(isFemale), c:Surname(isFemale)
end

return Culture
