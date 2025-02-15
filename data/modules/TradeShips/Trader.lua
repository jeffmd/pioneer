-- Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
-- Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

local Commodities = require 'Commodities'
local Engine = require 'Engine'
local Game = require 'Game'
local HullConfig  = require 'HullConfig'
local Ship = require 'Ship'
local Space = require 'Space'
local Timer = require 'Timer'

local utils       = require 'utils'

local ShipBuilder = require 'modules.MissionUtils.ShipBuilder'
local OutfitRules = ShipBuilder.OutfitRules

local Core = require 'modules.TradeShips.Core'

local ECMRule = {
	slot = "utility",
	filter = "utility.ecm",
	limit = 1,
}

local TraderTemplate = ShipBuilder.Template:clone {
	rules = {
		OutfitRules.DefaultHyperdrive,
		OutfitRules.DefaultAtmoShield,
		OutfitRules.DefaultAutopilot,
		OutfitRules.DefaultRadar,
		{ equip = "misc.cargo_life_support" },
		-- Defensive equipment is applied based on system lawlessness and a little luck
		utils.mixin(OutfitRules.EasyWeapon, { randomChance = 0.8 }),
		utils.mixin(OutfitRules.DefaultShieldGen, { randomChance = 1.0 }),
		utils.mixin(OutfitRules.DefaultShieldBooster, { randomChance = 0.4 }),
		-- ECM can't be used by NPC ships... yet
		utils.mixin(ECMRule, { maxSize = 2, randomChance = 0.3 }),
		-- Basic ECM is more prevalent than advanced ECM
		utils.mixin(ECMRule, { maxSize = 1, randomChance = 0.8 }),
		-- Extremely rare to have one of these onboard
		{
			slot = "hull",
			filter = "hull.autorepair",
			limit = 1,
			randomChance = 0.2
		}
	}
}

local Trader = {}
-- this module contains functions that work for single traders

Trader.addEquip = function (ship)
	assert(ship.usedCargo == 0, "equipment is only installed on an empty ship")
	local shipId = ship.shipId

	-- Compute a random modifier for more advanced equipment from lawlessness
	local lawlessness = Game.system.lawlessness
	local randomMod = 0.1 + lawlessness * 0.9

	local hullConfig = HullConfig.GetHullConfig(shipId)
	assert(hullConfig, "Can't find hull config for shipId " .. shipId)

	local template = TraderTemplate:clone {
		randomModifier = randomMod
	}

	local hullThreat = ShipBuilder.GetHullThreat(shipId).total

	-- TODO: Dummy threat value since we're not using it to select hulls
	local plan = ShipBuilder.MakePlan(template, hullConfig, hullThreat + 50 * randomMod)
	assert(plan, "Couldn't make an equipment plan for trader " .. shipId)

	ShipBuilder.ApplyPlan(ship, plan)
end

Trader.addCargo = function (ship, direction)
	---@type CargoManager
	local cargoMgr = ship:GetComponent('CargoManager')

	local total = 0
	local empty_space = cargoMgr:GetFreeSpace()
	local size_factor = empty_space / 20
	local ship_cargo = {}

	local cargoTypes = direction == 'import' and Core.params.imports or Core.params.exports

	if #cargoTypes == 1 then
		total = cargoMgr:AddCommodity(cargoTypes[1], empty_space)
		ship_cargo[cargoTypes[1]] = total
	elseif #cargoTypes > 1 then

		-- happens if there was very little space left to begin with (eg small
		-- ship with lots of equipment). if we let it through then we end up
		-- trying to add 0 cargo forever
		if size_factor < 1 then
			if Core.ships[ship] then
				Core.ships[ship]['cargo'] = ship_cargo
			end
			return 0
		end

		while total < empty_space do
			-- get random for direction
			local cargo_type = cargoTypes[Engine.rand:Integer(1, #cargoTypes)]

			-- amount based on price and size of ship
			local num = math.abs(Game.system:GetCommodityBasePriceAlterations(cargo_type.name)) * size_factor
			num = Engine.rand:Integer(num, num * 2)

			local added = math.min(num, empty_space - total)
			cargoMgr:AddCommodity(cargo_type, added)

			if ship_cargo[cargo_type] == nil then
				ship_cargo[cargo_type] = added
			else
				ship_cargo[cargo_type] = ship_cargo[cargo_type] + added
			end
			total = total + added
		end
	end
	if Core.ships[ship] then
		Core.ships[ship]['cargo'] = ship_cargo
	end
	-- if the table for direction was empty then cargo is empty and total is 0
	return total
end

Trader.doOrbit = function (ship)
	local trader = Core.ships[ship]
	local sbody = trader.starport.path:GetSystemBody()
	local body = Space.GetBody(sbody.parent.index)
	ship:AIEnterLowOrbit(body)
	trader['status'] = 'orbit'
	Core.log:add(ship, 'Ordering orbit of '..body.label)
end

local getSystem = function (ship)
	local max_range = ship:GetInstalledHyperdrive():GetMaximumRange(ship)
	max_range = math.min(max_range, 30)
	local min_range = max_range / 2;
	local systems_in_range = Game.system:GetNearbySystems(min_range)
	if #systems_in_range == 0 then
		systems_in_range = Game.system:GetNearbySystems(max_range)
	end
	if #systems_in_range == 0 then
		return nil end
	if #systems_in_range == 1 then
		return systems_in_range[1].path
	end

	local target_system = nil
	local best_prices = 0

	-- find best system for cargo
	for _, next_system in ipairs(systems_in_range) do
		if #next_system:GetStationPaths() > 0 then
			local next_prices = 0
			---@type CargoManager
			local cargoMgr = ship:GetComponent('CargoManager')

			for name, info in pairs(cargoMgr.commodities) do
				next_prices = next_prices + (next_system:GetCommodityBasePriceAlterations(name) * info.count)
			end

			if next_prices > best_prices then
				target_system, best_prices = next_system, next_prices
			end
		end
	end

	if target_system == nil then
		-- pick a random system as fallback
		target_system = systems_in_range[Engine.rand:Integer(1, #systems_in_range)]

		-- get closer systems
		local systems_half_range = Game.system:GetNearbySystems(min_range)

		if #systems_half_range > 1 then
			target_system = systems_half_range[Engine.rand:Integer(1, #systems_half_range)]
		end
	end

	-- pick a random starport, if there are any, so the game can simulate
	-- travel to it if player arrives after (see Space::DoHyperspaceTo)
	local target_starport_paths = target_system:GetStationPaths()
	if #target_starport_paths > 0 then
		return target_starport_paths[Engine.rand:Integer(1, #target_starport_paths)]
	end

	return target_system.path
end

local jumpToSystem = function (ship, target_path)
	if target_path == nil then return nil end

	local status, _, duration = ship:HyperjumpTo(target_path)

	if status ~= 'OK' then
		Core.log:add(ship, 'jump status is not OK: ' .. status)
		return status
	end

	-- update table for ship
	Core.ships[ship]['status'] = 'hyperspace_out'
	Core.ships[ship]['starport'] = nil
	Core.ships[ship]['dest_time'] = Game.time + duration
	Core.ships[ship]['jump_time'] = Game.time
	Core.ships[ship]['dest_path'] = target_path
	Core.ships[ship]['from_path'] = Game.system.path
	return status
end

Trader.getSystemAndJump = function (ship)
	-- attention all coders: trade_ships[ship].starport may be nil
	if Core.ships[ship].starport then
		local body = Space.GetBody(Core.ships[ship].starport.path:GetSystemBody().parent.index)
		local port = Core.ships[ship].starport
		return jumpToSystem(ship, getSystem(ship))
	end
end

local function isAtmo(starport)
	return starport.type ~= 'STARPORT_ORBITAL' and starport.path:GetSystemBody().parent.hasAtmosphere
end

local function canAtmo(ship)
	return (ship["atmo_shield_cap"] or 0) > 0
end

local THRUSTER_UP = Engine.GetEnumValue('ShipTypeThruster', 'UP')

Trader.isStarportAcceptableForShip = function(starport, ship)
	if not isAtmo(starport) then return true end
	if not canAtmo(ship) then return false end
	local bellyThrust = ship:GetThrusterAcceleration(THRUSTER_UP)
	local portGravity = starport.path:GetSystemBody().parent.gravity
	return bellyThrust > portGravity
end

Trader.getNearestStarport = function(ship, current)
	-- get all available routes for this model of ship
	local routes = Core.params.local_routes[ship.shipId]
	if not routes or #routes == 0 then return nil end

	-- Find the nearest starport that we can land at (other than current)
	local starport, distance
	for i = 1, #routes do
		local next_starport = routes[i].to
		if next_starport ~= current then
			local next_distance = ship:DistanceTo(next_starport)
			if Trader.isStarportAcceptableForShip(next_starport, ship) and ((starport == nil) or (next_distance < distance)) then
				starport, distance = next_starport, next_distance
			end
		end
	end
	return starport or current
end

-- Add fuel to the ship's hyperdrive
---@param ship Ship
---@param deducted number? Amount of fuel to leave empty in the drive
Trader.addHyperdriveFuel = function (ship, deducted)
	local drive = ship:GetInstalledHyperdrive()

	-- a drive must be installed
	if not drive then
		Core.log:add(ship, 'Ship has no drive!')
		return nil
	end

	-- fill the drive completely, less the amount that should be deducted
	drive:SetFuel(ship, math.max(0, drive:GetMaxFuel() - (deducted or 0)))
end

Trader.checkDistBetweenStarport = function (ship)
	local trader = Core.ships[ship]
	if not trader then return nil end
	local distance
	if trader.starport.type == "STARPORT_ORBITAL" then
		distance = ship:DistanceTo(trader.starport)
	else
		local stationsParent = trader.starport:GetSystemBody().parent.body
		distance = ship:GetAltitudeRelTo(stationsParent)
	end
	return distance >= trader.hyperjumpDist
end

-- TRADER DEFERRED TASKS
--
-- call the passed function in a specified time, checking whether we are still
-- in this system
Trader.callInThisSystem = function(t, fnc)
	local current_path = Game.system.path
	Timer:CallAt(t, function()
		if Game.system and current_path:IsSameSystem(Game.system.path) then fnc() end
	end)
end

local trader_task = {}
-- a table of functions that can be assigned for delayed execution by the trader
-- { ["fnc1"] = fnc1, ... }
-- made to serialize pending job execution
-- the task function prototype should be:
-- function(ship)

trader_task.hyperjumpAtDistance = function(ship)
	-- the player may have left the system
	local trader = Core.ships[ship]
	if not trader then return end
	if trader.status == "outbound" and trader.ts_error ~= "HIT" then
		-- if trader is not under attack and started to leave station
		if trader.starport ~= nil then
			-- if trader has not started to hyperjump
			if trader.hyperjumpDist == nil then
				trader.hyperjumpDist = Engine.rand:Integer(20000, 240000)
			end
			if Trader.checkDistBetweenStarport(ship) then
				-- if distance is large enough attempt to hyperjump
				local status = Trader.getSystemAndJump(ship)
				if status ~= 'OK' then
					ship:CancelAI()
					ship:AIDockWith(trader.starport)
					trader['status'] = 'inbound'
					trader.ts_error = 'cnt_jump_aicomp'
				end
				trader.hyperjumpDist = nil
			else
				Trader.assignTask(ship, Game.time + 10, 'hyperjumpAtDistance')
			end
		end
	else
		trader.hyperjumpDist = nil
	end
end

trader_task.doUndock = function(ship)
	-- the player may have left the system or the ship may have already undocked
	if ship:exists() and ship:GetDockedWith() then
		-- we load goods before departure
		Trader.addCargo(ship, 'export')
		if not ship:Undock() then
			-- unable to undock, try again later
			Trader.assignTask(ship, Game.time + Core.WAIT_FOR_NEXT_UNDOCK, 'doUndock')
			return true
		end
	end
end

trader_task.doRedock = function(ship)
	local trader = Core.ships[ship]
	if trader then
		if ship:exists() and ship.flightState ~= 'HYPERSPACE' then
			trader['starport'] = Trader.getNearestStarport(ship, trader.starport)

			-- TODO: needs a proper failure state
			if trader.starport then
				ship:AIDockWith(trader.starport)
				trader['status'] = 'inbound'
				trader.ts_error = "dock_aft_6h"
			end
		end
	end
end

-- at the appointed time the specified function will be executed, in addition,
-- it will be executed only in the same system in which it is assigned, and
-- also the assignment can be serialized
-- ship: object
-- delay: absolute game time, when to execute the task
-- fn_name: string, one of the keys in the table 'trader_task'
Trader.assignTask = function(ship, delay, fn_name)
	local trader = Core.ships[ship]
	if trader then
		trader.delay = delay
		trader.fnc = fn_name
		Trader.callInThisSystem(delay, function()
			trader.delay = nil
			trader.fnc = nil
			trader_task[fn_name](ship)
		end)
	end
end

Trader.spawnInCloud = function(ship_name, cloud, route, dest_time)
	-- choose random local route for choosen ship,
	-- the hyperjump target depends on this (for multiple system)
	local ship = Space.SpawnShip(ship_name, 0, 0, {cloud.from, route.from:GetSystemBody().path, dest_time})
	ship:SetLabel(Ship.MakeRandomLabel())
	Trader.addEquip(ship)
	Trader.addHyperdriveFuel(ship)
	Trader.addCargo(ship, 'import')
	Core.ships[ship] = {
		status		= 'hyperspace',
		dest_time	= dest_time,
		dest_path	= Game.system.path,
		from_path	= cloud.from,
		ship_name	= ship_name,
		route = route
	}
end

return Trader
