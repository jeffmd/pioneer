-- Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
-- Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

---@class Ship
local Ship = package.core['Ship']

local Game = package.core['Game']
local Engine = require 'Engine'
local Event = require 'Event'
local Serializer = require 'Serializer'
local ShipDef = require 'ShipDef'
local Timer = require 'Timer'
local Lang = require 'Lang'
local CargoManager = require 'CargoManager'
local CommodityType = require 'CommodityType'
local Character = require 'Character'
local Comms = require 'Comms'
local EquipSet = require 'EquipSet'

local l = Lang.GetResource("ui-core")

--
-- Class: Ship
--
-- Class representing a ship. Inherits from <Body>.
--

function Ship:Constructor()
	self:SetComponent('CargoManager', CargoManager.New(self))
	self:SetComponent('EquipSet', EquipSet.New(self))

	self:UpdateWeaponSlots()

	-- Timers cannot be started in ship constructors before Game is fully set,
	-- so trigger a lazy event to setup gameplay timers.
	--
	-- TODO: this feels a little bit hacky, but it's the best way to decouple
	-- Ship itself and "game balance" code that drags in a bunch of dependencies
	Event.Queue('onShipCreated', self)
end

function Ship:OnShipTypeChanged()
	-- immediately update any needed components or properties
	self:GetComponent('EquipSet'):OnShipTypeChanged()

	self:UpdateWeaponSlots()
end

---@private
function Ship:UpdateWeaponSlots()
	local equipSet = self:GetComponent('EquipSet')
	local gunManager = self:GetComponent('GunManager')

	for _, slot in ipairs(equipSet:GetAllSlotsOfType("weapon", true)) do
		if not slot.gimbal then
			logWarning('Missing hardpoint gimbal on ship {} for slot {}' % { self.shipId, slot.id })
		end

		local gimbal = Vector2(table.unpack(slot.gimbal or { 1, 1 }))
		local ok = gunManager:AddWeaponMount(slot.id, slot.tag, gimbal)

		if not ok then
			logWarning('Unable to add weapon mount slot {} on ship {}' % { slot.id, self.shipId })
		end
	end
end

-- class method
function Ship.MakeRandomLabel ()
	local letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	local a = Engine.rand:Integer(1, #letters)
	local b = Engine.rand:Integer(1, #letters)
	return string.format("%s%s-%04d", letters:sub(a,a), letters:sub(b,b), Engine.rand:Integer(10000))
end

-- This is a protected table (accessors only) in which details of each ship's crew
-- will be stored.
local CrewRoster = {}

--
-- Group: Methods
--

-- Method: GetInstalledHyperdrive
--
-- Return the ship's installed hyperdrive equipment item if present.
--
-- > if ship:GetInstalledHyperdrive() then HyperdriveOverloadAndExplode(ship) end
--
-- Status:
--
--   stable
--
---@return Equipment.HyperdriveType? hyperdrive
function Ship:GetInstalledHyperdrive()
	---@type Equipment.HyperdriveType[]
	local drives = self:GetComponent("EquipSet"):GetInstalledOfType("hyperdrive")
	return drives[1]
end


--
-- Method: IsHyperjumpAllowed
--
-- Check if hyperjump is allowed, return bool and minimum allowed
-- altitude / distance.
--
-- > is_allowed, distance = ship:IsHyperjumpAllowed()
--
--
-- Return:
--
--   is_allowed - Boolean. True if allowed at ships current position,
--                flase otherwise
--
--   distance - The minimum allowed altitude from planetary body, or
--              distance from orbital space station, for a legal hyper juump.
--
-- Availability:
--
--  2016 August
--
-- Status:
--
--  experimental
--
Ship.IsHyperjumpAllowed = function(self)

	-- in meters
	local MIN_PLANET_HYPERJUMP_ALTITUDE = 10000
	local MIN_SPACESTATION_HYPERJUMP_DISTANCE = 1000

	-- get closest non-dynamic (not ships, cargo, missiles) body of ship
	local body = self.frameBody

	-- If no body close by, anything goes
	if not body then
		return true, 0
	end

	-- if alt is nil, then outside frame of ref -> OK to jump
	local check = function(alt, dist) return not alt or alt >= dist, dist end

	-- if body exists and not a planet -> allowed
	if body and body.type == 'STARPORT_ORBITAL' then
		return check(body:DistanceTo(self), MIN_SPACESTATION_HYPERJUMP_DISTANCE)
	end

	-- if on a planet:
	-- always allowed if not body.hasAtmosphere?

	-- if body is a planet or a star:
	if body and body.type ~= 'GRAVPOINT' then
		local lat, long, altitude = self:GetGroundPosition()
		return check(altitude, MIN_PLANET_HYPERJUMP_ALTITUDE)
	end
end

--
-- Method: HyperjumpTo
--
-- Hyperjump ship to system. Makes sure the ship has a hyper drive,
-- that target is withn range, and ship has enough fuel, before
-- initiating the hyperjump countdown. In addition, through the
-- optional argument, the ship can fly to a safe distance, compliant
-- with local authorities' regulation, before initiating the jump.
--
-- > status = ship:HyperjumpTo(path)
-- > status = ship:HyperjumpTo(path, is_legal)
--
-- Parameters:
--
--   path - a <SystemPath> for the destination system
--
--   isLegal - an optional Boolean argument, defaults to false. If
--             true AI will fly the ship ship to legal distance
--             before jumping
--
-- Return:
--
--   status - a <Constants.ShipJumpStatus> string that tells if the ship can
--            hyperspace and if not, describes the reason
--
-- Availability:
--
--  2015
--
-- Status:
--
--  experimental
--
Ship.HyperjumpTo = function (self, path, is_legal)
	local engine = self:GetInstalledHyperdrive()
	local wheels = self:GetWheelState()
	if not engine then
		return "NO_DRIVE"
	end

	-- default to false, if nil:
	is_legal = not (is_legal == nil) or is_legal

	-- only jump from safe distance
	local is_allowed, distance = self:IsHyperjumpAllowed()

	if is_legal and self.frameBody and not is_allowed then
		print("---> Engage AI for safe distance of hyperjump")
		self:AIEnterLowOrbit(self.frameBody)
	end

	-- display warning if gear is not retracted
	if self == Game.player and wheels ~= 0 then
		Comms.ImportantMessage(l.ERROR_LANDING_GEAR_DOWN)
	end

	return engine:HyperjumpTo(self, path)
end


Ship.CanHyperjumpTo = function(self, path)
	return self:GetHyperspaceDetails(path) == 'OK'
end

-- ship:GetHyperspaceDetails(destination)      --  get details of jump from current system to 'destination'
-- ship:GetHyperspaceDetails(source, destination) -- get details of jump from 'source' to 'destination'
Ship.GetHyperspaceDetails = function (self, source, destination)
	if destination == nil then
		if not Game.system then
			return "DRIVE_ACTIVE", 0, 0, 0
		end
		destination = source
		source = Game.system.path
	end

	local engine = self:GetInstalledHyperdrive()
	if not engine then
		return "NO_DRIVE", 0, 0, 0
	elseif source:IsSameSystem(destination) then
		return "CURRENT_SYSTEM", 0, 0, 0
	end

	local distance, fuel, duration = engine:CheckJump(self, source, destination)
	local status = "OK"

	if not duration then
		duration = 0
		fuel = 0
		status = "OUT_OF_RANGE"
	elseif fuel > engine.storedFuel then
		status = "INSUFFICIENT_FUEL"
	end
	return status, distance, fuel, duration
end

Ship.GetHyperspaceRange = function (self)
	local engine = self:GetInstalledHyperdrive()
	if not engine then
		return 0, 0
	end
	return engine:GetRange(self)
end

-- Method: FireMissileAt
--
-- Fire a missile at the given target
--
-- > missile_object = ship:FireMissileAt(type, target)
--
-- Parameters:
--
--   missile - an equipment type object for the missile type.
--          Specifying "any" will launch the first available missile.
--          You can also provide a number matching the index
--          (within the 'missile' equipment slot) of the missile
--          you want to launch.
--
--   target - the <Ship> to fire the missile at
--
-- Return:
--
--   missile_object - the fired missile (a <Missile> object),
--                    or nil if no missile was fired
--
-- Availability:
--
--   alpha 10
--
-- Status:
--
--   experimental
--
---@param missile EquipType
function Ship:FireMissileAt(missile, target)
	local equipSet = self:GetComponent("EquipSet")

	if missile == "any" then
		missile = equipSet:GetInstalledOfType("missile")[1]
	end

	-- No missile available to fire, likely this function was called from a C++ action binding
	if not missile then
		return nil
	end

	-- FIXME: handle multiple-count missile mounts
	equipSet:Remove(missile)

	local missile_object = self:SpawnMissile(missile.missile_stats, target)

	if missile_object then
		if target then
			Event.Queue("onShipFiring", self)
		end
		-- Let's keep a safe distance before activating this device, shall we ?
		Timer:CallEvery(2, function ()
			if not missile_object:exists() then -- Usually means it has already exploded
				return true
			end
-- TODO: Due to the changes in missile, DistanceTo cause an error
--			if missile_object:DistanceTo(self) < 300 then
--				return false
--			else
				missile_object:Arm()
--				return true
--			end
		end)
	end

	return missile_object
end

--
-- Method: Refuel
--
-- Use the content of the cargo to refuel
--
-- > used_units = ship:Refuel(1)
--
-- Parameters:
--
--   fuelType - the type of fuel to remove from the cargo hold.
--   amount   - the amount of fuel (in tons) to take from the cargo
--
-- Result:
--
--   used_units - how much fuel units have been used to fuel the tank.
--
-- Availability:
--
--   alpha 26
--
-- Status:
--
--   experimental
--
---@param fuelType CommodityType
---@param amount integer
function Ship:Refuel(fuelType, amount)
	if self.fuel == 100 then return 0 end -- tank is completely full

	---@type CargoManager
	local cargoMgr = self:GetComponent('CargoManager')

	local fuelTankMass = ShipDef[self.shipId].fuelTankMass
	local needed = math.clamp(math.floor(fuelTankMass - self.fuelMassLeft), 0, amount)
	needed = math.min(needed, cargoMgr:CountCommodity(fuelType))

	local removed = cargoMgr:RemoveCommodity(fuelType, needed)
	self:SetFuelPercent(math.clamp(self.fuel + removed * 100 / fuelTankMass, 0, 100))
	return removed
end

--
-- Method: Jettison
--
-- Jettison one unit of the given cargo type.  The item must be present in
-- the ship's equipment/cargo set, and will be removed by this call.
--
-- > success = ship:Jettison(item)
--
-- On sucessful jettison, the <EventQueue.onJettison> event is triggered.
--
-- Parameters:
--
--   item - a commodity type object (e.g., Commodity.radioactives)
--          specifying the type of item to jettison.
--
-- Result:
--
--   success - true if the item was jettisoned, false if the ship has no items
--             of that type or the ship is not in open flight
--
-- Availability:
--
--   alpha 10
--
-- Status:
--
--   experimental
--
---@param cargoType CommodityType
function Ship:Jettison(cargoType)
	if self.flightState ~= "FLYING" and self.flightState ~= "DOCKED" and self.flightState ~= "LANDED" then
		return false
	end

	---@type CargoManager
	local cargoMgr = self:GetComponent('CargoManager')
	if cargoMgr:RemoveCommodity(cargoType, 1) < 1 then
		return false
	end

	if self.flightState == "FLYING" then
		self:SpawnCargo(cargoType)
		Event.Queue("onJettison", self, cargoType)
	elseif self.flightState == "DOCKED" then
		Event.Queue("onCargoUnload", self:GetDockedWith(), cargoType)
	elseif self.flightState == "LANDED" then
		Event.Queue("onCargoUnload", self.frameBody, cargoType)
	end
end

--
-- Method: OnScoopCargo
--
-- Function triggered from C++ to handle scooping a cargo body.
--
-- Triggers the <onShipScoopCargo> event when an attempt is made
-- to scoop the cargo into the ship's hold.
--
-- Returns true if the body was successfully scooped.
--
function Ship:OnScoopCargo(cargoType)
	---@type CargoManager
	local cargoMgr = self:GetComponent('CargoManager')

	if cargoType:Class() ~= CommodityType then
		return false
	end

	-- Rate-limit scooping to avoid triggering hundreds of events
	-- if the cargo hold is full
	local lastScoop = self:hasprop('last_scoop_time') and self.last_scoop_time or 0
	if Game.time - lastScoop < 0.5 then
		return false
	else
		self:setprop('last_scoop_time', Game.time)
	end

	local success = cargoMgr:AddCommodity(cargoType, 1)

	Event.Queue('onShipScoopCargo', self, success, cargoType)

	return success
end


--
-- Method: GetGPS
--
-- Get altitude, speed, and position of a ship
--
-- > alt, vspd, lat, long = ship:GetGPS()
--
-- Returns:
--
--   alt - altitude
--
--   vspd - vertical speed
--
--   lat - latitude
--
--   lon - longitude
--
-- Availability:
--
--   November, 2023
--
-- Status:
--
--   experimental
--
function Ship:GetGPS()
   if not self.frameBody then return end
   local lat, lon, altitude = self:GetGroundPosition()
   local vspd = self:GetVelocityRelTo(self.frameBody):dot(self:GetPositionRelTo(self.frameBody):normalized())
   lat = math.rad2deg(lat)
   lon = math.rad2deg(lon)
   return altitude, vspd, lat, lon
end

--
-- Method: Enroll
--
-- Enroll a [Character] as a member of the ship's crew
--
-- > success = ship:Enroll(newCrewMember)
--
-- Parameters:
--
--   newCrewMember - a [Character] instance
--
-- Returns:
--
--   success - True indicates that the Character became a member of the crew. False indicates
--             that the Character did not become a member of the crew, either because there
--             is no room for the Character on the crew roster, or because they are already
--             enrolled as crew on another ship.
--
-- Availability:
--
--   alpha 31
--
-- Status:
--
--   experimental
--
local isNotAlreadyEnrolled = function (crewmember)
	for ship,crew in pairs(CrewRoster) do
		for key,existingmember in pairs(crew) do
			if existingmember == crewmember
			then
				return false
			end
		end
	end
	return true
end

Ship.Enroll = function (self,newCrewMember)
	if not (
		type(newCrewMember) == "table" and
		getmetatable(newCrewMember) and
		getmetatable(newCrewMember).class == 'Character'
	) then
		error("Ship:Enroll: newCrewMember must be a Character object")
	end
	if not CrewRoster[self] then CrewRoster[self] = {} end
	if #CrewRoster[self] < ShipDef[self.shipId].maxCrew
	and isNotAlreadyEnrolled(newCrewMember)
	then
		newCrewMember:CheckOut() -- Don't want other scripts using our crew for missions etc
		table.insert(CrewRoster[self],newCrewMember)
		Event.Queue('onJoinCrew',self,newCrewMember) -- Signal any scripts that care!
		return true
	else
		return false
	end
end

--
-- Method: Dismiss
--
-- Dismiss a [Character] as a member of the ship's crew
--
-- > success = ship:Dismiss(crewMember)
--
-- Parameters:
--
--   crewMember - a [Character] instance
--
-- Returns:
--
--   success - True indicates that the Character is no longer a member of the crew. False
--             indicates that the Character was not removed, either because they were not
--             a member of the crew, or because they could not be removed because of a
--             special case. Currently the only special case is that the player's Character
--             cannot be dismissed from a crew.
--
-- Availability:
--
--   alpha 31
--
-- Status:
--
--   experimental
--

Ship.Dismiss = function (self,crewMember)
	if not CrewRoster[self] then return false end
	if not (
		type(crewMember) == "table" and
		getmetatable(crewMember) and
		getmetatable(crewMember).class == 'Character'
	) then
		error("Ship:Dismiss: crewMember must be a Character object")
	end
	if crewMember.player then return false end -- Can't dismiss the player
	for key,existingCrewMember in pairs(CrewRoster[self]) do
		if crewMember == existingCrewMember
		then
			table.remove(CrewRoster[self],key)
			Event.Queue('onLeaveCrew',self,crewMember) -- Signal any scripts that care!
			crewMember:Save() -- Crew member can pop up elsewhere
			return true
		end
	end
	return false
end

--
-- Method: GenerateCrew
--
-- Generates a full crew complement for a ship that has no initialised crew list.
-- Intended to be run automatically by [EachCrewMember] when querying arbitrary ships.
--
-- > ship:GenerateCrew()
--
-- Availability:
--
--   alpha 31
--
-- Status:
--
--   experimental
--
Ship.GenerateCrew = function (self)
	if CrewRoster[self] then return end -- Bottle out if there's ever been a crew
	for i = 1, ShipDef[self.shipId].maxCrew do
		local newCrew = Character.New()
		newCrew:RollNew(true)
		self:Enroll(newCrew)
	end
end

--
-- Method: CrewNumber
--
-- Returns the number of the current crew employed on the ship.
--
-- > ship:CrewNumber()
--
-- Availability:
--
--   20140404
--
-- Status:
--
--   experimental
--
Ship.CrewNumber = function (self)
	return CrewRoster[self] and #CrewRoster[self] or 0
end

--
-- Method: EachCrewMember
--
-- Returns an iterator function which returns each crew member in turn
--
-- > for crew in ship:EachCrewMember() do print(crew.name) end
--
-- Returns:
--
--   crew - A [Character], once per crew member per call
--
-- Availability:
--
--   alpha 31
--
-- Status:
--
--   experimental
--
Ship.EachCrewMember = function (self)
	-- If there's no crew, magic one up.
	if not CrewRoster[self] then self:GenerateCrew() end
	-- Initialise and return enclosed iterator
	local i = 0
	return function ()
		i = i + 1
		return CrewRoster[self][i]
	end
end

--
-- Method: HasCorrectCrew
--
-- Determine whether a ship has the minimum crew required for launch
--
-- > canLaunch = ship:HasCorrectCrew()
--
-- Returns:
--
--   canLaunch - Boolean, true if ship has minimum required crew for launch, otherwise false/nil
--
-- Availability:
--
--   alpha 31
--
-- Status:
--
--   experimental
--
Ship.HasCorrectCrew = function (self)
	return (CrewRoster[self] and (
		#CrewRoster[self] >= ShipDef[self.shipId].minCrew and
		#CrewRoster[self] <= ShipDef[self.shipId].maxCrew
	))
end

-- LOADING AND SAVING

local loaded_data

local onGameStart = function ()
	if loaded_data then
		CrewRoster = loaded_data
		Event.Queue('crewAvailable') -- Signal any scripts that depend on initialised crew
	end

	loaded_data = nil
end

local onGameEnd = function()
	CrewRoster = {}
end

local serialize = function ()
	-- Remove non-existent ships first, or the serializer will choke
	for crewedShip,crew in pairs(CrewRoster) do
		if not crewedShip:exists() then
			CrewRoster[crewedShip] = nil
		end
	end
	return CrewRoster
end

local unserialize = function (data)
	loaded_data = data
end

-- Function to check whether ships exist after hyperspace, and if they do not,
-- to remove their crew from the roster.
local onShipEnterSystem = function (ship)
	if ship:IsPlayer() then
		for crewedShip,crew in pairs(CrewRoster) do
			if not crewedShip:exists() then
				CrewRoster[crewedShip] = nil
			end
		end
	end

	local engine = ship:GetInstalledHyperdrive()
	if engine then
		engine:OnLeaveHyperspace(ship)
	end
end

local onShipDestroyed = function (ship, attacker)
	-- When a ship is destroyed, mark is crew as dead
	-- and remove the ship's crew from CrewRoster
	if CrewRoster[ship] then
		for key,crewMember in pairs(CrewRoster[ship]) do
			crewMember.dead = true
		end
		CrewRoster[ship] = nil
	end
end

-- Reinitialize cargo-related ship properties when changing ship type
---@param ship Ship
local onShipTypeChanged = function (ship)
	ship:GetComponent('CargoManager'):OnShipTypeChanged()
end

Event.Register("onShipEnterSystem", onShipEnterSystem)
Event.Register("onShipDestroyed", onShipDestroyed)
Event.Register("onGameStart", onGameStart)
Event.Register("onGameEnd", onGameEnd)
Event.Register("onShipTypeChanged", onShipTypeChanged)
Serializer:Register("ShipClass", serialize, unserialize)

return Ship
