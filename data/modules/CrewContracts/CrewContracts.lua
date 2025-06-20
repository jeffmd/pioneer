-- Copyright © 2013 Pioneer Developers. See AUTHORS.txt for details
-- Licensed under the terms of the GPL v3. See licenses/GPL-3.txt

local Lang = require 'Lang'
local Event = require 'Event'
local Serializer = require 'Serializer'
local Engine = require 'Engine'
local Game = require 'Game'
local Character = require 'Character'
local Format = require 'Format'
local Timer = require 'Timer'
local utils = require 'utils'
local PlayerState = require 'PlayerState'

-- This module allows the player to hire crew members through BB adverts
-- on stations, and handles periodic events such as their wages.

local l = Lang.GetResource("module-crewcontracts")
local lui = Lang.GetResource("ui-core")

local wage_period = 604800 -- a week of seconds

-- The contract for a crew member is a table containing their weekly wage,
-- the date that they should next be paid and the amount outstanding if
-- the player has been unable to pay them.
--
-- contract = {
--   wage = 0,
--   payday = 0,
--   outstanding = 0,
-- }

---------------------- Part 1 ----------------------
-- Life aboard ship

local boostCrewSkills = function (crewMember)
	-- Each week, there's a small chance that a crew member gets better
	-- at each skill, due to the experience of working on the ship.

	-- If they fail their intelligence roll, they learn nothing.
	if not crewMember:TestRoll('intelligence') then return end

	-- The attributes to be tested and possibly enhanced. These will be sorted
	-- by their current value, but appear first in arbitrary order.
	local attribute = {
		{'engineering',crewMember.engineering},
		{'piloting',crewMember.piloting},
		{'navigation',crewMember.navigation},
		{'sensors',crewMember.sensors},
	}
	table.sort(attribute,function (a,b) return a[2] > b[2] end)
	-- The sorted attributes mean that the highest scoring attribute gets the
	-- first opportunity for improvement. The next loop actually makes it harder
	-- for the highest scoring attributes to improve, so if they fail, the next
	-- one is given an opportunity. At most one attribute will be improved, and
	-- the distribution means that, for example, a pilot will improve in piloting
	-- first, but once that starts to get very good, the other skills will start
	-- to see an improvement.

	for i = 1,#attribute do
		-- A carefully weighted test here. Scores will creep up to the low 50s.
		if not crewMember:TestRoll(attribute[i][1],math.floor(attribute[i][2] * 0.2 - 10)) then
			-- They learned from their failure,
			crewMember[attribute[i][1]] = crewMember[attribute[i][1]]+1
			-- but only on this skill.
			break
		end
	end
end

local scheduleWages = function (crewMember)
	-- Must have a contract to be treated like crew
	if not crewMember.contract then return end

	local payWages
	payWages = function ()
		local contract = crewMember.contract
		-- Check if crew member has been dismissed
		if not contract then return end

		if PlayerState.GetMoney() > contract.wage then
			PlayerState.AddMoney(0 - contract.wage)
			-- Being paid can make awkward crew like you more
			if not crewMember:TestRoll('playerRelationship') then
				crewMember.playerRelationship = crewMember.playerRelationship + 1
			end
		else
			contract.outstanding = contract.outstanding + contract.wage
			crewMember.playerRelationship = crewMember.playerRelationship - 1
			Character.persistent.player.reputation = Character.persistent.player.reputation - 0.5
		end

		-- Attempt to pay off any arrears
		local arrears = math.min(PlayerState.GetMoney(),contract.outstanding)
		PlayerState.AddMoney(0 - arrears)
		contract.outstanding = contract.outstanding - arrears

		-- The crew gain experience each week, and might get better
		boostCrewSkills(crewMember)

		-- Schedule the next pay day, if there is one.
		if contract.payday and not crewMember.dead then
			contract.payday = contract.payday + wage_period
			Timer:CallAt(math.max(Game.time + 5,contract.payday),payWages)
		end
	end

	Timer:CallAt(math.max(Game.time + 1,crewMember.contract.payday),payWages)
end

-- This gets run just after crew are restored from a saved game
Event.Register('crewAvailable',function()
	-- scheduleWages() for everybody
	for crewMember in Game.player:EachCrewMember() do
		scheduleWages(crewMember)
	end
end)

-- This gets run whenever a crew member joins a ship
Event.Register('onJoinCrew',function(ship, crewMember)
	if ship:IsPlayer() then
		scheduleWages(crewMember)
	end
end)

-- This gets run whenever a crew member leaves a ship
Event.Register('onLeaveCrew',function(ship, crewMember)
	if ship:IsPlayer() and crewMember.contract then
		-- Prepare them for the job market
		crewMember.estimatedWage = crewMember.contract.wage + 5
		-- Terminate their contract
		crewMember.contract = nil
	end
end)

---------------------- Part 2 ----------------------
-- The bulletin board

local nonPersistentCharactersForCrew = {}
local stationsWithAdverts = {}

local wageFromScore = function(score)
	-- Default score is four 15s (=60). Anybody with that or less
	-- gets minimum wage offered.
	score = math.max(0,score-60)
	return math.floor(score * score / 100) + 10
end

local checkOffer = function(offer)
	-- Force wage offers to be in correct range
	return math.max(1,offer)
end

local crewInThisStation -- Table of available folk available for hire here
local candidate -- Run-time "static" variable for onChat
local offer

local onChat = function (form,ref,option)

	local station = stationsWithAdverts[ref]

	if option == -1 then
		-- Hang up
		candidate = nil
		form:Close()
		return
	end

	if option == 0 then
		-- Not currently candidate; option values indicate crewInThisStation index
		candidate = nil
		-- Re-initialise crew list, and build it fresh
		crewInThisStation = {}

		-- Add any non-persistent characters (which are persistent only in the sense
		-- that this BB ad is storing them)
		for k,c in ipairs(nonPersistentCharactersForCrew[station]) do
			table.insert(crewInThisStation,c)
			c.experience = c.engineering
							+c.piloting
							+c.navigation
							+c.sensors
			-- Base wage on experience
			c.estimatedWage = c.estimatedWage or wageFromScore(c.experience)
			c.estimatedWage = utils.round(c.estimatedWage, 1)
		end

		-- Now look for any persistent characters that are available in this station
		-- and have some sort of crew experience (however minor)
		-- and were last seen less than a month ago
		for c in Character.Find(function (c)
									return true
										and c.lastSavedSystemPath == station.path
										and c.available
										and (
											c.engineering > 16
											or c.piloting > 16
											or c.navigation > 16
											or c.sensors > 16
										)
										and Game.time - c.lastSavedTime < 2419200 -- (28 days)
								end) do
			table.insert(crewInThisStation,c)
			c.experience = c.engineering
											+c.piloting
											+c.navigation
											+c.sensors
			-- Either base wage on experience, or as a slight increase on their previous wage
			-- (which should only happen if this candidate was dismissed with wages owing)
			c.estimatedWage = math.max(c.contract and (c.contract.wage + 5) or 0, c.estimatedWage or wageFromScore(c.experience))
			c.estimatedWage = utils.round(c.estimatedWage, 1)
		end

		form:ClearFace()
		form:Clear()
		form:SetTitle(l.CREW_FOR_HIRE)
		local numCrewInThisStation = 0
		for k,c in ipairs(crewInThisStation) do
			numCrewInThisStation = numCrewInThisStation + 1
		end
		if numCrewInThisStation > 0 then
			form:SetMessage("\n"..l.POTENTIAL_CREW_MEMBERS:interp({station=station.label}))
			for k,c in ipairs(crewInThisStation) do
				form:AddOption(l.CREWMEMBER_WAGE_PER_WEEK:interp({potentialCrewMember = c.name,wage = Format.Money(c.estimatedWage)}),k)
			end
		else
			form:SetMessage("\n"..l.NO_CREW_AVAILABLE:interp({station=station.label}))
		end
	end

	local showCandidateDetails = function (response)
		local experience =
			candidate.experience > 160 and l.VETERAN_TIME_SERVED_CREW_MEMBER or
			candidate.experience > 140 and l.TIME_SERVED_CREW_MEMBER or
			candidate.experience > 100 and l.MINIMAL_TIME_SERVED_ABOARD_SHIP or
			candidate.experience >  60 and l.SOME_EXPERIENCE_IN_CONTROLLED_ENVIRONMENTS or
			candidate.experience >  10 and l.SIMULATOR_TRAINING_ONLY or
			l.NO_EXPERIENCE
		form:SetFace(candidate)
		form:Clear()
		form:SetTitle(candidate.name)
		candidate:PrintStats()
		print("Attitude: ",candidate.playerRelationship)
		print("Aspiration: ",candidate.estimatedWage)
		if response == "" then response = "\r" end
		form:SetMessage(l.CREWDETAILSHEETBB:interp({
			name = candidate.name,
			experience = experience,
			wage = Format.Money(offer),
			response = response,
		}))
		form:AddOption(l.MAKE_OFFER_OF_POSITION_ON_SHIP_FOR_STATED_AMOUNT,1)
		form:AddOption(l.SUGGEST_NEW_WEEKLY_WAGE_OF_N:interp({newAmount=Format.Money(checkOffer(offer+10))}),2)
		form:AddOption(l.SUGGEST_NEW_WEEKLY_WAGE_OF_N:interp({newAmount=Format.Money(checkOffer(offer+5))}),3)
		form:AddOption(l.SUGGEST_NEW_WEEKLY_WAGE_OF_N:interp({newAmount=Format.Money(checkOffer(offer-5))}),4)
		form:AddOption(l.SUGGEST_NEW_WEEKLY_WAGE_OF_N:interp({newAmount=Format.Money(checkOffer(offer-10))}),5)
		form:AddOption(l.ASK_CANDIDATE_TO_SIT_A_TEST,6)
		form:AddOption(l.GO_BACK, 0)
	end

	if option > 0 then

		if not candidate then
			-- Absence of candidate indicates that option is to select a candidate,
			-- and that is all.
			candidate = crewInThisStation[option]
			offer = candidate.estimatedWage
			showCandidateDetails('')
			return
		end

		if option == 1 then
			-- Offer of employment
			form:Clear()
			form:SetTitle(candidate.name)
			if candidate:TestRoll('playerRelationship',15) then
				-- Boosting roll by 15, because they want to work
				if Game.player:Enroll(candidate) then
					candidate.contract = {
						wage = offer,
						payday = Game.time + wage_period,
						outstanding = 0
					}
					form:SetMessage(l.THANKS_ILL_GET_SETTLED_ON_BOARD_IMMEDIATELY)
					form:AddOption(l.GO_BACK, 0)
					for k,v in ipairs(nonPersistentCharactersForCrew[station]) do
						-- Take them off the available list in the ad
						if v == candidate then table.remove(nonPersistentCharactersForCrew[station],k) end
					end
				else
					form:SetMessage(l.THERE_DOESNT_SEEM_TO_BE_SPACE_FOR_ME_ON_BOARD)
					form:AddOption(l.GO_BACK, 0)
				end
			else
				form:SetMessage(l.IM_SORRY_YOUR_OFFER_ISNT_ATTRACTIVE_TO_ME)
				form:AddOption(l.GO_BACK, 0)
			end
			offer = nil
			candidate = nil
		end

		if option == 2 then
			-- Player suggested to increase the offer with $10
			candidate.playerRelationship = candidate.playerRelationship + 2
			offer = checkOffer(offer + 10)
			candidate.estimatedWage = offer -- They'll now re-evaluate themself
			showCandidateDetails(l.THATS_EXTREMELY_GENEROUS_OF_YOU)
		end

		if option == 3 then
			-- Player suggested an extra $5
			candidate.playerRelationship = candidate.playerRelationship + 1
			offer = checkOffer(offer + 5)
			candidate.estimatedWage = offer -- They'll now re-evaluate themself
			showCandidateDetails(l.THAT_CERTAINLY_MAKES_THIS_OFFER_LOOK_BETTER)
		end

		if option == 4 then
			-- Player suggested $5 less
			candidate.playerRelationship = candidate.playerRelationship - 1
			if candidate:TestRoll('playerRelationship') then
				offer = checkOffer(offer - 5)
				showCandidateDetails(l.OK_I_SUPPOSE_THATS_ALL_RIGHT)
			else
				showCandidateDetails(l.IM_SORRY_IM_NOT_PREPARED_TO_GO_ANY_LOWER)
			end
		end

		if option == 5 then
			-- Player suggested lowering the offer with $10
			candidate.playerRelationship = candidate.playerRelationship - 2
			if candidate:TestRoll('playerRelationship') then
				offer = checkOffer(offer - 10)
				showCandidateDetails(l.OK_I_SUPPOSE_THATS_ALL_RIGHT)
			else
				showCandidateDetails(l.IM_SORRY_IM_NOT_PREPARED_TO_GO_ANY_LOWER)
			end
		end

		if option == 6 then
			-- Player asks candidate to perform a test
			form:Clear()
			form:SetTitle(candidate.name)
			local general,engineering,piloting,navigation,sensors = 0,0,0,0,0
			for i = 1,10 do
				if candidate:TestRoll('intelligence') then general = general + 10 end
				if candidate:TestRoll('engineering') then engineering = engineering + 10 end
				if candidate:TestRoll('piloting') then piloting = piloting + 10 end
				if candidate:TestRoll('navigation') then navigation = navigation + 10 end
				if candidate:TestRoll('sensors') then sensors = sensors + 10 end
			end
			-- Candidates hate being tested.
			candidate.playerRelationship = candidate.playerRelationship - 1
			-- Show results
			form:SetMessage(l.CREWTESTRESULTSBB:interp{
				general = general,
				engineering = engineering,
				piloting = piloting,
				navigation = navigation,
				sensors = sensors,
				overall = math.ceil((general+general+engineering+piloting+navigation+sensors)/6),
			})
			form:AddOption(l.GO_BACK, 7)
		end

		if option == 7 then
			-- Player is done looking at the test results, just return
			showCandidateDetails('')
		end

	end
end

local isEnabled = function (ref)
	local station = stationsWithAdverts[ref]
	local numCrewmenAvailable = 0
	for k,v in pairs(nonPersistentCharactersForCrew[station]) do
		numCrewmenAvailable = numCrewmenAvailable + 1
	end
	return numCrewmenAvailable > 0
end

local function N_equilibrium(station) -- This function is included unchanged from the Ship Market
	local pop = station.path:GetSystemBody().parent.population -- E.g. Earth=7, Mars=0.3
	local pop_bonus = 9 * math.log(pop * 0.45 + 1)       -- something that gives resonable result
	if station.type == "STARPORT_SURFACE" then
		pop_bonus = pop_bonus * 1.5
	end

	return 2 + pop_bonus
end

local newCrew = function()
	local hopefulCrew = Character.New()
	-- Roll new stats, with a 1/3 chance that they're utterly inexperienced
	hopefulCrew:RollNew(Engine.rand:Integer(0, 2) > 0)
	-- Make them a title if they're good at anything
	local maxScore = math.max(hopefulCrew.engineering,
								hopefulCrew.piloting,
								hopefulCrew.navigation,
								hopefulCrew.sensors)
	if maxScore > 45 then
		if hopefulCrew.engineering == maxScore then hopefulCrew.title = lui.SHIPS_ENGINEER end
		if hopefulCrew.piloting == maxScore then hopefulCrew.title = lui.PILOT end
		if hopefulCrew.navigation == maxScore then hopefulCrew.title = lui.NAVIGATOR end
		if hopefulCrew.sensors == maxScore then hopefulCrew.title = lui.SENSORS_AND_DEFENCE end
	end
	return hopefulCrew
end

local function removeAd (station, num)
	if not nonPersistentCharactersForCrew[station] then
		nonPersistentCharactersForCrew[station] = {}
	end
	table.remove(nonPersistentCharactersForCrew[station], num)
end

local onCreateBB = function (station)
	-- Create non-persistent Characters as available crew
	nonPersistentCharactersForCrew[station] = {}

	-- Only one crew hiring thingy per station
	stationsWithAdverts[station:AddAdvert({
		title       = l.CREW_FOR_HIRE_TITLE,
		description = l.CREW_FOR_HIRE_DESC,
		icon        = "crew_contracts",
		onChat      = onChat,
		isEnabled   = isEnabled})] = station

	-- Number is based on population, nicked from Assassinations.lua and tweaked
	for i = 1, Engine.rand:Poisson(N_equilibrium(station)) do
		table.insert(nonPersistentCharactersForCrew[station],newCrew())
	end
end
Event.Register("onCreateBB", onCreateBB)

local onUpdateBB = function (station)
	local tau = 7*24                              -- half life of a crew advert in hours
	local lambda = 0.693147 / tau                 -- removal probability= ln(2) / tau
	local prod = N_equilibrium(station) * lambda  -- creation probability

	-- remove with decay rate lambda. Call ONCE/hour for each crew advert in station
	for k,v in pairs(nonPersistentCharactersForCrew[station]) do
		if Engine.rand:Number(0,1) < lambda then  -- remove one random crew
			table.remove(nonPersistentCharactersForCrew[station],k)
		end
	end

	-- spawn a new crew advert, call for each station
	if Engine.rand:Number(0,1) <= prod then
		table.insert(nonPersistentCharactersForCrew[station], 1, newCrew())
	end

	if prod > 1 then print("Warning: crew market not in equilibrium") end
end
Event.Register("onUpdateBB", onUpdateBB)

-- Wipe temporary crew out when hyperspacing
Event.Register("onEnterSystem", function(ship)
	nonPersistentCharactersForCrew = {}
	stationsWithAdverts = {}
end)

-- Load temporary crew from saved data
local loaded_data
Event.Register("onGameStart", function()
	-- XXX Need to re-initialise these until Lua is re-initialised with a new game
	nonPersistentCharactersForCrew = {}
	stationsWithAdverts = {}
	if loaded_data and loaded_data.stationsWithAdverts then
		nonPersistentCharactersForCrew = loaded_data.nonPersistentCharactersForCrew
		for k,station in pairs(loaded_data.stationsWithAdverts) do
		stationsWithAdverts[station:AddAdvert({
			title       = l.CREW_FOR_HIRE_TITLE,
			description = l.CREW_FOR_HIRE_DESC,
			icon        = "crew_contracts",
			onChat      = onChat,
			isEnabled   = isEnabled})] = station
		end
		loaded_data = nil
	end
end)

local serialize = function ()
	return {
		nonPersistentCharactersForCrew = nonPersistentCharactersForCrew,
		stationsWithAdverts = stationsWithAdverts,
	}
end

local unserialize = function (data)
	loaded_data = data
end

Serializer:Register('CrewContracts',serialize,unserialize)
