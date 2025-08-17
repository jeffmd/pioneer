-- Copyright © 2008-2025 Pioneer Developers. See AUTHORS.txt for details
-- Licensed under the terms of the GPL v3. See licenses/GPL-3.txt
local Lang = require 'Lang'
local Engine = require 'Engine'
local pigui = Engine.pigui
local Game = require 'Game'
local ui = require 'pigui.baseui'

local lc = Lang.GetResource("core")
local Vector2 = _G.Vector2

local radialMenu = {}
local defaultRadialMenuIconSize = ui.theme.styles.MainButtonSize.x
local defaultRadialMenuPadding = ui.theme.styles.MainButtonPadding

--
-- Function: ui.openRadialMenu
--
-- ui.openRadialMenu(id, target, mouse_button, size, actions, padding, pos, action_binding)
--
-- Show a radial menu
--
--
-- Example:
--
-- >
--
-- Parameters:
--
--   id - any value, the value with which the radialMenu function will subsequently be launched
--   target - object that will be passed as an argument to the procedure of the
--            selected action
--   mouse_button - imgui id of the mouse button that caused the radial menu to open.
--                  ignored, if action_binding is not nil
--   size - icon size
--   actions - array of type:
--       icon - id of the icon from the theme
--       tooltip - string
--       action - function of one argument (target)
--   padding - number
--   pos - position of the center of the radial menu, if it is nil, the mouse
--         position is taken
--   action_binding - LuaInputAction, the input action that brought up the menu.
--
-- Returns:
--
--   nil
--
function ui.openRadialMenu(id, target, mouse_button, size, actions, padding, pos, action_binding)
	ui.openPopup("##radialmenupopup")
	radialMenu = {
		id = id,
		target = target,
		pos = pos or ui.getMousePos(),
		size = size,
		iconSize = Vector2(size),
		actions = actions,
		padding = padding or 0,
		actionBinding = action_binding
	}
	-- ignore mouse button if input action passed
	radialMenu.mouseButton = action_binding and -1 or mouse_button
	-- move away from screen edge
	radialMenu.pos.x = math.min(math.max(radialMenu.pos.x, size*3), ui.screenWidth - size*3)
	radialMenu.pos.y = math.min(math.max(radialMenu.pos.y, size*3), ui.screenHeight - size*3)
	if not action_binding then
		ui.clearMouse()
	end
end

local hasAutopilotLevel = function(level)
	return (Game.player["autopilot_cap"] or 0) >= level
end

local radial_menu_actions_hyperspace_cloud = {
	{
		icon = ui.theme.icons.hyperspace,
		tooltip = lc.SET_HYPERSPACE_TARGET_TO_FOLLOW_THIS_DEPARTURE,
		action = function(target)
			local hypercloud_level = (Game.player["hypercloud_analyzer_cap"] or 0)
			local ship = not target:IsArrival() and target:GetShip()
			if hypercloud_level > 0 then
				-- implicitly drops second return value of GetHyperspaceDestination() when using this "and" construct
				local destPath = ship and ship:GetHyperspaceDestination()
				if destPath then
					local target = destPath:IsBodyPath() and destPath:GetSystemBody().nearestJumpable.path or destPath:GetStarSystem().path
					Game.player:SetHyperspaceTarget(target)
					Game.sectorView:ClearRoute()
					Game.sectorView:AddToRoute(target)
					-- FIXME: lazy-require here due to insufficient event handling around hyperjump route management.
					require 'pigui.modules.hyperjump-planner'.updateRouteList()
					Game.sectorView:SwitchToPath(target)
					ui.playSfx("OK")
				end
			else
				Game.AddCommsLogLine(lc.NO_HYPERCLOUD_SCANNER_INSTALLED)
			end
		end

	}
}

local radial_menu_actions_station = {
	{
		icon=ui.theme.icons.comms, tooltip=lc.REQUEST_DOCKING_CLEARANCE,
		action=function(target)
			target:RequestDockingClearance(Game.player)
			-- TODO: play a negative sound if clearance is refused
			Game.player:SetNavTarget(target)
			ui.playSfx("OK")
		end
	},
	{
		icon=ui.theme.icons.autopilot_dock, tooltip=lc.AUTOPILOT_DOCK_WITH_STATION,
		action=function(target)
	 		if hasAutopilotLevel(1) then
		 		Game.player:SetFlightControlState("CONTROL_AUTOPILOT")
		 		Game.player:AIDockWith(target)
		 		Game.player:SetNavTarget(target)
				ui.playSfx("OK")
			else
				Game.AddCommsLogLine(lc.NO_AUTOPILOT_INSTALLED)
			end
		end
	},
}

local radial_menu_actions_all_bodies = {
	{
		icon=ui.theme.icons.autopilot_fly_to, tooltip=lc.AUTOPILOT_FLY_TO_VICINITY_OF,
		action=function(target)
			if hasAutopilotLevel(1) then
		 		Game.player:SetFlightControlState("CONTROL_AUTOPILOT")
		 		Game.player:AIFlyTo(target)
		 		Game.player:SetNavTarget(target)
				ui.playSfx("OK")
			else
				Game.AddCommsLogLine(lc.NO_AUTOPILOT_INSTALLED)
			end
		end
	},
}

local radial_menu_actions_systembody = {
	{
		icon=ui.theme.icons.autopilot_low_orbit, tooltip=lc.AUTOPILOT_ENTER_LOW_ORBIT_AROUND,
		action=function(target)
	 		if hasAutopilotLevel(1) then
		 		Game.player:SetFlightControlState("CONTROL_AUTOPILOT")
		 		Game.player:AIEnterLowOrbit(target)
		 		Game.player:SetNavTarget(target)
				ui.playSfx("OK")
			else
				Game.AddCommsLogLine(lc.NO_AUTOPILOT_INSTALLED)
			end
		end
	},
	{
		icon=ui.theme.icons.autopilot_medium_orbit, tooltip=lc.AUTOPILOT_ENTER_MEDIUM_ORBIT_AROUND,
		action=function(target)
	 		if hasAutopilotLevel(1) then
		 		Game.player:SetFlightControlState("CONTROL_AUTOPILOT")
		 		Game.player:AIEnterMediumOrbit(target)
		 		Game.player:SetNavTarget(target)
				ui.playSfx("OK")
			else
				Game.AddCommsLogLine(lc.NO_AUTOPILOT_INSTALLED)
			end
		end
	},
	{
		icon=ui.theme.icons.autopilot_high_orbit, tooltip=lc.AUTOPILOT_ENTER_HIGH_ORBIT_AROUND,
		action=function(target)
	 		if hasAutopilotLevel(1) then
		 		Game.player:SetFlightControlState("CONTROL_AUTOPILOT")
		 		Game.player:AIEnterHighOrbit(target)
		 		Game.player:SetNavTarget(target)
				ui.playSfx("OK")
			else
				Game.AddCommsLogLine(lc.NO_AUTOPILOT_INSTALLED)
			end
		end
	},
}

function ui.openDefaultRadialMenu(id, body, pos, action_binding)
	if body then
		local actions = {}
		table.append(actions, radial_menu_actions_all_bodies)
		if body:IsStation() then
			table.append(actions, radial_menu_actions_station)
		elseif body:IsHyperspaceCloud() then
			table.append(actions, radial_menu_actions_hyperspace_cloud)
		elseif body:GetSystemBody() then
			table.append(actions, radial_menu_actions_systembody)
		end
		ui.openRadialMenu(id, body, 1, defaultRadialMenuIconSize, actions, defaultRadialMenuPadding, pos, action_binding)
	end
end

function ui.radialMenu(id)
	if radialMenu.id ~= id then return end
	if not radialMenu.actions or #radialMenu.actions == 0 then
		return
 	end
	local icons = {}
	local tooltips = {}
	local colors = {}
	for _,action in pairs(radialMenu.actions) do
		local uv0, uv1 = ui.get_icon_tex_coords(action.icon)
		table.insert(icons, { id = ui.get_icons_texture(radialMenu.iconSize), uv0 = uv0, uv1 = uv1 })
		-- TODO: don't just assume that radialMenu.Target is a Body
		table.insert(tooltips, string.interp(action.tooltip, { target = radialMenu.target and radialMenu.target.label or "UNKNOWN" }))
		table.insert(colors, action.color or ui.theme.colors.white);
	end
	local n = pigui.RadialMenu(radialMenu.pos, "##radialmenupopup", radialMenu.mouseButton, icons, colors, tooltips, radialMenu.size, radialMenu.padding)

	-- radial menu launched by input action
	if radialMenu.actionBinding then
		if radialMenu.actionBinding:IsActive() then
			return n -- continue show the radial, while the input action is active
		end
	-- radial menu launched by mouse click
	elseif ui.isMouseDown(radialMenu.mouseButton) then
		pigui.DisableMouseFacing(true)
		return n -- continue show the radial, while the mouse button is pressed
	else
		pigui.DisableMouseFacing(false)
	end

	-- closing radial, fire item callback (if selected)
	radialMenu.id = nil
	if n > 0 then
		radialMenu.actions[n].action(radialMenu.target)
	end

	return n
end
