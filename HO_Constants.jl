          ############################################################
          #
          # BEGIN
          #
          # Hub Optimisation Model / Sean Matthews
          # ======================================
          # Version: as VCS
          # 
          # Constants and other stuff that we need for the model
          #
          # These  are specific  values  that we  need  - some  should
          # obviously be configured after having read in the data.
          #
          # Note that there is also  a type_constants module with more
          # abstract  stuff (type  definitions, etc.)   that _do  not_
          # depend on any particular data  (and in fact should be read
          # prior to looking at data.
          #
          # We   also  put   here  dictionaries   etc.  with   complex
          # type-dependencies.
          #
          ############################################################
          #

module Constants

export
    hubID_range          ,
    locationID_range     ,
    nonhubID_range       ,
    aircraftID_range     ,
    detailed_typeID_range,
    aircraft_description ,
    first_time_point     ,
    last_time_point      ,
    time_range           ,
    max_leg2_yield       ,
    ID_To_Airport        ,
    Is_Hub               ,
    noise_scale          ,
    descent_step         ,
    Transfer_Count       ,
    Reset_Transfer_Count ,
    debugging_flag

using Main.Type_Constants

const hubID_range           = Main.Data.hubID_range
const locationID_range      = Main.Data.airportID_range
const nonhubID_range        = (length(Main.Data.hubID_range)+1):length(locationID_range)
const aircraftID_range      = Main.Data.aircraftID_range
const detailed_typeID_range = Main.Data.AircraftDetailedType_R
const aircraft_description  = Main.Data.Aircraft_D
const first_time_point      = minimum(map(x -> x.earliest_departure, Main.Data.FlightList))
const last_time_point       = maximum(map(x -> x.latest_departure + x.duration, Main.Data.FlightList))
const time_range            = first_time_point:last_time_point
const max_leg2_yield        = maximum(vec(Main.Data.factored_yield[2,:,:,:]))


          # we also  need this, because Julia  doesn't accept circular
          # type dependencies. There may be some better way to finesse
          # this, but I don't know it.

const ID_To_Airport_Map = Vector{Any}()

function Configure_Airport_Map(v)

    @assert isempty(ID_To_Airport_Map)

    for e in v
        push!(ID_To_Airport_Map, e)
    end

    @assert issorted(ID_To_Airport_Map, by=(x -> x.locationID))
    @assert minimum(map(x -> x.locationID, ID_To_Airport_Map)) == 1
    @assert unique(diff(map(x -> x.locationID, ID_To_Airport_Map))) == [1]
    
end

function ID_To_Airport(locationID)

    ID_To_Airport_Map[locationID]
    
end

const max_hubID = maximum(hubID_range)
function Is_Hub(locationID :: LocationID) :: Bool

    return locationID <= max_hubID

end

noise_scale  = 0.0

function Set_Noise_Scale(k :: Float64)

    @assert 0 <= k <= 1
    
    global noise_scale

    noise_scale = k
    
end

descent_step = 1.0

function Set_Descent_Step(k :: Float64)

    @assert 0 <= k <= 1

    global descent_step
    
    descent_step = k
    
end

transfer_count = 0

function Transfer_Count() :: TransferTag

    global transfer_count
    
    return transfer_count += 1
    
end

function Reset_Transfer_Count()

    global transfer_count
    
    transfer_count = 0
    
end

debugging_flag = false

function Set_Debugging_Flag(flag :: Bool)

    global debugging_flag

    debugging_flag = flag
end

end

          # END
          #
          ############################################################
