## NOTE - TRANSFER MONITOR SHOULD  ONLY ACCEPT PASSENGERS TO PLACES TO
## WHICH  IT  CAN  ACTUALLY  TRANSFER  (!) AND  WE  CAN  SET  THIS  AT
## INITIALISATION  (BY  SETTING  THE   ACCEPTABLE  TRANSFER  PRICE  AS
## INIFINITELY HIGH)

          ############################################################
          #
          # BEGIN
          #
          # Title : X-hub optimizer
          # Author: Sean Matthews
          # Date  : [as VCS]
          #
          # This is the initial version.  It may need to be refactored
          # for large problems (not clear yet).
          #
          ############################################################
          #

# using .LoadData

include("HO_LoadData.jl"          )
include("HO_Constants.jl"         )
include("HO_LocationValue.jl"     )
include("HO_TransferMonitoring.jl")
include("HO_ArrivalMonitoring.jl" )

using Main.Cons, Main.Data

          # We start with  a set of aircraft.  Each  of these aircraft
          # can be modelled as a location value function together with
          # a  next  location [and  a  list  of details  for  previous
          # flights -  TBD]. Since we have  a map from aircraft  to an
          # initial  segment  of the  naturals,  we  can organise  our
          # collection of aircraft as a vector.

mutable struct AircraftInstance
    VF           :: LV.LocationValue
    NextLocation :: Loc_Val

    AircraftInstance(InitValue :: Mon_Val) =
        new(LV.LocationValue(InitValue), 0)
end

InitialValue = Dict("E90" =>  50000.0,
                    "320" => 100000.0,
                    "321" => 100000.0) :: Dict{String, Mon_Val}

@assert all(x -> x in AircraftType_N, keys(InitialValue))


mutable struct Hub
    Demand   :: Vector{N_Val}
    Arrivals :: AM.ArrivalMonitor
    Airport  :: Loc_Val
    TM       :: TM.TransferMonitor
    
    Hub(Airport :: Loc_Val) =
        new(Vector{N_Val](), AM.ArrivalMonitor(), Airport, TM.TransferMonitor(1000))
end

mutable struct Spoke
    Demand   :: Vector{N_Val}
    Arrivals :: AM.ArrivalMonitor
    Airport  :: Loc_Val

        Spoke(Airport :: Loc_Val) =
        new(Market[:,Airport], AM.ArrivalMonitor(), Airport)
end

AircraftSet = Vector{AircraftInstance}
AiportType  = Union{Hub, Spoke}

struct State
    AllAircraft :: AircraftSet
    AllAirports :: Vector{AirportType}

    State() =
        new([ AircraftInstance(InitialValue[Aircraft_D[i].ACType])
              for i in Aircraft_R ],
            [i in Hub_R ? Hub(i) : Spoke(i) for i in Airport_R])
end

function ResetState!(Sigma :: State)
    
          # Position  aircraft  at   optimal  starting  poisitions  at
          # initial timepoint and set initial demand.

    for AP in Airport_R
        Sigma.AllAirports[AP].Demand = Market[:,AP]
        AM.Reset!(Sigma.AllAirports[AP].Arrivals)
    end
    
    for AC in Aircraft_R
        InitialLocation = LV.GetOptimalStart(AC)
        Sigma.AllAircraft[AC].NextLocation = InitialLocation
        AM.WillArrive!(Sigma.AllAirports[InitialLocation].Arrival, AC, FirstTimePoint)
    end
end

function DispatchFlights!(AP::Spoke, Hubs :: vector{AirportType}, AllAircraft :: AircraftSet, T :: Time_Val)
    if PendingDeparture(AP.Arrivals, T)
        # do something
    end
end

function DispatchFlights!(AP::Hub  , Hubs :: vector{Hub}, AllAircraft :: AircraftSet, T :: Time_Val)
    
end


          #
          ############################################################
          #
          # Now we start

const Sigma = State()

CurrentTime = FirstTimePoint

function RunCycle!(Sigma :: State)

    global CurrentTime

    ResetState!(Sigma)
    
    CurrentTime = FirstTimePoint

    while CurrentTime <= LastTimePoint
        for AP in Airport_R
            DispatchFlights!(Sigma.Airports[AP], Sigma.Airports[Hub_R], Sigma.AllAircraft, T)
        end

        CurrentTime += 1
    end
   
end

print("finished\n")

          #
          # END
          #
          ############################################################
