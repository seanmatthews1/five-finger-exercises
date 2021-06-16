##Note added floor function to field here to coerce data
## Note ad hoc modification of loading of AptRestr (added types).


          ############################################################
          #
          # BEGIN
          #
          # Hub Optimisation Model / Sean Matthews
          # =====================================
          # Version: as VCS
          # 
          # Load data  from CSV files  (data received as excel  and an
          # ODBC connection would be better, but...)
          # 
          ############################################################
          # 

module Data

          # loads data  as delivered and  present it in  the following
          # form:
              
export
          #
          ##########
          #
          # A range coding the aircraft in the scenario (together with
          # a vector of their corresponding names)

    aircraftID_range, aircraft_names,

          #
          ##########
          #
          # A vector aircraftID_range -> Tuple reporting, for each aircraft
          #
          #  - PassengerCapacity
          #  - GroundTime
          #  - MinimumFlightDistance
          #  - MaximumFlightDistance

    Aircraft_D,

          #
          ##########
          #
          # A  range  coding  the   aircraft  types  in  the  scenario
          # (together with a vector of their corresponding names), and
          # a second one, with the operator added.

    AircraftType_R        , AircraftType_N,
    AircraftDetailedType_R, AircraftDetailedType_N,

          #
          ##########
          #
          # A range coding the airports in the scenario (together with
          # a vector of their corresponding  names)

    airportID_range, Airport_N,
    
          #
          ##########
          #
          # A range coding  the hubs in the scenario,  together with a
          # vector  of their  corresponding names  (these in  turn are
          # identical   to  initial   segments  of   AirportRange  and
          # AirportName

    hubID_range, Hub_N,

          #
          ##########
          #
          # Table [Destination::airportID_range, Origin::airportID_range] -> demand
          
    Market,

          #
          ##########
          #
          # collection of values which together define the demand function
          #
          # Table [Destination::airportID_range, Origin::airportID_range] -> demand

    U_d, U_0, U_a, U_C,

          # and some scalars

    global_t, global_mu, global_d,
     

          #
          ##########
          #
          # Table [Destination, Origin] -> (transit) yield/passenger

    Yield,
    
          #
          ##########
          #
          # Table [1:2, Destination, Origin, Hub] -> yield/passenger
          #
          # Yield split  up into legs  - assigned according  to flight
          # duration (1: first leg, 2: second leg)

    factored_yield,

          #
          ##########
          #
          # A table  Destination x Origin  aircraftID_range reporting,
          # for each possible flight for each aircraft
          #
          #  - Yield per point-to-point passenger
          #  - Cost per flight
          #  - duration of flight
          #  - Airport specific ground time adjustment
          #  - Minimum Connect Time at  destination (only relevant for
          #    hubs (?))

    Routes

          #
          ############################################################
          #

using CSV, Main.Type_Constants

          # Standard set of data files  are available in the following
          # (scenario specific) directory

function Scenario_Read_Table(CSV_Table :: String)
    
    return(CSV.read(Main.ScenarioDirectory * CSV_Table * ".csv", delim=';', decimal=','))
    
end

          #
          ##########
          #
          # Read in the aircraft we are to work with. Stored as a list
          # of tuples.

let
    global aircraftID_range, aircraft_names, AircraftType_R, AircraftType_N, Aircraft_D,
    AircraftDetailedType_R, AircraftDetailedType_N

    DF = Scenario_Read_Table("aircraft")

    const aircraft_names         = DF.id
    const aircraftID_range       = 1:length(aircraft_names)
    const AircraftType_N         = unique(DF.ACType)
    const AircraftType_R         = 1:length(AircraftType_N)
    const AircraftDetailedType_N = unique(DF.ACType .* "/" .* DF.Op)
    const AircraftDetailedType_R = 1:length(AircraftDetailedType_N)

    const Aircraft_D     = Vector{Aircraft_Details}(undef, length(aircraftID_range))
    F  = Dict(aircraft_names[i] => i for i in aircraftID_range)
    DT = Dict(AircraftDetailedType_N[i] => i for i in AircraftDetailedType_R)
    for A in eachrow(DF)
        Aircraft_D[F[A.id]] =
            Aircraft_Details(F[A.id]     ,
                             A.capacity  ,
                             A.minGT     ,
                             A.minRange  ,
                             A.maxRange  ,
                             DT[A.ACType * "/" * A.Op],
                             A.Op        ,
                             A.ACType    ,
                             A.id        ,
                             A.Cost_Fixed,
                             A.Cost_FH   )

        @assert ! any(isnan, DF.capacity)
        @assert ! any(isnan, DF.minGT)
        @assert ! any(isnan, DF.minRange)
        @assert ! any(isnan, DF.maxRange)
        @assert ! any(isnan, DF.Cost_Fixed)
        @assert ! any(isnan, DF.Cost_FH)

    end

    @assert length(unique(DF.id)) == size(DF, 1) "an aircraft appears twice in the list!"
end


          #
          ############################################################
          #
          # Need to  know which  airports are the  hubs.  These  are a
          # subset of the collection of all relevant airports
          #
          # Note: We don't define the Hub numbers yet, because we need
          # the full list of airports first.

const Hub_N =
    let
        DF = Scenario_Read_Table("hubs")
        @assert length(unique(DF.Apt)) == length(DF.Apt) "duplicated hubs!"
        [i::String for i in DF.Apt]
    end

const hubID_range = 1:length(Hub_N)

          #
          ############################################################
          #
          # The various market values are square matrix
          #
          #   Destination x Origin -> demand details
          #
          # together with  a dictionary that translates  airport names
          # into  indices.  Construct  the   dictionary  so  that  Hub
          # airports have the lowest indices.
          #
          # Note:  the data  loaded  here provides  only the  transfer
          # yield values.  The point  to point  yields (for  this data
          # set) are delivered separately in the flight data.


let
    global Market, Yield, Airport_N, airportID_range, AirportDict,
           U_d, U_0, U_a, U_C

    
    DF = Scenario_Read_Table("markets")

    @assert ! any(isnan, DF.demand)
    @assert ! any(isnan, DF.yield_conn)

    const Airport_N = vcat(Hub_N,
                           setdiff(union(DF.Origin,
                                         DF.Destination),
                                   Hub_N))

    const airportID_range = 1:length(Airport_N)

    const Market = zeros(N      , length(Airport_N), length(Airport_N))
    const Yield  = zeros(Money  , length(Airport_N), length(Airport_N))
    const U_d    = zeros(Float64, length(Airport_N), length(Airport_N))
    const U_0    = zeros(Float64, length(Airport_N), length(Airport_N))
    const U_a    = zeros(Float64, length(Airport_N), length(Airport_N))
    const U_C    = zeros(Float64, length(Airport_N), length(Airport_N))

    F = Dict(Airport_N[i] => airportID_range[i] for i in airportID_range)
    
    for M in eachrow(DF)
        Market[F[M.Destination], F[M.Origin]] = floor(M.demand)
        Yield[ F[M.Destination], F[M.Origin]] = M.yield_conn
        
        U_d[F[M.Destination], F[M.Origin]] = M.u_d
        U_0[F[M.Destination], F[M.Origin]] = M.u_0
        U_a[F[M.Destination], F[M.Origin]] = M.a
        U_C[F[M.Destination], F[M.Origin]] = M.U_C
    end
    
    const AirportDict = Dict(Airport_N[i] => airportID_range[i] for i in airportID_range)
end

          #
          ############################################################
          #
          # global variables

# global_t, global_mu, global_d

DF = Scenario_Read_Table("global_param")

@assert size(DF) == (1,3)

const global_t  = first(DF).t
const global_mu = first(DF).Mu
const global_d  = first(DF).d

          #
          ############################################################
          #
          # ** NEED  TO CHECK  HERE  THAT THESE  VALUES ARE  ADDITIVE!
          # ** (I.E. SPEND MORE TIME THE GROUND AT HUBS)

MinGTdiff = zeros(Int,length(airportID_range))

let
    global MinGTdiff
    
    DF = Scenario_Read_Table("airports")

    for R in eachrow(DF)
        if R.Apt in Airport_N
            MinGTdiff[AirportDict[R.Apt]] = R.minGTdiff
        end
    end
end

          #
          ############################################################
          #
          # Routes is a list of all 'routes', that is, point to point
          # connections along  which an aircraft might  choose to fly,
          # with various details - including yield

struct all_route_details
    operator              :: String
    origin_name           :: String
    destination_name      :: String
    earliest_departure    :: Time
    latest_departure      :: Time
    duration              :: Time
    distance              :: N
    minimum_transfer_time :: Time
    ptp_yield             :: Money
end

let
    global FlightList

    DF  = Scenario_Read_Table("flights")

    FlightList =
        map(all_route_details,
            DF.Op          ,
            DF.Origin      ,
            DF.Destination ,
            DF.EarliestDep ,
            DF.LatestDep   ,
            DF.duration    ,
            DF.dist        ,
            DF.MCT         ,
            DF.yield_loc   )

    @assert ! any(isnan, DF.EarliestDep) 
    @assert ! any(isnan, DF.LatestDep)   
    @assert ! any(isnan, DF.duration)    
    @assert ! any(isnan, DF.dist)        
    @assert ! any(isnan, DF.MCT)         
    @assert ! any(isnan, DF.yield_loc)

    if (  ! issubset(map(x -> x.origin_name, FlightList), keys(AirportDict))
        | ! issubset(map(x -> x.origin_name, FlightList), keys(AirportDict)))

        println("WARNING - IGNORING FLIGHTS FOR WHICH NO MARKET IS DEFINED")
        FlightList = filter(x -> (  (x.origin_name in keys(AirportDict))
                                    & (x.destination_name in keys(AirportDict))),
                            FlightList)
    end
    

end

          # We assume that  there is at most one flight  for any
          #
          # (Op x Origin x Destination)
          #
          # not clear that this i true for the small example, which we
          # will modify as appropriate.

@assert length(FlightList) == length(unique(map(x -> (x.operator, x.origin_name, x.destination_name), FlightList)))

          #
          ############################################################
          #
          # Airport -> allowed_aircraft
          #
          # Airports  for   which  access  for  certain   aircraft  is
          # restricted  (e.g. you  cannot  land an  AB320  at LCY)  is
          # reported in the AptRestr table.   We can convert that into
          # a map as above

let
    global allowed_aircraft

    DF  = Scenario_Read_Table("AptRestr")

    DF = CSV.read(Main.ScenarioDirectory * "AptRestr" * ".csv", delim=';', decimal=',', types=[String, Int64, String, Int64])

    allowed_aircraft = Vector{Vector{String}}(undef, length(airportID_range))
    for i in airportID_range
        airport_name = Airport_N[i]
        
        allowed_aircraft[i] =
            setdiff(AircraftType_N,
                    map(x -> x.ACType,
                        filter(x -> x.Apt == airport_name,
                               eachrow(DF))))
    end
end

          # A  table Destination  x Origin  aircraftID_range reporting,  for
          # each possible flight for each aircraft
          #
          #  - Yield per point-to-point passenger
          #  - Cost per flight
          #  - duration of flight
          #  - Airport specific ground time adjustment
          #  - Minimum Connect Time at  destination (only relevant for
          #    hubs (?))
          #
          #  NOTE - CAN BE UNDEFINED!! 


const Routes =
    Array{Union{Missing, Route_Details},3}(
        missing,
        length(airportID_range       ),
        length(airportID_range       ),
        length(aircraftID_range))

for i in aircraftID_range
    D = Aircraft_D[i]
    for F in filter(x -> (  (x.operator == D.operator)
                          & (D.minimum_range <= x.duration)
                          & (x.duration <= D.maximum_range)
                          & (D.aircraft_type in allowed_aircraft[AirportDict[x.origin_name]])
                          & (D.aircraft_type in allowed_aircraft[AirportDict[x.destination_name]])),
                    FlightList)
        Routes[AirportDict[F.destination_name], AirportDict[F.origin_name], i] =
            Route_Details(
                AirportDict[F.origin_name],
                AirportDict[F.destination_name],
                F.ptp_yield,
                D.cost_fixed + D.cost_FH * F.duration,
                F.duration,
                D.ground_time + MinGTdiff[AirportDict[F.destination_name]],
                F.minimum_transfer_time,
                F.earliest_departure,
                F.latest_departure)
    end
end


          #
          ############################################################
          #
          # Slots available at airports (if relevant)

struct slot
    Airport             :: String
    Time                :: Time
    MaxNumberArrivals   :: N
    MinNumberArrivals   :: N
    MaxNumberDepartures :: N
    MinNumberDepartures :: N
end


let
    global Slots

    DF = Scenario_Read_Table("slots")

    const Slots = map(slot, DF.Apt         ,
                            DF.time        ,
                            DF.MaxAmountArr,
                            DF.MinAmountArr,
                            DF.MaxAmountDep,
                            DF.MinAmountDep)
end


          # Durations[Destination, originID]

Durations = zeros(Time, length(airportID_range), length(airportID_range))
for F in FlightList
    Durations[AirportDict[F.destination_name], AirportDict[F.origin_name]] = F.duration
end

          # Note that factored_yield is  set to not-zero (i.e. defined)
          # only for transfers that are possible and make sense!
          #
          #  - No  transfer  via  a  hub  back to  the  place  itself!
          #
          #  - Only transfers  that are acutally possible  (this boils
          #    down to the requirement that  both legs of the transfer
          #    have duration longer than 0)

const factored_yield = zeros(Money, 2, length(Airport_N), length(Airport_N), length(Hub_N))

for originID in airportID_range
    for destinationID in airportID_range
        V = Yield[destinationID, originID]
        for hubID in hubID_range
            if length(unique([hubID, destinationID, originID])) == 3
                L1 = Durations[hubID, originID]
                L2 = Durations[destinationID, hubID]
                if (L1 > 0) & (L2 > 0)
                    factored_yield[1, destinationID, originID, hubID] = V * L1 / (L1 + L2)
                    factored_yield[2, destinationID, originID, hubID] = V * L2 / (L1 + L2)
                end
            end
        end
    end
end



end

          # END
          #
          ############################################################

