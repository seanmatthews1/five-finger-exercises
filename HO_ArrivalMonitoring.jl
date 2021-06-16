          ############################################################
          #
          # BEGIN
          #
          # Hub Optimisation Model / Sean Matthews
          # ======================================
          # Version: as VCS
          #
          # Arrival Monitoring
          #
          # The  Arrival  monitor tracks  _when  an  aircraft will  be
          # available  to  depart  again_, i.e.  strictly,  it  tracks
          # pending  departures: IT  DOES NOT  TRACK WHEN  AN AIRCRAFT
          # TOUCHES DOWN.
          #
          # It also tracks which aircraft  are 'parked' - that is, can
          # go nowhere.  This means  essentially aircraft for which no
          # route is any-longer available (because we have reached the
          # end of the simulation period).
          #
          ############################################################
          #

module Arrival_Monitoring_Module


          # We need to track future arrivals  at an airport. Not a lot
          # going on here  at the moment to be  honest. Just gathering
          # stuff in one place.

export

          # We have the following:
          # 
    
    Arrival_Monitor,
    
          #  Return a new (empty) Arrival_Monitor

    Will_Arrive!,

          #  Inform the  Arrival_Monitor that an aircraft  will arrive
          #  at a time

    Has_Departed!,

          #  Inform the Arrival_Monitor that an aircraft has departed

    Park!,
          #  Park  the  current  pending Aircraft  (because  there  is
          #  nowhere left for them to go in the current simulation).

    Park_Aircraft!,
          #  Park an aircraft (because there is nowhere left for it to
          #  go in the current simulation).

    ParkRest!,

          # Park aircraft  that would depart after  the last timepoint
          # in the cycle.

    Pending_Departure,

          #  Check if there  are (given the current  time) any pending
          #  departures, meaning  any aircraft currently  available to
          #  dispatch.

    All_Pending_Departures,

           #  Return list of all pending departures

    All_Future_Departures,

          #  Return list of all current _and upcoming_ arrivals

    All_Future_Departures_Filtered,

          #  Return  list  of  all  current  _and  upcoming_  arrivals
          #  _restricted  to the  earliest arriving  instance of  each
          #  type/operator_

    Arrival_Monitor_Copy,

          #  Return an independent copy of the Arrival Monitor
    
    Is_Pending,

          # The aircraft is currently available to depart
    
    Arrival_List,

          # the type returned by previous
    
    Reset!

          #  Reset Arrival_Monitor  - all we  need to do is  clear the
          #  current queue (at the start of a run) 
          #  
          ############################################################
          #  

using Main.Type_Constants,
      Main.Constants,
      Main.Aircraft_Module

struct Arrival
    aircraft :: Aircraft
    t        :: Time

    Arrival(A)    = new(A, first_time_point)
    Arrival(A, t) = new(A, t)
end
    
struct Arrival_Monitor
    
    arrivals :: Vector{Arrival}
    parked   :: Vector{Arrival}

    Arrival_Monitor() = new(Vector{Arrival}(), Vector{Arrival}())
    Arrival_Monitor(arrivals, parked) = new(arrivals, parked)
end

function Will_Arrive!(arrival_monitor :: Arrival_Monitor,
                      aircraft        :: Aircraft       ,
                      t               :: Time           )

    if ! isnothing(findfirst(x -> x.aircraft == aircraft, arrival_monitor.arrivals))
        a = arrival_monitor.arrivals[findfirst(x -> x.aircraft == aircraft, arrival_monitor.arrivals)]
        println((aircraft.details, t, a.aircraft.details, a.t))
    end
    @assert isnothing(findfirst(x -> x.aircraft == aircraft, arrival_monitor.arrivals))

    a = Arrival(aircraft, t)
    v = arrival_monitor.arrivals
    insert!(v, searchsortedfirst(v, a, by=(x -> x.t)), a)

    @assert issorted(arrival_monitor.arrivals, by=(x -> x.t))
    
end

function Has_Departed!(arrival_monitor :: Arrival_Monitor,
                       aircraft        :: Aircraft       )


    @assert ! isnothing(findfirst(x -> x.aircraft == aircraft, arrival_monitor.arrivals))

    deleteat!(arrival_monitor.arrivals,
              findfirst(x -> x.aircraft == aircraft,
                        arrival_monitor.arrivals))

end

function Park!(arrival_monitor :: Arrival_Monitor,
               t               :: Time           )

    @assert ! isnothing(findfirst(x -> x.t == t, arrival_monitor.arrivals))

    p = findfirst(x -> x.t == t, arrival_monitor.arrivals)
    while ! isnothing(p)
        push!(arrival_monitor.parked, arrival_monitor.arrivals[p])
        deleteat!(arrival_monitor.arrivals, p)
        p = findfirst(x -> x.t == t, arrival_monitor.arrivals)
    end
end

function Park_Aircraft!(aircraft        :: Aircraft       ,
                        arrival_monitor :: Arrival_Monitor,
                        t               :: Time           )

    @assert ! isnothing(findfirst(x -> x.aircraft == aircraft, arrival_monitor.arrivals))

    p = findfirst(x -> x.aircraft == aircraft, arrival_monitor.arrivals)
    push!(arrival_monitor.parked, arrival_monitor.arrivals[p])
    deleteat!(arrival_monitor.arrivals, p)

end


function ParkRest!(arrival_monitor :: Arrival_Monitor)

    for p in eachindex(arrival_monitor.arrivals)
        @assert arrival_monitor.arrivals[p].t > last_time_point
        push!(arrival_monitor.parked, arrival_monitor.arrivals[p])
    end
    empty!(arrival_monitor.arrivals)
end

function Pending_Departure(arrival_monitor :: Arrival_Monitor,
                           t               :: Time                 ) :: Bool

    return ! isnothing(findfirst(x -> x.t == t, arrival_monitor.arrivals))
    
end

function Is_Pending(arrival_monitor :: Arrival_Monitor,
                    aircraft        :: Aircraft       ,
                    t               :: Time           )

    @assert ! isnothing(findfirst(x -> x.aircraft == aircraft, arrival_monitor.arrivals))

    return arrival_monitor.arrivals[findfirst(x -> x.aircraft == aircraft, arrival_monitor.arrivals)].t == t

    
end

function All_Pending_Departures(arrival_monitor :: Arrival_Monitor,
                                t               :: Time           ) :: Vector{Aircraft}

    return map(x -> x.aircraft, filter(x -> x.t == t, arrival_monitor.arrivals))
    
end

                    

Arrival_List = Vector{Arrival}

function All_Future_Departures(arrival_monitor :: Arrival_Monitor) :: Arrival_List

    return arrival_monitor.arrivals
    
end

const AFDF_lookup = Vector{Bool}(undef, length(detailed_typeID_range))

function All_Future_Departures_Filtered(arrival_monitor :: Arrival_Monitor) :: Arrival_List

    global AFDF_lookup

    F(x) =
        if AFDF_lookup[x.aircraft.details.detailed_typeID]
            AFDF_lookup[x.aircraft.details.detailed_typeID] = false
            true
        else
            false
        end

    AFDF_lookup .= true

    return filter(F, arrival_monitor.arrivals)
end

function Arrival_Monitor_Copy(arrival_monitor :: Arrival_Monitor) :: Arrival_Monitor

    return Arrival_Monitor(copy(arrival_monitor.arrivals), copy(arrival_monitor.parked))
    
end

function Reset!(monitor :: Arrival_Monitor)

    @assert isempty(monitor.arrivals)
    
    empty!(monitor.arrivals)
    empty!(monitor.parked  )

end


end

          # END
          #
          ############################################################
