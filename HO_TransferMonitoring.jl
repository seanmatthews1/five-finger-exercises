          ############################################################
          #
          # BEGIN
          #
          # Hub Optimisation Model / Sean Matthews
          # ======================================
          # Version: as VCS
          # 
          # Transfer Monitoring
          #
          # A hub holds,  at any time, some number  (possibly zero) of
          # passengers  in transit  to  each airport  it connects  to.
          # This set changes  over time as new  transit passengers are
          # delivered to the hub by  incoming aircraft, and taken away
          # by outgoing ones.
          #
          # A  delivered  transit  passenger (they  are  delivered  in
          # uniform groups) has two  associated values: 1) the penalty
          # the delivering plane incurs (at  the time it picked up the
          # passenger) if  the passenger  never gets removed  from the
          # hub; 2) the  value to any outgoing plane  that removes the
          # the passenger from the hub (to its final destination).
          #
          #[The  penalty  includes  the  clawback  of  any  value  the
          # delivering aircraft  booked for bringing the  passenger to
          # the hub.]
          #
          # We  define  a  type  TransferMonitor to  perform  all  the
          # necessary tasks associated with  managing the evolving set
          # of in-transit  passengers waiting to  move one to  to some
          # airport, from a hub.
          #
          ############################################################
          #

module Transfer_Monitor_Module

using Main.Type_Constants,
      Main.Constants,
      Main.Location_Value_Module

export

          # Associated  with  a   TransferMonitor  are  the  following
          # queries and operations:

    Transfer_Monitor,

          #   Return a  new transfer  monitor with a  provided initial
          #   transfer  profile. This  later consists  of three  equal
          #   length vectors:  an ordered sequence of  time points, of
          #   which  the last  is  last_time_point, and  corresponding
          #   transfer volumes and minimum values for those transfers.
          #   Note that  _unlike_ the values returned  by Free_Places,
          #   these  values  are  _local_; i.e.   we  would  construct
          #   Free_Places-style  curves  by  aggregating  this  values
          #   along the time-line.

    Deliver_Passengers!,
  
          #   Specify  that  Aircraft A  at  time  T has  delivered  N
          #   passengers, each  with associated  penalty P  which will
          #   have been incurred at time  T0 if the passenger is never
          #   transfered  out,   and  realisable   value  V   for  the
          #   picker-upper are delivered.

    Pickup_Passengers!,

          #   With    reference   to    the    vector   returned    by
          #   Available_Pickups,  from point  at index  P (see  above)
          #   remove N  passengers from  the hub  (each of  which will
          #   have value V) and return their realised value, N * V.

    Pickup_Virtual_Passengers!,

          #   Add to the  training data the infomation  that n further
          #   passengers _could_  have been picked  up as late  as the
          #   specified time, had they been there.

    Transfers_Accepted,

          #   Return  the number  of tranfer  passengers with  (leg 2)
          #   value of at least the given size, who will be accepted.

    Reset!,

          #   Recalibrate by  a factor of k  (0 < k <=  1) the initial
          #   transfer places  and acceptable minimum  transfer values
          #   profiles,  and  reset  all other  maintained  values  to
          #   'beginning  of day'.   Uses  the (subsidiary)  operation
          #   Recalibrate!

    Get_Pending_Transfers,

          # Return  a vector  of all  transfers  (if any)  to a  given
          # destination pending at a location

    Transfer_Vector,

          #   The type of lists of pending transfers

    Transfer_Vector_View,

          # The type of views into Transfer_Vectors
    
    Transfer_Pending

          # Type tracking a packge of transfer passengers

          # Notes:  there is  a slight  simplification here  currently
          # that  might   make  things  slightly  less   optimal  than
          # otherwise  -  the  model   currently  tracks  the  minimum
          # acceptable  outleg value  and maximum  number of  transfer
          # passengers any time,  however we could refine  this a bit,
          # because  at the  lower bound,  the  model may  be able  to
          # accept _some_  low value transfer passengers,  but not all
          # of them.  Thus  it should also really  track precisely how
          # many  of _these_  are  possible at  any  time. This  would
          # clearly  be  do-able,  but  falls  into  the  category  of
          # features  for  the  second  release,  because  it  is  not
          # currently obvious  to me  that it  will make  any material
          # difference, even  less so if  we add further  structure to
          # the  model, which  may  make  it less  easy  to track  the
          # information anyway.
          #   
          ############################################################
          #   

mutable struct Transfer_Pending
    t             :: Time       # arrived at hub
    destinationID :: LocationID # where going (need this explicitly)
    n             :: N          # number at hub CAN CHANGE
    value         :: Money      # per passenger
    t0            :: Time       # when penalty would be incurred
    penalty       :: Money      # per passenger
    aircraft                    # on which passenger arrived
    originID      :: LocationID
    transfer_tag  :: TransferTag
end

struct Transfer_Done
    t     :: Time               # arrived at hubCSV
    n     :: N
    value :: Money

    Transfer_Done(t, n, value) =
        if (t in time_range) & (n >= 0) & (value >= 0.0)
            new(t, n, value)
        else
            println((t,n,value))
            error("problem instantiating Transfer_Done")
        end
end

Transfer_Vector  = Vector{Transfer_Pending}

          # Bit obscure(!), but it is useful to have a type of views into
          # lists of pending transfers

Transfer_Vector_View = SubArray{Transfer_Pending,1,Transfer_Vector, Tuple{UnitRange{Int64}}, true}


          # A Transfer_Monitor keeps track of the number of passengers
          # in transit now or in the future (The current profile) - we
          # don't delete the past, we just ignore it.
          #
          # We assume an initial minimum  transfer value of 0.

mutable struct Transfer_Monitor
    pending          :: Vector{Transfer_Pending}
    dispatched       :: Vector{Transfer_Done   }
    current_number   :: N
    capacity_profile :: Vector{N}
    value_profile    :: Vector{Money}

          # local scratch variables used during calibration
    
    forward_n_disp       :: Vector{N    }
    forward_val_stranded :: Vector{Money}
    forward_n_stranded   :: Vector{N    }
    forward_val_disp     :: Vector{Money}


    
    Transfer_Monitor(total_volume :: N) =
        if total_volume > 0
            new(Vector{Transfer_Pending}(),
                Vector{Transfer_Done   }(),
                0,
                map(x -> total_volume, time_range),
                map(x -> 0.0         , time_range),

                Vector{N    }(undef, length(time_range)),
                Vector{Money}(undef, length(time_range)),
                Vector{N    }(undef, length(time_range)),
                Vector{Money}(undef, length(time_range)))
        else
            error("error initialising Transfer_Monitor")
        end
end


function Deliver_Passengers!(transfer_monitor :: Transfer_Monitor,
                             destinationID    :: LocationID      ,
                             t                :: Time            ,
                             n                :: N               ,
                             value            :: Money           ,
                             t0               :: Time            ,
                             penalty          :: Money           ,
                             aircraft                            ,
                             originID         :: LocationID      ,
                             transfer_tag     :: TransferTag     )

          # Update  the  current places  (t  and  future buckets)  and
          # insert    the    package    of   passengers    into    the
          # pending_transfers (at the right place).
          #
          # Note that  ther destinationID is for  internal reference -
          # all the entries  in a particular transfer  monitor will of
          # course have the same destination, which is the destination
          # to which the transfer monitor is assigned.

    pending         = transfer_monitor.pending
    package         = Transfer_Pending(t, destinationID, n, value, t0, penalty, aircraft, originID, #0,
                                       transfer_tag)

    @assert issorted(pending, by=x -> (x.t, - x.value))

    transfer_monitor.current_number += n
    insert!(pending, searchsortedfirst(pending, package, by=x -> (x.t, - x.value)), package)
    
end

function Transfers_Accepted(transfer_monitor :: Transfer_Monitor,
                            t                :: Time            ,
                            value            :: Money           ) :: N
    
    J = t - first_time_point + 1

    if t > last_time_point
        n = 0
    elseif value > transfer_monitor.value_profile[J]
        
        current_capacity = transfer_monitor.capacity_profile[J]
        n =  max(0, current_capacity - transfer_monitor.current_number)
    else
         n = 0
    end

    return (rand() > noise_scale ? n : 1000)
end

function Pickup_Passengers!(transfer_monitor :: Transfer_Monitor,
                            n                :: N               ,
                            package          :: Transfer_Pending)

          # Remove the passengers from the package. If that package is
          # now empty  delete it.   Record the executed  transfer, and
          # return the value realised by the pickup.

    @assert package.n >= n

    package.n -= n
    push!(transfer_monitor.dispatched, Transfer_Done(package.t, n, package.value))

    if package.n == 0
        p = findfirst(x -> x.n == 0, transfer_monitor.pending)
        deleteat!(transfer_monitor.pending, p)
    end

    transfer_monitor.current_number -= n

    @assert all(map(x -> x.n > 0, transfer_monitor.pending))
end

empty_pending_transfers = Vector{Transfer_Pending}() 

function Get_Pending_Transfers(transfers     :: Vector{Transfer_Monitor},
                               destinationID :: LocationID              ) :: Vector{Transfer_Pending}


    return (isempty(transfers) ? empty_pending_transfers : transfers[destinationID].pending)

end

function Pickup_Virtual_Passengers!(transfer_monitor :: Transfer_Monitor,
                                    n                :: N               ,
                                    t                :: Time            )

          # There may be empty places on an aircraft that _could_ have
          # been filled  with passengers, had they  been available. In
          # such cases, We need to note this in the training data.

    push!(transfer_monitor.dispatched, Transfer_Done(t, n, 0.0))
end

function Reset!(transfer_monitor :: Transfer_Monitor)

    transfer_monitor.current_number = 0

    for e in transfer_monitor.pending
        Book_Generated_Value!(e.aircraft.location_value,
                              e.originID               ,
                              e.t0                     ,
                              e.penalty                )

    end
    
    Recalibrate!(transfer_monitor     )
    empty!(transfer_monitor.pending   )
    empty!(transfer_monitor.dispatched)
    
end

function Recalibrate!(transfer_monitor :: Transfer_Monitor)

          # Integrate into  the transfer monitor profiles  what we can
          # learn from the passengers who have been dispatched and the
          # passengers who have remained stranded in the hub.
          #
          # What is important here is the arrival at hub time, not the
          # dispatch time.


    forward_n_disp       = transfer_monitor.forward_n_disp
    forward_val_stranded = transfer_monitor.forward_val_stranded
    forward_n_stranded   = transfer_monitor.forward_n_stranded
    forward_val_disp     = transfer_monitor.forward_val_disp
    
    pending              = transfer_monitor.pending
    dispatched           = transfer_monitor.dispatched
    capacity_profile     = transfer_monitor.capacity_profile
    value_profile        = transfer_monitor.value_profile
    
    l_pending            = length(pending)
    l_dispatched         = length(dispatched)

    w1 = (1 - descent_step)
    w2 = descent_step

    sort!(dispatched, by=(x -> x.t))
    sort!(pending,    by=(x -> x.t))

    I_d                      = l_dispatched
    I_p                      = l_pending
    total_n_forward          = 0
    min_forward_val_stranded = max_leg2_yield

          # Construct the  profiles for number  of people who  will be
          # dispatched and the minimum value of passengers who will be
          # stranded    as   of    any    particular   arrival    time
          # (forward_n_disp/forward_val_stranded).
          #
          # The number of  people who will be  dispatched is monotonic
          # down,  and the  value of  people who  will be  stranded is
          # monotonic up w.r.t. arrival time (obviously).

    for t in reverse(time_range)
        J = t - first_time_point + 1
        
        while I_d > 0 && dispatched[I_d].t == t
            total_n_forward += dispatched[I_d].n
            I_d -= 1
        end
        forward_n_disp[J] = total_n_forward
        
        while I_p > 0 && pending[I_p].t == t
            min_forward_val_stranded = min(pending[I_p].value,
                                           min_forward_val_stranded)
            I_p -= 1
        end
        forward_val_stranded[J] = min_forward_val_stranded
    end

    I_d                        = (l_dispatched > 0 ? 1 : l_dispatched + 1)
    I_p                        = (l_pending    > 0 ? 1 : l_pending    + 1)
    total_n_forward_stranded   = 0
    max_forward_value_disp     = 0.0

          # Now  the profiles  for the  number of  people who  will be
          # stranded and the  maximum value of a  passenger shipped as
          # of  time of  arrival (i.e.   duals of  what has  just been
          # constructed).

    for t in time_range
        J = t - first_time_point + 1

        while I_d <= l_dispatched && dispatched[I_d].t == t
            max_forward_value_disp = max(dispatched[I_d].value,
                                         max_forward_value_disp)
            I_d += 1
        end
        forward_val_disp[J] = max_forward_value_disp
        
        while I_p <= l_pending && pending[I_p].t == t
            total_n_forward_stranded += pending[I_p].n
            I_p += 1
        end
        forward_n_stranded[J] = total_n_forward_stranded

    end

    @assert all(x -> x <= 0  , diff(forward_n_disp      ))
    @assert all(x -> x >= 0  , diff(forward_n_stranded  ))
    @assert all(x -> x >= 0.0, diff(forward_val_stranded))
    @assert all(x -> x >= 0.0, diff(forward_val_disp    ))

          # Now  adjust  the  capacity   and  value  profiles  in  the
          # direction of  the new  information: if  at any  point, the
          # number of passengers  dispatched is less than  or equal to
          # the  the   current  capacity   profile,  but   there  will
          # nevertheless  be  stranded  passengers,  then  adjust  the
          # profile  downwards,   while  if   the  maximum   value  of
          # passengers dispatched for any arrival time is greater than
          # or equal the  current value profile, but  the minium value
          # of  stranded  passengers  is   greater  than  the  current
          # profile, then adjust the current value profile upwards.
    
    for J in time_range .- first_time_point .+ 1
        if (forward_n_disp[J] > capacity_profile[J]) | (forward_n_stranded[J] > 0)
            capacity_profile[J] = convert(N, floor(w1 * capacity_profile[J] + w2 * forward_n_disp[J]))
        end

        if (forward_val_disp[J] < value_profile[J]) | (forward_val_stranded[J] > value_profile[J])
            value_profile[J] = w1 * value_profile[J] + w2 * forward_val_disp[J]
        end
    end

end

end

          # END
          #
          ############################################################
