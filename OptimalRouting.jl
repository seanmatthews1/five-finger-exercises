          ############################################################
          # 
          # BEGIN
          # 
          # Lidl Truck routing case study - solution from Cordeau et
          # al. 2007
          # 
          # Sean Matthews, 2021.03.19
          # 
          ############################################################

using JuMP
using Cbc
using Combinatorics

n_to_values = Dict( 0 => (X = 30.0, Y = 40.0, Demand =  0),
                    1 => (X = 37.0, Y = 52.0, Demand = 19),
                    2 => (X = 49.0, Y = 49.0, Demand = 30),
                    3 => (X = 52.0, Y = 64.0, Demand = 16),
                    4 => (X = 31.0, Y = 62.0, Demand = 23),
                    5 => (X = 52.0, Y = 33.0, Demand = 11),
                    6 => (X = 42.0, Y = 41.0, Demand = 31),
                    7 => (X = 52.0, Y = 41.0, Demand = 15),
                    8 => (X = 57.0, Y = 58.0, Demand = 28),
                    9 => (X = 62.0, Y = 42.0, Demand =  8),
                   10 => (X = 42.0, Y = 57.0, Demand =  8),
                   11 => (X = 27.0, Y = 68.0, Demand =  7),
                   12 => (X = 43.0, Y = 67.0, Demand = 14),
                   13 => (X = 58.0, Y = 48.0, Demand =  6),
                   14 => (X = 58.0, Y = 27.0, Demand = 19),
                   15 => (X = 37.0, Y = 69.0, Demand = 11))

depot_ID         =  0
number_vehicles  =  8
vehicle_capacity = 35

edges = [(i,j) for i in keys(n_to_values),
                   j in keys(n_to_values)
                   if i < j]

customers = [i for i in keys(n_to_values) if i != depot_ID]
customer_subsets = combinations(customers)

          # return edges connected to a  node, or edges connected to a
          # _set_ of nodes
    
delta(n  :: Int        ) = [i for (i, e) in pairs(edges) if xor(e[1] ==  n, e[2] ==  n)]
delta(ss :: Vector{Int}) = [i for (i, e) in pairs(edges) if xor(e[1] in ss, e[2] in ss)]

          ############################################################
          # 
          # Some extra functions to  optimise the 'required' function,
          # which by default is a decent heuristic (bpp), but for this
          # case, can be fairly easily lifted to a global optimum (the
          # gap between  the heuristic  and the  lower bound  is never
          # greater than one here  - and in most cases is  in fact 0 -
          # so we can simply add an  extra check for those cases where
          # there is a difference, and  use exhaustive search to check
          # if the lower bound is  in fact satisfiable. For the number
          # and scale  of the  problems we  encounter _here_,  this is
          # feasible).  This method does not work in general, alas!
          #
          # Note  that the  difference  between  the result  generated
          # using bin_packing_heuristic  and using the global  best is
          # not remotely  material (the  improvement in  the resulting
          # objective value is at the fourth significant figure).

function confirm_fit(vv, n)

    v = sort(vv, rev = true)
    l = length(v)
    bins = Vector{Int64}(undef, n)
    i = 1
    bins[1] = vehicle_capacity - v[1]

    function f(p)

        if p > l
            return(true)
        end

        a = v[p]
        for j = 1:i
            if bins[j] >= a
                bins[j] -= a
                if f(p + 1)
                    return(true)
                else
                    bins[j] += a
                end
            end
        end
        if (i < n)
            i += 1
            bins[i] = vehicle_capacity - a
            if f(p + 1)
                return(true)
            else
                i -= 1
            end
        end
        return(false)
    end
    
    return(f(2)) # note _not_ f(1)!
end

          # 
          ############################################################

function required(ss)

          # the number  of trucks required  to deliver the  demands of
          # the nodes in ss - this  is a bin packing problem for which
          # bin_packing_heuristic is a heuristic - there is some small
          # room under it, but not a  lot, I think (note - patch added
          # to remove this small gap, for the provided case study).
    
    function lower_bound(ss)
        v = map(x -> (n_to_values[x]).Demand, ss)
        max(sum(v .> 17), convert(Int, ceil(sum(v) / 35)))
    end

    function bin_packing_heuristic(ss)
        set = sort(map(x -> (n_to_values[x]).Demand, ss))
        bins = Vector{Int}()
        while ! isempty(set)
            p = pop!(set)
            i = 1
            while i <= length(bins) && bins[i] < p
                i += 1
            end
            if i <= length(bins) && bins[i] >= p
                bins[i] -= p
            else
                push!(bins, vehicle_capacity - p)
            end
        end
        return(length(bins))
    end

    he = bin_packing_heuristic(ss)
    lb = lower_bound(ss)

    @assert lb <= he <= lb + 1
    
    if (he > lb) && confirm_fit(ss, lb)
        return(lb)
    else
        return(he)
    end
end

          # Following basically  a direct  transcription of  the model
          # spec provided by Cordeau et al. (see ref above).

model = Model(Cbc.Optimizer)

depot_edges = delta(depot_ID)

@variable(model, x[1:length(edges)], Int)

for i in keys(edges)
    if i in depot_edges
        @constraint(model, 0 <= x[i] <= 2)
    else
        @constraint(model, 0 <= x[i] <= 1)
    end
end

A1 = zeros(Int, length(customers), length(edges))
for (i, customer) in pairs(customers)
    for j in delta(customer)
        A1[i, j] = 1
    end
end
b1 = map(_ -> 2, customers)

@constraint(model, A1 * x .== b1)

A2 = map(i -> (i in depot_edges ? 1 : 0), keys(edges))

@constraint(model, sum(A2 .* x) == 2 * number_vehicles)

A3  = zeros(Int, length(collect(customer_subsets)), length(edges))
for (i,ss) in enumerate(customer_subsets)
    for j in delta(ss)
        A3[i, j] = 1
    end
end
b3 = map(ss -> 2 * required(ss), customer_subsets)

@constraint(model, A3 * x .>= b3)

function distance(e)
    a = n_to_values[e[1]]
    b = n_to_values[e[2]]
    sqrt((a.X - b.X) ^ 2 + (a.Y - b.Y) ^ 2)
end

@objective(model, Min, sum(map(distance, edges) .* x))

optimize!(model)

println(objective_value(model))

result = [(edges[i], v) for (i, v) in enumerate(value.(x)) if abs(v) > 0.01]

          # END
          # 
          ############################################################
