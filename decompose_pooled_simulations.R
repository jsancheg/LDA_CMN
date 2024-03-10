source("ListScenarios.R")
source("SimulateScenario.R")


all_scenarios <- dir(pathScenarios)


scenarios_to_decompose <- intersect(all_scenarios,Scenarios100)

n_scenarios_to_decompose <-length(scenarios_to_decompose)

tic("Decompose")
sapply(1:n_scenarios_to_decompose,function(i)
  {
          Deconstruct_Pool_Scenarios(Scenarios100[i],pathScenarios,pathScenarios1)
})
toc()
