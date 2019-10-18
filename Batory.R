Batory <- setRefClass("Batory",
                      fields = list(
                        fuel="numeric", 
                        max_cargo_weight="numeric",
                        cargo_weight="numeric",
                        objects="list"),
                      
                      methods = list(
                        initialize = function(fuel, max_cargo_weight)
                        {
                          "Fuel and cargo weight"
                          #validate
                          fuel <<- fuel
                          max_cargo_weight <<- max_cargo_weight
                          cargo_weight <<- fuel
                          objects <<- list(Name="fuel", Weight = fuel)
                        },
                        
                        equip_Batory = function(name, weight)
                        {
                          "quepasa"
                          #validate
                          objects$Name <<- append(objects$Name, name)
                          objects$Weight <<- append(objects$Weight, weight)
                          cargo_weight <<- cargo_weight + weight
                          stopifnot(cargo_weight<=max_cargo_weight)
                        },
                        
                        embark_sailor = function(s_weight)
                        {
                          total_weight <- cargo_weight + s_weight
                          if (total_weight <= max_cargo_weight)
                          {
                            equip_Batory("sailor", s_weight)
                          }
                          else
                          {
                            for (i in length(objects$Name)) {
                              if (objects$Name[i] != "fuel" || objects$Name[i] != "sailor")
                              {
                                1+1
                              }
                            }
                          }
                        },
                        
                        print = function()
                        {
                          cat("\n")
                          cat("In the vessel of the batory you can find:")
                          cat("\n")
                          
                        }
                        
                      )
                      
)

build_Batory <- function(fuel, max_cargo_weight)
{
  b <- Batory$new(fuel=fuel, max_cargo_weight=max_cargo_weight)
  return(b)
}

fill_batory <- function(c,n)
{
  exponential <- rexp(n, 5)
  for (i in 1:n) 
  {
    c$equip_Batory(name = "Björn", weight = exponential[i])
  }
}

multiple_embark <- function(t,n)
{
  uniform <- runif(n, 0.08, 0.2)
  for (i in 1:n) {
    t$embark_sailor(s_weight = uniform[i])
  }
}