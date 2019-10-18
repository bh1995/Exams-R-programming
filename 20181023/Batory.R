Batory <- setRefClass("Batory",
    fields = list(
      fuel="numeric", 
      max_cargo_weight="numeric",
      cargo_weight="numeric",
      objects="list"),
    
    methods = list(
      initialize = function(fuel, max_cargo_weight) {
        "Fuel and cargo weight"
        #validate
        fuel <<- fuel
        max_cargo_weight <<- max_cargo_weight
        cargo_weight <<- fuel
        objects <<- list(Name="fuel", Weight = fuel)
      },
      
      equip_Batory = function(name, weight) {
        "quepasa"
        #validate
        if ( cargo_weight + weight <= max_cargo_weight ) {
          objects$Name <<- append(objects$Name, name)
          objects$Weight <<- append(objects$Weight, weight)
          update_weight(weight)
        } else {
          stop("Exceeded max cargo weight")
        }
      },
      
      update_weight = function(new_weight) {
        cargo_weight <<- cargo_weight + new_weight
      },
      
      embark_sailor = function(s_weight) {
        total_weight <- cargo_weight + s_weight
        if (total_weight <= max_cargo_weight) {
          equip_Batory("sailor", s_weight)
        } else {
          remove_items(s_weight)
          equip_Batory("sailor", s_weight)
        }
      },
      
      remove_items = function(weight) {
        for (i in 1:length(objects$Name)) {
          if ( objects$Name[i] != "fuel" &
               objects$Name[i] != "sailor" ) {
            update_weight(-objects$Weight[i])
            objects$Name <<- objects$Name[-i]
            objects$Weight <<- objects$Weight[-i]
          }
          if (cargo_weight + weight <= max_cargo_weight) {
            break
          }
        }
        
      },
      
      print = function() {
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