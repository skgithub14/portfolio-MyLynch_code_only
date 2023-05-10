#### Generic ####

load("./associated-cancers.RData")
cancer.choices <- paste0(assoc.cancers, " Cancer")

max.age <- 85

# cancers that have interventions available
rr.cancer.choices <- c("Colorectal Cancer", 
                       "Endometrial Cancer", 
                       "Ovarian Cancer")

#### BMI ####

# BMI calculation
get.BMI <- function(ht_FT, ht_IN, weight){
  
  # convert to then metric
  ht.in <- as.numeric(ht_FT) * 12 + as.numeric(ht_IN)
  ht.m <- ht.in * 0.0254
  wt.kg <- weight * 0.453592
  
  # return BMI
  wt.kg / (ht.m ^ 2)
}

# determine which input value for weight loss should be used
select.weight.loss.input <- function(gene, Hweight, gender, hadColo, hadEndo, hysterectomy, 
                                     colo.only, endo.only, endo.colo){
  
  # check eligibility for CRC weight loss
  if(gene == "MLH1" & Hweight & !hadColo){
    CRC <- TRUE
  } else {
    CRC <- FALSE
  }
  
  # check eligibility for endometrial weight loss
  if(gender == "Female" & gene != "EPCAM" & Hweight & !hadEndo & !hysterectomy){
    ENDO <- TRUE
  } else {
    ENDO <- FALSE
  }
  
  if(CRC & ENDO){
    selected <- endo.colo
  } else if(CRC & !ENDO){
    selected <- colo.only
  } else if(!CRC & ENDO){
    selected <- endo.only
  } else {
    selected <- colo.only
  }
  
  selected
}

# find maximum weight loss corresponding to 5 point BMI decrease
max.weight.loss <- function(tmp.bmi, ht_FT, ht_IN){
  
  bmi.delta <- ceiling(tmp.bmi) - 25
  if(bmi.delta > 5){ 
    bmi.delta <- 5 
  } 
  
  # find corresponding weight intervals and update the button options
  ht.in <- as.numeric(ht_FT) * 12 + as.numeric(ht_IN)
  ht.m <- ht.in * 0.0254
  lb.drop <- ceiling(bmi.delta * ht.m^2 / 0.453592)
  
  lb.drop
}

# find change in BMI
change.in.bmi <- function(wght, weight.loss, htFT, htIN){
  
  now.bmi <- get.BMI(ht_FT = htFT, ht_IN = htIN, weight = wght)
  future.weight <- wght - weight.loss
  future.bmi <- get.BMI(ht_FT = htFT, ht_IN = htIN, weight = future.weight)
  delta.bmi <- now.bmi - future.bmi
  if(delta.bmi > 5){ delta.bmi <- 5 }
    
  delta.bmi
}

