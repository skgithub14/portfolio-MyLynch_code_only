load("./panelpro-lynch-data.RData")

#### Main Data Frame ####

# colonoscopy impact
# relative risk of colorectal cancer when colonscopies applied: 0.44
# C = risk of CRC with colonoscopies
# NC = risk of CRC without colonoscopies
# X = risk or CRC with mixed colonoscopies and no colonoscopies
# proportion of subjects receive colonoscopsies: 0.818
# NC = C / 0.44; C = 0.44 * NC
# (1 - 0.821) * NC + 0.821 * C = X
# solve for NC to establish RR to transform estimates to baseline
# NC = X / 0.54 = 1.852 * X
colonoscopy.RR <- 0.44
no.colonoscopy.RR <- 1.845

# aspirin hazard rates from literature
aspirin2.HR <- 0.56
aspirin5.HR <- 0.63
aspirin10.HR <- 0.65

# bmi hazard rates and average BMIs
bmi5.HR <- 0.07
ave.bmi.male <- 27.1
ave.bmi.female <- 26.0
endo.bmi.OR <- 0.21

# Helper function to transform hazard to penetrance
# For discrete event times, remember that density(t)=hazard(t)*survival(t-1)
penetrance_to_hazard <- function(penetrance_vector){
  surv.vector <- 1 - cumsum(penetrance_vector)
  hazard_vector <- penetrance_vector
  for(i in 2:length(penetrance_vector)){
    hazard_vector[i] <- penetrance_vector[i] / surv.vector[i-1]
  }
  return(hazard_vector)
}

# Helper function to transform penetrance to hazard
hazard_to_penetrance <- function(hazard_vector){
  penet.vector <- hazard_vector
  for(i in 2:length(hazard_vector)){
    penet.vector[i] <- hazard_vector[i] * (1-sum(penet.vector[1:i-1]))
  }
  return(penet.vector)
}

main.df <- function(data, gender1, age1, race1, gene1, age.range, prev.cancers, 
                    ooph.input, hyst.input, ooph.status, hyst.status, new.hyst, new.ooph, 
                    aspirin.status, colonoscopy.status, bmi.status, user.bmi, weight1, heightFT1, heightIN1,
                    noEstimate){
  
  #### Non-carrier Risks ####
  
  nc.pens <- ppdb[, "SEER", race1, gender1, ]
  
  #### Carrier Risks ####
  
  c.pens <- ppdb[,paste0(gene1, "_hetero_anyPV"), race1, gender1, ]
  
  # use sex appropriate average bmi for endometrial and CRC risk adjustments
  ave.bmi <- ave.bmi.female
  if(gender1 == "Male"){ ave.bmi <- ave.bmi.male }
  
  # censor user BMI to prevent encouraging weightloss for health weight people
  # and to prevent overstating the benefits of weightloss for very obese people
  if(user.bmi < 25){
    mod.bmi <- 25
  } else if(user.bmi > 30){
    mod.bmi <- 30
  } else {
    mod.bmi <- user.bmi
  }
  
  # modify CRC risk assuming no colonoscopies and by their current BMI for MLH1 carriers
  if(gene1 == "MLH1"){
    
    # calculate new HR
    delta.bmi <- mod.bmi - ave.bmi
    if(delta.bmi < 0){ 
      bmi.direction <- -1
      delta.bmi <- abs(delta.bmi)
    } else { 
      bmi.direction <- 1
    }
    
    rel.bmi.HR <- (1 + bmi.direction * bmi5.HR) ^ delta.bmi
    
    c.pens[which(rownames(c.pens) == "Colorectal"),] <-
      hazard_to_penetrance(
        penetrance_to_hazard(
          c.pens[which(rownames(c.pens) == "Colorectal"),] * no.colonoscopy.RR
        ) * rel.bmi.HR
      )
    
    # for non-MLH1 carriers only modify CRC assuming no colonoscopies
  } else {
    c.pens[which(rownames(c.pens) == "Colorectal"),] <- 
      c.pens[which(rownames(c.pens) == "Colorectal"),] * no.colonoscopy.RR
  }
  
  # adjust risks for endometrial cancer based on user's weight
  if(gene1 != "EPCAM" & gender1 == "Female" & !"Endometrial Cancer" %in% prev.cancers & !hyst.input & !noEstimate){

    # determine normal weight for user
    norm.weight.cut.off <- 25 * ((as.numeric(heightFT1)*12 + as.numeric(heightIN1)) * 0.0254)^2

    # determine equivalent weight to a BMI of 30
    high.weight.cut.off <- 30 * ((as.numeric(heightFT1)*12 + as.numeric(heightIN1)) * 0.0254)^2

    # modify user's weight to fall between the equivalent BMIs between 25 and 30
    weight.kg <- weight1 * 0.4535924
    if(weight.kg < norm.weight.cut.off){
      mod.weight <- norm.weight.cut.off
    } else if(weight.kg > high.weight.cut.off){
      mod.weight <- high.weight.cut.off
    } else {
      mod.weight <- weight.kg
    }

    # determine average weight equivalent for all females
    ave.weight <- ave.bmi * ((as.numeric(heightFT1)*12 + as.numeric(heightIN1)) * 0.0254)^2

    # calculate OR for the user's BMI
    delta.weight <- mod.weight - ave.weight
    if(delta.weight < 0){
      weight.direction <- -1
      delta.weight <- abs(delta.weight)
    } else {
      weight.direction <- 1
    }
    
    rel.endo.OR <- (1 + weight.direction * endo.bmi.OR) ^ (delta.weight/5)

    # calculate RRs to apply (there is a unique RR for each future age)
    # baseline.probs <- as.numeric(all.cancers[which(all.cancers$cancer == "Endometrial" & all.cancers$age == age1), paste0("CR",age.range)]) / 100
    endo.bmi.RR <- rel.endo.OR / 
      (1 - c.pens[which(rownames(c.pens) == "Endometrial"), ] + 
         (c.pens[which(rownames(c.pens) == "Endometrial"), ] * rel.endo.OR))
    
    # apply RR to the penetrances
    c.pens[which(rownames(c.pens) == "Endometrial"), ] <-
      c.pens[which(rownames(c.pens) == "Endometrial"), ] * endo.bmi.RR
  }
  
  #### Reduced Risks ####
  
  # reduced risk probabilities subset
  r.pens <- ppdb[,paste0(gene1, "_hetero_anyPV"), race1, gender1, ]
  
  # adjust endometrial and ovarian cancer risks to 0 if previous prophylactic
  if(new.hyst == "Yes"){
    r.pens[which(rownames(r.pens) == "Endometrial"),] <- 0
  }
  if(new.ooph == "Yes"){
    r.pens[which(rownames(r.pens) == "Ovarian"),] <- 0
  }
  
  # modify CRC risk assuming no colonoscopies and by user's current BMI
  if(gene1 == "MLH1"){
    r.pens[which(rownames(r.pens) == "Colorectal"),] <-
      hazard_to_penetrance(
        penetrance_to_hazard(
          r.pens[which(rownames(r.pens) == "Colorectal"),] * no.colonoscopy.RR
        ) * rel.bmi.HR
      )
    
    # modify CRC risk assuming just no colonoscopies
  } else {
    r.pens[which(rownames(r.pens) == "Colorectal"),] <- 
      r.pens[which(rownames(r.pens) == "Colorectal"),] * no.colonoscopy.RR
  }
  
  # modify endometrial risk based on the users current weight/BMI
  if(gene1 != "EPCAM" & gender1 == "Female" & !"Endometrial Cancer" %in% prev.cancers & !hyst.input & new.hyst == "No" & !noEstimate){
    r.pens[which(rownames(r.pens) == "Endometrial"), ] <-
      r.pens[which(rownames(r.pens) == "Endometrial"), ] * endo.bmi.RR
  }
  
  # factor in non-prophylactic surgery iterventions
  if(aspirin.status == "Yes" | colonoscopy.status == "Yes" | bmi.status > 0){
    adj.risk <- r.pens[which(rownames(r.pens) == "Colorectal"), age1:max.age]
    
    # colonoscopies
    if(colonoscopy.status == "Yes"){
      adj.risk[2:length(adj.risk)] <- adj.risk[2:length(adj.risk)] * colonoscopy.RR
    }
    
    # aspirin (delayed effect, with smoothing from year 1 to 5)
    if(aspirin.status == "Yes"){
      
      # smoothing
      smooth.vals <- c(1,
                       aspirin2.HR / 2, 
                       aspirin2.HR, 
                       cumsum(rep((aspirin5.HR - aspirin2.HR) / 3, 2)),
                       aspirin5.HR,
                       cumsum(rep((aspirin10.HR - aspirin5.HR) / 4, 4)))
      
      if(age1 < 76){
        
        adj.risk[1:10] <- hazard_to_penetrance(penetrance_to_hazard(adj.risk[1:10] * smooth.vals)) # apply smoothing
        adj.risk[11:length(adj.risk)] <- hazard_to_penetrance(penetrance_to_hazard(adj.risk[11:length(adj.risk)]) * aspirin10.HR)
        
      } else {
        
        slots.cnt <- max.age - as.numeric(age1) + 1
        smooth.vals <- smooth.vals[1:slots.cnt]
        adj.risk[1:slots.cnt] <- hazard_to_penetrance(penetrance_to_hazard(adj.risk[1:slots.cnt] * smooth.vals)) # apply smoothing
        
      }
    }
    
    # weight loss
    if(bmi.status > 0){
      
      # smoothing
      adj.risk[1:5] <- 
        hazard_to_penetrance(
          penetrance_to_hazard(
            adj.risk[1:5]
          ) * c(1,(1 - cumsum(rep(bmi5.HR/5, 4)))^bmi.status)
        ) 
      
      adj.risk[6:length(adj.risk)] <- 
        hazard_to_penetrance(
          penetrance_to_hazard(
            adj.risk[6:length(adj.risk)]
          ) * (1 - bmi5.HR)^bmi.status
        )
    }
    r.pens[which(rownames(r.pens) == "Colorectal"), age1:max.age] <- adj.risk
  }
  
  # factor in weight loss for endometrial cancer
  if(gene1 != "EPCAM" & gender1 == "Female" & !"Endometrial Cancer" %in% prev.cancers & !hyst.input & bmi.status > 0 & new.hyst == "No" & !noEstimate){
    
    # convert selected weight loss option to units of 5 kg
    ht.m <- (as.numeric(heightFT1) * 12 + as.numeric(heightIN1)) * 0.0254
    old.kg <- mod.bmi * ht.m^2
    new.bmi <- mod.bmi - bmi.status
    new.kg <- new.bmi * ht.m^2
    kg.drop <- old.kg - new.kg
    kg.5.drop <- kg.drop / 5
    
    # calculate odds ratio for selected weight loss option
    rel.endo.OR <- (1 + -1 * endo.bmi.OR) ^ kg.5.drop
    
    # calculate relative risk to apply to penetrances (there is a unique RR for each future age)
    # baseline.probs <- as.numeric(reduce.risk[which(reduce.risk$cancer == "Endometrial" & reduce.risk$age == age1), paste0("CR",age.range)]) / 100
    endo.bmi.RR <- rel.endo.OR / 
      (1 - r.pens[which(rownames(r.pens) == "Endometrial"), (age1+1):max.age] + 
         (r.pens[which(rownames(r.pens) == "Endometrial"), (age1+1):max.age] * rel.endo.OR))
    
    # apply relative risk to penetrances
    r.pens[which(rownames(r.pens) == "Endometrial"), (age1+1):max.age] <- 
      r.pens[which(rownames(r.pens) == "Endometrial"), (age1+1):max.age] * endo.bmi.RR

  }
  
  # convert penetrances to cumulative risks in a table that ggplot can use
  nc.pens <-
    as.data.frame(nc.pens) %>%
    tibble::rownames_to_column("Cancer") %>%
    pivot_longer(cols = -Cancer, names_to = "Age", values_to = "Penetrance") %>%
    mutate(Who = "Average Person", .after = "Cancer")
  c.pens <-
    as.data.frame(c.pens) %>%
    tibble::rownames_to_column("Cancer") %>%
    pivot_longer(cols = -Cancer, names_to = "Age", values_to = "Penetrance") %>%
    mutate(Who = "Someone Like Me", .after = "Cancer")
  r.pens <-
    as.data.frame(r.pens) %>%
    tibble::rownames_to_column("Cancer") %>%
    pivot_longer(cols = -Cancer, names_to = "Age", values_to = "Penetrance") %>%
    mutate(Who = "Reduced Risk", .after = "Cancer")
  pens <- rbind(nc.pens, c.pens, r.pens)
  pens <- 
    pens %>% 
    mutate(across(.cols = c(Age, Penetrance), ~as.numeric(.))) %>%
    arrange(Cancer, Who, Age) %>%
    filter(Age <= max.age)
  
  # remove females cancers if surgery occurred prior
  if(hyst.input){
    pens <- filter(pens, Cancer != "Endometrial")
  }
  if(ooph.input){
    pens <- filter(pens, Cancer != "Ovarian")
  }
  
  # remove previous cancers
  if(length(prev.cancers) > 0){
    pens <- filter(pens, !Cancer %in% prev.cancers)
  }
  
  # remove cancers which do not have excess risk
  if(gender1 == "Male"){
    cn.gn.mat <- pp.male.gc.assoc
  } else if(gender1 == "Female"){
    cn.gn.mat <- pp.female.gc.assoc
  }
  cn.gn.vec <- cn.gn.mat[, grep(pattern = gene1, colnames(cn.gn.mat), value = T)]
  names(cn.gn.vec) <- rownames(cn.gn.mat)
  pens <- pens %>% filter(Cancer %in% names(cn.gn.vec)[cn.gn.vec])
  
  # calculate survival to each age
  pens <-
    pens %>%
    group_by(Cancer, Who) %>%
    mutate(Survival = 1 - cumsum(Penetrance)) %>%
    mutate(Percent = 0) %>%
    mutate(across(.cols = c(Survival, Percent), ~as.numeric(.)))
  
  # calculate cumulative future risk percentages
  for(w in unique(pens$Who)){
    for(c in unique(pens$Cancer)){
      tmp.pens <- pens[which(pens$Who == w & pens$Cancer == c),]
      for(a in (age1+1):max.age){
        pens$Percent[which(pens$Who == w & pens$Cancer == c & pens$Age == a)] <- 
          round((sum(tmp.pens$Penetrance[(age1+1):a]) / pens$Survival[age1]) * 100, 2)
      }
    }
  }
  
  # cap risks at 90%
  pens <- pens %>% mutate(Percent = ifelse(Percent > 90, 90, Percent))
  
  # ensure reduced risks are never higher than carrier risks
  # and never lower than non-carrier risks
  for(c in unique(pens$Cancer)){
    c.risks <- pens$Percent[which(pens$Who == "Someone Like Me" & pens$Cancer == c)]
    r.risks <- pens$Percent[which(pens$Who == "Reduced Risk" & pens$Cancer == c)]
    nc.risks <- pens$Percent[which(pens$Who == "Average Person" & pens$Cancer == c)]
    if(any(r.risks > c.risks)){
      pens$Percent[which(pens$Who == "Reduced Risk" & 
                           pens$Cancer == c & 
                           pens$Age %in% which(r.risks > c.risks))] <- 
        c.risks[which(r.risks > c.risks)]
    }
    if(!((c == "Endometrial" & new.hyst == "Yes") | (c == "Ovarian" & new.ooph == "Yes"))){
      if(any(r.risks < nc.risks)){
        pens$Percent[which(pens$Who == "Reduced Risk" & 
                             pens$Cancer == c & 
                             pens$Age %in% which(r.risks < nc.risks))] <- 
          nc.risks[which(r.risks < nc.risks)]
      }
    }
  }
  
  # format and filter
  pens <- 
    pens %>% 
    mutate(Cancer = paste0(Cancer, " Cancer")) %>%
    filter(Age >= age1) %>%
    select(-c(Penetrance, Survival))
  
  pens
}


#### Plot Data ####

plotly.data <- function(df, useRR, cancerView){
  
  tmp.carrier.df <-
    df %>%
    filter(Cancer %in% cancerView)
  
  if(!useRR){
    tmp.carrier.df <-
      tmp.carrier.df %>%
      filter(Who != "Reduced Risk")
  }
  
  tmp.carrier.df <-
    tmp.carrier.df %>%
    select(-Cancer) %>%
    mutate(Who = gsub(pattern = " ", replacement = "", Who)) %>%
    mutate(Percent = Percent/100) %>%
    pivot_wider(id_cols = Age, names_from = Who, values_from = Percent)
  
  tmp.carrier.df
  
}

line.data <- function(df, useRR, cancer){
  
  tmp.carrier.df <-
    df %>%
    filter(Cancer == cancer)
  
  if(!useRR){
    tmp.carrier.df <-
      tmp.carrier.df %>%
      filter(Who != "Reduced Risk")
  }
  
  tmp.carrier.df
  
}

bar.data <- function(df, useRR, cancer, age){
  
  tmp.carrier.df <-
    df %>%
    filter(Cancer == cancer)
  
  lvls <- c("Average Person","Reduced Risk","Someone Like Me")
  if(!useRR){
    tmp.carrier.df <-
      tmp.carrier.df %>%
      filter(Who != "Reduced Risk")
    
    lvls <- c("Average Person","Someone Like Me")
  }
  
  window.risks <-
    tmp.carrier.df %>%
    filter(Age %in% c(as.numeric(age)+5, max.age)) %>%
    mutate(Age = as.factor(Age)) %>%
    mutate(Who = factor(Who, levels = lvls)) %>%
    mutate(Who.Pattern = gsub(pattern = " ", replacement = "", Who))
  
  window.risks
  
}

waffle.data <- function(df, waffleTime, age, cancerView){
  
  # time horizon
  th <- max.age
  if(waffleTime != "In My Lifetime"){
    th <- as.numeric(age)+5
  }
  
  window.risks <-
    df %>%
    filter(Age == th & Cancer == cancerView) %>%
    
    # round to nearest 5
    mutate(Percent = (plyr::round_any(Percent, 5, f = ceiling) / 100) * 20) %>%
  
    # make sure nothing is rounded to 100% or 0%
    mutate(Percent = ifelse(Percent == 20, 19, 
                            ifelse(Percent == 0, 1, Percent)))
  
  window.risks
  
}