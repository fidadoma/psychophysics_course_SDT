
compute_FA_from_dprime <- function(d,H) {
  
  return(pnorm(qnorm(H) - d))
}


create_data <- function() {
  
  df1_1 <- tibble(subject_id = 1, condition = "sober",
                statement = c(rep("truth", 35),
                              rep("truth", 15),
                              rep("lie", 10),
                              rep("lie", 40)),
                answer = c(rep("I believe", 35),
                           rep("Do not believe", 15),
                           rep("I believe", 10),
                           rep("Do not believe", 40)))
  
  df1_2 <- tibble(subject_id = 1, condition = "drunk",
                  statement = c(rep("truth", 40),
                                rep("truth", 10),
                                rep("lie", 15),
                                rep("lie", 35)),
                  answer = c(rep("I believe", 40),
                             rep("Do not believe", 10),
                             rep("I believe", 15),
                             rep("Do not believe", 35)))
  
  df2_1 <- tibble(subject_id = 2, condition = "sober",
                  statement = c(rep("truth", 1),
                                rep("truth", 10),
                                rep("lie", 49),
                                rep("lie", 40)),
                  answer = c(rep("I believe", 1),
                             rep("Do not believe", 10),
                             rep("I believe", 49),
                             rep("Do not believe", 40)))
  
  df2_2 <- tibble(subject_id = 2, condition = "drunk",
                  statement = c(rep("truth", 25),
                                rep("truth", 15),
                                rep("lie", 25),
                                rep("lie", 35)),
                  answer = c(rep("I believe", 25),
                             rep("Do not believe", 15),
                             rep("I believe", 25),
                             rep("Do not believe", 35)))
  df3_1 <- tibble(subject_id = 3, condition = "sober",
                  statement = c(rep("truth", 35),
                                rep("truth", 17),
                                rep("lie", 15),
                                rep("lie", 33)),
                  answer = c(rep("I believe", 35),
                             rep("Do not believe", 17),
                             rep("I believe", 15),
                             rep("Do not believe", 33)))
  
  df3_2 <- tibble(subject_id = 3, condition = "drunk",
                  statement = c(rep("truth", 32),
                                rep("truth", 12),
                                rep("lie", 18),
                                rep("lie", 38)),
                  answer = c(rep("I believe", 32),
                             rep("Do not believe", 12),
                             rep("I believe", 18),
                             rep("Do not believe", 38)))
  df <- rbind(df1_1,df1_2,df2_1,df2_2,df3_1,df3_2) %>% group_by(subject_id, condition) %>% sample_frac(1) %>% ungroup()
  df$answer <-  factor(df$answer, levels = c("I believe", "Do not believe"))
  df$statement <-  factor(df$statement, levels = c("truth", "lie"))
  df$condition <-  factor(df$condition, levels = c("sober", "drunk"))
  df
}

compute_dprime <- function(H,FA) {
  return(qnorm(H) - qnorm(FA))
}

compute_bias <- function(H,FA, type = "C") {
  
  zH <- qnorm(H)
  zF <- qnorm(FA)
  
  bias <- case_when(
    type == "C" ~ -0.5 * (zH + zF),
    type == "C_rel" ~ -0.5 * ((zH + zF)/(zH - zF)),
    type == "lnB" ~ -0.5 * (zH^2 - zF^2)
    
  )
         
  return(bias)
    
}

plot_normal_ROC <- function(l) {
  df <- NULL 
  df_HF <- tibble(H = NA,FA = NA,label = l$label)
  Hs  <- seq(0, 1, 0.01)
  
  for (i in 1:length(l$H)) {
    H <- l$H[i]
    FA <- l$FA[i]
    if (H < FA) {
      FA   <- 1-FA
      H <- 1-H
    }
    
    df_HF$H[i] <- H
    df_HF$FA[i] <- FA
    d   <- compute_dprime(H, FA)
    
    FAs <- compute_FA_from_dprime(d,Hs)
    if(is.null(df)){ 
      df <- tibble(Hit=Hs,`False alarm` = FAs, label = l$label[i])
    } else {
      df <- rbind(df, tibble(Hit=Hs,`False alarm` = FAs, label = l$label[i]))
    }
      
    
  }    
  df %>% ggplot(aes(y = Hit, x = `False alarm`, group = label)) + 
    geom_line() + 
    ggtitle(sprintf("normal ROCs")) +
    geom_point(data = df_HF, mapping = aes(x = FA, y = H, col = label), size = 2) + 
    geom_text(data = df_HF, mapping = aes(x = FA, y = H+0.05, label = as.character(compute_dprime(H,FA) %>% round(2))), size = 2) + 
    theme(aspect.ratio = 1)
  
}

