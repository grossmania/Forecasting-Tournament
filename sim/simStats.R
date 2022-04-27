simStats = function(simFunction,simName,obsForecast,obsName = 'Experts'){

# Simulate predictions and compute MASE -----------------------------------

  print('Computing simulations')
  domainOrder = unique(obsForecast$domain)
  
  for(d in 1:length(domainOrder)){
    assign(paste('MASE',domainOrder[d],sep = '_'),
           simFunction(domainOrder[d]))
    }
  
  MASE_set = data.frame(MASE_posaffect,MASE_negaffect, MASE_eafric,
                         MASE_easian,MASE_egend,MASE_iafric,MASE_iasian,
                         MASE_igend,MASE_lifesat,
                         MASE_ideoldem,MASE_ideolrep,MASE_polar)
  
  MASE_set_long = data.frame(MASE = c(MASE_posaffect,MASE_negaffect, MASE_eafric,
                                       MASE_easian,MASE_egend,MASE_iafric,MASE_iasian,MASE_igend,MASE_lifesat,
                                       MASE_ideoldem,MASE_ideolrep,MASE_polar), 
                              domain = rep(c('posaffect','negaffect','eafric','easian','egend',
                                             'iafric','iasian','igend','lifesat','ideoldem',
                                             'ideolrep','polar'), each = 10000),
                             source = simName, method = simName)
  
  MASE_obsset_long = data.frame(MASE = obsForecast$MASE1_w1, 
                                 domain = obsForecast$domain, 
                                 method = obsForecast$Method.code,
                                 source = obsName)
  
  MASE_set_all = rbind(MASE_set_long, MASE_obsset_long)
  

# Graph comparison to observed MASE from source ------------------------------------

  Stats <- MASE_set_all %>% group_by(domain, source) %>% dplyr::summarize(Mean = mean(log(MASE)), SD = sd(log(MASE)),
                                                                           CI_L = Mean - (SD * 1.96)/sqrt(length(MASE)),
                                                                           CI_U = Mean + (SD * 1.96)/sqrt(length(MASE)))
  
  domainOrder = Stats$domain[Stats$source == obsName]
  # domainOrder = domainOrder[order(Stats$Mean[Stats$source == obsName])]
  domainOrder = domainOrder[order(Stats$Mean[Stats$source == obsName] - 
                                  Stats$Mean[Stats$source == simName])]
  
  MASE_set_all$domain = factor(MASE_set_all$domain, levels = domainOrder)

# T-tests -----------------------------------------------------------------
  print(paste('Computing T-tests'))
  pData = data.frame(var = domainOrder, mObs = NA,tval = NA, pval = NA)
  for(d in 1:length(domainOrder)){
    assign(paste(domainOrder[d],'T',sep = ""),
        with(MASE_set_all, 
             t.test(log(MASE[domain == domainOrder[d] & source == obsName]), 
                    log(MASE[domain == domainOrder[d] & source == simName]))))
    pData$tval[d] = get(paste(domainOrder[d],'T',sep = ""))$statistic
    pData$pval[d] = get(paste(domainOrder[d],'T',sep = ""))$p.value
    pData$mObs[d] = mean(log(obsForecast$MASE1_w1[obsForecast$domain == domainOrder[d]]))
    }
  
  #install.packages("fuzzySim", repos="http://R-Forge.R-project.org")
  require('fuzzySim')
  
  pData$pvalFDR = p.adjust(pData$pval, method = "fdr")
  

# Bootstrap Individual P-value --------------------------------------------

  calc_IProb = function(dset, dom){
    mObs = with(dset, mean(log(MASE[domain == dom & dset$source == obsName])))
    return(mean(log(dset$MASE[dset$domain == dom & dset$source == simName]) < mObs))
  }
  
  pData$meanSimI = pData$bootP_I = NA
  print(paste('Computing p-values of individual simulations'))
  for(d in 1:length(domainOrder)){
    assign(paste(domainOrder[d],'bootP_I',sep = ""),
        calc_IProb(MASE_set_all,domainOrder[d]))
    pData$meanSimI[d] = Stats$Mean[Stats$domain == domainOrder[d] & 
                                   Stats$source == simName]
    pData$bootP_I[d] = 
      get(paste(domainOrder[d],'bootP_I',sep = ""))
    }

  pData$bootP_I[pData$bootP_I > .5] = 1 - pData$bootP_I[pData$bootP_I > .5]
  pData$bootP_I_FDR = p.adjust(pData$bootP_I, method = "fdr")
  
# Bootstrap average of size N P-value -------------------------------------
  bootstrap_meanProb = function(dset, dom, nSim = 10000){
    mSim = vector(mode = 'logical', length = nSim)
    mObs = with(dset, mean(log(MASE[domain == dom & source == obsName]), na.rm = TRUE))
    nObs = sum(dset$domain == dom & dset$source == obsName)
    for(sim in 1:nSim){
      # compute mean of a random sample of size N (N = # of expert predictions)
      mSim[sim] = mean(log(sample(dset$MASE[dset$domain == dom & dset$source == simName], size = nObs,
                                  replace = TRUE)), na.rm = TRUE)
    }
    return(list(mObs = mObs, nObs = nObs, p.value = mean(mObs > mSim),simulatedMean = mSim))
  }
  
  pData$bootP_M = NA
  for(d in 1:length(domainOrder)){
    print(paste('Bootstrapping mean sim for', domainOrder[d]))
    assign(paste(domainOrder[d],'bootP_M',sep = ""),
      bootstrap_meanProb(MASE_set_all,domainOrder[d]))
    pData$meanSimM[d] = mean(get(paste(domainOrder[d],'bootP_M',sep = ""))$simulatedMean)
    pData$bootP_M[d] = 
      get(paste(domainOrder[d],'bootP_M',sep = ""))$p.value
    }
  
  pData$bootP_M[pData$bootP_M > .5] = 1 - pData$bootP_M[pData$bootP_M > .5]
  pData$bootP_M_FDR = p.adjust(pData$bootP_M, method = "fdr")
  
  save(list = ls(), file = paste('Benchmark_',simName,'_v_',obsName,'.RData',sep = ""))
  }