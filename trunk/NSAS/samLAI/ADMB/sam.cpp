  #include <time.h>
  time_t StartTime;   //Time variables, for use in timing loop
  #include <df1b2fun.h>
  #include "nLogNormal.h"
  ofstream clogf("program.log");
  ofstream dclone("dataclone.log");
  ofstream cclone("confclone.log");
  #define CLONE(object) dclone<<setprecision(15)<<object<<endl; 
  #define CONFCLONE(object) cclone<<#object" =\n"<<setprecision(20)<<object<<endl; 
  #define TRACE(object) clogf<<"line "<<__LINE__<<", file "<<__FILE__<<", "<<#object" =\n"<<object<<endl<<endl; 
  #define STRACE(object) cout<<"line "<<__LINE__<<", file "<<__FILE__<<", "<<#object" =\n"<<object<<endl<<endl; 
  #define PINTRACE(object) cpin<<"# "<<#object" =\n"<<object<<endl; 
  #define PINCHKTRACE(object) chkpin<<"# "<<#object" =\n"<<object<<endl; 
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  #define assignInit(object){                           \
    adstring adStr;                                     \
    adstring name=#object;				\
    adstring colon=':';					\
    name=name+colon;                                    \
    int idx=0;                                          \
    adstring fname=__FILE__;                            \
    adstring lname="par";                               \
    adstring parname=fname(1,fname.size()-3)+lname;     \
    ifstream in(parname);                               \
    while (!in.eof()){                                  \
      in>>adStr;                                        \
      if(adStr==name){                                  \
        in>>object;					\
      }                                                 \
    }                                                   \
    in.close();                                         \
  }                                                                                
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////////////
  bool fexists(const char *filename)
  {
    ifstream ifile(filename);
    return ifile;
  }
  dmatrix getSubMat(const dmatrix& X, const int i1, const int i2){
    dmatrix ret(i1,i2,X.colmin(),X.colmax());
    for(int i=i1; i<=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }
  
  dvector getSubVec(const dvector& X, const int i1, const int i2){
    dvector ret(i1,i2);
    for(int i=i1; i<=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }
  dvar_vector getSubVec(const dvar_vector& X, const int i1, const int i2){
    dvar_vector ret(i1,i2);
    for(int i=i1; i<=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }
  df1b2vector getSubVec(const df1b2vector& X, const int i1, const int i2){
    df1b2vector ret(i1,i2);
    for(int i=i1; i<=i2; ++i){
      ret(i)=X(i);
    }    
    return ret;
  }
#include <admodel.h>
#include <contrib.h>

#include <df1b2fun.h>

#include <adrndeff.h>

  extern "C"  {
    void ad_boundf(int i);
  }
#include <sam.htp>

  df1b2_parameters * df1b2_parameters::df1b2_parameters_ptr=0;
  model_parameters * model_parameters::model_parameters_ptr=0;
model_data::model_data(int argc,char * argv[]) : ad_comm(argc,argv)
{
 int datatest=0;
 if (option_match(ad_comm::argc,ad_comm::argv,"-datatestonly")>-1){
   datatest=1;
 }
 int dataconftest=0;
 if (option_match(ad_comm::argc,ad_comm::argv,"-dataconftestonly")>-1){
   dataconftest=1;
 }
  genpin=0;
 if (option_match(ad_comm::argc,ad_comm::argv,"-genpin")>-1){
   genpin=1;
 }
 cout<<"DATASECTION " << endl; cout.flush(); 
 cout << "sam.dat...";
 time(&StartTime);  
  noFleets.allocate("noFleets");
 CLONE(noFleets)
 TRACE(noFleets);
  fleetTypes.allocate(1,noFleets,"fleetTypes");
 CLONE(fleetTypes)
 TRACE(fleetTypes);
 ssbPhase=-1;
 ssbPowPhase=-1;
 for(int i=1; i<=noFleets; ++i){if(fleetTypes(i)==3){ssbPhase=1;}} 
 for(int i=1; i<=noFleets; ++i){if(fleetTypes(i)==4){ssbPhase=1;ssbPowPhase=1;}} 
 TRACE(ssbPhase);
 TRACE(ssbPowPhase);
  fleetTimes.allocate(1,noFleets,"fleetTimes");
 CLONE(fleetTimes)
 TRACE(fleetTimes);
  noYears.allocate("noYears");
 CLONE(noYears)
 TRACE(noYears);
  years.allocate(1,noYears,"years");
 CLONE(years)
 TRACE(years);
  times.allocate(0,noYears);
 times(0)=years(1)-1;
 times(1,noYears)=years;
 TRACE(times);
  noObs.allocate("noObs");
 CLONE(noObs)
 TRACE(noObs);
  idx1.allocate(1,noYears,"idx1");
 CLONE(idx1)
 TRACE(idx1);
  idx2.allocate(1,noYears,"idx2");
 CLONE(idx2)
 TRACE(idx2);
  data.allocate(1,noObs,1,4,"data");
 CLONE(data)
 TRACE(data);
  logObs.allocate(1,noObs);
 logObs=log(column(data,4));
 TRACE(logObs);
 minAgeObs=(int)min(column(data,3));
 TRACE(minAgeObs);
 maxAgeObs=8;
 TRACE(maxAgeObs);
  for(minYearResFleet=1; minYearResFleet<=noYears; ++minYearResFleet){
    dmatrix subData=getSubMat(data,idx1(minYearResFleet),idx2(minYearResFleet));
    if(min(column(subData,2))<1.2)break;    
  }
  for(maxYearResFleet=noYears; maxYearResFleet>=1; --maxYearResFleet){
    dmatrix subData=getSubMat(data,idx1(maxYearResFleet),idx2(maxYearResFleet));
    if(min(column(subData,2))<1.2)break;    
  }
  propMature.allocate(1,noYears,minAgeObs,maxAgeObs,"propMature");
 CLONE(propMature)
 TRACE(propMature);
  stockMeanWeight.allocate(1,noYears,minAgeObs,maxAgeObs,"stockMeanWeight");
 CLONE(stockMeanWeight)
 TRACE(stockMeanWeight);
  catchMeanWeight.allocate(minYearResFleet,maxYearResFleet,minAgeObs,maxAgeObs,"catchMeanWeight");
 CLONE(catchMeanWeight)
 TRACE(catchMeanWeight);
  natMor.allocate(1,noYears,minAgeObs,maxAgeObs,"natMor");
 CLONE(natMor)
 TRACE(natMor);
  landFrac.allocate(minYearResFleet,maxYearResFleet,minAgeObs,maxAgeObs,"landFrac");
 CLONE(landFrac)
 TRACE(landFrac);
  catchMeanWeightD.allocate(minYearResFleet,maxYearResFleet,minAgeObs,maxAgeObs,"catchMeanWeightD");
 CLONE(catchMeanWeightD)
 TRACE(catchMeanWeightD);  
  catchMeanWeightL.allocate(minYearResFleet,maxYearResFleet,minAgeObs,maxAgeObs,"catchMeanWeightL");
 CLONE(catchMeanWeightL)
 TRACE(catchMeanWeightL);
  Fprop.allocate(1,noYears,minAgeObs,maxAgeObs,"Fprop");
 CLONE(Fprop)
 TRACE(Fprop);
  Mprop.allocate(1,noYears,minAgeObs,maxAgeObs,"Mprop");
 CLONE(Mprop)
 TRACE(Mprop);
 if(datatest==1){ad_exit(1);}
 ad_comm::change_datafile_name("model.cfg");
 cout << "model.cfg...";
  minAge.allocate("minAge");
 CONFCLONE(minAge)  
 TRACE(minAge);
  maxAge.allocate("maxAge");
 CONFCLONE(maxAge) 
 TRACE(maxAge);
  maxAgePlusGroup.allocate("maxAgePlusGroup");
 CONFCLONE(maxAgePlusGroup)
 TRACE(maxAgePlusGroup);
 if((minAge>minAgeObs)||(maxAge<maxAgeObs)){cerr<<"Error: ages in model.cfg mismatch."<<endl;} 
 //init_ivector likelihoodConf(1,2);
 //  TRACE(likelihoodConf);
 //  if(likelihoodConf(1)!=0){cout<<"Warning: Trying to use unimplimented likelihood, so quitting!"<<endl; ad_exit(1);}
  keyLogFsta.allocate(1,noFleets,minAge,maxAge,"keyLogFsta");
 CONFCLONE(keyLogFsta)
 TRACE(keyLogFsta);
 noLogFsta=max(keyLogFsta); 
 TRACE(noLogFsta);
  corFlag.allocate("corFlag");
 CONFCLONE(corFlag)
 TRACE(corFlag);
  keyLogFpar.allocate(1,noFleets,minAge,maxAge,"keyLogFpar");
 CONFCLONE(keyLogFpar)
 TRACE(keyLogFpar);
 noLogFpar=max(keyLogFpar); 
 TRACE(noLogFpar);
  keyQpow.allocate(1,noFleets,minAge,maxAge,"keyQpow");
 CONFCLONE(keyQpow)
 TRACE(keyQpow);
 noQpow=max(keyQpow); 
 TRACE(noQpow);
  keyVarF.allocate(1,noFleets,minAge,maxAge,"keyVarF");
 CONFCLONE(keyVarF)
 TRACE(keyVarF);
 noVarF=max(keyVarF);
 TRACE(noVarF); 
  keyVarLogN.allocate(minAge,maxAge,"keyVarLogN");
 CONFCLONE(keyVarLogN)
 TRACE(keyVarLogN); 
 noVarLogN=max(keyVarLogN);
 TRACE(noVarLogN);  
  keyVarObs.allocate(1,noFleets,minAge,maxAge,"keyVarObs");
 CONFCLONE(keyVarObs)
 TRACE(keyVarObs); 
 noVarObs=max(keyVarObs);
 TRACE(noVarObs);  
  noCombs.allocate("noCombs");
 CONFCLONE(noCombs);
 TRACE(noCombs);
  maxSCBSurv.allocate("maxSCBSurv");
 CONFCLONE(maxSCBSurv)
 TRACE(maxSCBSurv)
  combSurv.allocate(1,maxSCBSurv,"combSurv");
 CONFCLONE(combSurv)
 TRACE(combSurv)
  stockRecruitmentModelCode.allocate("stockRecruitmentModelCode");
 CONFCLONE(stockRecruitmentModelCode)
 TRACE(stockRecruitmentModelCode); 
  noScaledYears.allocate("noScaledYears");
 CONFCLONE(noScaledYears)
 TRACE(noScaledYears); 
  keyScaledYears.allocate(1,noScaledYears,"keyScaledYears");
 if(noScaledYears>0) CONFCLONE(keyScaledYears)
 //TRACE(keyScaledYears); 
  keyParScaledYA.allocate(1,noScaledYears,minAge,maxAge,"keyParScaledYA");
 if(noScaledYears>0) CONFCLONE(keyParScaledYA)
 //TRACE(keyParScaledYA); 
 if(noScaledYears>1){noScaledPar=max(keyParScaledYA);}else{noScaledPar=0;}
 TRACE(noScaledPar);
 stateDim=maxAge-minAge+1+noLogFsta;
 TRACE(stateDim); 
  fbarRange.allocate(1,2,"fbarRange");
 CONFCLONE(fbarRange)
 TRACE(fbarRange); 
 if(dataconftest==1){ad_exit(1);}
 pinini=0;
 if(fexists("sam.pin")){
   pinini=1;
 }
 if(pinini==0){
   if(fexists("model.init")){
     ad_comm::change_datafile_name("model.init");
     cout << "model.init...";
   }
  varLogFstaInit.allocate("varLogFstaInit");
  varLogNInit.allocate("varLogNInit");
  varLogObsInit.allocate("varLogObsInit");
  logFparInit.allocate("logFparInit");
  rec_logaInit.allocate("rec_logaInit");
  rec_logbInit.allocate("rec_logbInit");
  alphaSCBInit.allocate(1,maxSCBSurv-noCombs,"alphaSCBInit");
 }
 LAI = 0;
  retro.allocate(1,noFleets);
 if(fexists("reduced.cfg")){
   ad_comm::change_datafile_name("reduced.cfg");
  tempretro.allocate(1,noFleets,"tempretro");
   retro=tempretro;
   if(sum(square(retro))>0){reducedRun=1;}else{reducedRun=0;} 
 }else{
   retro.initialize();
   reducedRun=0;
 } 
  lastYearData.allocate(1,noFleets);
  lastYearData=0;
  for(int i=1; i<=noObs; ++i){
    for(int f=1; f<=noFleets; ++f){
      if((lastYearData(f)<(int)data(i,1))&&((int)data(i,2)==f))lastYearData(f)=(int)data(i,1);
    }    
  }   
  for(int f=1; f<=noFleets; ++f){
    if(retro(f)>=1)lastYearData(f)-=retro(f);
  }
  noYears=noYears-((int)years(noYears)-(int)max(lastYearData));  
  residuals.allocate(1,noObs,1,6);
}

model_parameters::model_parameters(int sz,int argc,char * argv[]) : 
 model_data(argc,argv) , function_minimizer(sz)
{
  model_parameters_ptr=this;
  initializationfunction();
 cout<<"PARAMETERSECTION"; cout.flush(); 
  logFpar.allocate(1,noLogFpar,"logFpar");
  logQpow.allocate(1,noQpow,"logQpow");
  logSdLogFsta.allocate(1,noVarF,-3,3,"logSdLogFsta");
  logSdLogN.allocate(1,noVarLogN,-3,3,"logSdLogN");
  logSdLogObs.allocate(1,noVarObs,-3,3,"logSdLogObs");
 int rec_phase=1; 
 if(stockRecruitmentModelCode==0){rec_phase=-1;}
  rec_loga.allocate(rec_phase,"rec_loga");
  rec_logb.allocate(rec_phase,"rec_logb");
 int cor_phase=1;
 if(corFlag==0){cor_phase=-1;}
  rho.allocate(0.01,0.99,cor_phase,"rho");
  logScale.allocate(1,noScaledPar,"logScale");
  logScaleSSB.allocate("logScaleSSB");
  #ifndef NO_AD_INITIALIZE
  logScaleSSB.initialize();
  #endif
 logScaleSSB=0.0;
  logPowSSB.allocate("logPowSSB");
  #ifndef NO_AD_INITIALIZE
  logPowSSB.initialize();
  #endif
 logPowSSB=0.0;
  logSdSSB.allocate("logSdSSB");
  #ifndef NO_AD_INITIALIZE
  logSdSSB.initialize();
  #endif
 logSdSSB=0.0;
  scaledLogObs.allocate(1,noObs,"scaledLogObs");
  #ifndef NO_AD_INITIALIZE
    scaledLogObs.initialize();
  #endif
  X.allocate(1,noYears,1,stateDim,"X");
  #ifndef NO_AD_INITIALIZE
    X.initialize();
  #endif
  U.allocate(1,stateDim*noYears,"U");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  jnll.allocate("jnll");  /* ADOBJECTIVEFUNCTION */
 cout<<"  ---  Done."<<endl; cout.flush(); 
}
void model_parameters::userfunction(void)
{
  jnll =0.0;
  time_t currentTime;
  time(& currentTime);
  if(difftime(currentTime,StartTime)>1800){ // Terminate after 30 minutes 
    cerr<<endl;
    cerr<<"############################################################"<<endl; 
    cerr<<"############################################################"<<endl; 
    cerr<<"############################################################"<<endl; 
    cerr<<"     MAX TIME ALLOWED EXCEEDED - MODEL DID NOT FINISH"<<endl;  
    cerr<<"############################################################"<<endl; 
    cerr<<"############################################################"<<endl; 
    cerr<<"############################################################"<<endl; 
    cerr<<endl;
    ad_exit(1);
  } 
  jnll=0.0;
  if(reducedRun==1){
    int redIdx;
    for(int i=retro.indexmin(); i<=retro.indexmax(); ++i){
      if(retro(i)==-1){
        for(int a=minAge; a<=maxAge; ++a){
          redIdx=keyLogFpar(i,a);
          if(redIdx!=0){
            jnll+=square(logFpar(redIdx)-1.0);
          }
          redIdx=keyVarObs(i,a);
          if(redIdx!=0){
            jnll+=square(logSdLogObs(redIdx)-1.0);
          }
          redIdx=keyQpow(i,a);
          if(redIdx!=0){
            jnll+=square(logQpow(redIdx)-0.0)*1.0e-6;
          }
        }
      }
      if((retro(i)>0)&&(fleetTypes(i)==0)){
        for(int y=1; y<=noScaledYears; ++y){
          if(keyScaledYears(y)>lastYearData(i)){
            for(int a=minAge; a<=maxAge; ++a){
              jnll+=square(logScale(keyParScaledYA(y,a))-0.0);
            }
          }
        }
      }
    }  
  }
  // First the scale parameter is applied where requested;
  scaledLogObs=logObs;
  if(noScaledYears>=1){
    int y, a;
    for(int yi=1; yi<=noScaledYears; ++yi){
      y=keyScaledYears(yi);
      for(int i=1; i<=noObs; ++i){
        a=(int)data(i,3);
        if((data(i,1)==y)&&(fleetTypes((int)data(i,2))<2)&&(keyParScaledYA(yi,a)>0)){
          scaledLogObs(i)+=logScale(keyParScaledYA(yi,a));
        }
      }
    }
  }
  for(int y=1; y<=noYears-1; ++y){
    if(LAI == 0){ step(y,U((y-1)*stateDim+1,y*stateDim),U(y*stateDim+1,(y+1)*stateDim),logFpar,rec_loga,rec_logb,logSdLogN,logSdLogFsta,rho);}
    if(LAI == 1){ step(y,U((y-1)*(stateDim+noCombs-1)+1,y*(stateDim+noCombs-1)-(noCombs-1)),U(y*(stateDim+noCombs-1)+1,(y+1)*(stateDim+noCombs-1)-(noCombs-1)),logFpar,rec_loga,rec_logb,logSdLogN,logSdLogFsta,rho);}
  }
  for(int y=1; y<=noYears; ++y){
    int idxlow=idx1(y);
    int idxhigh=idx2(y);
    dmatrix subData=getSubMat(data,idxlow,idxhigh);     
    dvar_vector subObs=getSubVec(scaledLogObs,idxlow,idxhigh);
    if(noQpow>0){
      if(LAI == 0){ obs(U((y-1)*stateDim+1,y*stateDim),                      subData,subObs,logFpar,logSdLogObs,logQpow,logScaleSSB,logPowSSB,logSdSSB);}
      //if(LAI == 1){ obs(U((y-1)*(stateDim+noCombs-1)+1,y*(stateDim+noCombs-1)),subData,subObs,logFpar,logSdLogObs,logQpow,logScaleSSB,logPowSSB,logSdSSB,alphaSCB);}
    }else{
      dvar_vector fakeQpow(1,1);
      if(LAI == 0){ obs(U((y-1)*stateDim+1,y*stateDim),                      subData,subObs,logFpar,logSdLogObs,fakeQpow,logScaleSSB,logPowSSB,logSdSSB);}
      //if(LAI == 1){ obs(U((y-1)*(stateDim+noCombs-1)+1,y*(stateDim+noCombs-1)),subData,subObs,logFpar,logSdLogObs,fakeQpow,logScaleSSB,logPowSSB,logSdSSB,alphaSCB);}
    }
  }
  if(sd_phase()){
    // for(int y=1; y<=noYears; ++y){
    //   for(int i=1; i<=stateDim; ++i){
    //     X(y,i)=U((y-1)*stateDim+i);
    //   }
    //   ssb(y)=SSB(X(y),propMature(y),stockMeanWeight(y),Fprop(y),Mprop(y),natMor(y));
    //   logssb(y)=log(ssb(y));
    //   tsb(y)=TSB(X(y),stockMeanWeight(y));
    //   logtsb(y)=log(tsb(y));
    //   fbar(y)=FBAR(X(y),fbarRange(1),fbarRange(2));
    //   logfbar(y)=log(fbar(y));
    //   int idxlow=idx1(y);
    //   int idxhigh=idx2(y);
    //   dmatrix subData=getSubMat(data,idxlow,idxhigh);     
    //   dvar_vector subObs=getSubVec(scaledLogObs,idxlow,idxhigh);
    //   if((y>=minYearResFleet)&&(y<=maxYearResFleet)){
    //     logCatch(y)=log(CATCH(X(y),natMor(y),catchMeanWeight(y)));
    //   }
    // }    
  }
}

void SEPFUN1  model_parameters::step(const int y, const dvar_vector& u1,const dvar_vector& u2, const dvar_vector& logFpar, const dvariable& rec_loga, const dvariable& rec_logb, const dvar_vector& logSdLogN, const dvar_vector& logSdLogFsta, const dvariable& rho)
{
  begin_df1b2_funnel();
  //y = years
  //U = states U((y-1)*stateDim+1,y*stateDim),U(y*stateDim+1,(y+1)*stateDim),logFpar,rec_loga,rec_logb,logSdLogN,logSdLogFsta,rho
  //u1 = states of this year
  //u2 = states next year
  //from u1 we make a prediction to next year so we can compare u2 with prediction
  dvar_vector x1(1,stateDim);   
  int n1=u1.indexmin();
  int n2=u1.indexmax();
  for(int i=n1; i<=n2; ++i){
    x1(i-n1+1)=u1(i);
  }
  dvar_vector x2(1,stateDim);   
  n1=u2.indexmin();
  n2=u2.indexmax();
  for(int i=n1; i<=n2; ++i){
    x2(i-n1+1)=u2(i);
  }
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x1(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  dvar_vector pred(1,stateDim);   
  pred=x1;
  dvariable ssb=0.0; 
  for(int a=minAge; a<=maxAge; ++a){
    ssb+=exp(x1(a-minAge+1))*exp(-Ftot(a)*Fprop(y,a)-natMor(y,a)*Mprop(y,a))*propMature(y,a)*stockMeanWeight(y,a);
  }
  if(stockRecruitmentModelCode==0){
    // do nothing 
  }else{
    if(stockRecruitmentModelCode==1){//ricker
      pred(minAge-minAge+1)=rec_loga+log(ssb)-exp(rec_logb)*ssb; 
    }else{
      if(stockRecruitmentModelCode==2){//BH
        pred(minAge-minAge+1)=rec_loga+log(ssb)-log(1+exp(rec_logb)*ssb); 
      }else{
        cerr<<"SR model code not recognized"<<endl;
      }
    }
  }
  for(int a=minAge+1; a<=maxAge; ++a){
    pred(a-minAge+1)=x1(a-1-minAge+1)-Ftot(a-1)-natMor(y,a-1); 
  }
  
  if(maxAgePlusGroup==1){
    pred(maxAge-minAge+1)=log(exp(x1(maxAge-1-minAge+1)-Ftot(maxAge-1)-natMor(y,maxAge-1))+
                              exp(x1(maxAge-minAge+1)-Ftot(maxAge)-natMor(y,maxAge))); 
  }
  
  dvar_vector var(1,stateDim);
  for(int a=minAge; a<=maxAge; ++a){
    var(a-minAge+1)=exp(2.0*logSdLogN(keyVarLogN(a))); 
  }
  int idx; 
  for(int f=1; f<=noFleets; ++f){
    for(int a=minAge; a<=maxAge; ++a){
      idx=keyLogFsta(f,a); 
      if(idx>0){
        idx+=maxAge-minAge+1; 
        var(idx)=exp(2.0*logSdLogFsta(keyVarF(f,a))); 
      } 
    }
  }
  if(corFlag==0){ // not correlated 
    for(int i=1; i<=stateDim; ++i){
      jnll+=0.5*(log(2.0*M_PI*var(i))+square(x2(i)-pred(i))/var(i));
    } 
  }else{          // correlated
    for(int i=1; i<=(maxAge-minAge+1); ++i){
      jnll+=0.5*(log(2.0*M_PI*var(i))+square(x2(i)-pred(i))/var(i));
    } 
    int Fdim=stateDim-(maxAge-minAge+1);
    dvar_matrix fvar(maxAge-minAge+2,stateDim,maxAge-minAge+2,stateDim);
    dvar_matrix fcor(maxAge-minAge+2,stateDim,maxAge-minAge+2,stateDim);
    dvar_vector fsd(maxAge-minAge+2,stateDim);
    fvar.initialize();
    if(corFlag==1){  // symmetrical correlation
      for(int i=(maxAge-minAge+2); i<=stateDim; ++i){
        for(int j=(maxAge-minAge+2); j<=stateDim; ++j){
          if(i!=j){fcor(i,j)=rho;}else{fcor(i,j)=1.0;}
        }
      }
    } else { // AR1 correlation
      for(int i=(maxAge-minAge+2); i<=stateDim; ++i){
        for(int j=(maxAge-minAge+2); j<=stateDim; ++j){
          if(i!=j){fcor(i,j)=pow(rho,abs(i-j));}else{fcor(i,j)=1.0;}
        }
      }
    }
    for(int i=(maxAge-minAge+2); i<=stateDim; ++i){
      //fvar(i,i)=var(i);
      fsd(i)=sqrt(var(i));
      //fcor(i,i)=1.0;
    }
    fvar=elem_prod(outer_prod(fsd,fsd),fcor);
    //cout<<fcor<<endl<<endl;
    jnll+=nLogNormal(pred((maxAge-minAge+2),stateDim),x2(maxAge-minAge+2,stateDim),fvar);
  } 
  end_df1b2_funnel();
}

void SEPFUN1  model_parameters::obs(const dvar_vector& u, const dmatrix& data, const dvar_vector& obs, const dvar_vector& logFpar, const dvar_vector& logSdLogObs, const dvar_vector& logQpow, const dvariable& logScaleSSB, const dvariable& logPowSSB, const dvariable& logSdSSB)
{
  begin_df1b2_funnel();
  int n1=data.rowmin();
  int n2=data.rowmax();
  dvar_vector pred(n1,n2);
  dvar_vector x(1,stateDim+noCombs-1);
  int xn1=u.indexmin();
  int xn2=u.indexmax();
  //cout << " u.indexmin " << xn1 << " u.indexmax " << xn2 << endl << cout.flush();
  
  for(int i=xn1; i<=xn2; ++i){
    x(i-xn1+1)=u(i);
  }
  
  //cout << " x initilized " << endl << cout.flush();
  
  // Get probs per unit into vector
  dvar_vector props(1,noCombs);
  if(LAI == 1){
    int counter = 2;
    for(int i=(xn1+stateDim); i<=xn2; ++i){
      props(counter)=u(i);
      counter += 1;
    }
  }
  //cout << " props initilized " << endl << cout.flush();
  // Get numSurv into vector
  dvar_vector survNum(1,maxSCBSurv);
  if(LAI == 1){
    for(int i=1; i<=maxSCBSurv; ++i){
      survNum(i)=combSurv(i);
    }
  }
  //cout << " survNum initilized " << endl << cout.flush();
  // Normalize probs and make them amount to 1 (fill 4th parameter)
  dvariable totProp = 0.0;
  dvar_vector props_norm(1,noCombs);
  if(LAI == 1){
    for(int i=2; i<=noCombs; ++i){
      totProp += exp(props(i));
    }
    for(int i=2; i<=noCombs; ++i){
      props_norm(i) = exp(props(i)) / (1+totProp);
    }
    props_norm(1) = 1 - totProp / (1+totProp);
  }
  //cout << " props_norm " << props_norm << endl << cout.flush();
  //cout << " props_norm initilized " << endl << cout.flush();
  // Normalize probs_alpha and make them amount to 1
  dvar_vector alphaSCB_norm(1,maxSCBSurv);
  dvar_vector aSCB;
  if(LAI == 1){
    for(int i=1; i<=noCombs; ++i){
      dvariable totProp_alpha = 0.0;
      int idxmin=0; int idxmax=0;
      for(int k=1; k<=maxSCBSurv; ++k){
        if(value(survNum(k))==value(i) && idxmin==0){ idxmin=k;}
        if(value(survNum(k))==value(i)){ idxmax=k;}
      }
      for(int j=(idxmin+1); j<=idxmax; ++j){
        // Substract i because aSCB is only 7 long but I'm estimating 11 valus
        totProp_alpha += exp(aSCB(j-i));
      }
      for(int j=(idxmin+1); j<=idxmax; ++j){
        alphaSCB_norm(j) = exp(aSCB(j-i)) / (1+totProp_alpha);
      }
      alphaSCB_norm(idxmin) = 1 - totProp_alpha / (1+totProp_alpha);
    }
  }
  //cout << " sum alpha " << sum(alphaSCB_norm) << endl << cout.flush();
  //cout << " alphaSCB_norm initilized " << endl << cout.flush();
  //cout << " alphaSCB_norm" << alphaSCB_norm << endl << cout.flush();
  
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  //cout << " Ftot initilized " << endl << cout.flush();
  dvar_vector Z(Ftot.indexmin(),Ftot.indexmax());
  Z=Ftot; // missing M here  
  int isMadded=0; 
  int f;
  int ft;
  int a; 
  int y, yIdx; 
  for(int i=n1; i<=n2; ++i){
    f=(int)data(i,2);
    ft=fleetTypes(f);
    a=(int)data(i,3);
    y=(int)data(i,1);
    if(isMadded==0){
      for(int aa=minAge; aa<=maxAge; ++aa){Z(aa)+=natMor((int)(y-years(1)+1),aa);} // added M here
      isMadded=1; 
    }
    if(ft==0){// residual fleet
      pred(i)=x(a-minAge+1)-log(Z(a))+log(1-exp(-Z(a)));
      if(keyLogFsta(f,a)>0){
        pred(i)+=x(keyLogFsta(f,a)+maxAge-minAge+1);
      }
      if(keyLogFpar(f,a)>0){
        pred(i)+=logFpar(keyLogFpar(f,a));
      }
    }else{
      if(ft==1){// comm fleet
        cerr<<"Not implemented yet!!!"<<endl;  
        ad_exit(1);
      }else{
        if(ft==2){// survey
          pred(i)=x(a-minAge+1)-Z(a)*fleetTimes(f);
          if(keyQpow(f,a)>0){
            pred(i)*=exp(logQpow(keyQpow(f,a))); 
          }
          if(keyLogFsta(f,a)>0){
            pred(i)+=x(keyLogFsta(f,a)+maxAge-minAge+1);
          }
          if(keyLogFpar(f,a)>0){
            pred(i)+=logFpar(keyLogFpar(f,a));
          }
        }else{
          if(ft==3){// SSB survey
            dvariable ssb=0.0; 
            for(int aa=minAge; aa<=maxAge; ++aa){
              yIdx=(int)(y-years(1)+1); 
              ssb+=exp(x(aa-minAge+1))*exp(-Ftot(aa)*Fprop(yIdx,aa)-natMor(yIdx,aa)*Mprop(yIdx,aa))*propMature(yIdx,aa)*stockMeanWeight(yIdx,aa);
            }
            pred(i)=log(ssb)+logScaleSSB;
          }else{
            if(ft==4){
              dvariable ssb=0.0; 
              for(int aa=minAge; aa<=maxAge; ++aa){
                yIdx=(int)(y-years(1)+1); 
                ssb+=exp(x(aa-minAge+1))*exp(-Ftot(aa)*Fprop(yIdx,aa)-natMor(yIdx,aa)*Mprop(yIdx,aa))*propMature(yIdx,aa)*stockMeanWeight(yIdx,aa);
              }
              pred(i)=exp(logPowSSB)*log(ssb)+logScaleSSB;
            } else {
                if(ft==5){ // LAI indices
                  //Option to cancel out LAI estimation
                  if(LAI == 1){
                    dvariable ssb=0.0;
                    for(int aa=minAge; aa<=maxAge; ++aa){
                      yIdx=(int)(y-years(1)+1);
                      ssb+=exp(x(aa-minAge+1))*exp(-Ftot(aa)*Fprop(yIdx,aa)-natMor(yIdx,aa)*Mprop(yIdx,aa))*propMature(yIdx,aa)*stockMeanWeight(yIdx,aa);
                    }
                    pred(i) = logScaleSSB + log(ssb) + log(props_norm(value(survNum(a)))) + log(alphaSCB_norm(a));
                    //cout << logScaleSSB << " " << log(ssb) << " " << log(props_norm(value(survNum(a)))) << " "  << log(alphaSCB_norm(a)) << endl; cout.flush();
                    //cout << " pred(i) " << pred(i) << endl; cout.flush();
                  }
                }
              }
          }
        } 
      }
    }      
  } 
  dvariable var=0.0;
  for(int i=n1; i<=n2; ++i){
    f=(int)data(i,2);
    a=(int)data(i,3);
    if((retro(f)==-1)||((int)data(i,1)>lastYearData(f))){
      residuals(i,1)=data(i,1);
      residuals(i,2)=f;
      residuals(i,3)=a;
      residuals(i,4)=0;     
      residuals(i,5)=0;     
      residuals(i,6)=0;     
    }else{
      if((fleetTypes(f)==3)||(fleetTypes(f)==4)){
        var=exp(2.0*logSdSSB); 
      }else{
        if(fleetTypes(f)==5){
          //Option to cancel out LAI estimation
          if(LAI == 1){
            var=exp(2.0*logSdLogObs(keyVarObs(f,value(survNum(a))-1)));
          }
        }else{
            var=exp(2.0*logSdLogObs(keyVarObs(f,a)));
        }
      }
      //Option to cancel out LAI estimation
      if(LAI == 1){
        jnll+=0.5*(log(2.0*M_PI*var)+square(obs(i)-pred(i))/var);
      } else {
        if(fleetTypes(f)<=4){
          jnll+=0.5*(log(2.0*M_PI*var)+square(obs(i)-pred(i))/var);
        }
      }
      //if(fleetTypes(f) == 5){
      //  cout << " obs(i) " << obs(i) << " pred(i) " << pred(i) << endl << cout.flush();
      //}
      cout << " jnll " << jnll << endl << cout.flush();
      
      residuals(i,1)=data(i,1);
      residuals(i,2)=f;
      residuals(i,3)=a;
      residuals(i,4)=value(obs(i));     
      residuals(i,5)=value(pred(i));     
      residuals(i,6)=value((obs(i)-pred(i))/sqrt(var));     
    }
  } 
  
  end_df1b2_funnel();
}

dvariable model_parameters::SSB(dvar_vector x, dvector p, dvector w, dvector fprop, dvector mprop, dvector M)
{
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  dvariable ret;
  ret=0; 
  for(int a=minAge; a<=maxAge; ++a){
    ret+=exp(x(a-minAge+1))*exp(-Ftot(a)*fprop(a)-M(a)*mprop(a))*p(a)*w(a);
  }
  return ret;
}

dvariable model_parameters::CATCH(dvar_vector x, dvector M, dvector w)
{
  dvariable ret; 
  ret=0;
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  dvar_vector Z(minAge,maxAge);
  Z=Ftot+M; 
  dvar_vector logCatch(minAge,maxAge); 
  for(int a=minAge; a<=maxAge; ++a){
    logCatch(a)=x(a-minAge+1)-log(Z(a))+log(1-exp(-Z(a)))+log(Ftot(a));
  }
  ret=sum(elem_prod(exp(logCatch),w));
  return ret;
  
}

dvariable model_parameters::TSB(dvar_vector x, dvector w)
{
  dvariable ret;
  ret=0; 
  for(int a=minAge; a<=maxAge; ++a){
    ret+=exp(x(a-minAge+1))*w(a);
  }
  return ret;
}

dvariable model_parameters::FBAR(dvar_vector x, int from, int to)
{
  dvariable ret;
  dvar_vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  ret=mean(Ftot(from,to));
  return ret;
}

void model_parameters::report(const dvector& gradients)
{
 adstring ad_tmp=initial_params::get_reportfile_name();
  ofstream report((char*)(adprogram_name + ad_tmp));
  if (!report)
  {
    cerr << "error trying to open report file"  << adprogram_name << ".rep";
    return;
  }
  report<<stateDim<<endl;
  report<<years<<endl; 
  ofstream resout("sam.res");
  resout<<residuals<<endl;
}

void model_parameters::preliminary_calculations(void) 
{

  #if defined(USE_ADPVM)

  admaster_slave_variable_interface(*this);

  #endif
  cout<<"PRELIMINARYSECTION"; cout.flush(); 
  if(genpin==1){
    cout<<"1"<<endl;
    dmatrix aveC(minAgeObs,maxAgeObs,1,noFleets);
    aveC.initialize();
    dmatrix noC(minAgeObs,maxAgeObs,1,noFleets);
    noC.initialize();
    for(int a=minAgeObs; a<=maxAgeObs; ++a){
      for(int f=1; f<=noFleets; ++f){
        for(int i=1; i<=noObs; ++i){
          if((a==data(i,3))&&(f==data(i,2))){
            noC(a,f)+=1;
            aveC(a,f)+=data(i,4); 
          } 
        }
        if(noC(a,f)>.01){aveC(a,f)=aveC(a,f)/noC(a,f);}      
      }   
    }
    cout<<endl<<aveC<<endl;
    dvector setF(minAgeObs,maxAgeObs);
    setF.fill_seqadd(0.1,0.5/(maxAgeObs-minAgeObs));
    cout<<endl<<setF<<endl;
    dvector setM=colsum(natMor)/(natMor.rowmax()-natMor.rowmin()+1);
    cout<<endl<<setM<<endl;    
    dvector setZ=setF+setM;
    dvector setN(minAgeObs,maxAgeObs);
    for(int a=minAgeObs; a<=maxAgeObs; ++a){
      if(aveC(a,1)>.01){
        setN(a)=aveC(a,1)*setZ(a)/(setF(a)*(1.0-exp(-setZ(a))));
      }else{
        setN(a)=mean(column(aveC,1))*setZ(a)/(setF(a)*(1.0-exp(-setZ(a))));
      }
    }
    cout<<endl<<setN<<endl;    
    for(int i=keyLogFpar.rowmin(); i<=keyLogFpar.rowmax(); ++i){
      for(int j=keyLogFpar.colmin(); j<=keyLogFpar.colmax(); ++j){
        if(keyLogFpar(i,j)>0.1){
          if(fabs(logFpar(keyLogFpar(i,j)))<0.01){
            logFpar(keyLogFpar(i,j))=log(aveC(j,i))+setZ(j)*fleetTimes(i)-log(setN(j));
          }
        } 
      }
    }
    cout<<endl<<logFpar<<endl;    
    logSdLogFsta=log(.5);
    logSdLogN=log(.7);
    logSdLogObs=log(.7);
    rec_loga=1;
    rec_logb=-12;
    // do vpa stuff if we later feel like it
    ofstream cpin("sam.pin");  
    PINTRACE(logFpar);
    PINTRACE(logQpow);
    PINTRACE(logSdLogFsta);
    PINTRACE(logSdLogN);
    PINTRACE(logSdLogObs);
    PINTRACE(rec_loga);
    PINTRACE(rec_logb);
    PINTRACE(rho);
    PINTRACE(logScale);
    PINTRACE(logScaleSSB);
    PINTRACE(logPowSSB);
    PINTRACE(logSdSSB);
    PINTRACE(U);
  
    ad_exit(1);
  }
  if(pinini==0){
    cout<<endl<<"using model.init"<<endl;
    logSdLogFsta=log(sqrt(varLogFstaInit)); 
    logSdLogN=log(sqrt(varLogNInit)); 
    logSdLogObs=log(sqrt(varLogObsInit)); 
    logFpar=logFparInit;
    rec_loga=rec_logaInit;
    rec_logb=rec_logbInit;
    //for(int i=1;i<= (maxSCBSurv-noCombs); ++i){
    //  alphaSCB(i)=alphaSCBInit(i);
    //}
    cout<<endl<<"done using model.init"<<endl;
  }
  if(noQpow>0){ 
    logQpow=0.0;
  }
  
  if(reducedRun==1){
    cout<<endl<<"reducedRun"<<endl;
    if((pinini==0)&&(fexists("sam.par"))){
      assignInit(logFpar); 
      assignInit(logQpow);
      assignInit(logSdLogFsta); 
      assignInit(logSdLogN); 
      assignInit(logSdLogObs); 
      assignInit(rec_loga);
      assignInit(rec_logb);
      assignInit(logScale); 
      assignInit(U); 
    }
    int redIdx;
    for(int i=retro.indexmin(); i<=retro.indexmax(); ++i){
      if(retro(i)==-1){
        for(int a=minAge; a<=maxAge; ++a){
          redIdx=keyLogFpar(i,a);
          if(redIdx!=0){
            logFpar(redIdx)=1.0;
          }
          redIdx=keyVarObs(i,a);
          if(redIdx!=0){
            logSdLogObs(redIdx)=1.0;
          }
        }
      }
      if((retro(i)>0)&&(fleetTypes(i)==0)){
        for(int y=1; y<=noScaledYears; ++y){
          if(keyScaledYears(y)>lastYearData(i)){
            for(int a=minAge; a<=maxAge; ++a){
              logScale(keyParScaledYA(y,a))=0;
            }
          }
        }
      }
    }  
  }
  
  //initialize the proportions
  //for(int y=1; y<=noYears-1; ++y){
  //  for(int c=1; c<=(noCombs-1); ++c){
  //    U(y*(stateDim+noCombs-1)-(noCombs-1)+c) = log(0.25);
  //  }
  //}
  
  //initialize the numbers
  //for(int y=1; y<=noYears-1; ++y){
  //  for(int c=minAge; c<=maxAge; ++c){
  //    U(y*(stateDim+noCombs-1)- (stateDim+noCombs -1) + c + 1) = log(exp(12.5) * (maxAge-c+1));
  //  }
  //}
  
  //initialize the fs
  //for(int y=1; y<=noYears-1; ++y){
  //  for(int c=1; c<=8; ++c){
  //    U(y*(stateDim+noCombs-1)- (maxAge+noCombs -1) + c) = log(0.05);
  //  }
  //}
  
  ofstream chkpin("sam.pinchk");
  PINCHKTRACE(logFpar); 
  PINCHKTRACE(logQpow);
  PINCHKTRACE(logSdLogFsta); 
  PINCHKTRACE(logSdLogN); 
  PINCHKTRACE(logSdLogObs); 
  PINCHKTRACE(rec_loga);
  PINCHKTRACE(rec_logb);
  PINCHKTRACE(rho);
  PINCHKTRACE(logScale);
  PINCHKTRACE(logScaleSSB);
  PINCHKTRACE(logPowSSB);
  PINCHKTRACE(logSdSSB);  
  PINCHKTRACE(U);
  
  cout<<"  ----  Done."<<endl; cout.flush();
}
  long int arrmblsize=0;

int main(int argc,char * argv[])
{
  ad_set_new_handler();
  ad_exit=&ad_boundf;
  cout << "SAM State-space Assessment Model" << endl;
  cout << "More info at: http://www.stockassessment.org" << endl;
  cout << "--------------------------------" << endl;
  cout << "$Rev$" << endl << "$LastChangedDate$"  <<endl << endl;
  arrmblsize=2000000;
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(150000);
  gradient_structure::set_CMPDIF_BUFFER_SIZE(800000);
  gradient_structure::set_MAX_NVAR_OFFSET(100000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);
    gradient_structure::set_NO_DERIVATIVES();
    gradient_structure::set_YES_SAVE_VARIABLES_VALUES();
      if (!arrmblsize) arrmblsize=150000;
    df1b2variable::noallocate=1;
    df1b2_parameters mp(arrmblsize,argc,argv);
    mp.iprint=10;

    function_minimizer::random_effects_flag=1;
    df1b2variable::noallocate=0;
    mp.preliminary_calculations();
    initial_df1b2params::separable_flag=1;
    mp.computations(argc,argv);
    return 0;
}

extern "C"  {
  void ad_boundf(int i)
  {
    /* so we can stop here */
    exit(i);
  }
}

model_data::~model_data()
{}

model_parameters::~model_parameters()
{}

void model_parameters::final_calcs(void){}

void model_parameters::set_runtime(void){}

#ifdef _BORLANDC_
  extern unsigned _stklen=10000U;
#endif


#ifdef __ZTC__
  extern unsigned int _stack=10000U;
#endif

void df1b2_parameters::user_function(void)
{
  jnll =0.0;
  time_t currentTime;
  time(& currentTime);
  if(difftime(currentTime,StartTime)>1800){ // Terminate after 30 minutes 
    cerr<<endl;
    cerr<<"############################################################"<<endl; 
    cerr<<"############################################################"<<endl; 
    cerr<<"############################################################"<<endl; 
    cerr<<"     MAX TIME ALLOWED EXCEEDED - MODEL DID NOT FINISH"<<endl;  
    cerr<<"############################################################"<<endl; 
    cerr<<"############################################################"<<endl; 
    cerr<<"############################################################"<<endl; 
    cerr<<endl;
    ad_exit(1);
  } 
  jnll=0.0;
  if(reducedRun==1){
    int redIdx;
    for(int i=retro.indexmin(); i<=retro.indexmax(); ++i){
      if(retro(i)==-1){
        for(int a=minAge; a<=maxAge; ++a){
          redIdx=keyLogFpar(i,a);
          if(redIdx!=0){
            jnll+=square(logFpar(redIdx)-1.0);
          }
          redIdx=keyVarObs(i,a);
          if(redIdx!=0){
            jnll+=square(logSdLogObs(redIdx)-1.0);
          }
          redIdx=keyQpow(i,a);
          if(redIdx!=0){
            jnll+=square(logQpow(redIdx)-0.0)*1.0e-6;
          }
        }
      }
      if((retro(i)>0)&&(fleetTypes(i)==0)){
        for(int y=1; y<=noScaledYears; ++y){
          if(keyScaledYears(y)>lastYearData(i)){
            for(int a=minAge; a<=maxAge; ++a){
              jnll+=square(logScale(keyParScaledYA(y,a))-0.0);
            }
          }
        }
      }
    }  
  }
  // First the scale parameter is applied where requested;
  scaledLogObs=logObs;
  if(noScaledYears>=1){
    int y, a;
    for(int yi=1; yi<=noScaledYears; ++yi){
      y=keyScaledYears(yi);
      for(int i=1; i<=noObs; ++i){
        a=(int)data(i,3);
        if((data(i,1)==y)&&(fleetTypes((int)data(i,2))<2)&&(keyParScaledYA(yi,a)>0)){
          scaledLogObs(i)+=logScale(keyParScaledYA(yi,a));
        }
      }
    }
  }
  for(int y=1; y<=noYears-1; ++y){
    if(LAI == 0){ step(y,U((y-1)*stateDim+1,y*stateDim),U(y*stateDim+1,(y+1)*stateDim),logFpar,rec_loga,rec_logb,logSdLogN,logSdLogFsta,rho);}
    if(LAI == 1){ step(y,U((y-1)*(stateDim+noCombs-1)+1,y*(stateDim+noCombs-1)-(noCombs-1)),U(y*(stateDim+noCombs-1)+1,(y+1)*(stateDim+noCombs-1)-(noCombs-1)),logFpar,rec_loga,rec_logb,logSdLogN,logSdLogFsta,rho);}
  }
  for(int y=1; y<=noYears; ++y){
    int idxlow=idx1(y);
    int idxhigh=idx2(y);
    dmatrix subData=getSubMat(data,idxlow,idxhigh);     
    df1b2vector subObs=getSubVec(scaledLogObs,idxlow,idxhigh);
    if(noQpow>0){
      if(LAI == 0){ obs(U((y-1)*stateDim+1,y*stateDim),                      subData,subObs,logFpar,logSdLogObs,logQpow,logScaleSSB,logPowSSB,logSdSSB);}
      //if(LAI == 1){ obs(U((y-1)*(stateDim+noCombs-1)+1,y*(stateDim+noCombs-1)),subData,subObs,logFpar,logSdLogObs,logQpow,logScaleSSB,logPowSSB,logSdSSB,alphaSCB);}
    }else{
      df1b2vector fakeQpow(1,1);
      if(LAI == 0){ obs(U((y-1)*stateDim+1,y*stateDim),                      subData,subObs,logFpar,logSdLogObs,fakeQpow,logScaleSSB,logPowSSB,logSdSSB);}
      //if(LAI == 1){ obs(U((y-1)*(stateDim+noCombs-1)+1,y*(stateDim+noCombs-1)),subData,subObs,logFpar,logSdLogObs,fakeQpow,logScaleSSB,logPowSSB,logSdSSB,alphaSCB);}
    }
  }
  if(sd_phase()){
    // for(int y=1; y<=noYears; ++y){
    //   for(int i=1; i<=stateDim; ++i){
    //     X(y,i)=U((y-1)*stateDim+i);
    //   }
    //   ssb(y)=SSB(X(y),propMature(y),stockMeanWeight(y),Fprop(y),Mprop(y),natMor(y));
    //   logssb(y)=log(ssb(y));
    //   tsb(y)=TSB(X(y),stockMeanWeight(y));
    //   logtsb(y)=log(tsb(y));
    //   fbar(y)=FBAR(X(y),fbarRange(1),fbarRange(2));
    //   logfbar(y)=log(fbar(y));
    //   int idxlow=idx1(y);
    //   int idxhigh=idx2(y);
    //   dmatrix subData=getSubMat(data,idxlow,idxhigh);     
    //   df1b2vector subObs=getSubVec(scaledLogObs,idxlow,idxhigh);
    //   if((y>=minYearResFleet)&&(y<=maxYearResFleet)){
    //     logCatch(y)=log(CATCH(X(y),natMor(y),catchMeanWeight(y)));
    //   }
    // }    
  }
}

void   df1b2_pre_parameters::step(const int y, const funnel_init_df1b2vector& u1,const funnel_init_df1b2vector& u2, const funnel_init_df1b2vector& logFpar, const funnel_init_df1b2variable& rec_loga, const funnel_init_df1b2variable& rec_logb, const funnel_init_df1b2vector& logSdLogN, const funnel_init_df1b2vector& logSdLogFsta, const funnel_init_df1b2variable& rho)
{
  begin_df1b2_funnel();
  //y = years
  //U = states U((y-1)*stateDim+1,y*stateDim),U(y*stateDim+1,(y+1)*stateDim),logFpar,rec_loga,rec_logb,logSdLogN,logSdLogFsta,rho
  //u1 = states of this year
  //u2 = states next year
  //from u1 we make a prediction to next year so we can compare u2 with prediction
  df1b2vector x1(1,stateDim);   
  int n1=u1.indexmin();
  int n2=u1.indexmax();
  for(int i=n1; i<=n2; ++i){
    x1(i-n1+1)=u1(i);
  }
  df1b2vector x2(1,stateDim);   
  n1=u2.indexmin();
  n2=u2.indexmax();
  for(int i=n1; i<=n2; ++i){
    x2(i-n1+1)=u2(i);
  }
  df1b2vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x1(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  df1b2vector pred(1,stateDim);   
  pred=x1;
  df1b2variable ssb=0.0; 
  for(int a=minAge; a<=maxAge; ++a){
    ssb+=exp(x1(a-minAge+1))*exp(-Ftot(a)*Fprop(y,a)-natMor(y,a)*Mprop(y,a))*propMature(y,a)*stockMeanWeight(y,a);
  }
  if(stockRecruitmentModelCode==0){
    // do nothing 
  }else{
    if(stockRecruitmentModelCode==1){//ricker
      pred(minAge-minAge+1)=rec_loga+log(ssb)-exp(rec_logb)*ssb; 
    }else{
      if(stockRecruitmentModelCode==2){//BH
        pred(minAge-minAge+1)=rec_loga+log(ssb)-log(1+exp(rec_logb)*ssb); 
      }else{
        cerr<<"SR model code not recognized"<<endl;
      }
    }
  }
  for(int a=minAge+1; a<=maxAge; ++a){
    pred(a-minAge+1)=x1(a-1-minAge+1)-Ftot(a-1)-natMor(y,a-1); 
  }
  
  if(maxAgePlusGroup==1){
    pred(maxAge-minAge+1)=log(exp(x1(maxAge-1-minAge+1)-Ftot(maxAge-1)-natMor(y,maxAge-1))+
                              exp(x1(maxAge-minAge+1)-Ftot(maxAge)-natMor(y,maxAge))); 
  }
  
  df1b2vector var(1,stateDim);
  for(int a=minAge; a<=maxAge; ++a){
    var(a-minAge+1)=exp(2.0*logSdLogN(keyVarLogN(a))); 
  }
  int idx; 
  for(int f=1; f<=noFleets; ++f){
    for(int a=minAge; a<=maxAge; ++a){
      idx=keyLogFsta(f,a); 
      if(idx>0){
        idx+=maxAge-minAge+1; 
        var(idx)=exp(2.0*logSdLogFsta(keyVarF(f,a))); 
      } 
    }
  }
  if(corFlag==0){ // not correlated 
    for(int i=1; i<=stateDim; ++i){
      jnll+=0.5*(log(2.0*M_PI*var(i))+square(x2(i)-pred(i))/var(i));
    } 
  }else{          // correlated
    for(int i=1; i<=(maxAge-minAge+1); ++i){
      jnll+=0.5*(log(2.0*M_PI*var(i))+square(x2(i)-pred(i))/var(i));
    } 
    int Fdim=stateDim-(maxAge-minAge+1);
    df1b2matrix fvar(maxAge-minAge+2,stateDim,maxAge-minAge+2,stateDim);
    df1b2matrix fcor(maxAge-minAge+2,stateDim,maxAge-minAge+2,stateDim);
    df1b2vector fsd(maxAge-minAge+2,stateDim);
    fvar.initialize();
    if(corFlag==1){  // symmetrical correlation
      for(int i=(maxAge-minAge+2); i<=stateDim; ++i){
        for(int j=(maxAge-minAge+2); j<=stateDim; ++j){
          if(i!=j){fcor(i,j)=rho;}else{fcor(i,j)=1.0;}
        }
      }
    } else { // AR1 correlation
      for(int i=(maxAge-minAge+2); i<=stateDim; ++i){
        for(int j=(maxAge-minAge+2); j<=stateDim; ++j){
          if(i!=j){fcor(i,j)=pow(rho,abs(i-j));}else{fcor(i,j)=1.0;}
        }
      }
    }
    for(int i=(maxAge-minAge+2); i<=stateDim; ++i){
      //fvar(i,i)=var(i);
      fsd(i)=sqrt(var(i));
      //fcor(i,i)=1.0;
    }
    fvar=elem_prod(outer_prod(fsd,fsd),fcor);
    //cout<<fcor<<endl<<endl;
    jnll+=nLogNormal(pred((maxAge-minAge+2),stateDim),x2(maxAge-minAge+2,stateDim),fvar);
  } 
  end_df1b2_funnel();
}

void   df1b2_pre_parameters::obs(const funnel_init_df1b2vector& u, const dmatrix& data, const funnel_init_df1b2vector& obs, const funnel_init_df1b2vector& logFpar, const funnel_init_df1b2vector& logSdLogObs, const funnel_init_df1b2vector& logQpow, const funnel_init_df1b2variable& logScaleSSB, const funnel_init_df1b2variable& logPowSSB, const funnel_init_df1b2variable& logSdSSB)
{
  begin_df1b2_funnel();
  int n1=data.rowmin();
  int n2=data.rowmax();
  df1b2vector pred(n1,n2);
  df1b2vector x(1,stateDim+noCombs-1);
  int xn1=u.indexmin();
  int xn2=u.indexmax();
  //cout << " u.indexmin " << xn1 << " u.indexmax " << xn2 << endl << cout.flush();
  
  for(int i=xn1; i<=xn2; ++i){
    x(i-xn1+1)=u(i);
  }
  
  //cout << " x initilized " << endl << cout.flush();
  
  // Get probs per unit into vector
  df1b2vector props(1,noCombs);
  if(LAI == 1){
    int counter = 2;
    for(int i=(xn1+stateDim); i<=xn2; ++i){
      props(counter)=u(i);
      counter += 1;
    }
  }
  //cout << " props initilized " << endl << cout.flush();
  // Get numSurv into vector
  df1b2vector survNum(1,maxSCBSurv);
  if(LAI == 1){
    for(int i=1; i<=maxSCBSurv; ++i){
      survNum(i)=combSurv(i);
    }
  }
  //cout << " survNum initilized " << endl << cout.flush();
  // Normalize probs and make them amount to 1 (fill 4th parameter)
  df1b2variable totProp = 0.0;
  df1b2vector props_norm(1,noCombs);
  if(LAI == 1){
    for(int i=2; i<=noCombs; ++i){
      totProp += exp(props(i));
    }
    for(int i=2; i<=noCombs; ++i){
      props_norm(i) = exp(props(i)) / (1+totProp);
    }
    props_norm(1) = 1 - totProp / (1+totProp);
  }
  //cout << " props_norm " << props_norm << endl << cout.flush();
  //cout << " props_norm initilized " << endl << cout.flush();
  // Normalize probs_alpha and make them amount to 1
  df1b2vector alphaSCB_norm(1,maxSCBSurv);
  df1b2vector aSCB;
  if(LAI == 1){
    for(int i=1; i<=noCombs; ++i){
      df1b2variable totProp_alpha = 0.0;
      int idxmin=0; int idxmax=0;
      for(int k=1; k<=maxSCBSurv; ++k){
        if(value(survNum(k))==value(i) && idxmin==0){ idxmin=k;}
        if(value(survNum(k))==value(i)){ idxmax=k;}
      }
      for(int j=(idxmin+1); j<=idxmax; ++j){
        // Substract i because aSCB is only 7 long but I'm estimating 11 valus
        totProp_alpha += exp(aSCB(j-i));
      }
      for(int j=(idxmin+1); j<=idxmax; ++j){
        alphaSCB_norm(j) = exp(aSCB(j-i)) / (1+totProp_alpha);
      }
      alphaSCB_norm(idxmin) = 1 - totProp_alpha / (1+totProp_alpha);
    }
  }
  //cout << " sum alpha " << sum(alphaSCB_norm) << endl << cout.flush();
  //cout << " alphaSCB_norm initilized " << endl << cout.flush();
  //cout << " alphaSCB_norm" << alphaSCB_norm << endl << cout.flush();
  
  df1b2vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  //cout << " Ftot initilized " << endl << cout.flush();
  df1b2vector Z(Ftot.indexmin(),Ftot.indexmax());
  Z=Ftot; // missing M here  
  int isMadded=0; 
  int f;
  int ft;
  int a; 
  int y, yIdx; 
  for(int i=n1; i<=n2; ++i){
    f=(int)data(i,2);
    ft=fleetTypes(f);
    a=(int)data(i,3);
    y=(int)data(i,1);
    if(isMadded==0){
      for(int aa=minAge; aa<=maxAge; ++aa){Z(aa)+=natMor((int)(y-years(1)+1),aa);} // added M here
      isMadded=1; 
    }
    if(ft==0){// residual fleet
      pred(i)=x(a-minAge+1)-log(Z(a))+log(1-exp(-Z(a)));
      if(keyLogFsta(f,a)>0){
        pred(i)+=x(keyLogFsta(f,a)+maxAge-minAge+1);
      }
      if(keyLogFpar(f,a)>0){
        pred(i)+=logFpar(keyLogFpar(f,a));
      }
    }else{
      if(ft==1){// comm fleet
        cerr<<"Not implemented yet!!!"<<endl;  
        ad_exit(1);
      }else{
        if(ft==2){// survey
          pred(i)=x(a-minAge+1)-Z(a)*fleetTimes(f);
          if(keyQpow(f,a)>0){
            pred(i)*=exp(logQpow(keyQpow(f,a))); 
          }
          if(keyLogFsta(f,a)>0){
            pred(i)+=x(keyLogFsta(f,a)+maxAge-minAge+1);
          }
          if(keyLogFpar(f,a)>0){
            pred(i)+=logFpar(keyLogFpar(f,a));
          }
        }else{
          if(ft==3){// SSB survey
            df1b2variable ssb=0.0; 
            for(int aa=minAge; aa<=maxAge; ++aa){
              yIdx=(int)(y-years(1)+1); 
              ssb+=exp(x(aa-minAge+1))*exp(-Ftot(aa)*Fprop(yIdx,aa)-natMor(yIdx,aa)*Mprop(yIdx,aa))*propMature(yIdx,aa)*stockMeanWeight(yIdx,aa);
            }
            pred(i)=log(ssb)+logScaleSSB;
          }else{
            if(ft==4){
              df1b2variable ssb=0.0; 
              for(int aa=minAge; aa<=maxAge; ++aa){
                yIdx=(int)(y-years(1)+1); 
                ssb+=exp(x(aa-minAge+1))*exp(-Ftot(aa)*Fprop(yIdx,aa)-natMor(yIdx,aa)*Mprop(yIdx,aa))*propMature(yIdx,aa)*stockMeanWeight(yIdx,aa);
              }
              pred(i)=exp(logPowSSB)*log(ssb)+logScaleSSB;
            } else {
                if(ft==5){ // LAI indices
                  //Option to cancel out LAI estimation
                  if(LAI == 1){
                    df1b2variable ssb=0.0;
                    for(int aa=minAge; aa<=maxAge; ++aa){
                      yIdx=(int)(y-years(1)+1);
                      ssb+=exp(x(aa-minAge+1))*exp(-Ftot(aa)*Fprop(yIdx,aa)-natMor(yIdx,aa)*Mprop(yIdx,aa))*propMature(yIdx,aa)*stockMeanWeight(yIdx,aa);
                    }
                    pred(i) = logScaleSSB + log(ssb) + log(props_norm(value(survNum(a)))) + log(alphaSCB_norm(a));
                    //cout << logScaleSSB << " " << log(ssb) << " " << log(props_norm(value(survNum(a)))) << " "  << log(alphaSCB_norm(a)) << endl; cout.flush();
                    //cout << " pred(i) " << pred(i) << endl; cout.flush();
                  }
                }
              }
          }
        } 
      }
    }      
  } 
  df1b2variable var=0.0;
  for(int i=n1; i<=n2; ++i){
    f=(int)data(i,2);
    a=(int)data(i,3);
    if((retro(f)==-1)||((int)data(i,1)>lastYearData(f))){
      residuals(i,1)=data(i,1);
      residuals(i,2)=f;
      residuals(i,3)=a;
      residuals(i,4)=0;     
      residuals(i,5)=0;     
      residuals(i,6)=0;     
    }else{
      if((fleetTypes(f)==3)||(fleetTypes(f)==4)){
        var=exp(2.0*logSdSSB); 
      }else{
        if(fleetTypes(f)==5){
          //Option to cancel out LAI estimation
          if(LAI == 1){
            var=exp(2.0*logSdLogObs(keyVarObs(f,value(survNum(a))-1)));
          }
        }else{
            var=exp(2.0*logSdLogObs(keyVarObs(f,a)));
        }
      }
      //Option to cancel out LAI estimation
      if(LAI == 1){
        jnll+=0.5*(log(2.0*M_PI*var)+square(obs(i)-pred(i))/var);
      } else {
        if(fleetTypes(f)<=4){
          jnll+=0.5*(log(2.0*M_PI*var)+square(obs(i)-pred(i))/var);
        }
      }
      //if(fleetTypes(f) == 5){
      //  cout << " obs(i) " << obs(i) << " pred(i) " << pred(i) << endl << cout.flush();
      //}
      cout << " jnll " << jnll << endl << cout.flush();
      
      residuals(i,1)=data(i,1);
      residuals(i,2)=f;
      residuals(i,3)=a;
      residuals(i,4)=value(obs(i));     
      residuals(i,5)=value(pred(i));     
      residuals(i,6)=value((obs(i)-pred(i))/sqrt(var));     
    }
  } 
  
  end_df1b2_funnel();
}

df1b2variable df1b2_parameters::SSB(df1b2vector x, dvector p, dvector w, dvector fprop, dvector mprop, dvector M)
{
  df1b2vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  df1b2variable ret;
  ret=0; 
  for(int a=minAge; a<=maxAge; ++a){
    ret+=exp(x(a-minAge+1))*exp(-Ftot(a)*fprop(a)-M(a)*mprop(a))*p(a)*w(a);
  }
  return ret;
}

df1b2variable df1b2_parameters::CATCH(df1b2vector x, dvector M, dvector w)
{
  df1b2variable ret; 
  ret=0;
  df1b2vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  df1b2vector Z(minAge,maxAge);
  Z=Ftot+M; 
  df1b2vector logCatch(minAge,maxAge); 
  for(int a=minAge; a<=maxAge; ++a){
    logCatch(a)=x(a-minAge+1)-log(Z(a))+log(1-exp(-Z(a)))+log(Ftot(a));
  }
  ret=sum(elem_prod(exp(logCatch),w));
  return ret;
  
}

df1b2variable df1b2_parameters::TSB(df1b2vector x, dvector w)
{
  df1b2variable ret;
  ret=0; 
  for(int a=minAge; a<=maxAge; ++a){
    ret+=exp(x(a-minAge+1))*w(a);
  }
  return ret;
}

df1b2variable df1b2_parameters::FBAR(df1b2vector x, int from, int to)
{
  df1b2variable ret;
  df1b2vector Ftot(minAge,maxAge);  
  Ftot.initialize();
  for(int f=1; f<=noFleets; ++f){
    if(fleetTypes(f)<2){ // means that it is not a survey
      for(int a=minAge; a<=maxAge; ++a){
        if(keyLogFsta(f,a)>0){
          Ftot(a)+=exp(x(keyLogFsta(f,a)+maxAge-minAge+1));
        }
        if(keyLogFpar(f,a)>0){
          Ftot(a)+=exp(logFpar(keyLogFpar(f,a)));
        }
      }
    }
  }
  ret=mean(Ftot(from,to));
  return ret;
}
   
void df1b2_pre_parameters::setup_quadprior_calcs(void) 
{ 
  df1b2_gradlist::set_no_derivatives(); 
  quadratic_prior::in_qp_calculations=1; 
}  
  
void df1b2_pre_parameters::begin_df1b2_funnel(void) 
{ 
  (*re_objective_function_value::pobjfun)=0; 
  other_separable_stuff_begin(); 
  f1b2gradlist->reset();  
  if (!quadratic_prior::in_qp_calculations) 
  { 
    df1b2_gradlist::set_yes_derivatives();  
  } 
  funnel_init_var::allocate_all();  
}  
 
void df1b2_pre_parameters::end_df1b2_funnel(void) 
{  
  lapprox->do_separable_stuff(); 
  other_separable_stuff_end(); 
} 
  
void model_parameters::begin_df1b2_funnel(void) 
{ 
  if (lapprox)  
  {  
    {  
      begin_funnel_stuff();  
    }  
  }  
}  
 
void model_parameters::end_df1b2_funnel(void) 
{  
  if (lapprox)  
  {  
    end_df1b2_funnel_stuff();  
  }  
} 

void df1b2_parameters::allocate(void) 
{
 cout<<"PARAMETERSECTION"; cout.flush(); 
  logFpar.allocate(1,noLogFpar,"logFpar");
  logQpow.allocate(1,noQpow,"logQpow");
  logSdLogFsta.allocate(1,noVarF,-3,3,"logSdLogFsta");
  logSdLogN.allocate(1,noVarLogN,-3,3,"logSdLogN");
  logSdLogObs.allocate(1,noVarObs,-3,3,"logSdLogObs");
 int rec_phase=1; 
 if(stockRecruitmentModelCode==0){rec_phase=-1;}
  rec_loga.allocate(rec_phase,"rec_loga");
  rec_logb.allocate(rec_phase,"rec_logb");
 int cor_phase=1;
 if(corFlag==0){cor_phase=-1;}
  rho.allocate(0.01,0.99,cor_phase,"rho");
  logScale.allocate(1,noScaledPar,"logScale");
  logScaleSSB.allocate("logScaleSSB");
  #ifndef NO_AD_INITIALIZE
  logScaleSSB.initialize();
  #endif
 logScaleSSB=0.0;
  logPowSSB.allocate("logPowSSB");
  #ifndef NO_AD_INITIALIZE
  logPowSSB.initialize();
  #endif
 logPowSSB=0.0;
  logSdSSB.allocate("logSdSSB");
  #ifndef NO_AD_INITIALIZE
  logSdSSB.initialize();
  #endif
 logSdSSB=0.0;
  scaledLogObs.allocate(1,noObs,"scaledLogObs");
  #ifndef NO_AD_INITIALIZE
    scaledLogObs.initialize();
  #endif
  X.allocate(1,noYears,1,stateDim,"X");
  #ifndef NO_AD_INITIALIZE
    X.initialize();
  #endif
  U.allocate(1,stateDim*noYears,"U");
  prior_function_value.allocate("prior_function_value");
  likelihood_function_value.allocate("likelihood_function_value");
  jnll.allocate("jnll");  /* ADOBJECTIVEFUNCTION */
 cout<<"  ---  Done."<<endl; cout.flush(); 
}
