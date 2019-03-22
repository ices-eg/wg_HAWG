D:
cd "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\"
del /Q mdDir\*.*
del /Q retro\*.*
del /Q mcmc*.*
del /Q *.?0?
del /Q *.out
del /Q *.mcm
del /Q *.bin
del /Q admodel.*
del /Q *.csv
del /Q *.std
del /Q *.bar
del /Q *.mc2
del /Q *.cor
del /Q *.psv
del /Q *.ecm
del /Q *.xls
del /Q *.html
del /Q mcout*.all
del /Q *.wmf
del /Q *.png
del /Q *.lg
del /Q *.log
del /Q ud.dat
del /Q HCR_prob.dat
del /Q HCR_yield.dat
del /Q HCR_SSB.dat
del /Q *.par
del /Q *.rep
del /Q *.hst
del /Q *.eva
del /Q *.tmp
del /Q *.txt
D:
cd "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\"
del /f "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\sms.rep 
del /f "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\sms.par 
del /f "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\sms.std 
sms  -nox -ind run_oldcanumTestms0.dat 
copy /Y "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\sms.par" "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\run_oldcanumTestms0.par" 
copy /Y "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\sms.rep" "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\run_oldcanumTestms0.rep" 
copy /Y "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\sms.std" "D:\Sprat\Sprat_assessment\SMS_2019\Sprat-div-4_plus_IIIa\run_oldcanumTestms0.std" 
